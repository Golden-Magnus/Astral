package safety

import (
	"astral/internal/ast"
	"fmt"
)

// OwnershipChecker - Đảm bảo memory safety mà không cần GC
// Approach đơn giản: scope-based + automatic move/copy

type OwnershipChecker struct {
	errors      []string
	symbols     map[string]*VariableState
	scopes      []*Scope
	currentScope *Scope
}

type VariableState struct {
	Name      string
	Type      string
	Mutable   bool
	Moved     bool   // Đã bị move chưa
	Scope     *Scope // Scope chứa biến này
	LastUsed  int    // Instruction index cuối cùng sử dụng
}

type Scope struct {
	Parent   *Scope
	Variables map[string]*VariableState
	Depth    int
}

func NewOwnershipChecker() *OwnershipChecker {
	rootScope := &Scope{
		Parent:    nil,
		Variables: make(map[string]*VariableState),
		Depth:     0,
	}
	return &OwnershipChecker{
		errors:       []string{},
		symbols:      make(map[string]*VariableState),
		scopes:       []*Scope{rootScope},
		currentScope: rootScope,
	}
}

func (oc *OwnershipChecker) Check(program *ast.Program) []string {
	// Check all statements
	for _, stmt := range program.Statements {
		oc.checkStatement(stmt)
	}
	
	return oc.errors
}

func (oc *OwnershipChecker) Error(msg string) {
	oc.errors = append(oc.errors, msg)
}

// checkStatement checks a statement for ownership violations
func (oc *OwnershipChecker) checkStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.LetStatement:
		oc.checkLetStatement(s)
	case *ast.ExpressionStatement:
		if assign, ok := s.Expression.(*ast.AssignExpression); ok {
			oc.checkAssignExpression(assign)
		} else {
			oc.checkExpression(s.Expression)
		}
	case *ast.IfStatement:
		oc.checkIfStatement(s)
	case *ast.WhileStatement:
		oc.checkWhileStatement(s)
	case *ast.ForStatement:
		oc.checkForStatement(s)
	case *ast.FunctionStatement:
		oc.checkFunctionStatement(s)
	case *ast.ReturnStatement:
		oc.checkReturnStatement(s)
	case *ast.BlockStatement:
		oc.enterScope()
		for _, stmt := range s.Statements {
			oc.checkStatement(stmt)
		}
		oc.exitScope()
	}
}

func (oc *OwnershipChecker) checkLetStatement(ls *ast.LetStatement) {
	// Check if value expression uses moved variables
	oc.checkExpression(ls.Value)
	
	// Mark new variable as owned
	varType := "int"
	if ls.Type != nil {
		varType = ls.Type.Value
	}
	
	state := &VariableState{
		Name:    ls.Name.Value,
		Type:    varType,
		Mutable: ls.Mutable,
		Moved:   false,
		Scope:   oc.currentScope,
	}
	oc.currentScope.Variables[ls.Name.Value] = state
	oc.symbols[ls.Name.Value] = state
}

func (oc *OwnershipChecker) checkAssignExpression(assign *ast.AssignExpression) {
	if ident, ok := assign.Left.(*ast.Identifier); ok {
		// Check if variable was moved
		if state, exists := oc.symbols[ident.Value]; exists {
			if state.Moved {
				oc.Error(fmt.Sprintf("cannot assign to moved variable: %s", ident.Value))
				return
			}
			if !state.Mutable {
				oc.Error(fmt.Sprintf("cannot assign to immutable variable: %s", ident.Value))
				return
			}
		}
	}
	
	oc.checkExpression(assign.Left)
	oc.checkExpression(assign.Right)
	
	// If assigning a large type, mark source as moved
	oc.markMoveIfLarge(assign.Right)
}

func (oc *OwnershipChecker) checkExpression(exp ast.Expression) {
	switch e := exp.(type) {
	case *ast.Identifier:
		oc.checkIdentifier(e)
	case *ast.CallExpression:
		oc.checkCallExpression(e)
	case *ast.InfixExpression:
		oc.checkExpression(e.Left)
		oc.checkExpression(e.Right)
	case *ast.PrefixExpression:
		oc.checkExpression(e.Right)
	case *ast.IndexExpression:
		// Index access is read-only, doesn't move
		oc.checkExpression(e.Left)
		oc.checkExpression(e.Index)
	}
}

func (oc *OwnershipChecker) checkIdentifier(ident *ast.Identifier) {
	if state, exists := oc.symbols[ident.Value]; exists {
		if state.Moved {
			oc.Error(fmt.Sprintf("use of moved value: %s", ident.Value))
		}
	}
}

func (oc *OwnershipChecker) checkCallExpression(call *ast.CallExpression) {
	// Check all arguments first
	for _, arg := range call.Arguments {
		oc.checkExpression(arg)
	}
	
	// Check if it's a built-in that doesn't take ownership (like print)
	if ident, ok := call.Function.(*ast.Identifier); ok {
		readOnlyBuiltins := map[string]bool{
			"print": true, "strlen": true,
		}
		if readOnlyBuiltins[ident.Value] {
			// Read-only operations don't move
			return
		}
	}
	
	// Other function calls: assume they may take ownership for large types
	// Move semantics for arrays/structs to avoid expensive copies
	for _, arg := range call.Arguments {
		oc.markMoveIfLarge(arg)
	}
}

func (oc *OwnershipChecker) markMoveIfLarge(exp ast.Expression) {
	// Heuristic: if expression is a variable of large type, mark as moved
	if ident, ok := exp.(*ast.Identifier); ok {
		if state, exists := oc.symbols[ident.Value]; exists {
			if oc.isLargeType(state.Type) {
				state.Moved = true
			}
		}
	}
}

func (oc *OwnershipChecker) isLargeType(typ string) bool {
	// Primitive types are small (copy semantics)
	smallTypes := map[string]bool{
		"int": true, "i32": true, "i64": true,
		"float": true, "f64": true,
		"bool": true,
	}
	
	// Everything else is large (move semantics)
	return !smallTypes[typ]
}

func (oc *OwnershipChecker) checkIfStatement(is *ast.IfStatement) {
	oc.checkExpression(is.Condition)
	
	oc.checkBlock(is.Consequence)
	if is.Alternative != nil {
		oc.checkBlock(is.Alternative)
	}
}

func (oc *OwnershipChecker) checkWhileStatement(ws *ast.WhileStatement) {
	oc.checkExpression(ws.Condition)
	oc.checkBlock(ws.Body)
}

func (oc *OwnershipChecker) checkForStatement(fs *ast.ForStatement) {
	oc.checkExpression(fs.Iterable)
	oc.enterScope()
	
	// Add loop variable
	loopVar := &VariableState{
		Name:    fs.VarName.Value,
		Type:    fs.VarType.Value,
		Mutable: true,
		Moved:   false,
		Scope:   oc.currentScope,
	}
	oc.currentScope.Variables[fs.VarName.Value] = loopVar
	oc.symbols[fs.VarName.Value] = loopVar
	
	oc.checkBlock(fs.Body)
	oc.exitScope()
}

func (oc *OwnershipChecker) checkFunctionStatement(fs *ast.FunctionStatement) {
	oc.enterScope()
	
	// Add parameters
	for _, param := range fs.Parameters {
		paramType := "int"
		if param.Type != nil {
			paramType = param.Type.Value
		}
		paramState := &VariableState{
			Name:    param.Name.Value,
			Type:    paramType,
			Mutable: true,
			Moved:   false,
			Scope:   oc.currentScope,
		}
		oc.currentScope.Variables[param.Name.Value] = paramState
		oc.symbols[param.Name.Value] = paramState
	}
	
	oc.checkBlock(fs.Body)
	oc.exitScope()
}

func (oc *OwnershipChecker) checkReturnStatement(rs *ast.ReturnStatement) {
	if rs.ReturnValue != nil {
		oc.checkExpression(rs.ReturnValue)
		// Return value is moved out - OK, no need to mark as error
	}
}

func (oc *OwnershipChecker) checkBlock(block *ast.BlockStatement) {
	oc.enterScope()
	for _, stmt := range block.Statements {
		oc.checkStatement(stmt)
	}
	oc.exitScope()
}

func (oc *OwnershipChecker) enterScope() {
	newScope := &Scope{
		Parent:    oc.currentScope,
		Variables: make(map[string]*VariableState),
		Depth:     oc.currentScope.Depth + 1,
	}
	oc.scopes = append(oc.scopes, newScope)
	oc.currentScope = newScope
}

func (oc *OwnershipChecker) exitScope() {
	if oc.currentScope.Parent != nil {
		// Remove variables from symbols when exiting scope
		for name, state := range oc.currentScope.Variables {
			if state.Scope == oc.currentScope {
				delete(oc.symbols, name)
			}
		}
		oc.currentScope = oc.currentScope.Parent
	}
}
