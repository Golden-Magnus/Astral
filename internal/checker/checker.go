package checker

import (
	"fmt"
	"astral/internal/ast"
)

type Checker struct {
	errors      []string
	symbols     map[string]*Symbol
	functions   map[string]*ast.FunctionStatement
	structs     map[string]*ast.StructStatement
	currentFunc string // để check return type
	inLoop      bool   // để check break/continue nếu thêm sau
}

type Symbol struct {
	Name    string
	Type    string
	Mutable bool
}

func New() *Checker {
	return &Checker{
		symbols:   make(map[string]*Symbol),
		functions: make(map[string]*ast.FunctionStatement),
		structs:   make(map[string]*ast.StructStatement),
	}
}

func (c *Checker) Error(msg string) {
	c.errors = append(c.errors, msg)
}

func (c *Checker) ErrorWithLocation(msg string, line, column int) {
	c.errors = append(c.errors, fmt.Sprintf("%d:%d: %s", line, column, msg))
}

func (c *Checker) Check(program *ast.Program) []string {
	// First pass: collect structs and functions
	for _, stmt := range program.Statements {
		switch s := stmt.(type) {
		case *ast.StructStatement:
			if _, exists := c.structs[s.Name.Value]; exists {
				c.Error(fmt.Sprintf("struct %s already declared", s.Name.Value))
			}
			c.structs[s.Name.Value] = s
		case *ast.FunctionStatement:
			if _, exists := c.functions[s.Name.Value]; exists {
				c.Error(fmt.Sprintf("function %s already declared", s.Name.Value))
			}
			c.functions[s.Name.Value] = s
		}
	}

	// Second pass: check body
	for _, stmt := range program.Statements {
		c.checkStatement(stmt)
	}

	return c.errors
}

func (c *Checker) checkStatement(stmt ast.Statement) {
	switch s := stmt.(type) {
	case *ast.LetStatement:
		c.checkLetStatement(s)
	case *ast.ExpressionStatement:
		if assign, ok := s.Expression.(*ast.AssignExpression); ok {
			c.checkAssignExpression(assign)
		} else {
			c.checkExpression(s.Expression)
		}
	case *ast.IfStatement:
		c.checkExpression(s.Condition)
		c.checkBlock(s.Consequence)
		if s.Alternative != nil {
			c.checkBlock(s.Alternative)
		}
	case *ast.WhileStatement:
		c.checkExpression(s.Condition)
		oldLoop := c.inLoop
		c.inLoop = true
		c.checkBlock(s.Body)
		c.inLoop = oldLoop
	case *ast.ForStatement:
		c.checkExpression(s.Iterable)
		c.symbols[s.VarName.Value] = &Symbol{
			Name:    s.VarName.Value,
			Type:    s.VarType.Value,
			Mutable: true,
		}
		oldLoop := c.inLoop
		c.inLoop = true
		c.checkBlock(s.Body)
		c.inLoop = oldLoop
	case *ast.FunctionStatement:
		c.currentFunc = s.Name.Value
		// Add parameters to scope
		for _, param := range s.Parameters {
			c.symbols[param.Name.Value] = &Symbol{
				Name:    param.Name.Value,
				Type:    param.Type.Value,
				Mutable: true,
			}
		}
		c.checkBlock(s.Body)
		c.currentFunc = ""
	case *ast.ReturnStatement:
		if c.currentFunc == "" {
			c.Error("return outside function")
			return
		}
		// Get function return type
		fn, exists := c.functions[c.currentFunc]
		if !exists {
			return
		}
		if s.ReturnValue != nil {
			c.checkExpression(s.ReturnValue)
			returnType := c.inferType(s.ReturnValue)
			if fn.ReturnType != nil {
				expectedType := fn.ReturnType.Value
				if returnType != "unknown" && !c.isTypeCompatible(expectedType, returnType) {
					c.Error(fmt.Sprintf("return type mismatch: expected %s, got %s", expectedType, returnType))
				}
			}
		} else {
			// No return value
			if fn.ReturnType != nil {
				c.Error(fmt.Sprintf("function %s expects return value of type %s", c.currentFunc, fn.ReturnType.Value))
			}
		}
	case *ast.BreakStatement:
		if !c.inLoop {
			c.Error("break outside loop")
		}
	case *ast.ContinueStatement:
		if !c.inLoop {
			c.Error("continue outside loop")
		}
	case *ast.BlockStatement:
		c.checkBlock(s)
	}
}

func (c *Checker) checkBlock(block *ast.BlockStatement) {
	for _, stmt := range block.Statements {
		c.checkStatement(stmt)
	}
}

func (c *Checker) checkLetStatement(ls *ast.LetStatement) {
	if _, exists := c.symbols[ls.Name.Value]; exists {
		c.Error(fmt.Sprintf("identifier %s already declared in this scope", ls.Name.Value))
		return
	}

	var declaredType string
	if ls.Type != nil {
		declaredType = ls.Type.Value
	}

	inferredType := c.inferType(ls.Value)

	// Check type compatibility
	if declaredType != "" && inferredType != "unknown" && !c.isTypeCompatible(declaredType, inferredType) {
		c.Error(fmt.Sprintf("type mismatch in let %s: declared %s, got %s", ls.Name.Value, declaredType, inferredType))
	}

	useType := declaredType
	if useType == "" {
		useType = inferredType
	}

	c.symbols[ls.Name.Value] = &Symbol{
		Name:    ls.Name.Value,
		Type:    useType,
		Mutable: ls.Mutable,
	}
}

func (c *Checker) checkAssignExpression(assign *ast.AssignExpression) {
	if ident, ok := assign.Left.(*ast.Identifier); ok {
		if sym, exists := c.symbols[ident.Value]; exists {
			if !sym.Mutable {
				c.Error(fmt.Sprintf("cannot assign to const variable %s", ident.Value))
			}
		} else {
			c.Error(fmt.Sprintf("undefined variable %s in assignment", ident.Value))
		}
	}
	c.checkExpression(assign.Left)
	c.checkExpression(assign.Right)
}

func (c *Checker) checkExpression(exp ast.Expression) {
	switch e := exp.(type) {
	case *ast.Identifier:
		if _, exists := c.symbols[e.Value]; !exists {
			c.Error(fmt.Sprintf("undefined identifier %s", e.Value))
		}
	case *ast.CallExpression:
		if ident, ok := e.Function.(*ast.Identifier); ok {
			if ident.Value == "print" || ident.Value == "range" || ident.Value == "read" {
				// Built-in functions
				for _, arg := range e.Arguments {
					c.checkExpression(arg)
				}
				return
			}
			if _, exists := c.functions[ident.Value]; !exists {
				c.Error(fmt.Sprintf("call to undefined function %s", ident.Value))
			}
		}
		for _, arg := range e.Arguments {
			c.checkExpression(arg)
		}
	case *ast.PrefixExpression:
		c.checkExpression(e.Right)
	case *ast.InfixExpression:
		c.checkExpression(e.Left)
		c.checkExpression(e.Right)
	case *ast.IndexExpression:
		c.checkExpression(e.Left)
		c.checkExpression(e.Index)
	case *ast.StructLiteral:
		if _, exists := c.structs[e.Name.Value]; !exists {
			c.Error(fmt.Sprintf("undefined struct type %s", e.Name.Value))
		}
	}
}

func (c *Checker) inferType(exp ast.Expression) string {
	switch e := exp.(type) {
	case *ast.IntegerLiteral:
		return "int"
	case *ast.StringLiteral:
		return "string"
	case *ast.Boolean:
		return "bool"
	case *ast.Identifier:
		if sym, ok := c.symbols[e.Value]; ok {
			return sym.Type
		}
	case *ast.CallExpression:
		if ident, ok := e.Function.(*ast.Identifier); ok {
			if fn, exists := c.functions[ident.Value]; exists && fn.ReturnType != nil {
				return fn.ReturnType.Value
			}
		}
	}
	return "unknown"
}

func (c *Checker) isTypeCompatible(declaredType, inferredType string) bool {
	// Exact match
	if declaredType == inferredType {
		return true
	}
	// Integer types are compatible with each other
	intTypes := map[string]bool{"i32": true, "i64": true, "int": true}
	if intTypes[declaredType] && intTypes[inferredType] {
		return true
	}
	return false
}