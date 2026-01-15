package ssagen

import (
	"astral/internal/ast"
	"astral/internal/ir"
	"fmt"
)

// SSA Generator - Convert AST to SSA IR
type SSAGenerator struct {
	program      *ir.SSAProgram
	functions    map[string]*ir.SSAFunction
	currentFunc  *ir.SSAFunction
	currentBlock *ir.SSABasicBlock
	ssaCounter   int // Counter for SSA IDs
	labelCount   int
	blocks       map[string]*ir.SSABasicBlock // Label -> Block
}

func New() *SSAGenerator {
	return &SSAGenerator{
		program:   &ir.SSAProgram{Functions: []*ir.SSAFunction{}, Data: []*ir.Data{}},
		functions: make(map[string]*ir.SSAFunction),
		blocks:    make(map[string]*ir.SSABasicBlock),
	}
}

func (g *SSAGenerator) Generate(astProgram *ast.Program) (*ir.SSAProgram, error) {
	// First pass: collect functions
	for _, stmt := range astProgram.Statements {
		switch s := stmt.(type) {
		case *ast.FunctionStatement:
			g.genFunctionDecl(s)
		}
	}

	// Second pass: generate function bodies
	for _, stmt := range astProgram.Statements {
		switch s := stmt.(type) {
		case *ast.FunctionStatement:
			if err := g.genFunctionBody(s); err != nil {
				return nil, err
			}
		case *ast.LetStatement, *ast.ExpressionStatement, *ast.IfStatement,
			*ast.WhileStatement, *ast.ForStatement, *ast.ReturnStatement:
			if err := g.genTopLevelStatement(stmt); err != nil {
				return nil, err
			}
		}
	}

	// Ensure main exists
	if _, exists := g.functions["main"]; !exists {
		mainFunc := &ir.SSAFunction{
			Name:   "main",
			Params: []*ir.SSAParam{},
			Blocks: []*ir.SSABasicBlock{},
			Locals: make(map[string]*ir.SSALocal),
		}
		entryBlock := &ir.SSABasicBlock{
			Label:    "main_entry",
			Instrs:   []ir.SSAInstruction{},
			Preds:    []*ir.SSABasicBlock{},
			Succs:    []*ir.SSABasicBlock{},
			PhiNodes: []*ir.SSAPhi{},
		}
		mainFunc.Blocks = append(mainFunc.Blocks, entryBlock)
		g.functions["main"] = mainFunc
		g.program.Functions = append(g.program.Functions, mainFunc)
	}

	return g.program, nil
}

func (g *SSAGenerator) genFunctionDecl(fn *ast.FunctionStatement) {
	params := []*ir.SSAParam{}
	for _, p := range fn.Parameters {
		paramType := "int64"
		if p.Type != nil {
			paramType = g.mapType(p.Type.Value)
		}
		params = append(params, &ir.SSAParam{
			Name: p.Name.Value,
			Type: paramType,
			ID:   g.newSSAID(),
		})
	}

	returnType := ""
	if fn.ReturnType != nil {
		returnType = g.mapType(fn.ReturnType.Value)
	}

	ssaFunc := &ir.SSAFunction{
		Name:       fn.Name.Value,
		Params:     params,
		ReturnType: returnType,
		Blocks:     []*ir.SSABasicBlock{},
		Locals:     make(map[string]*ir.SSALocal),
	}

	entryBlock := &ir.SSABasicBlock{
		Label:    fn.Name.Value + "_entry",
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}
	ssaFunc.Blocks = append(ssaFunc.Blocks, entryBlock)
	g.blocks[entryBlock.Label] = entryBlock

	g.functions[fn.Name.Value] = ssaFunc
	g.program.Functions = append(g.program.Functions, ssaFunc)
}

func (g *SSAGenerator) genFunctionBody(fn *ast.FunctionStatement) error {
	g.currentFunc = g.functions[fn.Name.Value]
	g.currentBlock = g.currentFunc.Blocks[0]
	g.ssaCounter = len(g.currentFunc.Params) // Start after params

	// Map parameters to locals
	for _, param := range g.currentFunc.Params {
		g.currentFunc.Locals[param.Name] = &ir.SSALocal{
			Name: param.Name,
			Type: param.Type,
			ID:   param.ID,
		}
	}

	// Generate body
	for _, stmt := range fn.Body.Statements {
		if err := g.genStatement(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (g *SSAGenerator) genTopLevelStatement(stmt ast.Statement) error {
	mainFunc, exists := g.functions["main"]
	if !exists {
		mainFunc = &ir.SSAFunction{
			Name:   "main",
			Params: []*ir.SSAParam{},
			Blocks: []*ir.SSABasicBlock{},
			Locals: make(map[string]*ir.SSALocal),
		}
		entryBlock := &ir.SSABasicBlock{
			Label:    "main_entry",
			Instrs:   []ir.SSAInstruction{},
			Preds:    []*ir.SSABasicBlock{},
			Succs:    []*ir.SSABasicBlock{},
			PhiNodes: []*ir.SSAPhi{},
		}
		mainFunc.Blocks = append(mainFunc.Blocks, entryBlock)
		g.functions["main"] = mainFunc
		g.program.Functions = append(g.program.Functions, mainFunc)
	}

	g.currentFunc = mainFunc
	g.currentBlock = mainFunc.Blocks[0]

	return g.genStatement(stmt)
}

func (g *SSAGenerator) genStatement(stmt ast.Statement) error {
	switch s := stmt.(type) {
	case *ast.LetStatement:
		return g.genLetStatement(s)
	case *ast.ExpressionStatement:
		return g.genExpressionStatement(s)
	case *ast.IfStatement:
		return g.genIfStatement(s)
	case *ast.WhileStatement:
		return g.genWhileStatement(s)
	case *ast.ReturnStatement:
		return g.genReturnStatement(s)
	}
	return nil
}

func (g *SSAGenerator) genLetStatement(stmt *ast.LetStatement) error {
	varType := "int64"
	if stmt.Type != nil {
		varType = g.mapType(stmt.Type.Value)
	}

	// Generate value expression
	valueID, err := g.genExpression(stmt.Value)
	if err != nil {
		return err
	}

	// Create new SSA ID for variable
	ssaID := g.newSSAID()
	g.currentFunc.Locals[stmt.Name.Value] = &ir.SSALocal{
		Name: stmt.Name.Value,
		Type: varType,
		ID:   ssaID,
	}

	// Move value to variable (in SSA, this is just assigning the ID)
	// For simplicity, we'll use a move instruction
	g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAStore{
		To:   ssaID,
		From: valueID,
	})

	return nil
}

func (g *SSAGenerator) genExpressionStatement(stmt *ast.ExpressionStatement) error {
	_, err := g.genExpression(stmt.Expression)
	return err
}

func (g *SSAGenerator) genIfStatement(stmt *ast.IfStatement) error {
	condID, err := g.genExpression(stmt.Condition)
	if err != nil {
		return err
	}

	thenLabel := g.newLabel("then")
	elseLabel := g.newLabel("else")
	mergeLabel := g.newLabel("merge")

	// Create blocks
	thenBlock := &ir.SSABasicBlock{
		Label:    thenLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{g.currentBlock},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}
	elseBlock := &ir.SSABasicBlock{
		Label:    elseLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{g.currentBlock},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}
	mergeBlock := &ir.SSABasicBlock{
		Label:    mergeLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{thenBlock, elseBlock},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}

	g.blocks[thenLabel] = thenBlock
	g.blocks[elseLabel] = elseBlock
	g.blocks[mergeLabel] = mergeBlock

	// Add conditional jump
	g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSACondJump{
		Cond: condID,
		Then: thenLabel,
		Else: elseLabel,
	})
	g.currentBlock.Succs = []*ir.SSABasicBlock{thenBlock, elseBlock}

	// Generate then block
	g.currentBlock = thenBlock
	for _, s := range stmt.Consequence.Statements {
		if err := g.genStatement(s); err != nil {
			return err
		}
	}
	thenBlock.Instrs = append(thenBlock.Instrs, &ir.SSAJump{Target: mergeLabel})
	thenBlock.Succs = []*ir.SSABasicBlock{mergeBlock}

	// Generate else block
	g.currentBlock = elseBlock
	if stmt.Alternative != nil {
		for _, s := range stmt.Alternative.Statements {
			if err := g.genStatement(s); err != nil {
				return err
			}
		}
	}
	elseBlock.Instrs = append(elseBlock.Instrs, &ir.SSAJump{Target: mergeLabel})
	elseBlock.Succs = []*ir.SSABasicBlock{mergeBlock}

	// Continue with merge block
	g.currentBlock = mergeBlock
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, thenBlock, elseBlock, mergeBlock)

	return nil
}

func (g *SSAGenerator) genWhileStatement(stmt *ast.WhileStatement) error {
	loopLabel := g.newLabel("loop")
	bodyLabel := g.newLabel("body")
	exitLabel := g.newLabel("exit")

	loopBlock := &ir.SSABasicBlock{
		Label:    loopLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{g.currentBlock},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}
	bodyBlock := &ir.SSABasicBlock{
		Label:    bodyLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{loopBlock},
		Succs:    []*ir.SSABasicBlock{loopBlock},
		PhiNodes: []*ir.SSAPhi{},
	}
	exitBlock := &ir.SSABasicBlock{
		Label:    exitLabel,
		Instrs:   []ir.SSAInstruction{},
		Preds:    []*ir.SSABasicBlock{loopBlock},
		Succs:    []*ir.SSABasicBlock{},
		PhiNodes: []*ir.SSAPhi{},
	}

	g.blocks[loopLabel] = loopBlock
	g.blocks[bodyLabel] = bodyBlock
	g.blocks[exitLabel] = exitBlock

	// Jump to loop
	g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAJump{Target: loopLabel})
	g.currentBlock.Succs = []*ir.SSABasicBlock{loopBlock}

	// Generate condition in loop block
	g.currentBlock = loopBlock
	condID, err := g.genExpression(stmt.Condition)
	if err != nil {
		return err
	}
	loopBlock.Instrs = append(loopBlock.Instrs, &ir.SSACondJump{
		Cond: condID,
		Then: bodyLabel,
		Else: exitLabel,
	})
	loopBlock.Succs = []*ir.SSABasicBlock{bodyBlock, exitBlock}

	// Generate body
	g.currentBlock = bodyBlock
	for _, s := range stmt.Body.Statements {
		if err := g.genStatement(s); err != nil {
			return err
		}
	}
	bodyBlock.Instrs = append(bodyBlock.Instrs, &ir.SSAJump{Target: loopLabel})

	// Continue with exit block
	g.currentBlock = exitBlock
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, loopBlock, bodyBlock, exitBlock)

	return nil
}

func (g *SSAGenerator) genReturnStatement(stmt *ast.ReturnStatement) error {
	if stmt.ReturnValue != nil {
		valID, err := g.genExpression(stmt.ReturnValue)
		if err != nil {
			return err
		}
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAReturn{Val: valID})
	} else {
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAReturn{Val: -1})
	}
	return nil
}

func (g *SSAGenerator) genExpression(exp ast.Expression) (int, error) {
	switch e := exp.(type) {
	case *ast.IntegerLiteral:
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAConstInt{
			Dest: ssaID,
			Val:  e.Value,
		})
		return ssaID, nil

	case *ast.FloatLiteral:
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAConstFloat{
			Dest: ssaID,
			Val:  e.Value,
		})
		return ssaID, nil

	case *ast.StringLiteral:
		label := g.newLabel("str")
		g.program.Data = append(g.program.Data, &ir.Data{
			Label: label,
			Type:  "string",
			Value: e.Value,
		})
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAConstString{
			Dest:  ssaID,
			Val:   e.Value,
			Label: label,
		})
		return ssaID, nil

	case *ast.Boolean:
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAConstBool{
			Dest: ssaID,
			Val:  e.Value,
		})
		return ssaID, nil

	case *ast.Identifier:
		if local, ok := g.currentFunc.Locals[e.Value]; ok {
			return local.ID, nil
		}
		return -1, fmt.Errorf("undefined variable: %s", e.Value)

	case *ast.InfixExpression:
		leftID, err := g.genExpression(e.Left)
		if err != nil {
			return -1, err
		}
		rightID, err := g.genExpression(e.Right)
		if err != nil {
			return -1, err
		}
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSABinOp{
			Dest:  ssaID,
			Op:    e.Operator,
			Left:  leftID,
			Right: rightID,
		})
		return ssaID, nil

	case *ast.PrefixExpression:
		valID, err := g.genExpression(e.Right)
		if err != nil {
			return -1, err
		}
		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSAUnOp{
			Dest: ssaID,
			Op:   e.Operator,
			Val:  valID,
		})
		return ssaID, nil

	case *ast.CallExpression:
		args := []int{}
		for _, arg := range e.Arguments {
			argID, err := g.genExpression(arg)
			if err != nil {
				return -1, err
			}
			args = append(args, argID)
		}

		funcName := ""
		if ident, ok := e.Function.(*ast.Identifier); ok {
			funcName = ident.Value
		}

		if funcName == "print" {
			// Void call
			g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSACall{
				Dest: -1,
				Func: "print",
				Args: args,
			})
			return -1, nil
		}

		ssaID := g.newSSAID()
		g.currentBlock.Instrs = append(g.currentBlock.Instrs, &ir.SSACall{
			Dest: ssaID,
			Func: funcName,
			Args: args,
		})
		return ssaID, nil

	default:
		return -1, fmt.Errorf("unsupported expression type")
	}
}

func (g *SSAGenerator) newSSAID() int {
	id := g.ssaCounter
	g.ssaCounter++
	return id
}

func (g *SSAGenerator) newLabel(prefix string) string {
	label := fmt.Sprintf("%s_%d", prefix, g.labelCount)
	g.labelCount++
	return label
}

func (g *SSAGenerator) mapType(astType string) string {
	switch astType {
	case "int", "i32", "i64":
		return "int64"
	case "f64":
		return "float64"
	case "string":
		return "string"
	case "bool":
		return "bool"
	default:
		return "int64"
	}
}
