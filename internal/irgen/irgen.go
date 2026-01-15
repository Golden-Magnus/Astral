package irgen

import (
	"astral/internal/ast"
	"astral/internal/ir"
	"fmt"
)

type IRGenerator struct {
	program     *ir.Program
	functions   map[string]*ir.Function
	currentFunc *ir.Function
	labelCount  int
	tempCount   int
	locals      map[string]string // variable name -> temp/register name
}

func New() *IRGenerator {
	return &IRGenerator{
		program:   &ir.Program{Functions: []*ir.Function{}, Data: []*ir.Data{}},
		functions: make(map[string]*ir.Function),
		locals:    make(map[string]string),
	}
}

func (g *IRGenerator) Generate(astProgram *ast.Program) (*ir.Program, error) {
	// First pass: collect functions and structs
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
			// Top-level statements go into main
			if err := g.genTopLevelStatement(stmt); err != nil {
				return nil, err
			}
		}
	}

	// Ensure main function exists
	if _, exists := g.functions["main"]; !exists {
		mainFunc := &ir.Function{
			Name:   "main",
			Params: []*ir.Param{},
			Blocks: []*ir.BasicBlock{},
			Locals: make(map[string]*ir.Local),
		}
		mainFunc.Blocks = append(mainFunc.Blocks, &ir.BasicBlock{
			Label:  "main_entry",
			Instrs: []ir.Instruction{},
		})
		g.functions["main"] = mainFunc
		g.program.Functions = append(g.program.Functions, mainFunc)
	}

	return g.program, nil
}

func (g *IRGenerator) genFunctionDecl(fn *ast.FunctionStatement) {
	params := []*ir.Param{}
	for _, p := range fn.Parameters {
		paramType := "int64"
		if p.Type != nil {
			paramType = g.mapType(p.Type.Value)
		}
		params = append(params, &ir.Param{
			Name: p.Name.Value,
			Type: paramType,
		})
	}

	returnType := ""
	if fn.ReturnType != nil {
		returnType = g.mapType(fn.ReturnType.Value)
	}

	irFunc := &ir.Function{
		Name:       fn.Name.Value,
		Params:     params,
		ReturnType: returnType,
		Blocks:     []*ir.BasicBlock{},
		Locals:     make(map[string]*ir.Local),
	}

	// Create entry block
	entryBlock := &ir.BasicBlock{
		Label:  fn.Name.Value + "_entry",
		Instrs: []ir.Instruction{},
	}
	irFunc.Blocks = append(irFunc.Blocks, entryBlock)

	g.functions[fn.Name.Value] = irFunc
	g.program.Functions = append(g.program.Functions, irFunc)
}

func (g *IRGenerator) genFunctionBody(fn *ast.FunctionStatement) error {
	irFunc := g.functions[fn.Name.Value]
	g.currentFunc = irFunc
	g.locals = make(map[string]string)

	// Map parameters to locals
	for _, param := range irFunc.Params {
		g.locals[param.Name] = param.Name
		irFunc.Locals[param.Name] = &ir.Local{
			Name: param.Name,
			Type: param.Type,
		}
	}

	// Generate body
	currentBlock := irFunc.Blocks[0]
	for _, stmt := range fn.Body.Statements {
		block, err := g.genStatement(stmt, currentBlock)
		if err != nil {
			return err
		}
		if block != nil {
			currentBlock = block
		}
	}

	return nil
}

func (g *IRGenerator) genTopLevelStatement(stmt ast.Statement) error {
	// Add to main function
	mainFunc, exists := g.functions["main"]
	if !exists {
		mainFunc = &ir.Function{
			Name:   "main",
			Params: []*ir.Param{},
			Blocks: []*ir.BasicBlock{},
			Locals: make(map[string]*ir.Local),
		}
		mainFunc.Blocks = append(mainFunc.Blocks, &ir.BasicBlock{
			Label:  "main_entry",
			Instrs: []ir.Instruction{},
		})
		g.functions["main"] = mainFunc
		g.program.Functions = append(g.program.Functions, mainFunc)
	}

	g.currentFunc = mainFunc
	g.locals = make(map[string]string)

	currentBlock := mainFunc.Blocks[0]
	block, err := g.genStatement(stmt, currentBlock)
	if err != nil {
		return err
	}
	if block != nil {
		currentBlock = block
	}

	return nil
}

func (g *IRGenerator) genStatement(stmt ast.Statement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	switch s := stmt.(type) {
	case *ast.LetStatement:
		return g.genLetStatement(s, block)
	case *ast.ExpressionStatement:
		return g.genExpressionStatement(s, block)
	case *ast.IfStatement:
		return g.genIfStatement(s, block)
	case *ast.WhileStatement:
		return g.genWhileStatement(s, block)
	case *ast.ForStatement:
		return g.genForStatement(s, block)
	case *ast.ReturnStatement:
		return g.genReturnStatement(s, block)
	default:
		return block, nil
	}
}

func (g *IRGenerator) genLetStatement(stmt *ast.LetStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	// Allocate local
	varName := stmt.Name.Value
	varType := "int64"
	if stmt.Type != nil {
		varType = g.mapType(stmt.Type.Value)
	}

	temp := g.newTemp()
	g.locals[varName] = temp
	g.currentFunc.Locals[temp] = &ir.Local{
		Name: temp,
		Type: varType,
	}

	// Generate value expression
	valueTemp, err := g.genExpression(stmt.Value, block)
	if err != nil {
		return nil, err
	}

	// Move value to variable
	block.Instrs = append(block.Instrs, &ir.Move{
		Dest: temp,
		Src:  valueTemp,
	})

	return block, nil
}

func (g *IRGenerator) genExpressionStatement(stmt *ast.ExpressionStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	_, err := g.genExpression(stmt.Expression, block)
	return block, err
}

func (g *IRGenerator) genIfStatement(stmt *ast.IfStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	condTemp, err := g.genExpression(stmt.Condition, block)
	if err != nil {
		return nil, err
	}

	thenLabel := g.newLabel("then")
	elseLabel := g.newLabel("else")
	mergeLabel := g.newLabel("merge")

	// Jump instruction
	block.Instrs = append(block.Instrs, &ir.JumpIf{
		Cond: condTemp,
		Then: thenLabel,
		Else: elseLabel,
	})

	// Then block
	thenBlock := &ir.BasicBlock{
		Label:  thenLabel,
		Instrs: []ir.Instruction{},
	}
	currentBlock := thenBlock
	for _, s := range stmt.Consequence.Statements {
		block, err := g.genStatement(s, currentBlock)
		if err != nil {
			return nil, err
		}
		if block != nil {
			currentBlock = block
		}
	}
	thenBlock.Instrs = append(thenBlock.Instrs, &ir.Jump{Label: mergeLabel})
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, thenBlock)

	// Else block (if exists)
	if stmt.Alternative != nil {
		elseBlock := &ir.BasicBlock{
			Label:  elseLabel,
			Instrs: []ir.Instruction{},
		}
		currentBlock = elseBlock
		for _, s := range stmt.Alternative.Statements {
			block, err := g.genStatement(s, currentBlock)
			if err != nil {
				return nil, err
			}
			if block != nil {
				currentBlock = block
			}
		}
		elseBlock.Instrs = append(elseBlock.Instrs, &ir.Jump{Label: mergeLabel})
		g.currentFunc.Blocks = append(g.currentFunc.Blocks, elseBlock)
	} else {
		// No else, jump directly to merge
		elseBlock := &ir.BasicBlock{
			Label:  elseLabel,
			Instrs: []ir.Instruction{&ir.Jump{Label: mergeLabel}},
		}
		g.currentFunc.Blocks = append(g.currentFunc.Blocks, elseBlock)
	}

	// Merge block
	mergeBlock := &ir.BasicBlock{
		Label:  mergeLabel,
		Instrs: []ir.Instruction{},
	}
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, mergeBlock)

	return mergeBlock, nil
}

func (g *IRGenerator) genWhileStatement(stmt *ast.WhileStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	loopLabel := g.newLabel("loop")
	bodyLabel := g.newLabel("body")
	exitLabel := g.newLabel("exit")

	// Jump to loop start
	block.Instrs = append(block.Instrs, &ir.Jump{Label: loopLabel})

	// Loop condition block
	loopBlock := &ir.BasicBlock{
		Label:  loopLabel,
		Instrs: []ir.Instruction{},
	}
	condTemp, err := g.genExpression(stmt.Condition, loopBlock)
	if err != nil {
		return nil, err
	}
	loopBlock.Instrs = append(loopBlock.Instrs, &ir.JumpIf{
		Cond: condTemp,
		Then: bodyLabel,
		Else: exitLabel,
	})
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, loopBlock)

	// Body block
	bodyBlock := &ir.BasicBlock{
		Label:  bodyLabel,
		Instrs: []ir.Instruction{},
	}
	currentBlock := bodyBlock
	for _, s := range stmt.Body.Statements {
		block, err := g.genStatement(s, currentBlock)
		if err != nil {
			return nil, err
		}
		if block != nil {
			currentBlock = block
		}
	}
	bodyBlock.Instrs = append(bodyBlock.Instrs, &ir.Jump{Label: loopLabel})
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, bodyBlock)

	// Exit block
	exitBlock := &ir.BasicBlock{
		Label:  exitLabel,
		Instrs: []ir.Instruction{},
	}
	g.currentFunc.Blocks = append(g.currentFunc.Blocks, exitBlock)

	return exitBlock, nil
}

func (g *IRGenerator) genForStatement(stmt *ast.ForStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	// Similar to while, but with iterator
	// Simplified implementation
	return g.genWhileStatement(&ast.WhileStatement{
		Token:     stmt.Token,
		Condition: &ast.Boolean{Value: true}, // Placeholder
		Body:      stmt.Body,
	}, block)
}

func (g *IRGenerator) genReturnStatement(stmt *ast.ReturnStatement, block *ir.BasicBlock) (*ir.BasicBlock, error) {
	if stmt.ReturnValue != nil {
		valTemp, err := g.genExpression(stmt.ReturnValue, block)
		if err != nil {
			return nil, err
		}
		block.Instrs = append(block.Instrs, &ir.Return{Val: valTemp})
	} else {
		block.Instrs = append(block.Instrs, &ir.Return{})
	}
	return block, nil
}

func (g *IRGenerator) genExpression(exp ast.Expression, block *ir.BasicBlock) (string, error) {
	switch e := exp.(type) {
	case *ast.IntegerLiteral:
		temp := g.newTemp()
		block.Instrs = append(block.Instrs, &ir.ConstInt{
			Dest: temp,
			Val:  e.Value,
		})
		return temp, nil

	case *ast.FloatLiteral:
		temp := g.newTemp()
		block.Instrs = append(block.Instrs, &ir.ConstFloat{
			Dest: temp,
			Val:  e.Value,
		})
		return temp, nil

	case *ast.StringLiteral:
		// Store string in data section
		label := g.newLabel("str")
		g.program.Data = append(g.program.Data, &ir.Data{
			Label: label,
			Type:  "string",
			Value: e.Value,
		})
		temp := g.newTemp()
		block.Instrs = append(block.Instrs, &ir.Load{
			Dest: temp,
			Addr: label,
		})
		return temp, nil

	case *ast.Boolean:
		temp := g.newTemp()
		block.Instrs = append(block.Instrs, &ir.ConstBool{
			Dest: temp,
			Val:  e.Value,
		})
		return temp, nil

	case *ast.Identifier:
		if temp, ok := g.locals[e.Value]; ok {
			return temp, nil
		}
		return "", fmt.Errorf("undefined variable: %s", e.Value)

	case *ast.PrefixExpression:
		rightTemp, err := g.genExpression(e.Right, block)
		if err != nil {
			return "", err
		}
		temp := g.newTemp()
		switch e.Operator {
		case "-":
			block.Instrs = append(block.Instrs, &ir.Neg{
				Dest: temp,
				Val:  rightTemp,
			})
		case "!":
			block.Instrs = append(block.Instrs, &ir.Not{
				Dest: temp,
				Val:  rightTemp,
			})
		}
		return temp, nil

	case *ast.InfixExpression:
		leftTemp, err := g.genExpression(e.Left, block)
		if err != nil {
			return "", err
		}
		rightTemp, err := g.genExpression(e.Right, block)
		if err != nil {
			return "", err
		}
		temp := g.newTemp()

		switch e.Operator {
		case "+":
			block.Instrs = append(block.Instrs, &ir.Add{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "-":
			block.Instrs = append(block.Instrs, &ir.Sub{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "*":
			block.Instrs = append(block.Instrs, &ir.Mul{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "/":
			block.Instrs = append(block.Instrs, &ir.Div{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "%":
			block.Instrs = append(block.Instrs, &ir.Mod{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "==":
			block.Instrs = append(block.Instrs, &ir.Eq{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "!=":
			block.Instrs = append(block.Instrs, &ir.Ne{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "<":
			block.Instrs = append(block.Instrs, &ir.Lt{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case ">":
			block.Instrs = append(block.Instrs, &ir.Gt{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "and":
			block.Instrs = append(block.Instrs, &ir.And{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		case "or":
			block.Instrs = append(block.Instrs, &ir.Or{
				Dest: temp, Left: leftTemp, Right: rightTemp,
			})
		}
		return temp, nil

	case *ast.CallExpression:
		args := []string{}
		for _, arg := range e.Arguments {
			argTemp, err := g.genExpression(arg, block)
			if err != nil {
				return "", err
			}
			args = append(args, argTemp)
		}

		funcName := ""
		if ident, ok := e.Function.(*ast.Identifier); ok {
			funcName = ident.Value
		}

		// Built-in functions don't return values in temp
		if funcName == "print" {
			block.Instrs = append(block.Instrs, &ir.Call{
				Func: "print",
				Args: args,
			})
			return "", nil
		}

		temp := g.newTemp()
		block.Instrs = append(block.Instrs, &ir.Call{
			Dest: temp,
			Func: funcName,
			Args: args,
		})
		return temp, nil

	case *ast.AssignExpression:
		rightTemp, err := g.genExpression(e.Right, block)
		if err != nil {
			return "", err
		}
		if ident, ok := e.Left.(*ast.Identifier); ok {
			if temp, ok := g.locals[ident.Value]; ok {
				block.Instrs = append(block.Instrs, &ir.Move{
					Dest: temp,
					Src:  rightTemp,
				})
				return temp, nil
			}
			return "", fmt.Errorf("undefined variable: %s", ident.Value)
		}
		return "", fmt.Errorf("invalid assignment target")

	default:
		return "", fmt.Errorf("unsupported expression type")
	}
}

func (g *IRGenerator) newTemp() string {
	temp := fmt.Sprintf("t%d", g.tempCount)
	g.tempCount++
	return temp
}

func (g *IRGenerator) newLabel(prefix string) string {
	label := fmt.Sprintf("%s_%d", prefix, g.labelCount)
	g.labelCount++
	return label
}

func (g *IRGenerator) mapType(astType string) string {
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
