package parser

import (
	"fmt"
	"strconv"
	"astral/internal/ast"
	"astral/internal/lexer"
)

const (
	_ int = iota
	LOWEST
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
	INDEX
)

var precedences = map[lexer.TokenType]int{
	lexer.EQ:       EQUALS,
	lexer.NOT_EQ:   EQUALS,
	lexer.ASSIGN:   EQUALS,
	lexer.AND:      EQUALS,
	lexer.OR:       EQUALS,
	lexer.LT:       LESSGREATER,
	lexer.GT:       LESSGREATER,
	lexer.PLUS:     SUM,
	lexer.MINUS:    SUM,
	lexer.SLASH:    PRODUCT,
	lexer.ASTERISK: PRODUCT,
	lexer.MOD:      PRODUCT,
	lexer.LPAREN:   CALL,
	lexer.LBRACKET: INDEX,
}

type Parser struct {
	l              *lexer.Lexer
	curToken       lexer.Token
	peekToken      lexer.Token
	errors         []string
	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

type prefixParseFn func() ast.Expression
type infixParseFn  func(ast.Expression) ast.Expression

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:              l,
		prefixParseFns: make(map[lexer.TokenType]prefixParseFn),
		infixParseFns:  make(map[lexer.TokenType]infixParseFn),
	}

	// Register prefix
	p.prefixParseFns[lexer.IDENT]    = p.parseIdentifier
	p.prefixParseFns[lexer.INT]      = p.parseIntegerLiteral
	p.prefixParseFns[lexer.FLOAT]    = p.parseFloatLiteral
	p.prefixParseFns[lexer.STRING]   = p.parseStringLiteral
	p.prefixParseFns[lexer.TRUE]     = p.parseBoolean
	p.prefixParseFns[lexer.FALSE]    = p.parseBoolean
	p.prefixParseFns[lexer.BANG]     = p.parsePrefixExpression
	p.prefixParseFns[lexer.MINUS]    = p.parsePrefixExpression
	p.prefixParseFns[lexer.LPAREN]   = p.parseGroupedExpression
	p.prefixParseFns[lexer.IF]       = p.parseIfExpression
	p.prefixParseFns[lexer.FUNCTION] = p.parseFunctionLiteral
	p.prefixParseFns[lexer.LBRACKET] = p.parseArrayLiteral
	p.prefixParseFns[lexer.VAR]      = p.parseVarExpression
	// allow `print` keyword to act like an identifier for function calls
	p.prefixParseFns[lexer.PRINT] = p.parseIdentifier

	// Register infix
	p.infixParseFns[lexer.PLUS]     = p.parseInfixExpression
	p.infixParseFns[lexer.MINUS]    = p.parseInfixExpression
	p.infixParseFns[lexer.SLASH]    = p.parseInfixExpression
	p.infixParseFns[lexer.ASTERISK] = p.parseInfixExpression
	p.infixParseFns[lexer.MOD]      = p.parseInfixExpression
	p.infixParseFns[lexer.ASSIGN]   = p.parseAssignExpression
	p.infixParseFns[lexer.EQ]       = p.parseInfixExpression
	p.infixParseFns[lexer.NOT_EQ]   = p.parseInfixExpression
	p.infixParseFns[lexer.LT]       = p.parseInfixExpression
	p.infixParseFns[lexer.GT]       = p.parseInfixExpression
	p.infixParseFns[lexer.AND]      = p.parseInfixExpression
	p.infixParseFns[lexer.OR]       = p.parseInfixExpression
	p.infixParseFns[lexer.LPAREN]   = p.parseCallExpression
	p.infixParseFns[lexer.LBRACKET] = p.parseIndexExpression

	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != lexer.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case lexer.LET:
		return p.parseLetStatement()
	case lexer.MUT:
		return p.parseMutStatement()
	case lexer.RETURN:
		return p.parseReturnStatement()
	case lexer.IF:
		return p.parseIfStatement()
	case lexer.WHILE:
		return p.parseWhileStatement()
	case lexer.FOR:
		return p.parseForStatement()
	case lexer.BREAK:
		return &ast.BreakStatement{Token: p.curToken}
	case lexer.CONTINUE:
		return &ast.ContinueStatement{Token: p.curToken}
	case lexer.FUNCTION:
		return p.parseFunctionStatement()
	case lexer.STRUCT:
		return p.parseStructStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}
	stmt.Mutable = false

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// First identifier is the type (let string name = ...)
	stmt.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Second identifier is the name
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.ASSIGN) {
		return nil
	}

	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)

	// Check if value is var() call - if so, mark as array
	if isVarCall(stmt.Value) {
		stmt.IsArray = true
	}

	if p.peekToken.Type == lexer.SEMICOLON {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseMutStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}
	stmt.Mutable = true

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// First identifier is the type (mut string name = ...)
	stmt.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Second identifier is the name
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.ASSIGN) {
		return nil
	}

	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)

	// Check if value is var() call - if so, mark as array
	if isVarCall(stmt.Value) {
		stmt.IsArray = true
	}

	if p.peekToken.Type == lexer.SEMICOLON {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}

	p.nextToken()

	stmt.ReturnValue = p.parseExpression(LOWEST)

	if p.peekToken.Type == lexer.SEMICOLON {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseIfStatement() *ast.IfStatement {
	stmt := &ast.IfStatement{Token: p.curToken}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Consequence = p.parseBlockStatement()

	if p.peekToken.Type == lexer.ELSE {
		p.nextToken()

		if !p.expectPeek(lexer.LBRACE) {
			return nil
		}

		stmt.Alternative = p.parseBlockStatement()
	}

	return stmt
}

func (p *Parser) parseWhileStatement() *ast.WhileStatement {
	stmt := &ast.WhileStatement{Token: p.curToken}

	p.nextToken()
	stmt.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseForStatement() *ast.ForStatement {
	stmt := &ast.ForStatement{Token: p.curToken}

	// Expect: for TYPE IDENT in EXPR { ... }
	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Get type
	stmt.VarType = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	// Get variable name
	stmt.VarName = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.IN) {
		return nil
	}

	p.nextToken()
	stmt.Iterable = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseFunctionStatement() *ast.FunctionStatement {
	stmt := &ast.FunctionStatement{Token: p.curToken}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	stmt.Parameters = p.parseFunctionParameters()

	if p.peekToken.Type == lexer.COLON {
		p.nextToken()
		if !p.expectPeek(lexer.IDENT) {
			return nil
		}
		stmt.ReturnType = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Body = p.parseBlockStatement()

	return stmt
}

func (p *Parser) parseStructStatement() *ast.StructStatement {
	stmt := &ast.StructStatement{Token: p.curToken}

	if !p.expectPeek(lexer.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	stmt.Fields = p.parseStructFields()

	return stmt
}

func (p *Parser) parseStructFields() []*ast.StructField {
	fields := []*ast.StructField{}

	if p.peekToken.Type == lexer.RBRACE {
		p.nextToken()
		return fields
	}

	p.nextToken()
	field := &ast.StructField{
		Name: &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
	}
	if p.peekToken.Type == lexer.COLON {
		p.nextToken()
		p.nextToken()
		field.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	}
	fields = append(fields, field)

	for p.peekToken.Type == lexer.COMMA {
		p.nextToken()
		p.nextToken()
		field = &ast.StructField{
			Name: &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
		}
		if p.peekToken.Type == lexer.COLON {
			p.nextToken()
			p.nextToken()
			field.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		}
		fields = append(fields, field)
	}

	if !p.expectPeek(lexer.RBRACE) {
		return nil
	}

	return fields
}

func (p *Parser) parseFunctionParameters() []*ast.Parameter {
	params := []*ast.Parameter{}

	if p.peekToken.Type == lexer.RPAREN {
		p.nextToken()
		return params
	}

	p.nextToken()
	param := &ast.Parameter{
		Name: &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
	}
	if p.peekToken.Type == lexer.COLON {
		p.nextToken()
		p.nextToken()
		param.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	}
	params = append(params, param)

	for p.peekToken.Type == lexer.COMMA {
		p.nextToken()
		p.nextToken()
		param = &ast.Parameter{
			Name: &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
		}
		if p.peekToken.Type == lexer.COLON {
			p.nextToken()
			p.nextToken()
			param.Type = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		}
		params = append(params, param)
	}

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	return params
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken}
	block.Statements = []ast.Statement{}

	p.nextToken()

	for p.curToken.Type != lexer.RBRACE && p.curToken.Type != lexer.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	stmt.Expression = p.parseExpression(LOWEST)

	if p.peekToken.Type == lexer.SEMICOLON {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for p.peekToken.Type != lexer.SEMICOLON && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() ast.Expression {
	// Check if this is a struct literal: IDENT { ... }
	if p.peekToken.Type == lexer.LBRACE {
		return p.parseStructLiteral()
	}
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

func (p *Parser) parseFloatLiteral() ast.Expression {
	lit := &ast.FloatLiteral{Token: p.curToken}

	value, err := strconv.ParseFloat(p.curToken.Literal, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as float", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	lit.Value = value

	return lit
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curToken.Type == lexer.TRUE}
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekToken.Type == lexer.ELSE {
		p.nextToken()

		if !p.expectPeek(lexer.LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken}

	if !p.expectPeek(lexer.LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if p.peekToken.Type == lexer.COLON {
		p.nextToken()
		if !p.expectPeek(lexer.IDENT) {
			return nil
		}
		lit.ReturnType = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

func (p *Parser) parseStructLiteral() ast.Expression {
	lit := &ast.StructLiteral{
		Token:  p.curToken,
		Name:   &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal},
		Fields: make(map[string]ast.Expression),
	}

	if !p.expectPeek(lexer.LBRACE) {
		return nil
	}

	// Parse empty struct: StructName{}
	if p.peekToken.Type == lexer.RBRACE {
		p.nextToken()
		return lit
	}

	p.nextToken()

	// Parse first field: fieldName: value
	if p.curToken.Type != lexer.IDENT {
		p.peekError(lexer.IDENT)
		return nil
	}
	fieldName := p.curToken.Literal

	if !p.expectPeek(lexer.COLON) {
		return nil
	}

	p.nextToken()
	fieldValue := p.parseExpression(LOWEST)
	lit.Fields[fieldName] = fieldValue

	// Parse remaining fields: , fieldName: value
	for p.peekToken.Type == lexer.COMMA {
		p.nextToken() // consume comma
		p.nextToken() // consume next token

		if p.curToken.Type != lexer.IDENT {
			p.peekError(lexer.IDENT)
			return nil
		}
		fieldName = p.curToken.Literal

		if !p.expectPeek(lexer.COLON) {
			return nil
		}

		p.nextToken()
		fieldValue = p.parseExpression(LOWEST)
		lit.Fields[fieldName] = fieldValue
	}

	if !p.expectPeek(lexer.RBRACE) {
		return nil
	}

	return lit
}

func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken}

	array.Elements = p.parseExpressionList(lexer.RBRACKET)

	return array
}

func (p *Parser) parseExpressionList(end lexer.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekToken.Type == end {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekToken.Type == lexer.COMMA {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)

	return expression
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	exp.Arguments = p.parseExpressionList(lexer.RPAREN)
	return exp
}

func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.nextToken()
	exp.Index = p.parseExpression(LOWEST)

	if !p.expectPeek(lexer.RBRACKET) {
		return nil
	}

	return exp
}

func (p *Parser) parseAssignExpression(left ast.Expression) ast.Expression {
	exp := &ast.AssignExpression{Token: p.curToken, Left: left}

	precedence := p.curPrecedence()
	p.nextToken()
	exp.Right = p.parseExpression(precedence)

	return exp
}

func (p *Parser) noPrefixParseFnError(t lexer.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) expectPeek(t lexer.TokenType) bool {
	if p.peekToken.Type == t {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

func (p *Parser) peekError(t lexer.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseVarExpression() ast.Expression {
	varExpr := &ast.CallExpression{
		Token:    p.curToken,
		Function: &ast.Identifier{Token: p.curToken, Value: "var"},
	}

	if p.peekToken.Type == lexer.LPAREN {
		p.nextToken()
		varExpr.Arguments = p.parseExpressionList(lexer.RPAREN)
	}

	return varExpr
}

func isVarCall(exp ast.Expression) bool {
	if call, ok := exp.(*ast.CallExpression); ok {
		if ident, ok := call.Function.(*ast.Identifier); ok {
			return ident.Value == "var"
		}
	}
	return false
}