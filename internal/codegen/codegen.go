package codegen

import (
	"astral/internal/ast"
	"bytes"
	"fmt"
)

func Generate(program *ast.Program) (string, error) {
	buf := new(bytes.Buffer)

	buf.WriteString("package main\n\nimport (\n\t\"fmt\"\n\t\"bufio\"\n\t\"os\"\n\t\"strings\"\n\t\"strconv\"\n)\n\n")

	// Add helper function for range
	buf.WriteString("func rangeSlice(start, end int64) []int64 {\n")
	buf.WriteString("\tresult := make([]int64, 0)\n")
	buf.WriteString("\tfor i := start; i < end; i++ {\n")
	buf.WriteString("\t\tresult = append(result, i)\n")
	buf.WriteString("\t}\n")
	buf.WriteString("\treturn result\n")
	buf.WriteString("}\n\n")

	// Add helper function for read
	buf.WriteString("func read(prompt ...string) string {\n")
	buf.WriteString("\tvar p string\n")
	buf.WriteString("\tif len(prompt) > 0 {\n")
	buf.WriteString("\t\tp = prompt[0]\n")
	buf.WriteString("\t}\n")
	buf.WriteString("\tfmt.Print(p)\n")
	buf.WriteString("\treader := bufio.NewReader(os.Stdin)\n")
	buf.WriteString("\tinput, _ := reader.ReadString('\\n')\n")
	buf.WriteString("\treturn strings.TrimSpace(input)\n")
	buf.WriteString("}\n\n")

	// Add type conversion helpers
	buf.WriteString("func toInt(s string) int64 {\n")
	buf.WriteString("\tv, _ := strconv.ParseInt(strings.TrimSpace(s), 10, 64)\n")
	buf.WriteString("\treturn v\n")
	buf.WriteString("}\n\n")
	buf.WriteString("func toFloat(s string) float64 {\n")
	buf.WriteString("\tv, _ := strconv.ParseFloat(strings.TrimSpace(s), 64)\n")
	buf.WriteString("\treturn v\n")
	buf.WriteString("}\n\n")
	buf.WriteString("func toBool(s string) bool {\n")
	buf.WriteString("\tv, _ := strconv.ParseBool(strings.TrimSpace(s))\n")
	buf.WriteString("\treturn v\n")
	buf.WriteString("}\n\n")

	// Generate structs
	for _, stmt := range program.Statements {
		if ss, ok := stmt.(*ast.StructStatement); ok {
			genStruct(buf, ss)
		}
	}

	// Generate functions
	for _, stmt := range program.Statements {
		if fs, ok := stmt.(*ast.FunctionStatement); ok {
			genFunction(buf, fs)
		}
	}

	// Main wrapper
	hasMain := false
	for _, stmt := range program.Statements {
		if fs, ok := stmt.(*ast.FunctionStatement); ok && fs.Name.Value == "main" {
			hasMain = true
			break
		}
	}
	if !hasMain {
		buf.WriteString("func main() {\n")
		for _, stmt := range program.Statements {
			if _, ok := stmt.(*ast.StructStatement); ok {
				continue
			}
			if _, ok := stmt.(*ast.FunctionStatement); ok {
				continue
			}
			genStatement(buf, stmt, 1)
		}
		buf.WriteString("}\n")
	}

	return buf.String(), nil
}

func genStruct(buf *bytes.Buffer, ss *ast.StructStatement) {
	buf.WriteString(fmt.Sprintf("type %s struct {\n", ss.Name.Value))
	for _, field := range ss.Fields {
		t := "interface{}"
		if field.Type != nil {
			t = mapType(field.Type.Value)
		}
		buf.WriteString(fmt.Sprintf("\t%s %s\n", toCamelCase(field.Name.Value), t))
	}
	buf.WriteString("}\n\n")
}

func toCamelCase(s string) string {
	if len(s) == 0 {
		return s
	}
	if len(s) == 1 {
		return string(s[0] + 'A' - 'a')
	}
	// Only capitalize if it's a lowercase letter
	if s[0] >= 'a' && s[0] <= 'z' {
		return string(s[0]+'A'-'a') + s[1:]
	}
	return s
}

func genFunction(buf *bytes.Buffer, fs *ast.FunctionStatement) {
	buf.WriteString(fmt.Sprintf("func %s(", fs.Name.Value))
	for i, param := range fs.Parameters {
		t := "interface{}"
		if param.Type != nil {
			t = mapType(param.Type.Value)
		}
		buf.WriteString(fmt.Sprintf("%s %s", param.Name.Value, t))
		if i < len(fs.Parameters)-1 {
			buf.WriteString(", ")
		}
	}
	buf.WriteString(")")
	if fs.ReturnType != nil {
		buf.WriteString(" " + mapType(fs.ReturnType.Value))
	}
	buf.WriteString(" {\n")
	genBlock(buf, fs.Body, 1)
	buf.WriteString("}\n\n")
}

func genStatement(buf *bytes.Buffer, stmt ast.Statement, indent int) {
	ind := indentString(indent)
	switch s := stmt.(type) {
	case *ast.LetStatement:
		// For array types with var(), need special handling
		if s.IsArray && isVarCall(s.Value) {
			if s.Mutable {
				if s.Type != nil {
					goType := fmt.Sprintf("[]%s", mapType(s.Type.Value))
					// Generate var initialization with correct type
					value := genVarArrayInit(s.Value, s.Type.Value)
					buf.WriteString(fmt.Sprintf("%svar %s %s = %s\n", ind, s.Name.Value, goType, value))
				} else {
					buf.WriteString(fmt.Sprintf("%s%s := []interface{}{}\n", ind, s.Name.Value))
				}
			}
			return
		}

		value := genExpr(s.Value)
		// Auto-convert read() output based on variable type
		if s.Type != nil && isReadCall(s.Value) {
			value = wrapReadWithConversion(value, s.Type.Value)
		}
		// For let (immutable), use const syntax
		// For mut (mutable), use var/short decl
		if s.Mutable {
			if s.Type != nil {
				// Handle array types
				if s.IsArray {
					goType := fmt.Sprintf("[]%s", mapType(s.Type.Value))
					buf.WriteString(fmt.Sprintf("%svar %s %s = %s\n", ind, s.Name.Value, goType, value))
				} else {
					buf.WriteString(fmt.Sprintf("%svar %s %s = %s\n", ind, s.Name.Value, mapType(s.Type.Value), value))
				}
			} else {
				buf.WriteString(fmt.Sprintf("%s%s := %s\n", ind, s.Name.Value, value))
			}
		} else {
			// For immutable, gen as const
			if s.Type != nil {
				buf.WriteString(fmt.Sprintf("%sconst %s %s = %s\n", ind, s.Name.Value, mapType(s.Type.Value), value))
			} else {
				buf.WriteString(fmt.Sprintf("%sconst %s = %s\n", ind, s.Name.Value, value))
			}
		}
	case *ast.ExpressionStatement:
		if assign, ok := s.Expression.(*ast.AssignExpression); ok {
			buf.WriteString(fmt.Sprintf("%s%s = %s\n", ind, genExpr(assign.Left), genExpr(assign.Right)))
		} else if call, ok := s.Expression.(*ast.CallExpression); ok && isPrint(call) {
			args := genCallArgs(call.Arguments)
			buf.WriteString(fmt.Sprintf("%sfmt.Println(%s)\n", ind, args))
		} else {
			buf.WriteString(fmt.Sprintf("%s%s\n", ind, genExpr(s.Expression)))
		}
	case *ast.IfStatement:
		buf.WriteString(fmt.Sprintf("%sif %s {\n", ind, genExpr(s.Condition)))
		genBlock(buf, s.Consequence, indent+1)
		if s.Alternative != nil {
			buf.WriteString(fmt.Sprintf("%s} else {\n", ind))
			genBlock(buf, s.Alternative, indent+1)
		}
		buf.WriteString(fmt.Sprintf("%s}\n", ind))
	case *ast.WhileStatement:
		buf.WriteString(fmt.Sprintf("%sfor %s {\n", ind, genExpr(s.Condition)))
		genBlock(buf, s.Body, indent+1)
		buf.WriteString(fmt.Sprintf("%s}\n", ind))
	case *ast.ForStatement:
		// Generate: for i := range rangeSlice(0, 5) { ... }
		iterableExpr := genExpr(s.Iterable)
		// Check if it's a range() call and convert to rangeSlice()
		if call, ok := s.Iterable.(*ast.CallExpression); ok {
			if ident, ok := call.Function.(*ast.Identifier); ok && ident.Value == "range" {
				args := genCallArgs(call.Arguments)
				iterableExpr = fmt.Sprintf("rangeSlice(%s)", args)
			}
		}
		buf.WriteString(fmt.Sprintf("%sfor %s := range %s {\n", ind, s.VarName.Value, iterableExpr))
		genBlock(buf, s.Body, indent+1)
		buf.WriteString(fmt.Sprintf("%s}\n", ind))
	case *ast.BreakStatement:
		buf.WriteString(fmt.Sprintf("%sbreak\n", ind))
	case *ast.ContinueStatement:
		buf.WriteString(fmt.Sprintf("%scontinue\n", ind))
	case *ast.ReturnStatement:
		if s.ReturnValue == nil {
			buf.WriteString(fmt.Sprintf("%sreturn\n", ind))
		} else {
			buf.WriteString(fmt.Sprintf("%sreturn %s\n", ind, genExpr(s.ReturnValue)))
		}
	}
}

func genBlock(buf *bytes.Buffer, block *ast.BlockStatement, indent int) {
	for _, stmt := range block.Statements {
		genStatement(buf, stmt, indent)
	}
}

func isPrint(call *ast.CallExpression) bool {
	if ident, ok := call.Function.(*ast.Identifier); ok {
		return ident.Value == "print"
	}
	return false
}

func genCallArgs(args []ast.Expression) string {
	s := ""
	for i, arg := range args {
		if i > 0 {
			s += ", "
		}
		s += genExpr(arg)
	}
	return s
}

func genExpr(exp ast.Expression) string {
	switch e := exp.(type) {
	case *ast.Identifier:
		return e.Value
	case *ast.IntegerLiteral:
		return fmt.Sprintf("%d", e.Value)
	case *ast.FloatLiteral:
		return fmt.Sprintf("%f", e.Value)
	case *ast.StringLiteral:
		return fmt.Sprintf("\"%s\"", e.Value)
	case *ast.Boolean:
		return fmt.Sprintf("%t", e.Value)
	case *ast.PrefixExpression:
		return fmt.Sprintf("%s%s", e.Operator, genExpr(e.Right))
	case *ast.InfixExpression:
		return fmt.Sprintf("%s %s %s", genExpr(e.Left), e.Operator, genExpr(e.Right))
	case *ast.CallExpression:
		fn := genExpr(e.Function)
		// Special handling for range
		if ident, ok := e.Function.(*ast.Identifier); ok && ident.Value == "range" {
			args := genCallArgs(e.Arguments)
			return fmt.Sprintf("range(%s)", args)
		}
		// Special handling for var
		if ident, ok := e.Function.(*ast.Identifier); ok && ident.Value == "var" {
			if len(e.Arguments) == 0 {
				// var() - empty array
				return "[]interface{}{}"
			} else {
				// var(n) - array with n elements
				args := genCallArgs(e.Arguments)
				return fmt.Sprintf("make([]interface{}, %s)", args)
			}
		}
		return fmt.Sprintf("%s(%s)", fn, genCallArgs(e.Arguments))
	case *ast.ArrayLiteral:
		elems := ""
		for i, elem := range e.Elements {
			if i > 0 {
				elems += ", "
			}
			elems += genExpr(elem)
		}
		return fmt.Sprintf("[]interface{}{%s}", elems)
	case *ast.IndexExpression:
		return fmt.Sprintf("%s[%s]", genExpr(e.Left), genExpr(e.Index))
	case *ast.StructLiteral:
		fields := ""
		i := 0
		for fieldName, fieldValue := range e.Fields {
			if i > 0 {
				fields += ", "
			}
			fields += fmt.Sprintf("%s: %s", toCamelCase(fieldName), genExpr(fieldValue))
			i++
		}
		return fmt.Sprintf("%s{%s}", e.Name.Value, fields)
	default:
		return "/* unsupported */"
	}
}

func mapType(t string) string {
	m := map[string]string{
		"int":    "int64",
		"i32":    "int32",
		"i64":    "int64",
		"f64":    "float64",
		"string": "string",
		"bool":   "bool",
	}
	if v, ok := m[t]; ok {
		return v
	}
	return "interface{}"
}

func isReadCall(exp ast.Expression) bool {
	if call, ok := exp.(*ast.CallExpression); ok {
		if ident, ok := call.Function.(*ast.Identifier); ok {
			return ident.Value == "read"
		}
	}
	return false
}

func isVarCall(exp ast.Expression) bool {
	if call, ok := exp.(*ast.CallExpression); ok {
		if ident, ok := call.Function.(*ast.Identifier); ok {
			return ident.Value == "var"
		}
	}
	return false
}

func genVarArrayInit(exp ast.Expression, elementType string) string {
	call, ok := exp.(*ast.CallExpression)
	if !ok {
		return "[]interface{}{}"
	}

	goType := mapType(elementType)
	if len(call.Arguments) == 0 {
		return fmt.Sprintf("make([]%s, 0)", goType)
	} else {
		args := genCallArgs(call.Arguments)
		return fmt.Sprintf("make([]%s, %s)", goType, args)
	}
}

func wrapReadWithConversion(value string, targetType string) string {
	switch targetType {
	case "int", "i32", "i64":
		return fmt.Sprintf("toInt(%s)", value)
	case "f64":
		return fmt.Sprintf("toFloat(%s)", value)
	case "bool":
		return fmt.Sprintf("toBool(%s)", value)
	default:
		return value
	}
}

func indentString(indent int) string {
	return string(bytes.Repeat([]byte("\t"), indent))
}
