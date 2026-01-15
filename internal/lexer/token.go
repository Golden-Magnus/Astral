package lexer

type TokenType int

const (
	EOF TokenType = iota
	ILLEGAL
	IDENT
	INT
	FLOAT
	STRING
	ASSIGN
	PLUS
	MINUS
	ASTERISK
	SLASH
	MOD
	BANG
	EQ
	NOT_EQ
	LT
	GT
	COMMA
	SEMICOLON
	COLON
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	FUNCTION
	LET
	MUT
	TRUE
	FALSE
	IF
	ELSE
	RETURN
	WHILE
	STRUCT
	PRINT
	AND
	OR
	FOR
	IN
	BREAK
	CONTINUE
	VAR
	BORROW
	OWN
)

var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"let":    LET,
	"mut":    MUT,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
	"while":  WHILE,
	"struct": STRUCT,
	"print":  PRINT,
	"and":    AND,
	"or":     OR,
	"for":      FOR,
	"in":       IN,
	"break":    BREAK,
	"continue": CONTINUE,
	"var":      VAR,
	"borrow":   BORROW,
	"own":      OWN,
}

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}