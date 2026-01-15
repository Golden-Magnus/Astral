package ir

import "fmt"

// IR (Intermediate Representation) for Astral compiler
// Simple 3-address code format

type Program struct {
	Functions []*Function
	Data      []*Data
}

type Function struct {
	Name       string
	Params     []*Param
	ReturnType string
	Blocks     []*BasicBlock
	Locals     map[string]*Local
}

type Param struct {
	Name string
	Type string
}

type Local struct {
	Name string
	Type string
}

type BasicBlock struct {
	Label  string
	Instrs []Instruction
	Next   *BasicBlock
	Branch *BasicBlock // for conditional jumps
}

type Instruction interface {
	String() string
}

// Arithmetic operations
type Add struct {
	Dest  string
	Left  string
	Right string
}

func (a *Add) String() string {
	return a.Dest + " = " + a.Left + " + " + a.Right
}

type Sub struct {
	Dest  string
	Left  string
	Right string
}

func (s *Sub) String() string {
	return s.Dest + " = " + s.Left + " - " + s.Right
}

type Mul struct {
	Dest  string
	Left  string
	Right string
}

func (m *Mul) String() string {
	return m.Dest + " = " + m.Left + " * " + m.Right
}

type Div struct {
	Dest  string
	Left  string
	Right string
}

func (d *Div) String() string {
	return d.Dest + " = " + d.Left + " / " + d.Right
}

type Mod struct {
	Dest  string
	Left  string
	Right string
}

func (m *Mod) String() string {
	return m.Dest + " = " + m.Left + " % " + m.Right
}

// Comparison operations
type Eq struct {
	Dest  string
	Left  string
	Right string
}

func (e *Eq) String() string {
	return e.Dest + " = " + e.Left + " == " + e.Right
}

type Ne struct {
	Dest  string
	Left  string
	Right string
}

func (n *Ne) String() string {
	return n.Dest + " = " + n.Left + " != " + n.Right
}

type Lt struct {
	Dest  string
	Left  string
	Right string
}

func (l *Lt) String() string {
	return l.Dest + " = " + l.Left + " < " + l.Right
}

type Gt struct {
	Dest  string
	Left  string
	Right string
}

func (g *Gt) String() string {
	return g.Dest + " = " + g.Left + " > " + g.Right
}

// Logic operations
type And struct {
	Dest  string
	Left  string
	Right string
}

func (a *And) String() string {
	return a.Dest + " = " + a.Left + " && " + a.Right
}

type Or struct {
	Dest  string
	Left  string
	Right string
}

func (o *Or) String() string {
	return o.Dest + " = " + o.Left + " || " + o.Right
}

// Unary operations
type Neg struct {
	Dest string
	Val  string
}

func (n *Neg) String() string {
	return n.Dest + " = -" + n.Val
}

type Not struct {
	Dest string
	Val  string
}

func (n *Not) String() string {
	return n.Dest + " = !" + n.Val
}

// Load/Store
type Load struct {
	Dest string
	Addr string
}

func (l *Load) String() string {
	return l.Dest + " = load " + l.Addr
}

type Store struct {
	Addr string
	Val  string
}

func (s *Store) String() string {
	return "store " + s.Val + " -> " + s.Addr
}

type Move struct {
	Dest string
	Src  string
}

func (m *Move) String() string {
	return m.Dest + " = " + m.Src
}

// Function calls
type Call struct {
	Dest string
	Func string
	Args []string
}

func (c *Call) String() string {
	args := ""
	for i, arg := range c.Args {
		if i > 0 {
			args += ", "
		}
		args += arg
	}
	if c.Dest != "" {
		return c.Dest + " = call " + c.Func + "(" + args + ")"
	}
	return "call " + c.Func + "(" + args + ")"
}

// Control flow
type Jump struct {
	Label string
}

func (j *Jump) String() string {
	return "jump " + j.Label
}

type JumpIf struct {
	Cond string
	Then string
	Else string
}

func (j *JumpIf) String() string {
	return "jumpif " + j.Cond + " " + j.Then + " " + j.Else
}

type Return struct {
	Val string
}

func (r *Return) String() string {
	if r.Val != "" {
		return "return " + r.Val
	}
	return "return"
}

// Constants
type ConstInt struct {
	Dest string
	Val  int64
}

func (c *ConstInt) String() string {
	return fmt.Sprintf("%s = %d", c.Dest, c.Val)
}

type ConstFloat struct {
	Dest string
	Val  float64
}

func (c *ConstFloat) String() string {
	return fmt.Sprintf("%s = %f", c.Dest, c.Val)
}

type ConstString struct {
	Dest string
	Val  string
}

func (c *ConstString) String() string {
	return c.Dest + " = \"" + c.Val + "\""
}

type ConstBool struct {
	Dest string
	Val  bool
}

func (c *ConstBool) String() string {
	if c.Val {
		return c.Dest + " = true"
	}
	return c.Dest + " = false"
}

// Data section
type Data struct {
	Label string
	Type  string
	Value interface{}
}
