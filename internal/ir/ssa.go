package ir

import "fmt"

// SSA (Static Single Assignment) IR format
// Mỗi biến chỉ được assign 1 lần, dễ optimize hơn

type SSAProgram struct {
	Functions []*SSAFunction
	Data      []*Data
}

type SSAFunction struct {
	Name       string
	Params     []*SSAParam
	ReturnType string
	Blocks     []*SSABasicBlock
	Locals     map[string]*SSALocal
}

type SSAParam struct {
	Name string
	Type string
	ID   int // SSA ID
}

type SSALocal struct {
	Name string
	Type string
	ID   int // SSA ID
}

type SSABasicBlock struct {
	Label    string
	Instrs   []SSAInstruction
	Preds    []*SSABasicBlock // Predecessors
	Succs    []*SSABasicBlock // Successors
	PhiNodes []*SSAPhi        // Phi nodes for SSA
}

type SSAInstruction interface {
	String() string
	Defs() []int // SSA IDs được define
	Uses() []int // SSA IDs được sử dụng
}

// Phi node (cho SSA - merge values từ nhiều predecessors)
type SSAPhi struct {
	Dest int            // SSA ID đích
	Args map[string]int // predecessor label -> SSA ID
}

func (p *SSAPhi) String() string {
	return fmt.Sprintf("phi(%d) = ...", p.Dest)
}

func (p *SSAPhi) Defs() []int {
	return []int{p.Dest}
}

func (p *SSAPhi) Uses() []int {
	uses := []int{}
	for _, id := range p.Args {
		uses = append(uses, id)
	}
	return uses
}

// SSA Constant
type SSAConstInt struct {
	Dest int
	Val  int64
}

func (c *SSAConstInt) String() string {
	return fmt.Sprintf("v%d = const %d", c.Dest, c.Val)
}

func (c *SSAConstInt) Defs() []int {
	return []int{c.Dest}
}

func (c *SSAConstInt) Uses() []int {
	return []int{}
}

type SSAConstFloat struct {
	Dest int
	Val  float64
}

func (c *SSAConstFloat) String() string {
	return fmt.Sprintf("v%d = const %f", c.Dest, c.Val)
}

func (c *SSAConstFloat) Defs() []int {
	return []int{c.Dest}
}

func (c *SSAConstFloat) Uses() []int {
	return []int{}
}

type SSAConstString struct {
	Dest  int
	Val   string
	Label string // Label trong data section
}

func (c *SSAConstString) String() string {
	return fmt.Sprintf("v%d = const \"%s\"", c.Dest, c.Val)
}

func (c *SSAConstString) Defs() []int {
	return []int{c.Dest}
}

func (c *SSAConstString) Uses() []int {
	return []int{}
}

type SSAConstBool struct {
	Dest int
	Val  bool
}

func (c *SSAConstBool) String() string {
	return fmt.Sprintf("v%d = const %t", c.Dest, c.Val)
}

func (c *SSAConstBool) Defs() []int {
	return []int{c.Dest}
}

func (c *SSAConstBool) Uses() []int {
	return []int{}
}

// SSA Binary Operations
type SSABinOp struct {
	Dest  int
	Op    string // +, -, *, /, %, ==, !=, <, >, &&, ||
	Left  int
	Right int
}

func (b *SSABinOp) String() string {
	return fmt.Sprintf("v%d = v%d %s v%d", b.Dest, b.Left, b.Op, b.Right)
}

func (b *SSABinOp) Defs() []int {
	return []int{b.Dest}
}

func (b *SSABinOp) Uses() []int {
	return []int{b.Left, b.Right}
}

// SSA Unary Operations
type SSAUnOp struct {
	Dest int
	Op   string // -, !
	Val  int
}

func (u *SSAUnOp) String() string {
	return fmt.Sprintf("v%d = %s v%d", u.Dest, u.Op, u.Val)
}

func (u *SSAUnOp) Defs() []int {
	return []int{u.Dest}
}

func (u *SSAUnOp) Uses() []int {
	return []int{u.Val}
}

// SSA Load (load từ memory/parameter)
type SSALoad struct {
	Dest int
	From int // SSA ID của address hoặc param
}

func (l *SSALoad) String() string {
	return fmt.Sprintf("v%d = load v%d", l.Dest, l.From)
}

func (l *SSALoad) Defs() []int {
	return []int{l.Dest}
}

func (l *SSALoad) Uses() []int {
	return []int{l.From}
}

// SSA Store (store vào memory)
type SSAStore struct {
	To   int // SSA ID của address
	From int // SSA ID của value
}

func (s *SSAStore) String() string {
	return fmt.Sprintf("store v%d -> v%d", s.From, s.To)
}

func (s *SSAStore) Defs() []int {
	return []int{}
}

func (s *SSAStore) Uses() []int {
	return []int{s.To, s.From}
}

// SSA Call
type SSACall struct {
	Dest int
	Func string
	Args []int // SSA IDs của arguments
}

func (c *SSACall) String() string {
	args := ""
	for i, arg := range c.Args {
		if i > 0 {
			args += ", "
		}
		args += fmt.Sprintf("v%d", arg)
	}
	if c.Dest >= 0 {
		return fmt.Sprintf("v%d = call %s(%s)", c.Dest, c.Func, args)
	}
	return fmt.Sprintf("call %s(%s)", c.Func, args)
}

func (c *SSACall) Defs() []int {
	if c.Dest >= 0 {
		return []int{c.Dest}
	}
	return []int{}
}

func (c *SSACall) Uses() []int {
	return c.Args
}

// SSA Return
type SSAReturn struct {
	Val int // -1 nếu void
}

func (r *SSAReturn) String() string {
	if r.Val >= 0 {
		return fmt.Sprintf("return v%d", r.Val)
	}
	return "return"
}

func (r *SSAReturn) Defs() []int {
	return []int{}
}

func (r *SSAReturn) Uses() []int {
	if r.Val >= 0 {
		return []int{r.Val}
	}
	return []int{}
}

// SSA Jump
type SSAJump struct {
	Target string // Label của target block
}

func (j *SSAJump) String() string {
	return fmt.Sprintf("jump %s", j.Target)
}

func (j *SSAJump) Defs() []int {
	return []int{}
}

func (j *SSAJump) Uses() []int {
	return []int{}
}

// SSA Conditional Jump
type SSACondJump struct {
	Cond int
	Then string
	Else string
}

func (j *SSACondJump) String() string {
	return fmt.Sprintf("if v%d then %s else %s", j.Cond, j.Then, j.Else)
}

func (j *SSACondJump) Defs() []int {
	return []int{}
}

func (j *SSACondJump) Uses() []int {
	return []int{j.Cond}
}
