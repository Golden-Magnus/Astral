package asmgen

import (
	"astral/internal/ir"
	"bytes"
	"fmt"
)

type ASMGenerator struct {
	buf     *bytes.Buffer
	strings map[string]string // string value -> label
}

func New() *ASMGenerator {
	return &ASMGenerator{
		buf:     new(bytes.Buffer),
		strings: make(map[string]string),
	}
}

// Generate generates assembly from 3-address IR (legacy)
func (g *ASMGenerator) Generate(irProgram *ir.Program) (string, error) {
	return g.generateRawAsm(irProgram)
}

// GenerateFromSSA generates assembly from SSA IR
func (g *ASMGenerator) GenerateFromSSA(ssaProgram *ir.SSAProgram) (string, error) {
	return g.generateRawAsmFromSSA(ssaProgram)
}

func (g *ASMGenerator) generateRawAsmFromSSA(ssaProgram *ir.SSAProgram) (string, error) {
	buf := new(bytes.Buffer)

	buf.WriteString("; Generated assembly from SSA IR\n")
	buf.WriteString("; Compiled by Astral Assembler\n\n")

	// Generate functions
	for _, fn := range ssaProgram.Functions {
		if err := g.genSSAFunction(buf, fn); err != nil {
			return "", err
		}
	}

	// Data section
	if len(ssaProgram.Data) > 0 {
		buf.WriteString("\nsection .data\n")
		for _, data := range ssaProgram.Data {
			if data.Type == "string" {
				strVal := data.Value.(string)
				buf.WriteString(fmt.Sprintf("%s: db \"%s\", 0\n", data.Label, strVal))
			}
		}
	}

	return buf.String(), nil
}

func (g *ASMGenerator) genSSAFunction(buf *bytes.Buffer, fn *ir.SSAFunction) error {
	stackSize := len(fn.Locals) * 8
	if stackSize == 0 {
		stackSize = 16
	}
	if stackSize%16 != 0 {
		stackSize = ((stackSize / 16) + 1) * 16
	}

	if fn.Name == "main" {
		buf.WriteString("section .text\n")
		buf.WriteString("global _start\n")
		buf.WriteString("_start:\n")
	} else {
		buf.WriteString(fmt.Sprintf("global %s\n", fn.Name))
		buf.WriteString(fmt.Sprintf("%s:\n", fn.Name))
	}

	buf.WriteString("    push rbp\n")
	buf.WriteString("    mov rbp, rsp\n")
	buf.WriteString(fmt.Sprintf("    sub rsp, %d\n", stackSize))

	// Generate instructions from SSA blocks
	for _, block := range fn.Blocks {
		if block.Label != "" {
			buf.WriteString(fmt.Sprintf("%s:\n", block.Label))
		}
		for _, instr := range block.Instrs {
			if err := g.genSSAInstruction(buf, instr, stackSize); err != nil {
				return err
			}
		}
	}

	buf.WriteString("    mov rsp, rbp\n")
	buf.WriteString("    pop rbp\n")

	if fn.Name == "main" {
		buf.WriteString("    mov rax, 60\n")
		buf.WriteString("    mov rdi, 0\n")
		buf.WriteString("    syscall\n")
	} else {
		buf.WriteString("    ret\n")
	}
	buf.WriteString("\n")

	return nil
}

func (g *ASMGenerator) genSSAInstruction(buf *bytes.Buffer, instr ir.SSAInstruction, stackSize int) error {
	switch i := instr.(type) {
	case *ir.SSAConstInt:
		offset := g.getStackOffsetForSSA(i.Dest, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, %d\n", i.Val))
		buf.WriteString(fmt.Sprintf("    mov [rbp-%d], rax\n", offset))
	case *ir.SSABinOp:
		leftOffset := g.getStackOffsetForSSA(i.Left, stackSize)
		rightOffset := g.getStackOffsetForSSA(i.Right, stackSize)
		destOffset := g.getStackOffsetForSSA(i.Dest, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", leftOffset))
		switch i.Op {
		case "+":
			buf.WriteString(fmt.Sprintf("    add rax, [rbp-%d]\n", rightOffset))
		case "-":
			buf.WriteString(fmt.Sprintf("    sub rax, [rbp-%d]\n", rightOffset))
		case "*":
			buf.WriteString(fmt.Sprintf("    imul rax, [rbp-%d]\n", rightOffset))
		case "/":
			buf.WriteString(fmt.Sprintf("    idiv qword [rbp-%d]\n", rightOffset))
		}
		buf.WriteString(fmt.Sprintf("    mov [rbp-%d], rax\n", destOffset))
	case *ir.SSACall:
		if i.Func == "print" {
			if len(i.Args) > 0 {
				argOffset := g.getStackOffsetForSSA(i.Args[0], stackSize)
				buf.WriteString("    ; Print\n")
				buf.WriteString(fmt.Sprintf("    mov rsi, [rbp-%d]\n", argOffset))
				buf.WriteString("    mov rax, 1\n")
				buf.WriteString("    mov rdi, 1\n")
				buf.WriteString("    mov rdx, 10\n")
				buf.WriteString("    syscall\n")
			}
		} else {
			buf.WriteString(fmt.Sprintf("    call %s\n", i.Func))
		}
	case *ir.SSAReturn:
		if i.Val >= 0 {
			valOffset := g.getStackOffsetForSSA(i.Val, stackSize)
			buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", valOffset))
		}
		buf.WriteString("    ret\n")
	case *ir.SSAJump:
		buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Target))
	case *ir.SSACondJump:
		condOffset := g.getStackOffsetForSSA(i.Cond, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", condOffset))
		buf.WriteString("    test rax, rax\n")
		buf.WriteString(fmt.Sprintf("    jnz %s\n", i.Then))
		buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Else))
	}
	return nil
}

func (g *ASMGenerator) getStackOffsetForSSA(ssaID int, stackSize int) int {
	// Map SSA ID to stack offset
	offset := (ssaID % (stackSize / 8)) * 8
	if offset < 0 {
		offset = -offset
	}
	return offset
}

func (g *ASMGenerator) generateRawAsm(irProgram *ir.Program) (string, error) {
	// Detect OS at compile time - we'll generate platform-specific assembly
	// For now, generate NASM format (works on both Windows and Linux)
	buf := new(bytes.Buffer)

	buf.WriteString("; Generated NASM assembly for Astral\n")
	buf.WriteString("; Compiles directly to machine code using NASM + linker\n")
	buf.WriteString("; No dependency on Go toolchain\n\n")

	// Generate functions
	for _, fn := range irProgram.Functions {
		if err := g.genRawAsmFunction(buf, fn); err != nil {
			return "", err
		}
	}

	// Data section for strings
	if len(irProgram.Data) > 0 {
		buf.WriteString("\nsection .data\n")
		for _, data := range irProgram.Data {
			if data.Type == "string" {
				strVal := data.Value.(string)
				buf.WriteString(fmt.Sprintf("%s: db \"%s\", 0\n", data.Label, strVal))
				buf.WriteString(fmt.Sprintf("%s_len: equ $ - %s\n", data.Label, data.Label))
			}
		}
	}

	return buf.String(), nil
}

func (g *ASMGenerator) genRawAsmFunction(buf *bytes.Buffer, fn *ir.Function) error {
	// Calculate stack size
	stackSize := len(fn.Locals) * 8
	if stackSize == 0 {
		stackSize = 16 // Minimum stack frame (16-byte aligned)
	}
	// Align to 16 bytes
	if stackSize%16 != 0 {
		stackSize = ((stackSize / 16) + 1) * 16
	}

	// Function label
	if fn.Name == "main" {
		buf.WriteString("section .text\n")
		buf.WriteString("global _start\n")
		buf.WriteString("_start:\n")
	} else {
		buf.WriteString(fmt.Sprintf("global %s\n", fn.Name))
		buf.WriteString(fmt.Sprintf("%s:\n", fn.Name))
	}

	// Function prologue (x86-64 calling convention)
	buf.WriteString("    push rbp\n")
	buf.WriteString("    mov rbp, rsp\n")
	buf.WriteString(fmt.Sprintf("    sub rsp, %d\n", stackSize))

	// Generate instructions for each block
	for _, block := range fn.Blocks {
		if block.Label != "" {
			buf.WriteString(fmt.Sprintf("%s:\n", block.Label))
		}
		for _, instr := range block.Instrs {
			if err := g.genRawAsmInstruction(buf, instr, stackSize); err != nil {
				return err
			}
		}
	}

	// Function epilogue
	buf.WriteString("    mov rsp, rbp\n")
	buf.WriteString("    pop rbp\n")

	if fn.Name == "main" {
		// Exit on Windows: call ExitProcess
		// Exit on Linux: syscall exit
		buf.WriteString("    ; Exit program\n")
		buf.WriteString("    mov rax, 60    ; sys_exit on Linux\n")
		buf.WriteString("    mov rdi, 0     ; exit code\n")
		buf.WriteString("    syscall        ; Linux syscall\n")
	} else {
		buf.WriteString("    ret\n")
	}
	buf.WriteString("\n")

	return nil
}

func (g *ASMGenerator) genRawAsmInstruction(buf *bytes.Buffer, instr ir.Instruction, stackSize int) error {
	switch i := instr.(type) {
	case *ir.ConstInt:
		offset := g.getStackOffset(i.Dest, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, %d\n", i.Val))
		buf.WriteString(fmt.Sprintf("    mov [rbp-%d], rax\n", offset))
	case *ir.Add:
		leftOffset := g.getStackOffset(i.Left, stackSize)
		rightOffset := g.getStackOffset(i.Right, stackSize)
		destOffset := g.getStackOffset(i.Dest, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", leftOffset))
		buf.WriteString(fmt.Sprintf("    add rax, [rbp-%d]\n", rightOffset))
		buf.WriteString(fmt.Sprintf("    mov [rbp-%d], rax\n", destOffset))
	case *ir.Move:
		srcOffset := g.getStackOffset(i.Src, stackSize)
		destOffset := g.getStackOffset(i.Dest, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", srcOffset))
		buf.WriteString(fmt.Sprintf("    mov [rbp-%d], rax\n", destOffset))
	case *ir.Call:
		if i.Func == "print" {
			// System call for print (Linux: write syscall)
			if len(i.Args) > 0 {
				argOffset := g.getStackOffset(i.Args[0], stackSize)
				buf.WriteString("    ; Print string\n")
				buf.WriteString(fmt.Sprintf("    mov rsi, [rbp-%d]\n", argOffset))
				buf.WriteString("    mov rax, 1      ; sys_write\n")
				buf.WriteString("    mov rdi, 1      ; stdout\n")
				buf.WriteString("    mov rdx, 10     ; length (simplified)\n")
				buf.WriteString("    syscall\n")
			}
		} else {
			// Regular function call
			buf.WriteString(fmt.Sprintf("    call %s\n", i.Func))
		}
	case *ir.Return:
		if i.Val != "" {
			valOffset := g.getStackOffset(i.Val, stackSize)
			buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", valOffset))
		}
		buf.WriteString("    ret\n")
	case *ir.Jump:
		buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Label))
	case *ir.JumpIf:
		condOffset := g.getStackOffset(i.Cond, stackSize)
		buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%d]\n", condOffset))
		buf.WriteString("    test rax, rax\n")
		buf.WriteString(fmt.Sprintf("    jnz %s\n", i.Then))
		buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Else))
	}
	return nil
}

func (g *ASMGenerator) getStackOffset(name string, stackSize int) int {
	// Simplified offset calculation based on variable name
	// In real implementation, would track actual offsets in symbol table
	hash := 0
	for _, c := range name {
		hash = hash*31 + int(c)
	}
	offset := (hash % (stackSize / 8)) * 8
	if offset < 0 {
		offset = -offset
	}
	return offset
}

// generateWindows and generateUnix are legacy methods
// They are kept for potential future use but currently unused
// as we use generateRawAsm instead
//
// func (g *ASMGenerator) generateWindows(irProgram *ir.Program) (string, error) {
// 	// Windows x64 calling convention
// 	// Generate assembly for Windows (MASM/NASM style)
// 	g.buf.WriteString("; Windows x64 Assembly\n")
// 	g.buf.WriteString("section .text\n")
// 	g.buf.WriteString("global main\n")
// 	g.buf.WriteString("extern ExitProcess\n")
// 	g.buf.WriteString("extern GetStdHandle\n")
// 	g.buf.WriteString("extern WriteFile\n")
// 	g.buf.WriteString("extern ReadFile\n")
// 	g.buf.WriteString("\n")
//
// 	// Generate functions
// 	for _, fn := range irProgram.Functions {
// 		if err := g.genFunction(fn); err != nil {
// 			return "", err
// 		}
// 	}
//
// 	// Data section
// 	g.buf.WriteString("\nsection .data\n")
// 	for _, data := range irProgram.Data {
// 		if data.Type == "string" {
// 			g.buf.WriteString(fmt.Sprintf("%s: db \"%s\", 0\n", data.Label, data.Value))
// 		}
// 	}
//
// 	return g.buf.String(), nil
// }
//
// func (g *ASMGenerator) generateUnix(irProgram *ir.Program) (string, error) {
// 	// Linux/Unix x64 calling convention (System V ABI)
// 	g.buf.WriteString("; Linux x64 Assembly\n")
// 	g.buf.WriteString(".section .text\n")
// 	g.buf.WriteString(".global _start\n")
// 	g.buf.WriteString("\n")
//
// 	// Generate functions
// 	for _, fn := range irProgram.Functions {
// 		if err := g.genFunctionUnix(fn); err != nil {
// 			return "", err
// 		}
// 	}
//
// 	// Data section
// 	g.buf.WriteString("\n.section .data\n")
// 	for _, data := range irProgram.Data {
// 		if data.Type == "string" {
// 			g.buf.WriteString(fmt.Sprintf("%s: .asciz \"%s\"\n", data.Label, data.Value))
// 		}
// 	}
//
// 	return g.buf.String(), nil
// }

// Legacy methods - kept for potential future use but currently unused
// as we use generateRawAsm and genSSAFunction instead

// func (g *ASMGenerator) genFunction(fn *ir.Function) error {
// 	g.buf.WriteString(fmt.Sprintf("%s:\n", fn.Name))
// 	g.buf.WriteString("    push rbp\n")
// 	g.buf.WriteString("    mov rbp, rsp\n")
//
// 	// Allocate stack space for locals
// 	localCount := len(fn.Locals)
// 	if localCount > 0 {
// 		stackSize := localCount * 8 // 8 bytes per local (int64)
// 		g.buf.WriteString(fmt.Sprintf("    sub rsp, %d\n", stackSize))
// 	}
//
// 	// Generate instructions for each block
// 	for _, block := range fn.Blocks {
// 		if block.Label != "" {
// 			g.buf.WriteString(fmt.Sprintf("%s:\n", block.Label))
// 		}
// 		for _, instr := range block.Instrs {
// 			if err := g.genInstruction(instr); err != nil {
// 				return err
// 			}
// 		}
// 	}
//
// 	g.buf.WriteString("    mov rsp, rbp\n")
// 	g.buf.WriteString("    pop rbp\n")
// 	if fn.Name == "main" {
// 		g.buf.WriteString("    mov ecx, 0\n")
// 		g.buf.WriteString("    call ExitProcess\n")
// 	} else {
// 		g.buf.WriteString("    ret\n")
// 	}
// 	g.buf.WriteString("\n")
//
// 	return nil
// }
//
// func (g *ASMGenerator) genFunctionUnix(fn *ir.Function) error {
// 	if fn.Name == "main" {
// 		g.buf.WriteString("_start:\n")
// 	} else {
// 		g.buf.WriteString(fmt.Sprintf("%s:\n", fn.Name))
// 	}
// 	g.buf.WriteString("    push rbp\n")
// 	g.buf.WriteString("    mov rbp, rsp\n")
//
// 	// Allocate stack space for locals
// 	localCount := len(fn.Locals)
// 	if localCount > 0 {
// 		stackSize := localCount * 8
// 		g.buf.WriteString(fmt.Sprintf("    sub rsp, %d\n", stackSize))
// 	}
//
// 	// Generate instructions for each block
// 	for _, block := range fn.Blocks {
// 		if block.Label != "" {
// 			g.buf.WriteString(fmt.Sprintf("%s:\n", block.Label))
// 		}
// 		for _, instr := range block.Instrs {
// 			if err := g.genInstructionUnix(instr); err != nil {
// 				return err
// 			}
// 		}
// 	}
//
// 	g.buf.WriteString("    mov rsp, rbp\n")
// 	g.buf.WriteString("    pop rbp\n")
// 	if fn.Name == "main" {
// 		g.buf.WriteString("    mov rax, 60\n") // sys_exit
// 		g.buf.WriteString("    mov rdi, 0\n")  // exit code
// 		g.buf.WriteString("    syscall\n")
// 	} else {
// 		g.buf.WriteString("    ret\n")
// 	}
// 	g.buf.WriteString("\n")
//
// 	return nil
// }
//
// func (g *ASMGenerator) genInstruction(instr ir.Instruction) error {
// 	switch i := instr.(type) {
// 	case *ir.ConstInt:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, %d\n", i.Val))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Add:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Left)))
// 		g.buf.WriteString(fmt.Sprintf("    add rax, [rbp-%s]\n", g.getLocalOffset(i.Right)))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Move:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Src)))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Call:
// 		if i.Func == "print" {
// 			return g.genPrintCall(i.Args)
// 		}
// 		// Regular function call
// 		for j, arg := range i.Args {
// 			if j < 4 { // Windows: RCX, RDX, R8, R9
// 				regs := []string{"rcx", "rdx", "r8", "r9"}
// 				g.buf.WriteString(fmt.Sprintf("    mov %s, [rbp-%s]\n", regs[j], g.getLocalOffset(arg)))
// 			} else {
// 				// Stack parameters
// 				g.buf.WriteString(fmt.Sprintf("    push [rbp-%s]\n", g.getLocalOffset(arg)))
// 			}
// 		}
// 		g.buf.WriteString(fmt.Sprintf("    call %s\n", i.Func))
// 		if i.Dest != "" {
// 			g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 		}
// 	case *ir.Return:
// 		if i.Val != "" {
// 			g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Val)))
// 		}
// 		g.buf.WriteString("    ret\n")
// 	case *ir.Jump:
// 		g.buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Label))
// 	case *ir.JumpIf:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Cond)))
// 		g.buf.WriteString("    test rax, rax\n")
// 		g.buf.WriteString(fmt.Sprintf("    jnz %s\n", i.Then))
// 		g.buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Else))
// 	}
// 	return nil
// }
//
// func (g *ASMGenerator) genInstructionUnix(instr ir.Instruction) error {
// 	switch i := instr.(type) {
// 	case *ir.ConstInt:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, %d\n", i.Val))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Add:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Left)))
// 		g.buf.WriteString(fmt.Sprintf("    add rax, [rbp-%s]\n", g.getLocalOffset(i.Right)))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Move:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Src)))
// 		g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 	case *ir.Call:
// 		if i.Func == "print" {
// 			return g.genPrintCallUnix(i.Args)
// 		}
// 		// System V ABI: RDI, RSI, RDX, RCX, R8, R9
// 		regs := []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}
// 		for j, arg := range i.Args {
// 			if j < 6 {
// 				g.buf.WriteString(fmt.Sprintf("    mov %s, [rbp-%s]\n", regs[j], g.getLocalOffset(arg)))
// 			} else {
// 				g.buf.WriteString(fmt.Sprintf("    push [rbp-%s]\n", g.getLocalOffset(arg)))
// 			}
// 		}
// 		g.buf.WriteString(fmt.Sprintf("    call %s\n", i.Func))
// 		if i.Dest != "" {
// 			g.buf.WriteString(fmt.Sprintf("    mov [rbp-%s], rax\n", g.getLocalOffset(i.Dest)))
// 		}
// 	case *ir.Return:
// 		if i.Val != "" {
// 			g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Val)))
// 		}
// 		g.buf.WriteString("    ret\n")
// 	case *ir.Jump:
// 		g.buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Label))
// 	case *ir.JumpIf:
// 		g.buf.WriteString(fmt.Sprintf("    mov rax, [rbp-%s]\n", g.getLocalOffset(i.Cond)))
// 		g.buf.WriteString("    test rax, rax\n")
// 		g.buf.WriteString(fmt.Sprintf("    jnz %s\n", i.Then))
// 		g.buf.WriteString(fmt.Sprintf("    jmp %s\n", i.Else))
// 	}
// 	return nil
// }
//
// func (g *ASMGenerator) genPrintCall(_ []string) error {
// 	// Windows: Use WriteFile
// 	// Simplified - would need proper string handling
// 	g.buf.WriteString("    ; print call - simplified\n")
// 	return nil
// }
//
// func (g *ASMGenerator) genPrintCallUnix(args []string) error {
// 	// Linux: Use write syscall
// 	if len(args) > 0 {
// 		g.buf.WriteString("    mov rax, 1\n") // sys_write
// 		g.buf.WriteString("    mov rdi, 1\n") // stdout
// 		g.buf.WriteString(fmt.Sprintf("    mov rsi, [rbp-%s]\n", g.getLocalOffset(args[0])))
// 		g.buf.WriteString("    mov rdx, 10\n") // length (simplified)
// 		g.buf.WriteString("    syscall\n")
// 	}
// 	return nil
// }
//
// func (g *ASMGenerator) getLocalOffset(name string) string {
// 	// Simplified offset calculation
// 	// In real implementation, would track actual stack offsets
// 	return fmt.Sprintf("0x%x", len(name)*8)
// }
