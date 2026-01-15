package main

import (
	"astral/internal/asmgen"
	"astral/internal/assembler"
	"astral/internal/checker"
	"astral/internal/lexer"
	"astral/internal/linker"
	"astral/internal/optimizer"
	"astral/internal/parser"
	"astral/internal/safety"
	"astral/internal/ssagen"
	"astral/internal/utils"
	"fmt"
	"os"
	"path/filepath"
)

func main() {
	if len(os.Args) != 3 {
		fmt.Println("Usage: astralc <input.astral> <output.exe>")
		os.Exit(1)
	}

	inputFile := os.Args[1]
	outputFile := os.Args[2]

	src, err := os.ReadFile(inputFile)
	if err != nil {
		utils.Error(fmt.Sprintf("Error reading file: %v", err))
		os.Exit(1)
	}

	// Lexing
	l := lexer.New(string(src))
	p := parser.New(l)
	program := p.ParseProgram()

	if len(p.Errors()) > 0 {
		for _, e := range p.Errors() {
			utils.Error(e)
		}
		os.Exit(1)
	}

	// Type checking
	c := checker.New()
	errors := c.Check(program)
	if len(errors) > 0 {
		for _, e := range errors {
			utils.Error(e)
		}
		os.Exit(1)
	}

	// Memory safety checking (Ownership & Move semantics)
	safetyChecker := safety.NewOwnershipChecker()
	safetyErrors := safetyChecker.Check(program)
	if len(safetyErrors) > 0 {
		for _, e := range safetyErrors {
			utils.Error(fmt.Sprintf("Safety error: %s", e))
		}
		os.Exit(1)
	}

	// Generate SSA IR (thay vì 3-address code)
	ssaGen := ssagen.New()
	ssaProgram, err := ssaGen.Generate(program)
	if err != nil {
		utils.Error(fmt.Sprintf("SSA IR generation error: %v", err))
		os.Exit(1)
	}

	// Optimize SSA IR (Constant Folding & Dead Code Elimination)
	opt := optimizer.New()
	ssaProgram, err = opt.Optimize(ssaProgram)
	if err != nil {
		utils.Error(fmt.Sprintf("Optimization error: %v", err))
		os.Exit(1)
	}

	// Generate Assembly from SSA IR
	asmGen := asmgen.New()
	asmCode, err := asmGen.GenerateFromSSA(ssaProgram)
	if err != nil {
		utils.Error(fmt.Sprintf("Assembly generation error: %v", err))
		os.Exit(1)
	}

	// Write assembly to temp file
	tempDir := filepath.Join(os.TempDir(), "astral_temp")
	os.MkdirAll(tempDir, os.ModePerm)
	asmFile := filepath.Join(tempDir, "output.asm")
	objFile := filepath.Join(tempDir, "output.o")

	if err := os.WriteFile(asmFile, []byte(asmCode), 0644); err != nil {
		utils.Error(fmt.Sprintf("Error writing assembly file: %v", err))
		os.Exit(1)
	}

	// Step 1: Compile assembly to object file using Astral Assembler
	asmblr := assembler.New()
	if err := asmblr.Assemble(asmCode, objFile); err != nil {
		utils.Error(fmt.Sprintf("Assembly error: %v", err))
		fmt.Printf("Assembly file (for debugging): %s\n", asmFile)
		os.Exit(1)
	}

	// Step 2: Link object file to executable using Astral Linker
	lnkr := linker.New()
	if err := lnkr.Link([]string{objFile}, outputFile, "_start"); err != nil {
		utils.Error(fmt.Sprintf("Linking error: %v", err))
		os.Exit(1)
	}

	// Clean up temp files
	os.Remove(asmFile)
	os.Remove(objFile)

	fmt.Printf("Compiled to %s successfully! Run %s\n", outputFile, outputFile)
}
