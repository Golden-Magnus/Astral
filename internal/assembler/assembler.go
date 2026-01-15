package assembler

import (
	"encoding/binary"
	"fmt"
	"os"
	"runtime"
	"strings"
)

// Assembler - Compile assembly to object file
// Supports COFF (Windows) and ELF (Linux) formats

type Assembler struct {
	symbols        map[string]*Symbol
	sections       []*Section
	currentSection *Section
}

type Symbol struct {
	Name    string
	Value   uint64
	Section int
	Type    int // 0 = undefined, 1 = function, 2 = data
	Size    uint64
}

type Section struct {
	Name   string
	Data   []byte
	Relocs []*Relocation
}

type Relocation struct {
	Offset uint64
	Symbol string
	Type   int
}

type Instruction struct {
	Opcode   []byte
	Operands []Operand
}

type Operand struct {
	Type  int // 0 = register, 1 = immediate, 2 = memory
	Value interface{}
}

func New() *Assembler {
	return &Assembler{
		symbols:  make(map[string]*Symbol),
		sections: []*Section{},
	}
}

// Assemble compiles assembly code to object file
func (a *Assembler) Assemble(asmCode string, outputFile string) error {
	lines := strings.Split(asmCode, "\n")

	// Simple parser - parse assembly line by line
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, ";") {
			continue
		}

		if err := a.parseLine(line); err != nil {
			return fmt.Errorf("parse error: %v", err)
		}
	}

	// Generate object file
	goos := runtime.GOOS
	if goos == "windows" {
		return a.writeCOFF(outputFile)
	} else {
		return a.writeELF(outputFile)
	}
}

func (a *Assembler) parseLine(line string) error {
	// Simple parser - handle basic cases
	parts := strings.Fields(line)
	if len(parts) == 0 {
		return nil
	}

	// Section directive
	if parts[0] == "section" && len(parts) > 1 {
		sectionName := strings.Trim(parts[1], ".")
		a.currentSection = &Section{
			Name:   sectionName,
			Data:   []byte{},
			Relocs: []*Relocation{},
		}
		a.sections = append(a.sections, a.currentSection)
		return nil
	}

	// Label (ends with :)
	if strings.HasSuffix(parts[0], ":") {
		label := strings.TrimSuffix(parts[0], ":")
		if a.currentSection == nil {
			// Default to .text
			a.currentSection = &Section{
				Name:   "text",
				Data:   []byte{},
				Relocs: []*Relocation{},
			}
			a.sections = append(a.sections, a.currentSection)
		}
		a.symbols[label] = &Symbol{
			Name:    label,
			Value:   uint64(len(a.currentSection.Data)),
			Section: len(a.sections) - 1,
			Type:    1, // Function
		}
		return nil
	}

	// Instruction
	if a.currentSection == nil {
		a.currentSection = &Section{
			Name:   "text",
			Data:   []byte{},
			Relocs: []*Relocation{},
		}
		a.sections = append(a.sections, a.currentSection)
	}

	// Encode instruction
	encoded, err := a.encodeInstruction(parts)
	if err != nil {
		return err
	}
	a.currentSection.Data = append(a.currentSection.Data, encoded...)

	return nil
}

func (a *Assembler) encodeInstruction(parts []string) ([]byte, error) {
	if len(parts) == 0 {
		return nil, nil
	}

	op := parts[0]

	// Simple encoding for common x86-64 instructions
	switch op {
	case "mov":
		return a.encodeMov(parts[1:])
	case "add":
		return a.encodeAdd(parts[1:])
	case "sub":
		return a.encodeSub(parts[1:])
	case "push":
		return a.encodePush(parts[1:])
	case "pop":
		return a.encodePop(parts[1:])
	case "ret":
		return []byte{0xC3}, nil // RET
	case "syscall":
		return []byte{0x0F, 0x05}, nil // SYSCALL
	case "jmp":
		return a.encodeJmp(parts[1:])
	case "jnz", "jne":
		return a.encodeJcc(parts[1:], 0x75) // JNZ
	case "jz", "je":
		return a.encodeJcc(parts[1:], 0x74) // JZ
	default:
		// Unknown instruction - return placeholder
		return []byte{0x90}, nil // NOP
	}
}

func (a *Assembler) encodeMov(operands []string) ([]byte, error) {
	// Simplified - handle mov rax, imm64
	if len(operands) >= 2 {
		if strings.HasPrefix(operands[0], "rax") && strings.HasPrefix(operands[1], "$") {
			// mov rax, $imm
			imm := operands[1][1:]
			var val int64
			fmt.Sscanf(imm, "%d", &val)
			code := []byte{0x48, 0xB8} // MOV RAX, imm64
			immBytes := make([]byte, 8)
			binary.LittleEndian.PutUint64(immBytes, uint64(val))
			code = append(code, immBytes...)
			return code, nil
		}
	}
	return []byte{0x90}, nil // NOP placeholder
}

func (a *Assembler) encodeAdd(_ []string) ([]byte, error) {
	// Simplified encoding
	return []byte{0x48, 0x01}, nil // ADD placeholder
}

func (a *Assembler) encodeSub(_ []string) ([]byte, error) {
	// Simplified encoding
	return []byte{0x48, 0x29}, nil // SUB placeholder
}

func (a *Assembler) encodePush(operands []string) ([]byte, error) {
	if len(operands) > 0 && strings.HasPrefix(operands[0], "rbp") {
		return []byte{0x55}, nil // PUSH RBP
	}
	return []byte{0x90}, nil
}

func (a *Assembler) encodePop(operands []string) ([]byte, error) {
	if len(operands) > 0 && strings.HasPrefix(operands[0], "rbp") {
		return []byte{0x5D}, nil // POP RBP
	}
	return []byte{0x90}, nil
}

func (a *Assembler) encodeJmp(operands []string) ([]byte, error) {
	// JMP with relocation
	if len(operands) > 0 {
		label := operands[0]
		code := []byte{0xE9, 0x00, 0x00, 0x00, 0x00} // JMP rel32
		if a.currentSection != nil {
			reloc := &Relocation{
				Offset: uint64(len(a.currentSection.Data)),
				Symbol: label,
				Type:   1, // R_X86_64_PC32
			}
			a.currentSection.Relocs = append(a.currentSection.Relocs, reloc)
		}
		return code, nil
	}
	return []byte{0x90}, nil
}

func (a *Assembler) encodeJcc(operands []string, opcode byte) ([]byte, error) {
	// Conditional jump
	if len(operands) > 0 {
		label := operands[0]
		code := []byte{opcode, 0x00, 0x00, 0x00, 0x00} // Jcc rel32
		if a.currentSection != nil {
			reloc := &Relocation{
				Offset: uint64(len(a.currentSection.Data)) + 1,
				Symbol: label,
				Type:   1,
			}
			a.currentSection.Relocs = append(a.currentSection.Relocs, reloc)
		}
		return code, nil
	}
	return []byte{0x90}, nil
}

// writeCOFF writes Windows COFF object file
func (a *Assembler) writeCOFF(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// COFF header (simplified)
	header := make([]byte, 20)
	// Machine: IMAGE_FILE_MACHINE_AMD64
	binary.LittleEndian.PutUint16(header[0:2], 0x8664)
	// Number of sections
	binary.LittleEndian.PutUint16(header[2:4], uint16(len(a.sections)))
	// Timestamp
	binary.LittleEndian.PutUint32(header[4:8], 0)
	// Pointer to symbol table
	symTableOffset := 20 + uint32(len(a.sections)*40) // Section headers
	binary.LittleEndian.PutUint32(header[8:12], symTableOffset)
	// Number of symbols
	binary.LittleEndian.PutUint32(header[12:16], uint32(len(a.symbols)))
	// Size of optional header (0 for object files)
	binary.LittleEndian.PutUint16(header[16:18], 0)
	// Characteristics
	binary.LittleEndian.PutUint16(header[18:20], 0x0104) // IMAGE_FILE_32BIT_MACHINE | IMAGE_FILE_DEBUG_STRIPPED

	file.Write(header)

	// Write section headers and data
	for _, section := range a.sections {
		// Section header (40 bytes)
		secHeader := make([]byte, 40)
		copy(secHeader[0:8], []byte(section.Name))
		// Virtual size
		binary.LittleEndian.PutUint32(secHeader[8:12], uint32(len(section.Data)))
		// Virtual address
		binary.LittleEndian.PutUint32(secHeader[12:16], 0)
		// Size of raw data
		binary.LittleEndian.PutUint32(secHeader[16:20], uint32(len(section.Data)))
		// Pointer to raw data (simplified - calculate after writing headers)
		rawDataOffset := uint32(20) + 40*uint32(len(a.sections))
		binary.LittleEndian.PutUint32(secHeader[20:24], rawDataOffset)
		// Characteristics
		if section.Name == "text" {
			binary.LittleEndian.PutUint32(secHeader[36:40], 0x60000020) // CODE | EXECUTE | READ
		} else {
			binary.LittleEndian.PutUint32(secHeader[36:40], 0x40000040) // INITIALIZED_DATA | READ
		}
		file.Write(secHeader)
	}

	// Write section data
	for _, section := range a.sections {
		file.Write(section.Data)
	}

	// Write symbol table (simplified)
	for _, sym := range a.symbols {
		symEntry := make([]byte, 18)
		copy(symEntry[0:8], []byte(sym.Name))
		binary.LittleEndian.PutUint32(symEntry[8:12], uint32(sym.Value))
		binary.LittleEndian.PutUint16(symEntry[12:14], uint16(sym.Section))
		binary.LittleEndian.PutUint16(symEntry[14:16], uint16(sym.Type))
		binary.LittleEndian.PutUint16(symEntry[16:18], 0) // Storage class
		file.Write(symEntry)
	}

	return nil
}

// writeELF writes Linux ELF object file
func (a *Assembler) writeELF(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// ELF header (64 bytes)
	header := make([]byte, 64)
	// ELF magic
	copy(header[0:4], []byte{0x7F, 'E', 'L', 'F'})
	// Class: 64-bit
	header[4] = 2
	// Data: little-endian
	header[5] = 1
	// Version
	header[6] = 1
	// OS ABI: System V
	header[7] = 0
	// Type: Relocatable
	binary.LittleEndian.PutUint16(header[16:18], 1)
	// Machine: x86-64
	binary.LittleEndian.PutUint16(header[18:20], 0x3E)
	// Version
	binary.LittleEndian.PutUint32(header[20:24], 1)
	// Entry point (0 for object files)
	binary.LittleEndian.PutUint64(header[24:32], 0)
	// Program header offset (0 for object files)
	binary.LittleEndian.PutUint64(header[32:40], 0)
	// Section header offset
	shOffset := 64 + uint64(len(a.sections)*64) // After headers
	binary.LittleEndian.PutUint64(header[40:48], shOffset)
	// Flags
	binary.LittleEndian.PutUint32(header[48:52], 0)
	// ELF header size
	binary.LittleEndian.PutUint16(header[52:54], 64)
	// Program header size
	binary.LittleEndian.PutUint16(header[54:56], 0)
	// Number of program headers
	binary.LittleEndian.PutUint16(header[56:58], 0)
	// Section header size
	binary.LittleEndian.PutUint16(header[58:60], 64)
	// Number of section headers
	binary.LittleEndian.PutUint16(header[60:62], uint16(len(a.sections)+1)) // +1 for null section
	// String table index
	binary.LittleEndian.PutUint16(header[62:64], 1)

	file.Write(header)

	// Write section data first
	dataOffset := uint64(64)
	for _, section := range a.sections {
		file.Write(section.Data)
		dataOffset += uint64(len(section.Data))
	}

	// Write section headers
	// Null section header
	nullHeader := make([]byte, 64)
	file.Write(nullHeader)

	// Section headers
	for i, section := range a.sections {
		sh := make([]byte, 64)
		// Name offset (simplified - use index)
		binary.LittleEndian.PutUint32(sh[0:4], uint32(i+1))
		// Type
		if section.Name == "text" {
			binary.LittleEndian.PutUint32(sh[4:8], 1) // SHT_PROGBITS
		} else {
			binary.LittleEndian.PutUint32(sh[4:8], 1) // SHT_PROGBITS
		}
		// Flags
		if section.Name == "text" {
			binary.LittleEndian.PutUint64(sh[8:16], 6) // SHF_ALLOC | SHF_EXECINSTR
		} else {
			binary.LittleEndian.PutUint64(sh[8:16], 3) // SHF_ALLOC | SHF_WRITE
		}
		// Address
		binary.LittleEndian.PutUint64(sh[16:24], 0)
		// Offset
		offset := 64 + uint64(i*64)
		binary.LittleEndian.PutUint64(sh[24:32], offset)
		// Size
		binary.LittleEndian.PutUint64(sh[32:40], uint64(len(section.Data)))
		// Link
		binary.LittleEndian.PutUint32(sh[40:44], 0)
		// Info
		binary.LittleEndian.PutUint32(sh[44:48], 0)
		// Align
		binary.LittleEndian.PutUint64(sh[48:56], 16)
		// Entsize
		binary.LittleEndian.PutUint64(sh[56:64], 0)
		file.Write(sh)
	}

	return nil
}
