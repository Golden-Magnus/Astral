package linker

import (
	"encoding/binary"
	"fmt"
	"os"
	"runtime"
)

// Linker - Link object files into executable
// Supports COFF (Windows) and ELF (Linux) formats

type Linker struct {
	objects    []*ObjectFile
	symbols    map[string]*Symbol
	sections   []*Section
	entryPoint string
}

type ObjectFile struct {
	Filename string
	Symbols  map[string]*Symbol
	Sections []*Section
}

type Symbol struct {
	Name    string
	Value   uint64
	Section int
	Type    int
	Size    uint64
}

type Section struct {
	Name   string
	Data   []byte
	Relocs []*Relocation
	Addr   uint64
}

type Relocation struct {
	Offset uint64
	Symbol string
	Type   int
}

func New() *Linker {
	return &Linker{
		objects:  []*ObjectFile{},
		symbols:  make(map[string]*Symbol),
		sections: []*Section{},
	}
}

// Link links object files into executable
func (l *Linker) Link(objectFiles []string, outputFile string, entryPoint string) error {
	l.entryPoint = entryPoint

	// Load object files
	for _, objFile := range objectFiles {
		obj, err := l.loadObjectFile(objFile)
		if err != nil {
			return fmt.Errorf("failed to load %s: %v", objFile, err)
		}
		l.objects = append(l.objects, obj)
	}

	// Resolve symbols
	if err := l.resolveSymbols(); err != nil {
		return err
	}

	// Layout sections
	if err := l.layoutSections(); err != nil {
		return err
	}

	// Apply relocations
	if err := l.applyRelocations(); err != nil {
		return err
	}

	// Write executable
	goos := runtime.GOOS
	if goos == "windows" {
		return l.writePE(outputFile)
	} else {
		return l.writeELFExecutable(outputFile)
	}
}

func (l *Linker) loadObjectFile(filename string) (*ObjectFile, error) {
	// Simplified - in real implementation would parse COFF/ELF
	obj := &ObjectFile{
		Filename: filename,
		Symbols:  make(map[string]*Symbol),
		Sections: []*Section{},
	}
	// For now, return empty object
	return obj, nil
}

func (l *Linker) resolveSymbols() error {
	// Collect all symbols from all object files
	for _, obj := range l.objects {
		for name, sym := range obj.Symbols {
			if existing, exists := l.symbols[name]; exists {
				if existing.Type == 0 { // Undefined
					l.symbols[name] = sym
				} else if sym.Type == 0 {
					// Keep existing
				} else {
					return fmt.Errorf("duplicate symbol: %s", name)
				}
			} else {
				l.symbols[name] = sym
			}
		}
	}

	// Check for undefined symbols
	for name, sym := range l.symbols {
		if sym.Type == 0 {
			return fmt.Errorf("undefined symbol: %s", name)
		}
	}

	return nil
}

func (l *Linker) layoutSections() error {
	// Simple layout: merge sections of same type
	textSection := &Section{
		Name:   ".text",
		Data:   []byte{},
		Relocs: []*Relocation{},
		Addr:   0x400000, // Base address
	}
	dataSection := &Section{
		Name:   ".data",
		Data:   []byte{},
		Relocs: []*Relocation{},
		Addr:   0x500000,
	}

	for _, obj := range l.objects {
		for _, sec := range obj.Sections {
			if sec.Name == "text" || sec.Name == ".text" {
				offset := uint64(len(textSection.Data))
				textSection.Data = append(textSection.Data, sec.Data...)
				// Adjust relocations
				for _, reloc := range sec.Relocs {
					reloc.Offset += offset
					textSection.Relocs = append(textSection.Relocs, reloc)
				}
			} else {
				offset := uint64(len(dataSection.Data))
				dataSection.Data = append(dataSection.Data, sec.Data...)
				for _, reloc := range sec.Relocs {
					reloc.Offset += offset
					dataSection.Relocs = append(dataSection.Relocs, reloc)
				}
			}
		}
	}

	l.sections = []*Section{textSection, dataSection}
	return nil
}

func (l *Linker) applyRelocations() error {
	// Apply relocations to resolve symbol references
	for _, section := range l.sections {
		for _, reloc := range section.Relocs {
			sym, exists := l.symbols[reloc.Symbol]
			if !exists {
				return fmt.Errorf("relocation to undefined symbol: %s", reloc.Symbol)
			}

			// Calculate address
			symAddr := sym.Value + section.Addr
			relocAddr := reloc.Offset + section.Addr

			// Write relocation (simplified)
			switch reloc.Type {
			case 1: // R_X86_64_PC32
				offset := int32(symAddr - relocAddr - 4)
				binary.LittleEndian.PutUint32(section.Data[reloc.Offset:reloc.Offset+4], uint32(offset))
			case 2: // R_X86_64_64
				binary.LittleEndian.PutUint64(section.Data[reloc.Offset:reloc.Offset+8], symAddr)
			}
		}
	}
	return nil
}

func (l *Linker) writePE(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// DOS stub
	dosStub := make([]byte, 64)
	copy(dosStub[0:2], []byte("MZ"))
	file.Write(dosStub)

	// PE signature
	file.Write([]byte("PE\x00\x00"))

	// COFF header
	coffHeader := make([]byte, 20)
	binary.LittleEndian.PutUint16(coffHeader[0:2], 0x8664) // Machine
	binary.LittleEndian.PutUint16(coffHeader[2:4], uint16(len(l.sections)))
	binary.LittleEndian.PutUint32(coffHeader[4:8], 0)        // Timestamp
	binary.LittleEndian.PutUint32(coffHeader[8:12], 0)       // Symbol table
	binary.LittleEndian.PutUint32(coffHeader[12:16], 0)      // Num symbols
	binary.LittleEndian.PutUint16(coffHeader[16:18], 0x010E) // Optional header size
	binary.LittleEndian.PutUint16(coffHeader[18:20], 0x0102) // Characteristics
	file.Write(coffHeader)

	// Optional header (PE32+)
	optHeader := make([]byte, 112)
	binary.LittleEndian.PutUint16(optHeader[0:2], 0x20B) // Magic (PE32+)
	binary.LittleEndian.PutUint32(optHeader[4:8], 0)     // Linker version
	binary.LittleEndian.PutUint32(optHeader[8:12], 0)    // Size of code
	binary.LittleEndian.PutUint32(optHeader[12:16], 0)   // Size of initialized data
	binary.LittleEndian.PutUint32(optHeader[16:20], 0)   // Size of uninitialized data
	entryPoint := uint64(0x1000)
	if l.entryPoint != "" {
		if sym, ok := l.symbols[l.entryPoint]; ok {
			entryPoint = sym.Value
		}
	}
	binary.LittleEndian.PutUint32(optHeader[16:20], uint32(entryPoint))
	binary.LittleEndian.PutUint64(optHeader[24:32], 0x400000) // Image base
	binary.LittleEndian.PutUint32(optHeader[56:60], 0x1000)   // Section alignment
	binary.LittleEndian.PutUint32(optHeader[60:64], 0x200)    // File alignment
	binary.LittleEndian.PutUint16(optHeader[64:66], 6)        // Subsystem (console)
	binary.LittleEndian.PutUint16(optHeader[66:68], 0)        // DLL characteristics
	file.Write(optHeader)

	// Write sections
	for _, section := range l.sections {
		file.Write(section.Data)
	}

	return nil
}

func (l *Linker) writeELFExecutable(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	// ELF header
	header := make([]byte, 64)
	copy(header[0:4], []byte{0x7F, 'E', 'L', 'F'})
	header[4] = 2                                      // 64-bit
	header[5] = 1                                      // Little-endian
	header[6] = 1                                      // Version
	binary.LittleEndian.PutUint16(header[16:18], 2)    // ET_EXEC
	binary.LittleEndian.PutUint16(header[18:20], 0x3E) // x86-64
	binary.LittleEndian.PutUint32(header[20:24], 1)    // Version
	entryPoint := uint64(0x400000)
	if l.entryPoint != "" {
		if sym, ok := l.symbols[l.entryPoint]; ok {
			entryPoint = sym.Value + 0x400000
		}
	}
	binary.LittleEndian.PutUint64(header[24:32], entryPoint)
	binary.LittleEndian.PutUint64(header[32:40], 64)                      // Program header offset
	binary.LittleEndian.PutUint64(header[40:48], 0)                       // Section header offset
	binary.LittleEndian.PutUint32(header[48:52], 0)                       // Flags
	binary.LittleEndian.PutUint16(header[52:54], 64)                      // ELF header size
	binary.LittleEndian.PutUint16(header[54:56], 56)                      // Program header size
	binary.LittleEndian.PutUint16(header[56:58], uint16(len(l.sections))) // Num program headers
	file.Write(header)

	// Program headers
	offset := uint64(64 + len(l.sections)*56)
	for _, section := range l.sections {
		ph := make([]byte, 56)
		if section.Name == ".text" {
			binary.LittleEndian.PutUint32(ph[0:4], 1) // PT_LOAD
			binary.LittleEndian.PutUint32(ph[4:8], 5) // PF_R | PF_X
			binary.LittleEndian.PutUint64(ph[8:16], offset)
			binary.LittleEndian.PutUint64(ph[16:24], section.Addr)
			binary.LittleEndian.PutUint64(ph[24:32], section.Addr)
			binary.LittleEndian.PutUint64(ph[32:40], uint64(len(section.Data)))
			binary.LittleEndian.PutUint64(ph[40:48], uint64(len(section.Data)))
			binary.LittleEndian.PutUint64(ph[48:56], 0x1000) // Align
		}
		file.Write(ph)
		offset += uint64(len(section.Data))
	}

	// Write section data
	for _, section := range l.sections {
		file.Write(section.Data)
	}

	return nil
}
