package linker

import (
	"encoding/binary"
	"fmt"
	"os"
	"runtime"
	"strings"
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
	obj := &ObjectFile{
		Filename: filename,
		Symbols:  make(map[string]*Symbol),
		Sections: []*Section{},
	}

	data, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read object file: %v", err)
	}

	if len(data) < 4 {
		return nil, fmt.Errorf("object file too small")
	}

	// Detect format: ELF or COFF
	if data[0] == 0x7F && data[1] == 'E' && data[2] == 'L' && data[3] == 'F' {
		return l.parseELFObject(data, obj)
	} else if len(data) >= 2 && binary.LittleEndian.Uint16(data[0:2]) == 0x8664 {
		// COFF/PE (check for AMD64 machine type)
		return l.parseCOFFObject(data, obj)
	}

	return nil, fmt.Errorf("unknown object file format")
}

func (l *Linker) parseELFObject(data []byte, obj *ObjectFile) (*ObjectFile, error) {
	if len(data) < 64 {
		return nil, fmt.Errorf("ELF header too small")
	}

	// Check 64-bit
	if data[4] != 2 {
		return nil, fmt.Errorf("only 64-bit ELF supported")
	}

	// Read ELF header
	shOffset := binary.LittleEndian.Uint64(data[40:48])
	numSections := binary.LittleEndian.Uint16(data[60:62])
	_ = binary.LittleEndian.Uint16(data[62:64]) // shStrTableIdx - for future use

	// Read section headers
	shSize := 64
	for i := uint16(0); i < numSections; i++ {
		shOff := int(shOffset) + int(i)*shSize
		if shOff+shSize > len(data) {
			continue
		}

		shData := data[shOff : shOff+shSize]
		shType := binary.LittleEndian.Uint32(shData[4:8])
		shOffset := binary.LittleEndian.Uint64(shData[24:32])
		shSize := binary.LittleEndian.Uint64(shData[32:40])

		// Read section name from string table (simplified - use name offset directly)
		nameOffset := binary.LittleEndian.Uint32(shData[0:4])
		// Find string table section to read names from
		sectionName := fmt.Sprintf("section_%d", i)
		if nameOffset > 0 {
			// In real implementation, would read from .shstrtab section
			sectionName = l.readELFString(data, int(nameOffset))
		}

		// Skip null section
		if sectionName == "" && shType == 0 {
			continue
		}

		// Read section data
		secData := []byte{}
		if shOffset > 0 && shSize > 0 && int(shOffset+shSize) <= len(data) {
			secData = data[shOffset : shOffset+shSize]
		}

		section := &Section{
			Name:   sectionName,
			Data:   secData,
			Relocs: []*Relocation{},
			Addr:   0,
		}
		obj.Sections = append(obj.Sections, section)

		// Extract symbols from .symtab section if present
		if sectionName == ".symtab" {
			l.parseELFSymbolTable(secData, obj)
		}
	}

	return obj, nil
}

func (l *Linker) parseCOFFObject(data []byte, obj *ObjectFile) (*ObjectFile, error) {
	if len(data) < 20 {
		return nil, fmt.Errorf("COFF header too small")
	}

	// Read COFF header
	numSections := binary.LittleEndian.Uint16(data[2:4])
	symTableOffset := binary.LittleEndian.Uint32(data[8:12])
	numSymbols := binary.LittleEndian.Uint32(data[12:16])

	// Read section headers (40 bytes each)
	secHeadersOffset := 20
	for i := uint16(0); i < numSections; i++ {
		shOff := secHeadersOffset + int(i)*40
		if shOff+40 > len(data) {
			continue
		}

		shData := data[shOff : shOff+40]
		sectionName := string(shData[0:8])
		// Trim null bytes
		sectionName = strings.TrimRight(sectionName, "\x00")

		rawDataPtr := binary.LittleEndian.Uint32(shData[20:24])
		rawDataSize := binary.LittleEndian.Uint32(shData[16:20])

		// Read section data
		secData := []byte{}
		if rawDataPtr > 0 && rawDataSize > 0 && int(rawDataPtr+rawDataSize) <= len(data) {
			secData = data[rawDataPtr : rawDataPtr+rawDataSize]
		}

		section := &Section{
			Name:   sectionName,
			Data:   secData,
			Relocs: []*Relocation{},
			Addr:   0,
		}
		obj.Sections = append(obj.Sections, section)
	}

	// Parse symbol table if present
	if symTableOffset > 0 && numSymbols > 0 {
		l.parseCOFFSymbolTable(data, int(symTableOffset), int(numSymbols), obj)
	}

	return obj, nil
}

func (l *Linker) parseELFSymbolTable(data []byte, obj *ObjectFile) {
	// ELF symbol table entry is 24 bytes
	entrySize := 24
	numEntries := len(data) / entrySize

	for i := 0; i < numEntries; i++ {
		off := i * entrySize
		if off+entrySize > len(data) {
			break
		}

		entry := data[off : off+entrySize]
		nameOffset := binary.LittleEndian.Uint32(entry[0:4])
		symType := entry[4] & 0xF
		symValue := binary.LittleEndian.Uint64(entry[8:16])
		shIndex := binary.LittleEndian.Uint16(entry[6:8])

		if nameOffset == 0 {
			continue
		}

		// Simplified: read name (in real implementation would use string table)
		name := fmt.Sprintf("sym_%d", i)

		sym := &Symbol{
			Name:    name,
			Value:   symValue,
			Section: int(shIndex),
			Type:    int(symType),
			Size:    0,
		}
		obj.Symbols[name] = sym
	}
}

func (l *Linker) parseCOFFSymbolTable(data []byte, offset, numSymbols int, obj *ObjectFile) {
	// COFF symbol entry is 18 bytes
	entrySize := 18
	pos := offset

	for i := 0; i < numSymbols; i++ {
		if pos+entrySize > len(data) {
			break
		}

		entry := data[pos : pos+entrySize]
		nameBytes := entry[0:8]
		name := strings.TrimRight(string(nameBytes), "\x00")
		value := uint64(binary.LittleEndian.Uint32(entry[8:12]))
		section := int(binary.LittleEndian.Uint16(entry[12:14]))
		symType := int(binary.LittleEndian.Uint16(entry[14:16]))

		if name == "" || name[0] == 0 {
			continue
		}

		sym := &Symbol{
			Name:    name,
			Value:   value,
			Section: section,
			Type:    symType,
			Size:    0,
		}
		obj.Symbols[name] = sym

		pos += entrySize
	}
}

func (l *Linker) readELFString(data []byte, offset int) string {
	if offset < 0 || offset >= len(data) {
		return ""
	}
	end := offset
	for end < len(data) && data[end] != 0 {
		end++
	}
	return string(data[offset:end])
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
