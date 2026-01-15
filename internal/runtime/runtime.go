//go:build ignore
// +build ignore

// Minimal runtime for Astral
// This will be compiled separately and linked with generated assembly

package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"unsafe"
)

// Built-in functions that will be called from generated code
// These are implemented in Go for now, but could be in assembly later
// They are exported for linking with generated assembly code

//export astral_print
func astral_print(s *byte, len int) {
	// Print string
	fmt.Print(string((*[1 << 30]byte)(unsafe.Pointer(s))[:len:len]))
}

//export astral_read
func astral_read(prompt *byte, promptLen int, buf *byte, bufLen int) int {
	if promptLen > 0 {
		fmt.Print(string((*[1 << 30]byte)(unsafe.Pointer(prompt))[:promptLen:promptLen]))
	}
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')
	input = strings.TrimSpace(input)

	// Copy to buffer
	copyLen := len(input)
	if copyLen > bufLen-1 {
		copyLen = bufLen - 1
	}
	copy((*[1 << 30]byte)(unsafe.Pointer(buf))[:copyLen:copyLen], []byte(input))
	(*[1 << 30]byte)(unsafe.Pointer(buf))[copyLen] = 0
	return copyLen
}

//export astral_exit
func astral_exit(code int) {
	os.Exit(code)
}

// Keep references to prevent "unused" warnings
// These functions will be called from generated assembly
var _ = astral_print
var _ = astral_read
var _ = astral_exit

func main() {
	// This is just for building the runtime library
	// The actual main will be in generated assembly
}
