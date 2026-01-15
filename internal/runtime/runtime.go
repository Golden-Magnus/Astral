//go:build ignore
// +build ignore

// Minimal runtime for Astral
// This will be compiled separately and linked with generated assembly

package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
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

// Math functions
//export astral_abs
func astral_abs(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

//export astral_min
func astral_min(a, b int64) int64 {
	if a < b {
		return a
	}
	return b
}

//export astral_max
func astral_max(a, b int64) int64 {
	if a > b {
		return a
	}
	return b
}

//export astral_sqrt
func astral_sqrt(x float64) float64 {
	return math.Sqrt(x)
}

//export astral_pow
func astral_pow(base, exp float64) float64 {
	return math.Pow(base, exp)
}

// String functions
//export astral_strlen
func astral_strlen(s *byte, len int) int {
	return len
}

//export astral_strconcat
func astral_strconcat(s1 *byte, len1 int, s2 *byte, len2 int, buf *byte, bufLen int) int {
	str1 := string((*[1 << 30]byte)(unsafe.Pointer(s1))[:len1:len1])
	str2 := string((*[1 << 30]byte)(unsafe.Pointer(s2))[:len2:len2])
	result := str1 + str2
	
	copyLen := len(result)
	if copyLen > bufLen-1 {
		copyLen = bufLen - 1
	}
	copy((*[1 << 30]byte)(unsafe.Pointer(buf))[:copyLen:copyLen], []byte(result))
	(*[1 << 30]byte)(unsafe.Pointer(buf))[copyLen] = 0
	return copyLen
}

//export astral_substring
func astral_substring(s *byte, len int, start, end int, buf *byte, bufLen int) int {
	str := string((*[1 << 30]byte)(unsafe.Pointer(s))[:len:len])
	if start < 0 {
		start = 0
	}
	if end > len {
		end = len
	}
	if start > end {
		start = end
	}
	
	result := str[start:end]
	copyLen := len(result)
	if copyLen > bufLen-1 {
		copyLen = bufLen - 1
	}
	copy((*[1 << 30]byte)(unsafe.Pointer(buf))[:copyLen:copyLen], []byte(result))
	(*[1 << 30]byte)(unsafe.Pointer(buf))[copyLen] = 0
	return copyLen
}

// Type conversion functions
//export astral_toInt
func astral_toInt(s *byte, len int) int64 {
	str := strings.TrimSpace(string((*[1 << 30]byte)(unsafe.Pointer(s))[:len:len]))
	val, err := strconv.ParseInt(str, 10, 64)
	if err != nil {
		return 0
	}
	return val
}

//export astral_toFloat
func astral_toFloat(s *byte, len int) float64 {
	str := strings.TrimSpace(string((*[1 << 30]byte)(unsafe.Pointer(s))[:len:len]))
	val, err := strconv.ParseFloat(str, 64)
	if err != nil {
		return 0.0
	}
	return val
}

//export astral_toString
func astral_toString(val int64, buf *byte, bufLen int) int {
	str := strconv.FormatInt(val, 10)
	copyLen := len(str)
	if copyLen > bufLen-1 {
		copyLen = bufLen - 1
	}
	copy((*[1 << 30]byte)(unsafe.Pointer(buf))[:copyLen:copyLen], []byte(str))
	(*[1 << 30]byte)(unsafe.Pointer(buf))[copyLen] = 0
	return copyLen
}

//export astral_toStringFloat
func astral_toStringFloat(val float64, buf *byte, bufLen int) int {
	str := strconv.FormatFloat(val, 'f', -1, 64)
	copyLen := len(str)
	if copyLen > bufLen-1 {
		copyLen = bufLen - 1
	}
	copy((*[1 << 30]byte)(unsafe.Pointer(buf))[:copyLen:copyLen], []byte(str))
	(*[1 << 30]byte)(unsafe.Pointer(buf))[copyLen] = 0
	return copyLen
}

//export astral_toBool
func astral_toBool(s *byte, len int) int {
	str := strings.TrimSpace(strings.ToLower(string((*[1 << 30]byte)(unsafe.Pointer(s))[:len:len])))
	if str == "true" || str == "1" || str == "yes" {
		return 1
	}
	return 0
}

// Keep references to prevent "unused" warnings
// These functions will be called from generated assembly
var _ = astral_print
var _ = astral_read
var _ = astral_exit
var _ = astral_abs
var _ = astral_min
var _ = astral_max
var _ = astral_sqrt
var _ = astral_pow
var _ = astral_strlen
var _ = astral_strconcat
var _ = astral_substring
var _ = astral_toInt
var _ = astral_toFloat
var _ = astral_toString
var _ = astral_toStringFloat
var _ = astral_toBool

func main() {
	// This is just for building the runtime library
	// The actual main will be in generated assembly
}
