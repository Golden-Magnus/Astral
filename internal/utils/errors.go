package utils

import (
	"fmt"
	"os"
)

type ErrorInfo struct {
	Message  string
	Line     int
	Column   int
	Filename string
	Context  string
}

func Error(msg string) {
	fmt.Fprintf(os.Stderr, "error: %s\n", msg)
}

func ErrorWithLocation(msg string, line, column int, filename string) {
	fmt.Fprintf(os.Stderr, "%s:%d:%d: error: %s\n", filename, line, column, msg)
}

func ErrorWithContext(msg string, line, column int, filename, context string) {
	fmt.Fprintf(os.Stderr, "%s:%d:%d: error: %s\n", filename, line, column, msg)
	if context != "" {
		fmt.Fprintf(os.Stderr, "  %d | %s\n", line, context)
		fmt.Fprintf(os.Stderr, "     | %s^\n", repeat(" ", column-1))
	}
}

func Warning(msg string, line, column int, filename string) {
	fmt.Fprintf(os.Stderr, "%s:%d:%d: warning: %s\n", filename, line, column, msg)
}

func repeat(s string, n int) string {
	result := ""
	for i := 0; i < n; i++ {
		result += s
	}
	return result
}