package optimizer

import (
	"astral/internal/ir"
)

// Optimizer - Performs optimization passes on SSA IR
type Optimizer struct {
	constants map[int]ConstantValue // SSA ID -> constant value
}

type ConstantValue struct {
	IsInt    bool
	IsFloat  bool
	IsBool   bool
	IsString bool
	IntVal   int64
	FloatVal float64
	BoolVal  bool
	StringVal string
}

func New() *Optimizer {
	return &Optimizer{
		constants: make(map[int]ConstantValue),
	}
}

// Optimize performs all optimization passes on the SSA program
func (o *Optimizer) Optimize(program *ir.SSAProgram) (*ir.SSAProgram, error) {
	// Perform multiple passes of constant folding until no more changes
	changed := true
	passes := 0
	maxPasses := 10 // Prevent infinite loops
	
	for changed && passes < maxPasses {
		o.buildConstantMap(program)
		changed = o.constantFolding(program)
		passes++
	}
	
	// Perform dead code elimination
	o.deadCodeElimination(program)
	
	return program, nil
}

// buildConstantMap builds a map of SSA IDs to their constant values
func (o *Optimizer) buildConstantMap(program *ir.SSAProgram) {
	o.constants = make(map[int]ConstantValue)
	
	for _, fn := range program.Functions {
		for _, block := range fn.Blocks {
			for _, instr := range block.Instrs {
				switch v := instr.(type) {
				case *ir.SSAConstInt:
					o.constants[v.Dest] = ConstantValue{
						IsInt:  true,
						IntVal: v.Val,
					}
				case *ir.SSAConstFloat:
					o.constants[v.Dest] = ConstantValue{
						IsFloat:  true,
						FloatVal: v.Val,
					}
				case *ir.SSAConstBool:
					o.constants[v.Dest] = ConstantValue{
						IsBool:  true,
						BoolVal: v.Val,
					}
				case *ir.SSAConstString:
					o.constants[v.Dest] = ConstantValue{
						IsString: true,
						StringVal: v.Val,
					}
				}
			}
		}
	}
}

// constantFolding evaluates constant expressions at compile time
// Returns true if any changes were made
func (o *Optimizer) constantFolding(program *ir.SSAProgram) bool {
	changed := false
	
	for _, fn := range program.Functions {
		for _, block := range fn.Blocks {
			newInstrs := []ir.SSAInstruction{}
			
			for _, instr := range block.Instrs {
				switch v := instr.(type) {
				case *ir.SSABinOp:
					// Try to fold binary operations with constants
					if folded := o.foldBinOp(v); folded != nil {
						newInstrs = append(newInstrs, folded)
						changed = true
					} else {
						newInstrs = append(newInstrs, v)
					}
				case *ir.SSAUnOp:
					// Try to fold unary operations with constants
					if folded := o.foldUnOp(v); folded != nil {
						newInstrs = append(newInstrs, folded)
						changed = true
					} else {
						newInstrs = append(newInstrs, v)
					}
				case *ir.SSACondJump:
					// Try to fold conditional jumps with constant conditions
					if folded := o.foldCondJump(v, block); folded != nil {
						newInstrs = append(newInstrs, folded)
						changed = true
					} else {
						newInstrs = append(newInstrs, v)
					}
				default:
					newInstrs = append(newInstrs, instr)
				}
			}
			
			block.Instrs = newInstrs
		}
	}
	
	return changed
}

// foldBinOp folds a binary operation if both operands are constants
func (o *Optimizer) foldBinOp(op *ir.SSABinOp) ir.SSAInstruction {
	left, leftOk := o.constants[op.Left]
	right, rightOk := o.constants[op.Right]
	
	if !leftOk || !rightOk {
		return nil
	}
	
	switch op.Op {
	case "+":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstInt{Dest: op.Dest, Val: left.IntVal + right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstFloat{Dest: op.Dest, Val: left.FloatVal + right.FloatVal}
		}
	case "-":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstInt{Dest: op.Dest, Val: left.IntVal - right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstFloat{Dest: op.Dest, Val: left.FloatVal - right.FloatVal}
		}
	case "*":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstInt{Dest: op.Dest, Val: left.IntVal * right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstFloat{Dest: op.Dest, Val: left.FloatVal * right.FloatVal}
		}
	case "/":
		if left.IsInt && right.IsInt {
			if right.IntVal != 0 {
				return &ir.SSAConstInt{Dest: op.Dest, Val: left.IntVal / right.IntVal}
			}
		}
		if left.IsFloat && right.IsFloat {
			if right.FloatVal != 0 {
				return &ir.SSAConstFloat{Dest: op.Dest, Val: left.FloatVal / right.FloatVal}
			}
		}
	case "%":
		if left.IsInt && right.IsInt {
			if right.IntVal != 0 {
				return &ir.SSAConstInt{Dest: op.Dest, Val: left.IntVal % right.IntVal}
			}
		}
	case "==":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.IntVal == right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.FloatVal == right.FloatVal}
		}
		if left.IsBool && right.IsBool {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.BoolVal == right.BoolVal}
		}
		if left.IsString && right.IsString {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.StringVal == right.StringVal}
		}
	case "!=":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.IntVal != right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.FloatVal != right.FloatVal}
		}
		if left.IsBool && right.IsBool {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.BoolVal != right.BoolVal}
		}
		if left.IsString && right.IsString {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.StringVal != right.StringVal}
		}
	case "<":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.IntVal < right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.FloatVal < right.FloatVal}
		}
	case ">":
		if left.IsInt && right.IsInt {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.IntVal > right.IntVal}
		}
		if left.IsFloat && right.IsFloat {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.FloatVal > right.FloatVal}
		}
	case "&&":
		if left.IsBool && right.IsBool {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.BoolVal && right.BoolVal}
		}
	case "||":
		if left.IsBool && right.IsBool {
			return &ir.SSAConstBool{Dest: op.Dest, Val: left.BoolVal || right.BoolVal}
		}
	}
	
	return nil
}

// foldUnOp folds a unary operation if the operand is a constant
func (o *Optimizer) foldUnOp(op *ir.SSAUnOp) ir.SSAInstruction {
	val, ok := o.constants[op.Val]
	if !ok {
		return nil
	}
	
	switch op.Op {
	case "-":
		if val.IsInt {
			return &ir.SSAConstInt{Dest: op.Dest, Val: -val.IntVal}
		}
		if val.IsFloat {
			return &ir.SSAConstFloat{Dest: op.Dest, Val: -val.FloatVal}
		}
	case "!":
		if val.IsBool {
			return &ir.SSAConstBool{Dest: op.Dest, Val: !val.BoolVal}
		}
	}
	
	return nil
}

// foldCondJump folds a conditional jump if the condition is a constant
func (o *Optimizer) foldCondJump(jump *ir.SSACondJump, block *ir.SSABasicBlock) ir.SSAInstruction {
	cond, ok := o.constants[jump.Cond]
	if !ok || !cond.IsBool {
		return nil
	}
	
	// Replace with unconditional jump
	if cond.BoolVal {
		return &ir.SSAJump{Target: jump.Then}
	} else {
		return &ir.SSAJump{Target: jump.Else}
	}
}

// deadCodeElimination removes unreachable blocks and unused definitions
func (o *Optimizer) deadCodeElimination(program *ir.SSAProgram) {
	for _, fn := range program.Functions {
		if len(fn.Blocks) == 0 {
			continue
		}
		
		// Build label -> block map
		labelToBlock := make(map[string]*ir.SSABasicBlock)
		for _, block := range fn.Blocks {
			labelToBlock[block.Label] = block
		}
		
		// Mark reachable blocks starting from entry block
		reachable := make(map[*ir.SSABasicBlock]bool)
		o.markReachable(fn.Blocks[0], reachable, labelToBlock)
		
		// Remove unreachable blocks
		newBlocks := []*ir.SSABasicBlock{}
		for _, block := range fn.Blocks {
			if reachable[block] {
				// Update predecessors and successors to only include reachable blocks
				newPreds := []*ir.SSABasicBlock{}
				for _, pred := range block.Preds {
					if reachable[pred] {
						newPreds = append(newPreds, pred)
					}
				}
				block.Preds = newPreds
				
				newSuccs := []*ir.SSABasicBlock{}
				for _, succ := range block.Succs {
					if reachable[succ] {
						newSuccs = append(newSuccs, succ)
					}
				}
				block.Succs = newSuccs
				
				newBlocks = append(newBlocks, block)
			}
		}
		fn.Blocks = newBlocks
		
		// Remove unused definitions (simplified - only removes unused constants)
		o.removeUnusedDefinitions(fn)
	}
}

// markReachable marks all blocks reachable from the given block
func (o *Optimizer) markReachable(block *ir.SSABasicBlock, reachable map[*ir.SSABasicBlock]bool, labelToBlock map[string]*ir.SSABasicBlock) {
	if reachable[block] {
		return
	}
	reachable[block] = true
	
	// Follow jumps and conditional jumps by label
	for _, instr := range block.Instrs {
		switch v := instr.(type) {
		case *ir.SSAJump:
			if target, ok := labelToBlock[v.Target]; ok {
				o.markReachable(target, reachable, labelToBlock)
			}
		case *ir.SSACondJump:
			if thenBlock, ok := labelToBlock[v.Then]; ok {
				o.markReachable(thenBlock, reachable, labelToBlock)
			}
			if elseBlock, ok := labelToBlock[v.Else]; ok {
				o.markReachable(elseBlock, reachable, labelToBlock)
			}
		}
	}
	
	// Follow successors
	for _, succ := range block.Succs {
		o.markReachable(succ, reachable, labelToBlock)
	}
}

// removeUnusedDefinitions removes definitions that are never used
func (o *Optimizer) removeUnusedDefinitions(fn *ir.SSAFunction) {
	// Count uses of each SSA ID
	uses := make(map[int]int)
	
	// Count uses in all blocks
	for _, block := range fn.Blocks {
		// Count uses in phi nodes
		for _, phi := range block.PhiNodes {
			for _, arg := range phi.Args {
				uses[arg]++
			}
		}
		
		// Count uses in instructions
		for _, instr := range block.Instrs {
			for _, useID := range instr.Uses() {
				uses[useID]++
			}
		}
	}
	
	// Mark definitions that are used (by parameters, return values, stores, calls)
	used := make(map[int]bool)
	for _, param := range fn.Params {
		used[param.ID] = true
	}
	
	for _, block := range fn.Blocks {
		for _, instr := range block.Instrs {
			// Instructions with side effects or outputs are always kept
			switch instr.(type) {
			case *ir.SSAStore, *ir.SSACall, *ir.SSAReturn:
				// These use values but we can't remove them
				for _, useID := range instr.Uses() {
					used[useID] = true
				}
			}
			
			// Return values are used
			for _, defID := range instr.Defs() {
				if uses[defID] > 0 || used[defID] {
					used[defID] = true
					// Mark all uses as used too (transitively)
					for _, useID := range instr.Uses() {
						used[useID] = true
					}
				}
			}
		}
	}
	
	// Remove unused constant definitions
	for _, block := range fn.Blocks {
		newInstrs := []ir.SSAInstruction{}
		for _, instr := range block.Instrs {
			// Keep all non-constant instructions and used constants
			defs := instr.Defs()
			if len(defs) == 0 {
				// Instructions without defs (store, call void, return, jumps) are kept
				newInstrs = append(newInstrs, instr)
			} else {
				// Check if any def is used
				keep := false
				for _, defID := range defs {
					if used[defID] || uses[defID] > 0 {
						keep = true
						break
					}
				}
				
				// Always keep non-constant instructions (they might have side effects)
				switch instr.(type) {
				case *ir.SSAConstInt, *ir.SSAConstFloat, *ir.SSAConstBool, *ir.SSAConstString:
					if keep {
						newInstrs = append(newInstrs, instr)
					}
					// Otherwise, remove unused constant
				default:
					// Keep non-constant instructions (they might have side effects or be needed)
					newInstrs = append(newInstrs, instr)
				}
			}
		}
		block.Instrs = newInstrs
	}
}
