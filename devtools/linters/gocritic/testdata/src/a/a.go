package a

func good(x int) int {
	if x > 0 {
		return 1
	} else if x < 0 {
		return -1
	}
	return 0
}

func bad(x int) int {
	if x > 0 {
		return 1
	} else { // want "elseif: can replace 'else {if cond {}}' with 'else if cond {}'"
		if x < 0 {
			return -1
		}
	}
	return 0
}

func dup(x int) bool {
	return x == 1 || x == 1 // want "dupSubExpr: suspicious identical LHS and RHS for `\\|\\|` operator"
}
