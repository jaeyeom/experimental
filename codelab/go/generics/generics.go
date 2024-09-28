package main

import (
	"fmt"
)

type Number interface {
	int | float32
}

func Sum[V Number](vs []V) V {
	var total V
	for _, v := range vs {
		total += v
	}
	return total
}

func main() {
	fmt.Println(Sum([]int{10, 20, 30}))
}
