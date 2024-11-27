// Binary partialconsumption demonstrates goroutine leak for partial
// consumption.
package main

import (
	"fmt"
	"runtime"
	"time"
)

func produce() <-chan int {
	c := make(chan int)
	go func(c chan<- int) {
		defer func() {
			if r := recover(); r != nil {
				fmt.Println("Recovered in goroutine spawned by produce", r)
			}
			fmt.Println("Finshied: goroutine spawned by produce")
		}()
		for i := 0; ; i++ {
			c <- i
		}
	}(c)
	return c
}

func consume() {
	for i := range produce() {
		fmt.Printf("Number of goroutines within consume loop: %d\n", runtime.NumGoroutine())
		fmt.Println(i)
		if i == 3 {
			return
		}
	}
}

func main() {
	fmt.Printf("Number of goroutines before consume: %d\n", runtime.NumGoroutine())
	consume()
	fmt.Printf("Number of goroutines after consume: %d\n", runtime.NumGoroutine())
	time.Sleep(10 * time.Second)
	fmt.Printf("Number of goroutines after sleep: %d\n", runtime.NumGoroutine())
	fmt.Println("Finished: main")
}
