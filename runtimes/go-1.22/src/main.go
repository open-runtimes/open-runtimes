package main

import "fmt"
import "openruntimes/handler"

func main() {
	awaitCh := make(chan struct{})

	go func() {
		handler.Main()
		awaitCh <- struct{}{}
	}() // b

 	<- awaitCh

	fmt.Println("Finished")
}
