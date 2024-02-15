package main

import (
	"fmt"
	"net/http"
	"net/url"
	"strings"
)

func action(w http.ResponseWriter, r *http.Request) error {
	// fmt.Fprintf(w, "Hello!")
	// return errors.New("This is bad")

	return nil
}

func main() {
	handler := func(w http.ResponseWriter, r *http.Request) {
		err := action(w, r)
		if err != nil {
			errors := []string{
				err.Error(),
			}

			w.Header().Set("x-open-runtimes-errors", url.QueryEscape(strings.Join(errors, "\n")))
			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("content-type", "text/plain")
			fmt.Fprintf(w, "")
		}
	}

	err := http.ListenAndServe(":3000", http.HandlerFunc(handler))

	if err != nil {
		panic(err)
	}
}

/*
Execute user code:
awaitCh := make(chan struct{})

go func() {
	handler.Main()
	awaitCh <- struct{}{}
}() // b

<- awaitCh

fmt.Println("Finished")



Timeout a coroutine:

import "fmt"
import "time"

func timeout(ch chan string) {
	time.Sleep(1000 * time.Millisecond)
	ch <- "FAIL"
}

func execute(ch chan string) {
	time.Sleep(500 * time.Millisecond)
	ch <- "OK"
}

func main() {
	done := make(chan string)
	go timeout(done)
	go execute(done)
	response := <- done
	fmt.Printf("%s\n", response)
}
*/
