package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
)

func Main(req Request, res *Response) error {

	var id string
	if id = req.Payload; id == "{}" || id == "" {
		fmt.Println("Got to sting type {}")
		id = "1"

	} else {
		temp := id[1 : len(id)-1]
		temp = strings.Split(temp, ":")[1]
		id = temp[1 : len(temp)-1]
	}

	headerData := req.Headers["x-test-header"]
	variableData := req.Variables["test-variable"]

	todoResp, err := http.Get("https://jsonplaceholder.typicode.com/todos/" + id)
	if err != nil {
		return err
	}

	todoBody, err := io.ReadAll(todoResp.Body)
	if err != nil {
		return err
	}

	// As the response we're going to receive from the above url is unstructured
	// use interface{}
	var f interface{}
	if err := json.Unmarshal([]byte(todoBody), &f); err != nil {
		return err
	}

	// Type asserting to get the data
	todo, _ := f.(map[string]interface{})

	fmt.Fprintln(&res.buffStdout, "log1")
	fmt.Fprintln(&res.buffStdout, "{hello: world}")
	fmt.Fprintln(&res.buffStdout, "[hello, world]")

	// Use maps to respond with all the necessary data
	data := make(map[string]interface{})
	data["isTest"] = true
	data["message"] = "Hello Open Runtimes 👋"
	data["header"] = headerData
	data["variable"] = variableData
	data["todo"] = todo

	res.json(data, 200)
	return nil
}
