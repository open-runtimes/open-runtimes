package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

/*
   'req' variable has:
       'headers' - object with request headers
       'payload' - object with request body data
       'variables' - object with function variables
   'res' variable has:
       'send(text, status)' - function to return text response. Status code defaults to 200
       'json(obj, status)' - function to return JSON response. Status code defaults to 200

   If an error is thrown, a response with code 500 will be returned.
*/

func Main(req Request, res Response) error {

	var id string
	if id = req.Payload; id == "" || id == "{}" {
		id = "1"
	}

	headerData := req.Headers["x-test-header"]
	variableData := req.Variables["test-variables"]

	todoResp, err := http.Get("https://jsonplaceholder.typicode.com/todos/" + id)
	if err != nil {
		return err
	}

	todoBody, err := io.ReadAll(todoResp.Body)
	if err != nil {
		return err
	}

	var f interface{}
	if err := json.Unmarshal([]byte(todoBody), &f); err != nil {
		return err
	}

	// Type asserting to get the data
	todo, _ := f.(map[string]interface{})

	fmt.Println("log1")
	fmt.Println("{hello: world}")
	fmt.Println("[hello, world]")

	// Using maps to respond with all the necessary data
	data := make(map[string]interface{})
	data["isTest"] = true
	data["message"] = "Hello Open Runtimes 👋"
	data["header"] = headerData
	data["variables"] = variableData
	data["todo"] = todo

	return nil
}
