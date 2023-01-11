package main

import (
	"encoding/json"
	"io"
	"net/http"
)

/*
   'req' variable has:
       'Headers' - map with request headers
       'Payload' - string with request body data
       'Variables' - map with function variables
   'res' variable has:
       'send(text, status)' - function to return text response.
       'json(obj, status)' - function to return JSON response.

	If an error occures, return it to server, server will handle it.

	If you want to use fmt.Println() or fmt.Printf(), etc to print stdout
	kindly use res.buffStdout buffer, to write into it. The server will handle
	then for you. Like as show below:
	   fmt.Fprintln(&res.buffStdout, "{hello: world}")
*/

func Main(req Request, res *Response) error {
	todoResp, err := http.Get("https://jsonplaceholder.typicode.com/todos/1")
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

	// Use maps to respond with all the necessary data
	data := make(map[string]interface{})

	data["message"] = "Hello Open Runtimes 👋"
	data["todo"] = todo

	res.json(data, 200)
	return nil
}
