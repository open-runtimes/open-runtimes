package main

import (
	"encoding/json"
	"io"
	"net/http"
)

/*
   'req' variable has:
       'headers' - object with request headers
       'payload' - object with request body data
       'variables' - object with function variables
   'res' variable has:
       'send(text, status)' - function to return text response.
       'json(obj, status)' - function to return JSON response.

   If an error is thrown, a response with code 500 will be returned.
*/

func Main(req Request, res *Response) error {
	todo, err := http.Get("https://jsonplaceholder.typicode.com/todos/1")
	if err != nil {
		return err
	}

	body, err := io.ReadAll(todo.Body)
	if err != nil {
		return err
	}

	// As the JSON we're receiving is unstructured. So using interface{}
	var f interface{}
	if err := json.Unmarshal([]byte(body), &f); err != nil {
		return err
	}

	// Type asserting to get the data
	resp_todo, _ := f.(map[string]interface{})

	// Using maps to send the all the necessary data
	data := make(map[string]interface{})

	data["message"] = "Hello from Openruntime"
	data["todo"] = resp_todo

	res.json(data, 200)
	return nil
}
