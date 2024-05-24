package handler

import (
	"encoding/json"
	"openruntimes/types"

	"github.com/go-resty/resty/v2"
)

type RequestBody struct {
	Id string `json:"id"`
}

type TodoObject struct {
	UserId    int    `json:"userId"`
	Id        int    `json:"id"`
	Title     string `json:"title"`
	Completed bool   `json:"completed"`
}

type Response struct {
	Message string     `json:"message"`
	Todo    TodoObject `json:"todo"`
}

func Main(Context types.Context) types.ResponseOutput {
	var payload RequestBody
	errRequest := json.Unmarshal([]byte(Context.Req.BodyRaw), &payload)
	if errRequest != nil {
		Context.Error(errRequest)
		return Context.Res.Send("", 500, nil)
	}

	id := payload.Id
	if id == "" {
		id = "1"
	}

	client := resty.New()
	resp, errResponse := client.R().
		SetHeader("Accept", "application/json").
		Get("https://jsonplaceholder.typicode.com/todos/" + id)

	if errResponse != nil {
		Context.Error(errResponse)
		return Context.Res.Send("", 500, nil)
	}

	body := resp.String()

	var todo TodoObject
	errJson := json.Unmarshal([]byte(body), &todo)

	if errJson != nil {
		Context.Error(errJson)
		return Context.Res.Send("", 500, nil)
	}

	return Context.Res.Json(Response{
		Message: "Hello Open Runtimes 👋",
		Todo:    todo,
	}, 200, nil)
}
