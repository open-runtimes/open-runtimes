package types

import (
	"encoding/json"
	"errors"
)

type Context struct {
	Req    Request
	Res    Response
	Logs   []string
	Errors []string
}

type Log struct {
	Message string
}

func (l Log) String() string {
	return l.Message
}

func (c *Context) Log(message any) {
	switch message.(type) {
	case string:
		c.Logs = append(c.Logs, message.(string))
	case Log:
		log := message.(Log)
		c.Logs = append(c.Logs, log.String())
	default:
		c.Logs = append(c.Logs, "Unsupported log. Please provide string or Log when logging into context.")
	}
}

func (c *Context) Error(message any) {
	switch message.(type) {
	case string:
		c.Errors = append(c.Errors, message.(string))
	case Log:
		log := message.(Log)
		c.Errors = append(c.Errors, log.String())
	default:
		c.Errors = append(c.Errors, "Unsupported log. Please provide string or Log when logging into context.")
	}
}

type Request struct {
	BodyRaw     string
	Body        any
	Headers     map[string]string
	Method      string
	Url         string
	Path        string
	Port        int
	Scheme      string
	Host        string
	QueryString string
	Query       map[string]string
}

type ResponseOutput struct {
	Body       string
	StatusCode int
	Headers    map[string]string
}

type Response struct{}

func (r Response) Send(body string, statusCode int, headers map[string]string) ResponseOutput {
	if headers == nil {
		headers = map[string]string{}
	}

	if statusCode == 0 {
		statusCode = 200
	}

	return ResponseOutput{
		Body:       body,
		StatusCode: statusCode,
		Headers:    headers,
	}
}

func (r Response) Json(bodyStruct any, statusCode int, headers map[string]string) (ResponseOutput, error) {
	if headers == nil {
		headers = map[string]string{}
	}

	if statusCode == 0 {
		statusCode = 200
	}

	headers["content-type"] = "application/json"

	jsonData, err := json.Marshal(bodyStruct)
	if err != nil {
		return ResponseOutput{}, errors.New("Error encoding JSON.")
	}

	jsonString := string(jsonData[:])

	return r.Send(jsonString, statusCode, headers), nil
}

func (r Response) Empty() ResponseOutput {
	return r.Send("", 204, map[string]string{})
}

func (r Response) Redirect(url string, statusCode int, headers map[string]string) ResponseOutput {
	if headers == nil {
		headers = map[string]string{}
	}

	if statusCode == 0 {
		statusCode = 301
	}

	headers["location"] = url

	return r.Send("", statusCode, headers)
}
