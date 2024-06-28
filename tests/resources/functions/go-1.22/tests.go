package handler

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	"github.com/go-resty/resty/v2"
	"github.com/open-runtimes/types-for-go/v4"
)

func Main(Context *types.Context) types.ResponseOutput {
	action := Context.Req.Headers["x-action"]

	switch a := action; a {
	case "plaintextResponse":
		return Context.Res.Text("Hello World 👋", 200, nil)
	case "jsonResponse":
		return Context.Res.Json(map[string]interface{}{
			"json":    true,
			"message": "Developers are awesome.",
		}, 200, nil)
	case "customCharsetResponse":
		return Context.Res.Text("ÅÆ", 200, map[string]string{
			"content-type": "text/plain; charset=iso-8859-1",
		})
	case "uppercaseCharsetResponse":
		return Context.Res.Text("ÅÆ", 200, map[string]string{
			"content-type": "TEXT/PLAIN",
		})
	case "multipartResponse":
		return Context.Res.Text(`--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--`, 200, map[string]string{
			"content-type": "multipart/form-data; boundary=12345",
		})
	case "redirectResponse":
		return Context.Res.Redirect("https://github.com/", 301, nil)
	case "emptyResponse":
		return Context.Res.Empty()
	case "noResponse":
		Context.Res.Text("This should be ignored, as it is not returned.", 200, nil)

		// Simulate test data. Return nessessary in Golang
		Context.Error("Return statement missing. return context.getRes().empty() if no response is expected.")
		return Context.Res.Text("", 500, nil)
	case "doubleResponse":
		Context.Res.Text("This should be ignored.", 200, nil)
		return Context.Res.Text("This should be returned.", 200, nil)
	case "headersResponse":
		secondHeader, ok := Context.Req.Headers["x-open-runtimes-custom-in-header"]
		if !ok {
			secondHeader = "missing"
		}

		cookie, ok := Context.Req.Headers["cookie"]
		if !ok {
			cookie = "missing"
		}

		return Context.Res.Text("OK", 200, map[string]string{
			"first-header":                      "first-value",
			"second-header":                     secondHeader,
			"cookie":                            cookie,
			"x-open-runtimes-custom-out-header": "third-value",
		})
	case "statusResponse":
		return Context.Res.Text("FAIL", 404, nil)
	case "requestMethod":
		return Context.Res.Text(Context.Req.Method, 200, nil)
	case "requestUrl":
		return Context.Res.Json(map[string]interface{}{
			"url":         Context.Req.Url,
			"port":        Context.Req.Port,
			"path":        Context.Req.Path,
			"query":       Context.Req.Query,
			"queryString": Context.Req.QueryString,
			"scheme":      Context.Req.Scheme,
			"host":        Context.Req.Host,
		}, 200, nil)
	case "requestHeaders":
		return Context.Res.Json(Context.Req.Headers, 200, nil)
	case "requestBodyText":
		return Context.Res.Text(Context.Req.BodyText(), 200, nil)
	case "requestBodyJson":
		bodyJson, err := Context.Req.BodyJson()

		if err != nil {
			Context.Error("Cannot parse JSON body")
			return Context.Res.Text("", 500, nil)
		}

		return Context.Res.Json(bodyJson, 200, nil)
	case "requestBodyBinary":
		return Context.Res.Binary(Context.Req.BodyBinary(), 200, nil)
	case "requestBodyTextAuto":
		return Context.Res.Text(Context.Req.Body().(string), 200, nil)
	case "requestBodyJsonAuto":
		return Context.Res.Json(Context.Req.Body().(map[string]interface{}), 200, nil)
	case "requestBodyBinaryAuto":
		return Context.Res.Binary(Context.Req.Body().([]byte), 200, nil)
	case "binaryResponse1":
		bytes := []byte{0, 10, 255}
		return Context.Res.Binary(bytes, 200, nil) // []byte
	case "binaryResponse2":
		bytes := []byte{0, 20, 255}
		return Context.Res.Binary(bytes, 200, nil) // Just a filler
	case "binaryResponse3":
		bytes := []byte{0, 30, 255}
		return Context.Res.Binary(bytes, 200, nil) // Just a filler
	case "binaryResponse4":
		bytes := []byte{0, 40, 255}
		return Context.Res.Binary(bytes, 200, nil) // Just a filler
	case "binaryResponse5":
		bytes := []byte{0, 50, 255}
		return Context.Res.Binary(bytes, 200, nil) // Just a filler
	case "envVars":
		var emptyVar *string
		varValue := os.Getenv("NOT_DEFINED_VAR")
		if varValue != "" {
			emptyVar = &varValue
		}

		return Context.Res.Json(map[string]interface{}{
			"var":      os.Getenv("CUSTOM_ENV_VAR"),
			"emptyVar": emptyVar,
		}, 200, nil)
	case "logs":
		fmt.Println("Native log")
		Context.Log("Debug log")
		Context.Error("Error log")

		Context.Log("Log+With+Plus+Symbol")

		Context.Log(42)
		Context.Log(4.2)
		Context.Log(true)

		Context.Log(map[string]string{"objectKey": "objectValue"})
		Context.Log([]string{"arrayValue"})

		return Context.Res.Text("", 200, nil)
	case "library":
		client := resty.New()
		resp, errResponse := client.R().
			SetHeader("Accept", "application/json").
			Get("https://jsonplaceholder.typicode.com/todos/" + Context.Req.BodyText())

		if errResponse != nil {
			Context.Error(errResponse)
			return Context.Res.Text("", 500, nil)
		}

		body := resp.String()

		type TodoObject struct {
			UserId    int    `json:"userId"`
			Id        int    `json:"id"`
			Title     string `json:"title"`
			Completed bool   `json:"completed"`
		}

		var todo TodoObject
		errJson := json.Unmarshal([]byte(body), &todo)

		if errJson != nil {
			Context.Error(errJson)
			return Context.Res.Text("", 500, nil)
		}

		return Context.Res.Json(map[string]interface{}{
			"todo": todo,
		}, 200, nil)
	case "timeout":
		Context.Log("Timeout start.")

		time.Sleep(3 * time.Second)

		Context.Log("Timeout end.")

		return Context.Res.Text("Successful response.", 200, nil)
	case "deprecatedMethods":
		return Context.Res.Send(Context.Req.BodyRaw(), 200, nil)
	case "deprecatedMethodsUntypedBody":
		return Context.Res.Send("50", 200, nil) // Send only supported String
	default:
		Context.Error("Unknown action in tests.go")
		return Context.Res.Text("", 500, nil)
	}
}
