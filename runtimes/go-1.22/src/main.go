package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"openruntimes/handler"
	"openruntimes/types"
	"os"
	"strconv"
	"strings"
	"time"
)

func action(w http.ResponseWriter, r *http.Request) error {
	timeout := r.Header.Get("x-open-runtimes-timeout")

	var safeTimeout int

	if timeout != "" {
		safeTimeoutInteger, err := strconv.Atoi(timeout)

		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("content-type", "text/plain")
			fmt.Fprintf(w, "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.")
			return nil
		}

		safeTimeout = safeTimeoutInteger
	}

	secret := r.Header.Get("x-open-runtimes-secret")
	serverSecret := os.Getenv("OPEN_RUNTIMES_SECRET")

	if secret == "" || secret != serverSecret {
		w.WriteHeader(http.StatusInternalServerError)
		w.Header().Set("content-type", "text/plain")
		fmt.Fprintf(w, "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
		return nil
	}

	contentType := r.Header.Get("content-type")
	if contentType == "" {
		contentType = "text/plain"
	}

	bodyBytes, err := io.ReadAll(r.Body)

	if err != nil {
		return errors.New("Could not parse body into a string.")
	}

	bodyRaw := string(bodyBytes)

	var body any = bodyRaw

	if contentType == "application/json" {
		if bodyRaw == "" {
			var bodyJson map[string]interface{}
			err := json.Unmarshal([]byte(bodyRaw), &bodyJson)

			if err != nil {
				return errors.New("Could not parse body into a JSON.")
			}

			body = bodyJson

		} else {
			body = map[string]string{}
		}
	}

	headers := map[string]string{}
	for key, value := range r.Header {
		key = strings.ToLower(key)

		if strings.HasPrefix(key, "x-open-runtimes-") == false {
			headers[key] = value[0]
		}
	}

	method := r.Method

	scheme := "http"
	schemeHeader := r.Header.Get("x-forwarded-proto")
	if schemeHeader != "" {
		scheme = schemeHeader
	}

	defaultPort := 80
	if scheme == "https" {
		defaultPort = 443
	}

	var host string
	var port int

	hostHeader := r.Header.Get("host")
	if strings.Contains(hostHeader, ":") {
		chunks := strings.SplitN(hostHeader, ":", 2)
		host = chunks[0]
		parsedPort, err := strconv.Atoi(chunks[1])

		if err != nil {
			return errors.New("Host header could not be parsed into host and port.")
		}

		port = parsedPort
	} else {
		host = hostHeader
		port = defaultPort
	}

	path := r.URL.Path

	queryString := r.URL.RawQuery

	query := map[string]string{}
	queryChunks := strings.Split(queryString, "&")
	for _, chunk := range queryChunks {
		queryChunk := strings.SplitN(chunk, "=", 2)

		var queryKey string
		var queryValue string

		if len(queryChunk) > 0 {
			queryKey = queryChunk[0]
		}

		if len(queryChunk) > 1 {
			queryValue = queryChunk[1]
		}

		query[queryKey] = queryValue
	}

	portInUrl := ""
	if port != defaultPort {
		portInUrl = ":" + string(port)
	}

	queryStringInUrl := ""
	if queryString != "" {
		queryStringInUrl = "?" + queryString
	}

	requestUrl := scheme + "://" + host + portInUrl + path + queryStringInUrl

	context := types.Context{
		Req: types.Request{
			BodyRaw:     bodyRaw,
			Body:        body,
			Headers:     headers,
			Method:      method,
			Url:         requestUrl,
			Scheme:      scheme,
			Host:        host,
			Port:        port,
			Path:        path,
			QueryString: queryString,
			Query:       query,
		},
		Res: types.Response{},
	}

	output := types.ResponseOutput{
		StatusCode: 500,
		Body:       "",
		Headers:    map[string]string{},
	}

	timeoutPromise := func(ch chan types.ResponseOutput) {
		time.Sleep(time.Duration(safeTimeout) * time.Second)

		context.Error("Execution timed out.")
		ch <- context.Res.Send("", 500, map[string]string{})
	}

	actionPromise := func(ch chan types.ResponseOutput) {
		output = handler.Main(&context)
		ch <- output
	}

	outputChan := make(chan types.ResponseOutput)

	if safeTimeout != 0 {
		go timeoutPromise(outputChan)
	}

	go actionPromise(outputChan)

	output = <-outputChan

	if output.StatusCode == 0 {
		output.StatusCode = 200
	}

	if output.Headers == nil {
		output.Headers = map[string]string{}
	}

	outputHeaders := map[string]string{}
	for key, value := range output.Headers {
		key = strings.ToLower(key)

		if strings.HasPrefix(key, "x-open-runtimes-") == false {
			outputHeaders[key] = value
		}
	}

	var contentTypeValue string

	if value, exists := outputHeaders["content-type"]; exists {
		contentTypeValue = value
	} else {
		contentTypeValue = "text/plain"
	}

	if strings.HasPrefix(contentTypeValue, "multipart/") == false && strings.Contains(contentTypeValue, "charset=") == false {
		outputHeaders["content-type"] = contentTypeValue + "; charset=utf-8"
	}

	w.Header().Set("x-open-runtimes-logs", url.QueryEscape(strings.Join(context.Logs, "\n")))
	w.Header().Set("x-open-runtimes-errors", url.QueryEscape(strings.Join(context.Errors, "\n")))

	for key, value := range outputHeaders {
		w.Header().Set(key, value)
	}

	w.WriteHeader(output.StatusCode)
	fmt.Fprintf(w, output.Body)

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
