package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"openruntimes/handler"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/open-runtimes/types-for-go/v3"
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

	var body interface{} = bodyRaw

	if contentType == "application/json" {
		if bodyRaw != "" {
			err := json.Unmarshal([]byte(bodyRaw), &body)

			if err != nil {
				return errors.New("Could not parse body into a JSON.")
			}
		} else {
			body = map[string]interface{}{}
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

	hostHeader := r.Host

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

	queryValues, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		return errors.New("Could not parse url params.")
	}

	query := map[string]string{}
	for key, value := range queryValues {
		query[key] = value[0]
	}

	portInUrl := ""
	if port != defaultPort {
		portString := strconv.Itoa(port)
		portInUrl = ":" + string(portString)
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

	stdout := os.Stdout
	stderr := os.Stderr
	defer func() {
		os.Stdout = stdout
		os.Stderr = stderr
		log.SetOutput(os.Stderr)
	}()

	reader, writer, err := os.Pipe()
	if err != nil {
		return errors.New("Could not prepare log capturing.")
	}

	os.Stdout = writer
	os.Stderr = writer
	log.SetOutput(writer)

	customstd := make(chan string)
	go func() {
		var buf bytes.Buffer
		io.Copy(&buf, reader)
		customstd <- buf.String()
	}()

	if safeTimeout != 0 {
		go timeoutPromise(outputChan)
	}

	go actionPromise(outputChan)

	output = <-outputChan

	writer.Close()

	os.Stdout = stdout
	os.Stderr = stderr
	log.SetOutput(os.Stderr)

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

	customLog := <-customstd
	if customLog != "" {
		context.Log("")
		context.Log(
			"----------------------------------------------------------------------------")
		context.Log(
			"Unsupported logs detected. Use Context.Log() or Context.Error() for logging.")

		context.Log(
			"----------------------------------------------------------------------------")
		context.Log(customLog)
		context.Log(
			"----------------------------------------------------------------------------")
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
