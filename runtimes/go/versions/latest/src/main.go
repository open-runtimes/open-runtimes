package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"openruntimes/handler"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/open-runtimes/types-for-go/v4"
)

func action(w http.ResponseWriter, r *http.Request, logger types.Logger) error {
	timeout := r.Header.Get("x-open-runtimes-timeout")

	var safeTimeout int

	if timeout != "" {
		safeTimeoutInteger, err := strconv.Atoi(timeout)

		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("content-type", "text/plain")
			w.Write([]byte("Header \"x-open-runtimes-timeout\" must be an integer greater than 0."))
			return nil
		}

		safeTimeout = safeTimeoutInteger
	}

	secret := r.Header.Get("x-open-runtimes-secret")
	serverSecret := os.Getenv("OPEN_RUNTIMES_SECRET")

	if serverSecret != "" && secret != serverSecret {
		w.WriteHeader(http.StatusInternalServerError)
		w.Header().Set("content-type", "text/plain")
		w.Write([]byte("Unauthorized. Provide correct \"x-open-runtimes-secret\" header."))
		return nil
	}

	bodyBytes, err := io.ReadAll(r.Body)

	if err != nil {
		return errors.New("Could not parse body into a string.")
	}

	headers := map[string]string{}
	for key, value := range r.Header {
		key = strings.ToLower(key)

		if key == "content-type" {
			value[0] = strings.ToLower(value[0])
		}

		if strings.HasPrefix(key, "x-open-runtimes-") == false {
			headers[key] = value[0]
		}
	}

	headersEnv := os.Getenv("OPEN_RUNTIMES_HEADERS")
	if headersEnv == "" {
		headersEnv = "{}"
	}

	var enforcedHeaders map[string]interface{}
	err = json.Unmarshal([]byte(headersEnv), &enforcedHeaders)
	if err != nil {
		enforcedHeaders = map[string]interface{}{}
	}

	for key, value := range enforcedHeaders {
		valueString := ""
		switch v := value.(type) {
		default:
			valueString = fmt.Sprintf("%#v", value)
		case string:
			valueString = v
		}

		headers[strings.ToLower(key)] = valueString
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
		Logger: logger,
		Req: types.Request{
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

	context.Req.SetBodyBinary(bodyBytes)

	output := types.ResponseOutput{
		StatusCode: 500,
		Body:       []byte{},
		Headers:    map[string]string{},
	}

	timeoutPromise := func(ch chan types.ResponseOutput) {
		time.Sleep(time.Duration(safeTimeout) * time.Second)

		context.Error("Execution timed out.")
		ch <- context.Res.Text("", 500, map[string]string{})
	}

	actionPromise := func(ch chan types.ResponseOutput) {
		output = handler.Main(&context)
		ch <- output
	}

	outputChan := make(chan types.ResponseOutput)

	logger.OverrideNativeLogs()

	if safeTimeout != 0 {
		go timeoutPromise(outputChan)
	}

	go actionPromise(outputChan)

	output = <-outputChan

	logger.RevertNativeLogs()

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

	contentTypeValue = strings.ToLower(contentTypeValue)

	if strings.HasPrefix(contentTypeValue, "multipart/") == false && strings.Contains(contentTypeValue, "charset=") == false {
		outputHeaders["content-type"] = contentTypeValue + "; charset=utf-8"
	}

	logger.End()
	w.Header().Set("x-open-runtimes-log-id", logger.Id)

	for key, value := range outputHeaders {
		w.Header().Set(key, value)
	}

	w.WriteHeader(output.StatusCode)
	w.Write(output.Body)

	return nil
}

func main() {
	handler := func(w http.ResponseWriter, r *http.Request) {
		logging := r.Header.Get("x-open-runtimes-logging")
		logId := r.Header.Get("x-open-runtimes-log-id")

		logger, loggerErr := types.NewLogger(logging, logId)

		if loggerErr != nil {
			logger.Write([]string{
				loggerErr.Error(),
			}, types.LOGGER_TYPE_ERROR, false)

			logger.End()

			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("x-open-runtimes-log-id", logger.Id)
			w.Header().Set("content-type", "text/plain")
			w.Write([]byte{})
			return
		}

		err := action(w, r, logger)

		if err != nil {
			logger.Write([]string{
				err.Error(),
			}, types.LOGGER_TYPE_ERROR, false)

			logger.End()

			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("x-open-runtimes-log-id", logger.Id)
			w.Header().Set("content-type", "text/plain")
			w.Write([]byte{})
		}
	}

	err := http.ListenAndServe(":3000", http.HandlerFunc(handler))

	if err != nil {
		panic(err)
	}
}
