package main

import (
	gctx "context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"openruntimes/handler"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/open-runtimes/types-for-go/v4/openruntimes"
)

func action(w http.ResponseWriter, r *http.Request, logger openruntimes.Logger) error {
	timeout := r.Header.Get("x-open-runtimes-timeout")
	
	var safeTimeout int = 30 // Default timeout of 30 seconds
	
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

	// Use LimitReader to prevent unbounded memory usage
	bodyBytes, err := io.ReadAll(io.LimitReader(r.Body, 20*1024*1024))
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

	context := openruntimes.NewContext(logger)

	context.Req = openruntimes.ContextRequest{
		Headers:     headers,
		Method:      method,
		Url:         requestUrl,
		Scheme:      scheme,
		Host:        host,
		Port:        port,
		Path:        path,
		QueryString: queryString,
		Query:       query,
	}

	context.Res = openruntimes.ContextResponse{}

	context.Req.SetBodyBinary(bodyBytes)

	// Create a context with timeout
	ctx, cancel := gctx.WithTimeout(r.Context(), time.Duration(safeTimeout)*time.Second)
	defer cancel()

	// Create buffered channel to prevent goroutine leak
	outputChan := make(chan openruntimes.Response, 1)
	defer close(outputChan)

	// Start the handler in a goroutine
	go func() {
		output := handler.Main(context)
		select {
		case outputChan <- output:
		case <-ctx.Done():
			// Context cancelled, do nothing
		}
	}()

	// Wait for either completion or timeout
	var output openruntimes.Response
	select {
	case output = <-outputChan:
	case <-ctx.Done():
		context.Error("Execution timed out.")
		w.WriteHeader(504)
		w.Header().Set("content-type", "text/plain")
		w.Write([]byte("Execution timed out."))
		return nil
	}

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
	if !strings.HasPrefix(contentTypeValue, "multipart/") && !strings.Contains(contentTypeValue, "charset=") {
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

		logger, loggerErr := openruntimes.NewLogger(logging, logId)

		if loggerErr != nil {
			logger.Write([]interface{}{
				loggerErr.Error(),
			}, openruntimes.LOGGER_TYPE_ERROR, false)

			logger.End()

			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("x-open-runtimes-log-id", logger.Id)
			w.Header().Set("content-type", "text/plain")
			w.Write([]byte{})
			return
		}

		err := action(w, r, logger)

		if err != nil {
			logger.Write([]interface{}{
				err.Error(),
			}, openruntimes.LOGGER_TYPE_ERROR, false)

			logger.End()

			w.WriteHeader(http.StatusInternalServerError)
			w.Header().Set("x-open-runtimes-log-id", logger.Id)
			w.Header().Set("content-type", "text/plain")
			w.Write([]byte{})
		}
	}

	listener, err := net.Listen("tcp", ":3000")
	if err != nil {
		panic(err)
	}

	fmt.Println("HTTP server successfully started!")

	// Create server with timeouts
	server := &http.Server{
		Handler:      http.MaxBytesHandler(http.HandlerFunc(handler), 20*1024*1024),
		ReadTimeout:  15 * time.Second,
		WriteTimeout: 15 * time.Second,
	}

	if err := server.Serve(listener); err != nil {
		panic(err)
	}
}
