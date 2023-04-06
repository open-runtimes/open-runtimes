package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"strings"
)

type Request struct {
	Variables map[string]string `json:"variables"`
	Headers   map[string]string `json:"headers"`
	Payload   string            `json:"payload"`
}

type Response struct {
	Status     int          `json:"status,omitempty"`
	Response   interface{}  `json:"response"`
	Stdout     string       `json:"stdout"`
	buffStdout bytes.Buffer `json:"-"`
}

func (res *Response) send(text string, status int) {
	res.Response = text
	res.Status = status
}

func (res *Response) json(obj interface{}, status int) {
	res.Response = obj
	if status != 200 {
		res.Status = status
	}
}

func handler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not supported", http.StatusMethodNotAllowed)
		return
	}

	xInternalChallenge := r.Header.Get("x-internal-challenge")
	internalKey := os.Getenv("INTERNAL_RUNTIME_KEY")

	if xInternalChallenge != internalKey {
		http.Error(w, "Unauthorized, no runtime key", 401)
		return
	}

	userPath := os.Getenv("INTERNAL_RUNTIME_ENTRYPOINT")
	if userPath == "" {
		http.Error(w, "File entry point not set or file not found", 404)
		return
	}

	if !strings.HasSuffix(userPath, ".go") {
		http.Error(w, "Unsupported code file", 500)
		return
	}

	request := Request{}
	err := json.NewDecoder(r.Body).Decode(&request)
	if err != nil {
		http.Error(w, "Error while parsing the request data", http.StatusBadRequest)
		return
	}

	response := new(Response)

	newEncoder := json.NewEncoder(w)

	// Call User Function, Main()
	err = Main(request, response)
	if err != nil {
		errFromFunc := fmt.Sprintf("Error: ", err)
		response.send(errFromFunc, 500)

		newEncoder.Encode(response)
		w.WriteHeader(500)
		return
	}

	response.Stdout = response.buffStdout.String()
	newEncoder.Encode(response)

	return
}

func main() {
	http.HandleFunc("/", handler)

	log.Fatal(http.ListenAndServe(":3000", nil))
}
