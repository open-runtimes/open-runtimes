package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"
	"strings"
)

type Request struct {
	variables map[string]string
	headers   map[string]string
	payload   string
}

type Response struct {
	statusCode int
	data       interface{}
}

func (r Response) Send(input string) {

}

func (r Response) Json(object interface{}) {

}

func baseHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method is not supported.", http.StatusNotFound)
		return
	}

	xInternalChallenge := r.Header.Get("x-internal-challenge")
	envInternalChallengeKey := os.Getenv("INTERNAL_RUNTIME_KEY")

	if xInternalChallenge != envInternalChallengeKey {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	var request Request
	decoder := json.NewDecoder(r.Body)
	err := decoder.Decode(&request)
	if err != nil {
		http.Error(w, "Internal decoding error", http.StatusInternalServerError)
		return
	}

	if len(request.payload) == 0 {
		http.Error(w, "Bad request", http.StatusBadRequest)
		return
	}

	userPath := os.Getenv("INTERNAL_RUNTIME_ENTRYPOINT")
	if strings.HasSuffix(userPath, ".go") {
		userPath = userPath[:len(userPath)-3]
	}
	userPath = strings.Replace(userPath, "/", ".", 1)
}

func main() {
	http.HandleFunc("/", baseHandler)

	if err := http.ListenAndServe(":3000", nil); err != nil {
		log.Fatal(err)
	}
}
