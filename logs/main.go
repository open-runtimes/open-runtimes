// Command logs serves a runtime's execution logs to the executor over HTTP.
//
// Runtimes write each execution's output to <dir>/<id>_logs.log and
// <dir>/<id>_errors.log and return the id in x-open-runtimes-log-id. This
// server runs beside the runtime, in its own container sharing that
// directory, so the executor can collect the files without mounting the
// directory itself, and still can after the runtime process has crashed.
//
// GET /logs/<id>?max=<bytes> answers with the logs file followed by the
// errors file in one body, x-open-runtimes-logs-length giving where the first
// ends, and deletes both. Each part is cut at max+1 bytes, so the caller can
// tell a truncated file from one that fits.
package main

import (
	"crypto/subtle"
	"errors"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"syscall"
)

var validID = regexp.MustCompile(`^[A-Za-z0-9]{1,128}$`)

func main() {
	dir := env("OPEN_RUNTIMES_LOGS_DIR", "/mnt/logs")
	addr := ":" + env("OPEN_RUNTIMES_LOGS_PORT", "3001")
	log.Printf("serving %s on %s", dir, addr)
	log.Fatal(http.ListenAndServe(addr, handler(dir, os.Getenv("OPEN_RUNTIMES_SECRET"))))
}

func env(key, fallback string) string {
	if value := os.Getenv(key); value != "" {
		return value
	}
	return fallback
}

func handler(dir, secret string) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /logs/{id}", func(w http.ResponseWriter, r *http.Request) {
		// With no secret configured, the network decides who may connect.
		if secret != "" && subtle.ConstantTimeCompare([]byte(r.Header.Get("x-open-runtimes-secret")), []byte(secret)) != 1 {
			http.Error(w, "unauthorized", http.StatusUnauthorized)
			return
		}

		id := r.PathValue("id")
		if !validID.MatchString(id) {
			http.Error(w, "invalid id", http.StatusBadRequest)
			return
		}

		max, err := strconv.ParseInt(r.URL.Query().Get("max"), 10, 64)
		if err != nil || max < 0 {
			http.Error(w, "invalid max", http.StatusBadRequest)
			return
		}

		logs := take(filepath.Join(dir, id+"_logs.log"), max+1)
		errs := take(filepath.Join(dir, id+"_errors.log"), max+1)

		w.Header().Set("Content-Type", "application/octet-stream")
		w.Header().Set("x-open-runtimes-logs-length", strconv.Itoa(len(logs)))
		w.Header().Set("Content-Length", strconv.Itoa(len(logs)+len(errs)))
		_, _ = w.Write(logs)
		_, _ = w.Write(errs)
	})
	return mux
}

// take reads up to limit bytes of a regular file and deletes it. The runtime
// shares the directory and could leave a symlink, FIFO or device in a file's
// place: those are deleted unread.
func take(path string, limit int64) []byte {
	defer os.Remove(path) // removes a symlink itself, never its target

	file, err := os.OpenFile(path, os.O_RDONLY|syscall.O_NOFOLLOW|syscall.O_NONBLOCK, 0)
	if err != nil {
		if !errors.Is(err, os.ErrNotExist) {
			log.Printf("open %s: %v", path, err)
		}
		return nil
	}
	defer file.Close()

	info, err := file.Stat()
	if err != nil || !info.Mode().IsRegular() {
		return nil
	}

	data, err := io.ReadAll(io.LimitReader(file, limit))
	if err != nil {
		log.Printf("read %s: %v", path, err)
	}
	return data
}
