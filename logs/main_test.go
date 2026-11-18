package main

import (
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
)

func get(t *testing.T, h http.Handler, url, secret string) *httptest.ResponseRecorder {
	t.Helper()
	req := httptest.NewRequest(http.MethodGet, url, nil)
	if secret != "" {
		req.Header.Set("x-open-runtimes-secret", secret)
	}
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, req)
	return rec
}

func write(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func gone(t *testing.T, path string) {
	t.Helper()
	if _, err := os.Lstat(path); !os.IsNotExist(err) {
		t.Errorf("%s still exists", path)
	}
}

func TestReturnsBothFilesAndDeletesThem(t *testing.T) {
	dir := t.TempDir()
	write(t, filepath.Join(dir, "abc_logs.log"), "hello\n")
	write(t, filepath.Join(dir, "abc_errors.log"), "boom\n")

	rec := get(t, handler(dir, ""), "/logs/abc?max=1024", "")

	if rec.Code != http.StatusOK {
		t.Fatalf("status %d", rec.Code)
	}
	if got := rec.Header().Get("x-open-runtimes-logs-length"); got != "6" {
		t.Errorf("logs length %q", got)
	}
	if got := rec.Body.String(); got != "hello\nboom\n" {
		t.Errorf("body %q", got)
	}
	gone(t, filepath.Join(dir, "abc_logs.log"))
	gone(t, filepath.Join(dir, "abc_errors.log"))

	// A second collection finds nothing: the files went with the first.
	rec = get(t, handler(dir, ""), "/logs/abc?max=1024", "")
	if rec.Body.Len() != 0 || rec.Header().Get("x-open-runtimes-logs-length") != "0" {
		t.Errorf("second take returned %q", rec.Body.String())
	}
}

func TestCutsEachFileOneByteOverMax(t *testing.T) {
	dir := t.TempDir()
	write(t, filepath.Join(dir, "abc_logs.log"), strings.Repeat("a", 100))
	write(t, filepath.Join(dir, "abc_errors.log"), strings.Repeat("e", 100))

	rec := get(t, handler(dir, ""), "/logs/abc?max=10", "")

	if got := rec.Header().Get("x-open-runtimes-logs-length"); got != "11" {
		t.Errorf("logs length %q", got)
	}
	if got := rec.Body.String(); got != strings.Repeat("a", 11)+strings.Repeat("e", 11) {
		t.Errorf("body %q", got)
	}
}

func TestDeletesSymlinkUnreadAndKeepsTarget(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(t.TempDir(), "secret")
	write(t, target, "not yours")
	if err := os.Symlink(target, filepath.Join(dir, "abc_logs.log")); err != nil {
		t.Fatal(err)
	}

	rec := get(t, handler(dir, ""), "/logs/abc?max=1024", "")

	if strings.Contains(rec.Body.String(), "not yours") {
		t.Fatalf("followed the symlink: %q", rec.Body.String())
	}
	gone(t, filepath.Join(dir, "abc_logs.log"))
	if _, err := os.Stat(target); err != nil {
		t.Errorf("target removed: %v", err)
	}
}

func TestDeletesFifoWithoutBlocking(t *testing.T) {
	dir := t.TempDir()
	fifo := filepath.Join(dir, "abc_logs.log")
	if err := syscall.Mkfifo(fifo, 0o644); err != nil {
		t.Skip("mkfifo:", err)
	}

	rec := get(t, handler(dir, ""), "/logs/abc?max=1024", "")

	if rec.Code != http.StatusOK || rec.Body.Len() != 0 {
		t.Errorf("status %d body %q", rec.Code, rec.Body.String())
	}
	gone(t, fifo)
}

func TestRefusesIdsThatLeaveTheDirectory(t *testing.T) {
	dir := t.TempDir()
	outside := filepath.Join(filepath.Dir(dir), "x_logs.log")
	write(t, outside, "outside")
	t.Cleanup(func() { os.Remove(outside) })

	for _, id := range []string{"..%2Fx", "a.b", "a-b"} {
		rec := get(t, handler(dir, ""), "/logs/"+id+"?max=1024", "")
		if rec.Code == http.StatusOK {
			t.Errorf("id %q accepted", id)
		}
	}
	if _, err := os.Stat(outside); err != nil {
		t.Errorf("file outside the directory touched: %v", err)
	}
}

func TestSecretGuardsTheFiles(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "abc_logs.log")
	write(t, path, "hello")
	h := handler(dir, "s3cret")

	if rec := get(t, h, "/logs/abc?max=1024", "wrong"); rec.Code != http.StatusUnauthorized {
		t.Errorf("wrong secret: status %d", rec.Code)
	}
	if _, err := os.Stat(path); err != nil {
		t.Fatalf("refused request deleted the file: %v", err)
	}
	if rec := get(t, h, "/logs/abc?max=1024", "s3cret"); rec.Body.String() != "hello" {
		t.Errorf("right secret: body %q", rec.Body.String())
	}
}

func TestRejectsMissingMax(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "abc_logs.log")
	write(t, path, "hello")

	if rec := get(t, handler(dir, ""), "/logs/abc", ""); rec.Code != http.StatusBadRequest {
		t.Errorf("status %d", rec.Code)
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("rejected request deleted the file: %v", err)
	}
}
