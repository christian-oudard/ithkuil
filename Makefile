# Top-level Makefile. The Go module lives under go/; running cd-into-go
# from each target keeps callers in the repo root.

.DEFAULT_GOAL := build

GO_DIR := go
BIN_DIR := bin

# Disable CGO so builds work on hosts without a C toolchain.
export CGO_ENABLED := 0

.PHONY: build test clean fmt vet install ithkuil ithkuil-mcp

build: ithkuil ithkuil-mcp

ithkuil:
	cd $(GO_DIR) && go build -o ../$(BIN_DIR)/ithkuil ./cmd/ithkuil

ithkuil-mcp:
	cd $(GO_DIR) && go build -o ../$(BIN_DIR)/ithkuil-mcp ./cmd/ithkuil-mcp

test:
	cd $(GO_DIR) && go test ./...

fmt:
	cd $(GO_DIR) && gofmt -w .

vet:
	cd $(GO_DIR) && go vet ./...

# install drops the binaries into $GOBIN (defaults to ~/go/bin) so they
# end up on PATH instead of in this repo's bin/.
install:
	cd $(GO_DIR) && go install ./cmd/ithkuil ./cmd/ithkuil-mcp

clean:
	rm -f $(BIN_DIR)/ithkuil $(BIN_DIR)/ithkuil-mcp
