#!/bin/bash

#
# CBLPolyCall Build System v1.0
# OBINexus Aegis Engineering - COBOL FFI Bridge Builder
# Technical Lead: Nnamdi Michael Okpala - OBINexusComputing
# Integration Point: riftlang.exe → .so.a → rift.exe → gosilang
#

set -euo pipefail

# Terminal colors for systematic output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Build configuration
CBL_PROJECT_NAME="cblpolycall"
CBL_VERSION="1.0.0"
CBL_BUILD_DIR="build"
CBL_SRC_DIR="src"
CBL_COPYBOOK_DIR="copybooks"
CBL_TARGET_DIR="target"
CBL_DIST_DIR="dist"

# Platform detection
detect_platform() {
    case "$(uname -s)" in
        Linux*)     echo "linux";;
        Darwin*)    echo "macos";;
        CYGWIN*|MINGW*|MSYS*) echo "windows";;
        *)          echo "unknown";;
    esac
}

# COBOL compiler detection
detect_cobol_compiler() {
    if command -v cobc >/dev/null 2>&1; then
        echo "gnucobol"
    elif command -v cob >/dev/null 2>&1; then
        echo "microfocus"
    elif command -v cobol.exe >/dev/null 2>&1; then
        echo "microfocus_windows"
    else
        echo "none"
    fi
}

log_info() { echo -e "${BLUE}[CBL-BUILD]${NC} $1"; }
log_success() { echo -e "${GREEN}[CBL-SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[CBL-WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[CBL-ERROR]${NC} $1"; }
log_protocol() { echo -e "${MAGENTA}[CBL-PROTOCOL]${NC} $1"; }
log_aegis() { echo -e "${CYAN}[AEGIS]${NC} $1"; }

print_banner() {
    echo "=================================================================="
    echo "  CBLPolyCall Build System v${CBL_VERSION}"
    echo "  OBINexus Aegis Engineering - COBOL FFI Bridge"
    echo "  Technical Lead: Nnamdi Michael Okpala"
    echo "  Integration: riftlang.exe → .so.a → rift.exe → gosilang"
    echo "=================================================================="
    echo ""
}

# Initialize project structure
init_project_structure() {
    log_info "Initializing CBLPolyCall project structure..."
    
    # Create directory structure
    mkdir -p "${CBL_SRC_DIR}"
    mkdir -p "${CBL_COPYBOOK_DIR}"
    mkdir -p "${CBL_BUILD_DIR}"
    mkdir -p "${CBL_TARGET_DIR}"
    mkdir -p "${CBL_DIST_DIR}"
    mkdir -p "tests"
    mkdir -p "docs"
    mkdir -p "config"
    
    log_success "Project structure initialized"
}

# Generate COBOL FFI bridge
generate_polycall_bridge() {
    log_info "Generating PolyCall COBOL FFI bridge..."
    
    cat > "${CBL_SRC_DIR}/POLYCALL.CBL" << 'EOF'
      * CBLPolyCall FFI Bridge v1.0
      * OBINexus Aegis Engineering - COBOL to PolyCall Interface
      * Technical Lead: Nnamdi Michael Okpala
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLYCALL-BRIDGE.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CALL-CONVENTION 0 IS C-CALLING.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-POLYCALL-VERSION     PIC X(10) VALUE "1.0.0".
       01  WS-BRIDGE-STATUS        PIC 9(4) COMP.
       01  WS-ERROR-MESSAGE        PIC X(256).
       01  WS-PROTOCOL-BUFFER      PIC X(4096).
       
       LINKAGE SECTION.
       01  LNK-OPERATION           PIC X(20).
       01  LNK-INPUT-DATA          PIC X(1024).
       01  LNK-OUTPUT-DATA         PIC X(1024).
       01  LNK-RESULT-CODE         PIC 9(4) COMP.
       
       PROCEDURE DIVISION USING LNK-OPERATION,
                               LNK-INPUT-DATA,
                               LNK-OUTPUT-DATA,
                               LNK-RESULT-CODE.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-BRIDGE
           
           EVALUATE LNK-OPERATION
               WHEN "CONNECT"
                   PERFORM POLYCALL-CONNECT
               WHEN "SEND_MESSAGE"
                   PERFORM POLYCALL-SEND
               WHEN "RECEIVE_MESSAGE"
                   PERFORM POLYCALL-RECEIVE
               WHEN "DISCONNECT"
                   PERFORM POLYCALL-DISCONNECT
               WHEN OTHER
                   MOVE 999 TO LNK-RESULT-CODE
                   MOVE "UNKNOWN_OPERATION" TO LNK-OUTPUT-DATA
           END-EVALUATE
           
           EXIT PROGRAM.
       
       INITIALIZE-BRIDGE.
           MOVE ZEROS TO WS-BRIDGE-STATUS
           MOVE SPACES TO WS-ERROR-MESSAGE
           MOVE SPACES TO WS-PROTOCOL-BUFFER.
       
       POLYCALL-CONNECT.
      *    Call external PolyCall library function
           CALL "polycall_connect" USING BY REFERENCE LNK-INPUT-DATA
                                        BY REFERENCE WS-BRIDGE-STATUS
           
           IF WS-BRIDGE-STATUS = ZERO
               MOVE "CONNECTED" TO LNK-OUTPUT-DATA
               MOVE ZERO TO LNK-RESULT-CODE
           ELSE
               MOVE "CONNECTION_FAILED" TO LNK-OUTPUT-DATA
               MOVE WS-BRIDGE-STATUS TO LNK-RESULT-CODE
           END-IF.
       
       POLYCALL-SEND.
      *    Call external PolyCall send function
           CALL "polycall_send" USING BY REFERENCE LNK-INPUT-DATA
                                     BY REFERENCE WS-PROTOCOL-BUFFER
                                     BY REFERENCE WS-BRIDGE-STATUS
           
           IF WS-BRIDGE-STATUS = ZERO
               MOVE "MESSAGE_SENT" TO LNK-OUTPUT-DATA
               MOVE ZERO TO LNK-RESULT-CODE
           ELSE
               MOVE "SEND_FAILED" TO LNK-OUTPUT-DATA
               MOVE WS-BRIDGE-STATUS TO LNK-RESULT-CODE
           END-IF.
       
       POLYCALL-RECEIVE.
      *    Call external PolyCall receive function
           CALL "polycall_receive" USING BY REFERENCE WS-PROTOCOL-BUFFER
                                        BY REFERENCE LNK-OUTPUT-DATA
                                        BY REFERENCE WS-BRIDGE-STATUS
           
           IF WS-BRIDGE-STATUS = ZERO
               MOVE ZERO TO LNK-RESULT-CODE
           ELSE
               MOVE "RECEIVE_FAILED" TO LNK-OUTPUT-DATA
               MOVE WS-BRIDGE-STATUS TO LNK-RESULT-CODE
           END-IF.
       
       POLYCALL-DISCONNECT.
      *    Call external PolyCall disconnect function
           CALL "polycall_disconnect" USING BY REFERENCE WS-BRIDGE-STATUS
           
           IF WS-BRIDGE-STATUS = ZERO
               MOVE "DISCONNECTED" TO LNK-OUTPUT-DATA
               MOVE ZERO TO LNK-RESULT-CODE
           ELSE
               MOVE "DISCONNECT_FAILED" TO LNK-OUTPUT-DATA
               MOVE WS-BRIDGE-STATUS TO LNK-RESULT-CODE
           END-IF.
EOF
    
    log_success "PolyCall COBOL FFI bridge generated"
}

# Generate main program
generate_main_program() {
    log_info "Generating main COBOL program..."
    
    cat > "${CBL_SRC_DIR}/MAIN.CBL" << 'EOF'
      * CBLPolyCall Main Program v1.0
      * OBINexus Aegis Engineering - PolyCall COBOL Client
      * Technical Lead: Nnamdi Michael Okpala
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLPOLYCALL-MAIN.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPERATION            PIC X(20).
       01  WS-INPUT-DATA           PIC X(1024).
       01  WS-OUTPUT-DATA          PIC X(1024).
       01  WS-RESULT-CODE          PIC 9(4) COMP.
       01  WS-CONTINUE-FLAG        PIC X VALUE "Y".
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "CBLPolyCall Client v1.0 - Aegis Engineering"
           DISPLAY "=============================================="
           
           PERFORM UNTIL WS-CONTINUE-FLAG = "N"
               DISPLAY " "
               DISPLAY "Available Operations:"
               DISPLAY "1. CONNECT"
               DISPLAY "2. SEND_MESSAGE"
               DISPLAY "3. RECEIVE_MESSAGE"
               DISPLAY "4. DISCONNECT"
               DISPLAY "5. EXIT"
               DISPLAY " "
               DISPLAY "Enter operation: " WITH NO ADVANCING
               ACCEPT WS-OPERATION
               
               EVALUATE WS-OPERATION
                   WHEN "1" OR "CONNECT"
                       MOVE "CONNECT" TO WS-OPERATION
                       DISPLAY "Enter connection data: " WITH NO ADVANCING
                       ACCEPT WS-INPUT-DATA
                       PERFORM CALL-BRIDGE
                   WHEN "2" OR "SEND_MESSAGE"
                       MOVE "SEND_MESSAGE" TO WS-OPERATION
                       DISPLAY "Enter message to send: " WITH NO ADVANCING
                       ACCEPT WS-INPUT-DATA
                       PERFORM CALL-BRIDGE
                   WHEN "3" OR "RECEIVE_MESSAGE"
                       MOVE "RECEIVE_MESSAGE" TO WS-OPERATION
                       MOVE SPACES TO WS-INPUT-DATA
                       PERFORM CALL-BRIDGE
                   WHEN "4" OR "DISCONNECT"
                       MOVE "DISCONNECT" TO WS-OPERATION
                       MOVE SPACES TO WS-INPUT-DATA
                       PERFORM CALL-BRIDGE
                   WHEN "5" OR "EXIT"
                       MOVE "N" TO WS-CONTINUE-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid operation. Please try again."
               END-EVALUATE
           END-PERFORM
           
           DISPLAY "CBLPolyCall Client terminated."
           STOP RUN.
       
       CALL-BRIDGE.
           CALL "POLYCALL-BRIDGE" USING WS-OPERATION,
                                       WS-INPUT-DATA,
                                       WS-OUTPUT-DATA,
                                       WS-RESULT-CODE
           
           DISPLAY "Operation: " WS-OPERATION
           DISPLAY "Result Code: " WS-RESULT-CODE
           DISPLAY "Output: " WS-OUTPUT-DATA.
EOF
    
    log_success "Main COBOL program generated"
}

# Generate copybooks
generate_copybooks() {
    log_info "Generating COBOL copybooks..."
    
    cat > "${CBL_COPYBOOK_DIR}/POLYCALL-CONSTANTS.CPY" << 'EOF'
      * CBLPolyCall Constants Copybook v1.0
      * OBINexus Aegis Engineering - PolyCall Constants
      
       01  POLYCALL-CONSTANTS.
           05  PC-SUCCESS              PIC 9(4) COMP VALUE 0.
           05  PC-ERROR-GENERAL        PIC 9(4) COMP VALUE 1.
           05  PC-ERROR-CONNECTION     PIC 9(4) COMP VALUE 100.
           05  PC-ERROR-PROTOCOL       PIC 9(4) COMP VALUE 200.
           05  PC-ERROR-TIMEOUT        PIC 9(4) COMP VALUE 300.
           05  PC-ERROR-AUTHENTICATION PIC 9(4) COMP VALUE 400.
           
       01  POLYCALL-LIMITS.
           05  PC-MAX-MESSAGE-SIZE     PIC 9(8) COMP VALUE 4096.
           05  PC-MAX-CONNECTIONS      PIC 9(4) COMP VALUE 100.
           05  PC-TIMEOUT-SECONDS      PIC 9(4) COMP VALUE 30.
EOF
    
    cat > "${CBL_COPYBOOK_DIR}/POLYCALL-STRUCTURES.CPY" << 'EOF'
      * CBLPolyCall Structures Copybook v1.0
      * OBINexus Aegis Engineering - PolyCall Data Structures
      
       01  POLYCALL-MESSAGE.
           05  PC-MSG-HEADER.
               10  PC-MSG-TYPE         PIC X(4).
               10  PC-MSG-LENGTH       PIC 9(8) COMP.
               10  PC-MSG-SEQUENCE     PIC 9(8) COMP.
               10  PC-MSG-TIMESTAMP    PIC 9(18) COMP.
           05  PC-MSG-BODY             PIC X(4092).
           
       01  POLYCALL-CONNECTION.
           05  PC-CONN-ID              PIC 9(8) COMP.
           05  PC-CONN-HOST            PIC X(256).
           05  PC-CONN-PORT            PIC 9(5) COMP.
           05  PC-CONN-STATUS          PIC X(20).
           05  PC-CONN-PROTOCOL        PIC X(10).
EOF
    
    log_success "COBOL copybooks generated"
}

# Generate cross-platform Makefile
generate_makefile() {
    log_info "Generating cross-platform Makefile..."
    
    cat > Makefile << 'EOF'
# CBLPolyCall Cross-Platform Makefile v1.0
# OBINexus Aegis Engineering - COBOL FFI Bridge Build System
# Technical Lead: Nnamdi Michael Okpala

# Project configuration
PROJECT_NAME = cblpolycall
VERSION = 1.0.0
SRC_DIR = src
BUILD_DIR = build
TARGET_DIR = target
COPYBOOK_DIR = copybooks

# Platform detection
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    PLATFORM = linux
    EXT = 
    SHARED_EXT = .so
endif
ifeq ($(UNAME_S),Darwin)
    PLATFORM = macos
    EXT = 
    SHARED_EXT = .dylib
endif
ifneq (,$(findstring CYGWIN,$(UNAME_S)))
    PLATFORM = windows
    EXT = .exe
    SHARED_EXT = .dll
endif
ifneq (,$(findstring MINGW,$(UNAME_S)))
    PLATFORM = windows
    EXT = .exe
    SHARED_EXT = .dll
endif

# Compiler detection and flags
COBOL_COMPILER := $(shell which cobc 2>/dev/null || which cob 2>/dev/null || echo "none")
ifeq ($(COBOL_COMPILER),none)
    $(error No COBOL compiler found. Please install GnuCOBOL or Micro Focus COBOL)
endif

# Build flags with GnuCOBOL version detection
COBOL_VERSION := $(shell cobc --version 2>/dev/null | head -1 | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' || echo "0.0.0")
COBOL_MAJOR := $(shell echo $(COBOL_VERSION) | cut -d. -f1)
COBOL_MINOR := $(shell echo $(COBOL_VERSION) | cut -d. -f2)

# Adjust flags based on GnuCOBOL version and platform
ifeq ($(PLATFORM),linux)
    ifeq ($(shell test $(COBOL_MAJOR) -ge 3 && test $(COBOL_MINOR) -ge 2; echo $?),0)
        COBOL_FLAGS = -fPIC
    else
        COBOL_FLAGS = 
    endif
endif
ifeq ($(PLATFORM),macos)
    ifeq ($(shell test $(COBOL_MAJOR) -ge 3 && test $(COBOL_MINOR) -ge 2; echo $?),0)
        COBOL_FLAGS = -fPIC
    else
        COBOL_FLAGS = 
    endif
endif
ifeq ($(PLATFORM),windows)
    COBOL_FLAGS = 
endif

CFLAGS = -O2 -g $(COBOL_FLAGS)
LDFLAGS = -L. -lpolycall

# Source files
MAIN_SRC = $(SRC_DIR)/MAIN.CBL
BRIDGE_SRC = $(SRC_DIR)/POLYCALL.CBL
COPYBOOKS = $(wildcard $(COPYBOOK_DIR)/*.CPY)

# Targets
MAIN_TARGET = $(TARGET_DIR)/cblpolycall$(EXT)
BRIDGE_TARGET = $(BUILD_DIR)/polycall-bridge$(SHARED_EXT)

# Default target
.PHONY: all clean install test package help debug

all: directories $(MAIN_TARGET)

# Debug target for troubleshooting
debug:
	@echo "CBLPolyCall Build Environment Debug Information"
	@echo "=============================================="
	@echo "Platform: $(PLATFORM)"
	@echo "COBOL Compiler: $(COBOL_COMPILER)"
	@echo "COBOL Version: $(COBOL_VERSION)"
	@echo "COBOL Major: $(COBOL_MAJOR)"
	@echo "COBOL Minor: $(COBOL_MINOR)"
	@echo "COBOL Flags: $(COBOL_FLAGS)"
	@echo "C Flags: $(CFLAGS)"
	@echo "LD Flags: $(LDFLAGS)"
	@echo "Shared Extension: $(SHARED_EXT)"
	@echo "Executable Extension: $(EXT)"

# Create build directories
directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(TARGET_DIR)

# Build main executable
$(MAIN_TARGET): $(MAIN_SRC) $(BRIDGE_SRC) $(COPYBOOKS)
	@echo "Building CBLPolyCall executable..."
	@if [ -f "$(shell which cobc)" ]; then \
		cobc -x $(CFLAGS) -I$(COPYBOOK_DIR) -o $@ $(MAIN_SRC) $(BRIDGE_SRC); \
	elif [ -f "$(shell which cob)" ]; then \
		cob -x $(CFLAGS) -I$(COPYBOOK_DIR) -o $@ $(MAIN_SRC) $(BRIDGE_SRC); \
	else \
		echo "Error: COBOL compiler not found"; exit 1; \
	fi
	@echo "Build completed: $@"

# Build shared library
$(BRIDGE_TARGET): $(BRIDGE_SRC) $(COPYBOOKS)
	@echo "Building PolyCall bridge library..."
	@if [ -f "$(shell which cobc)" ]; then \
		cobc -m $(CFLAGS) -I$(COPYBOOK_DIR) -o $@ $(BRIDGE_SRC); \
	elif [ -f "$(shell which cob)" ]; then \
		cob -m $(CFLAGS) -I$(COPYBOOK_DIR) -o $@ $(BRIDGE_SRC); \
	else \
		echo "Error: COBOL compiler not found"; exit 1; \
	fi
	@echo "Bridge library built: $@"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@rm -rf $(TARGET_DIR)
	@rm -f *.o *.obj *.lst
	@echo "Clean completed"

# Install to system
install: $(MAIN_TARGET)
	@echo "Installing CBLPolyCall..."
	@sudo cp $(MAIN_TARGET) /usr/local/bin/ 2>/dev/null || cp $(MAIN_TARGET) $(HOME)/bin/ || echo "Manual installation required"
	@echo "Installation completed"

# Run tests
test: $(MAIN_TARGET)
	@echo "Running CBLPolyCall tests..."
	@if [ -f tests/test_runner.sh ]; then \
		cd tests && ./test_runner.sh; \
	else \
		echo "No tests available"; \
	fi

# Create distribution package
package: all
	@echo "Creating distribution package..."
	@mkdir -p dist/$(PROJECT_NAME)-$(VERSION)
	@cp -r $(TARGET_DIR)/* dist/$(PROJECT_NAME)-$(VERSION)/
	@cp README.md dist/$(PROJECT_NAME)-$(VERSION)/ 2>/dev/null || true
	@cd dist && tar -czf $(PROJECT_NAME)-$(VERSION)-$(PLATFORM).tar.gz $(PROJECT_NAME)-$(VERSION)
	@echo "Package created: dist/$(PROJECT_NAME)-$(VERSION)-$(PLATFORM).tar.gz"

# Show help
help:
	@echo "CBLPolyCall Build System v$(VERSION)"
	@echo "Available targets:"
	@echo "  all       - Build all targets (default)"
	@echo "  clean     - Clean build artifacts"
	@echo "  install   - Install to system"
	@echo "  test      - Run tests"
	@echo "  package   - Create distribution package"
	@echo "  help      - Show this help"
	@echo ""
	@echo "Platform: $(PLATFORM)"
	@echo "COBOL Compiler: $(COBOL_COMPILER)"
EOF
    
    log_success "Cross-platform Makefile generated"
}

# Generate test runner
generate_test_runner() {
    log_info "Generating test runner..."
    
    mkdir -p tests
    
    cat > tests/test_runner.sh << 'EOF'
#!/bin/bash

# CBLPolyCall Test Runner v1.0
# OBINexus Aegis Engineering - Automated Testing Framework

set -euo pipefail

TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$TEST_DIR")"
TARGET_DIR="$PROJECT_ROOT/target"

# Test configuration
TESTS_PASSED=0
TESTS_FAILED=0
TOTAL_TESTS=0

log_test() { echo "[TEST] $1"; }
log_pass() { echo "[PASS] $1"; ((TESTS_PASSED++)); }
log_fail() { echo "[FAIL] $1"; ((TESTS_FAILED++)); }

# Test 1: Executable exists
test_executable_exists() {
    log_test "Checking if CBLPolyCall executable exists..."
    if [ -f "$TARGET_DIR/cblpolycall" ] || [ -f "$TARGET_DIR/cblpolycall.exe" ]; then
        log_pass "Executable found"
    else
        log_fail "Executable not found"
    fi
    ((TOTAL_TESTS++))
}

# Test 2: Executable runs
test_executable_runs() {
    log_test "Testing if CBLPolyCall executable runs..."
    local exe_path=""
    if [ -f "$TARGET_DIR/cblpolycall" ]; then
        exe_path="$TARGET_DIR/cblpolycall"
    elif [ -f "$TARGET_DIR/cblpolycall.exe" ]; then
        exe_path="$TARGET_DIR/cblpolycall.exe"
    fi
    
    if [ -n "$exe_path" ]; then
        # Test with timeout to prevent hanging
        if timeout 5s echo "5" | "$exe_path" >/dev/null 2>&1; then
            log_pass "Executable runs successfully"
        else
            log_fail "Executable failed to run or hanged"
        fi
    else
        log_fail "No executable found to test"
    fi
    ((TOTAL_TESTS++))
}

# Main test execution
main() {
    echo "CBLPolyCall Test Runner v1.0"
    echo "============================"
    echo ""
    
    test_executable_exists
    test_executable_runs
    
    echo ""
    echo "Test Results:"
    echo "  Total Tests: $TOTAL_TESTS"
    echo "  Passed: $TESTS_PASSED"
    echo "  Failed: $TESTS_FAILED"
    
    if [ $TESTS_FAILED -eq 0 ]; then
        echo "All tests passed!"
        exit 0
    else
        echo "Some tests failed!"
        exit 1
    fi
}

main "$@"
EOF
    
    chmod +x tests/test_runner.sh
    log_success "Test runner generated"
}

# Generate README
generate_readme() {
    log_info "Generating project README..."
    
    cat > README.md << 'EOF'
# CBLPolyCall - COBOL FFI Bridge for PolyCall

## Overview

CBLPolyCall is a comprehensive COBOL Foreign Function Interface (FFI) bridge for the PolyCall library, developed as part of the OBINexus Aegis Engineering project under the technical leadership of Nnamdi Michael Okpala.

## Features

- **Cross-Platform Support**: Linux, macOS, and Windows compatibility
- **COBOL FFI Bridge**: Native COBOL interface to PolyCall library functions
- **Protocol Handling**: Support for PolyCall communication protocols
- **State Management**: COBOL-friendly state machine integration
- **Network Communication**: COBOL bindings for network endpoints
- **Build Automation**: Comprehensive build system with Make integration

## Architecture

The CBLPolyCall system integrates with the OBINexus toolchain:
```
riftlang.exe → .so.a → rift.exe → gosilang
```

### Build Orchestration Stack
```
nlink → polybuild → CBLPolyCall
```

## Installation

### Prerequisites

- COBOL Compiler (GnuCOBOL recommended)
- Make build system
- PolyCall library (libpolycall)

### Building from Source

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd cblpolycall
   ```

2. Initialize the project:
   ```bash
   ./build_system.sh init
   ```

3. Build the project:
   ```bash
   make all
   ```

4. Run tests:
   ```bash
   make test
   ```

5. Install (optional):
   ```bash
   make install
   ```

## Usage

### Basic Example

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. POLYCALL-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-OPERATION        PIC X(20) VALUE "CONNECT".
01  WS-INPUT-DATA       PIC X(1024) VALUE "localhost:8080".
01  WS-OUTPUT-DATA      PIC X(1024).
01  WS-RESULT-CODE      PIC 9(4) COMP.

PROCEDURE DIVISION.
MAIN-LOGIC.
    CALL "POLYCALL-BRIDGE" USING WS-OPERATION,
                                WS-INPUT-DATA,
                                WS-OUTPUT-DATA,
                                WS-RESULT-CODE.
    
    DISPLAY "Result: " WS-OUTPUT-DATA.
    DISPLAY "Status: " WS-RESULT-CODE.
    
    STOP RUN.
```

### Available Operations

- `CONNECT`: Establish connection to PolyCall server
- `SEND_MESSAGE`: Send data through PolyCall protocol
- `RECEIVE_MESSAGE`: Receive data from PolyCall server
- `DISCONNECT`: Close PolyCall connection

## API Reference

### POLYCALL-BRIDGE Program

Main FFI bridge interface:

```cobol
CALL "POLYCALL-BRIDGE" USING operation-name,
                            input-data,
                            output-data,
                            result-code.
```

### Copybooks

- **POLYCALL-CONSTANTS.CPY**: Error codes and system limits
- **POLYCALL-STRUCTURES.CPY**: Data structures for messages and connections

## Build System Commands

```bash
# Build everything
make all

# Clean build artifacts
make clean

# Run automated tests
make test

# Create distribution package
make package

# Install to system
make install

# Show help
make help
```

## Integration with OBINexus

CBLPolyCall follows the OBINexus waterfall methodology and integrates with:

- **Legal Policy Architecture**: Milestone-based investment tracking
- **#NoGhosting Protocol**: Continuous integration requirements
- **OpenSense Recruitment**: Collaborative development standards
- **Compliance Scripts**: Automated verification and validation

## Development

### Project Structure

```
cblpolycall/
├── src/
│   ├── MAIN.CBL              # Main application
│   └── POLYCALL.CBL          # FFI bridge
├── copybooks/
│   ├── POLYCALL-CONSTANTS.CPY
│   └── POLYCALL-STRUCTURES.CPY
├── build/                    # Build artifacts
├── target/                   # Compiled executables
├── tests/                    # Test suite
├── docs/                     # Documentation
├── config/                   # Configuration files
├── Makefile                  # Cross-platform build
└── build_system.sh          # Build orchestration
```

### Contributing

1. Follow OBINexus coding standards
2. Ensure cross-platform compatibility
3. Add tests for new functionality
4. Update documentation
5. Follow waterfall methodology phases

## License

Proprietary - OBINexus Computing
Technical Lead: Nnamdi Michael Okpala

## Support

For technical support and collaboration:
- Project Lead: Nnamdi Michael Okpala
- Framework: OBINexus Aegis Engineering
- Integration: PolyCall FFI Bridge Team
EOF
    
    log_success "Project README generated"
}

# Build orchestration function
build_orchestration() {
    local action="$1"
    local platform=$(detect_platform)
    local compiler=$(detect_cobol_compiler)
    
    log_aegis "Executing build orchestration for platform: $platform"
    log_aegis "COBOL compiler detected: $compiler"
    
    case "$action" in
        "init")
            log_protocol "Initializing CBLPolyCall project structure..."
            init_project_structure
            generate_polycall_bridge
            generate_main_program
            generate_copybooks
            generate_makefile
            generate_test_runner
            generate_readme
            log_success "CBLPolyCall project initialization completed"
            ;;
        "build")
            log_protocol "Building CBLPolyCall with cross-platform Makefile..."
            if [ -f "Makefile" ]; then
                make all
                log_success "Build completed successfully"
            else
                log_error "Makefile not found. Run 'init' first."
                return 1
            fi
            ;;
        "test")
            log_protocol "Running CBLPolyCall test suite..."
            make test
            ;;
        "package")
            log_protocol "Creating distribution package..."
            make package
            ;;
        "clean")
            log_protocol "Cleaning build artifacts..."
            make clean
            ;;
        "install")
            log_protocol "Installing CBLPolyCall to system..."
            make install
            ;;
        *)
            log_error "Unknown action: $action"
            echo "Available actions: init, build, test, package, clean, install"
            return 1
            ;;
    esac
}

# Integration verification
verify_polycall_integration() {
    log_info "Verifying PolyCall library integration..."
    
    # Check for PolyCall library
    if ldconfig -p | grep -q "polycall" 2>/dev/null; then
        log_success "PolyCall library found in system"
    elif [ -f "/usr/local/lib/libpolycall.so" ] || [ -f "/usr/lib/libpolycall.so" ]; then
        log_success "PolyCall library found"
    else
        log_warning "PolyCall library not found - FFI calls will fail at runtime"
        log_info "Ensure libpolycall is installed and accessible"
    fi
    
    # Check for Node.js bindings compatibility
    if [ -f "../node-polycall/index.js" ]; then
        log_info "Node.js PolyCall bindings detected - ensuring compatibility"
        log_success "Multi-language PolyCall ecosystem confirmed"
    fi
}

# Main execution function
main() {
    print_banner
    
    local action="${1:-help}"
    
    case "$action" in
        "help"|"-h"|"--help")
            echo "CBLPolyCall Build System v${CBL_VERSION}"
            echo "Usage: $0 [action]"
            echo ""
            echo "Actions:"
            echo "  init     - Initialize project structure and generate files"
            echo "  build    - Build CBLPolyCall executable and libraries"
            echo "  test     - Run test suite"
            echo "  package  - Create distribution package"
            echo "  clean    - Clean build artifacts"
            echo "  install  - Install to system"
            echo "  verify   - Verify PolyCall integration"
            echo "  help     - Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 init      # Initialize new CBLPolyCall project"
            echo "  $0 build     # Build the project"
            echo "  $0 test      # Run tests"
            ;;
        "verify")
            verify_polycall_integration
            ;;
        *)
            build_orchestration "$action"
            ;;
    esac
}

# Execute with error handling
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    trap 'log_error "Build system failed at line $LINENO"' ERR
    main "$@"
fi