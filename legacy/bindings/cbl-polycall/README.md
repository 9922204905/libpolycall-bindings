# CBLPolyCall - COBOL FFI Bridge for PolyCall

## Overview
https://www.arnoldtrembley.com/GnuCOBOL.htm 
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
