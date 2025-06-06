@echo off
REM CBLPolyCall Windows Build System v1.0
REM OBINexus Aegis Engineering - COBOL FFI Bridge Windows Builder
REM Technical Lead: Nnamdi Michael Okpala

setlocal enabledelayedexpansion

REM Build configuration
set CBL_PROJECT_NAME=cblpolycall
set CBL_VERSION=1.0.0
set CBL_SRC_DIR=src
set CBL_BUILD_DIR=build
set CBL_TARGET_DIR=target
set CBL_COPYBOOK_DIR=copybooks

REM Colors for output (if supported)
set GREEN=[92m
set RED=[91m
set YELLOW=[93m
set BLUE=[94m
set NC=[0m

echo.
echo ==================================================================
echo   CBLPolyCall Windows Build System v%CBL_VERSION%
echo   OBINexus Aegis Engineering - COBOL FFI Bridge
echo   Technical Lead: Nnamdi Michael Okpala
echo   Platform: Windows
echo ==================================================================
echo.

REM Detect COBOL compiler
where cobc >nul 2>&1
if %errorlevel% == 0 (
    set COBOL_COMPILER=cobc
    echo %BLUE%[BUILD]%NC% GnuCOBOL detected: cobc
) else (
    where cob >nul 2>&1
    if %errorlevel% == 0 (
        set COBOL_COMPILER=cob
        echo %BLUE%[BUILD]%NC% Micro Focus COBOL detected: cob
    ) else (
        echo %RED%[ERROR]%NC% No COBOL compiler found. Please install GnuCOBOL or Micro Focus COBOL
        exit /b 1
    )
)

REM Parse command line arguments
set ACTION=%1
if "%ACTION%"=="" set ACTION=build

if "%ACTION%"=="help" goto :show_help
if "%ACTION%"=="clean" goto :clean_build
if "%ACTION%"=="debug" goto :debug_info
if "%ACTION%"=="build" goto :build_project
if "%ACTION%"=="test" goto :run_tests
if "%ACTION%"=="init" goto :init_project

echo %RED%[ERROR]%NC% Unknown action: %ACTION%
goto :show_help

:init_project
echo %BLUE%[BUILD]%NC% Initializing project structure...
if not exist %CBL_SRC_DIR% mkdir %CBL_SRC_DIR%
if not exist %CBL_BUILD_DIR% mkdir %CBL_BUILD_DIR%
if not exist %CBL_TARGET_DIR% mkdir %CBL_TARGET_DIR%
if not exist %CBL_COPYBOOK_DIR% mkdir %CBL_COPYBOOK_DIR%
if not exist tests mkdir tests
echo %GREEN%[SUCCESS]%NC% Project structure initialized
goto :eof

:clean_build
echo %BLUE%[BUILD]%NC% Cleaning build artifacts...
if exist %CBL_BUILD_DIR% rmdir /s /q %CBL_BUILD_DIR%
if exist %CBL_TARGET_DIR% rmdir /s /q %CBL_TARGET_DIR%
del /q *.obj *.lst 2>nul
echo %GREEN%[SUCCESS]%NC% Clean completed
goto :eof

:debug_info
echo CBLPolyCall Build Environment Debug Information
echo ==============================================
echo Platform: Windows
echo COBOL Compiler: %COBOL_COMPILER%
echo Project Name: %CBL_PROJECT_NAME%
echo Version: %CBL_VERSION%
echo Source Directory: %CBL_SRC_DIR%
echo Build Directory: %CBL_BUILD_DIR%
echo Target Directory: %CBL_TARGET_DIR%
echo Copybook Directory: %CBL_COPYBOOK_DIR%
goto :eof

:build_project
echo %BLUE%[BUILD]%NC% Building CBLPolyCall executable...

REM Create directories if they don't exist
if not exist %CBL_BUILD_DIR% mkdir %CBL_BUILD_DIR%
if not exist %CBL_TARGET_DIR% mkdir %CBL_TARGET_DIR%

REM Check if source files exist
if not exist %CBL_SRC_DIR%\MAIN.CBL (
    echo %RED%[ERROR]%NC% Source file not found: %CBL_SRC_DIR%\MAIN.CBL
    exit /b 1
)

if not exist %CBL_SRC_DIR%\POLYCALL.CBL (
    echo %RED%[ERROR]%NC% Source file not found: %CBL_SRC_DIR%\POLYCALL.CBL
    exit /b 1
)

REM Build executable
echo %BLUE%[BUILD]%NC% Compiling COBOL sources...
%COBOL_COMPILER% -x -I%CBL_COPYBOOK_DIR% -o %CBL_TARGET_DIR%\%CBL_PROJECT_NAME%.exe %CBL_SRC_DIR%\MAIN.CBL %CBL_SRC_DIR%\POLYCALL.CBL

if %errorlevel% == 0 (
    echo %GREEN%[SUCCESS]%NC% Build completed: %CBL_TARGET_DIR%\%CBL_PROJECT_NAME%.exe
) else (
    echo %RED%[ERROR]%NC% Build failed with error code %errorlevel%
    exit /b %errorlevel%
)
goto :eof

:run_tests
echo %BLUE%[BUILD]%NC% Running CBLPolyCall tests...
if not exist %CBL_TARGET_DIR%\%CBL_PROJECT_NAME%.exe (
    echo %RED%[ERROR]%NC% Executable not found. Run 'build.bat build' first.
    exit /b 1
)

echo %BLUE%[TEST]%NC% Testing executable existence...
if exist %CBL_TARGET_DIR%\%CBL_PROJECT_NAME%.exe (
    echo %GREEN%[PASS]%NC% Executable found
) else (
    echo %RED%[FAIL]%NC% Executable not found
    exit /b 1
)

echo %BLUE%[TEST]%NC% Testing executable execution...
echo 5 | %CBL_TARGET_DIR%\%CBL_PROJECT_NAME%.exe >nul 2>&1
if %errorlevel% == 0 (
    echo %GREEN%[PASS]%NC% Executable runs successfully
) else (
    echo %YELLOW%[WARNING]%NC% Executable test completed with exit code %errorlevel%
)

echo %GREEN%[SUCCESS]%NC% Test suite completed
goto :eof

:show_help
echo CBLPolyCall Windows Build System v%CBL_VERSION%
echo Usage: build.bat [action]
echo.
echo Actions:
echo   init     - Initialize project structure
echo   build    - Build CBLPolyCall executable (default)
echo   clean    - Clean build artifacts
echo   debug    - Show build environment information
echo   test     - Run test suite
echo   help     - Show this help message
echo.
echo Examples:
echo   build.bat init      # Initialize new project
echo   build.bat build     # Build the project
echo   build.bat test      # Run tests
echo.
goto :eof
