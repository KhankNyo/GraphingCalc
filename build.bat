@echo off

if "clean"=="%1" (
    if exist bin\ RMDIR /q /s bin

) else if "cl"=="%1" (
    if "%VisualStudioVersion%"=="" call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

    if not exist bin\ MKDIR bin
    PUSHD bin
        cl /Od /source-charset:utf-8 /Zi -I..\Include                   ^
            /DJIT_BACKEND_X64_WINDOWS                                     ^
            ..\x64_Windows_Build.c /FeGraph.exe gdi32.lib user32.lib    ^
            /link /subsystem:console /stack:0x1000000
    POPD

) else (
SETLOCAL EnableDelayedExpansion
    if not exist bin\ MKDIR bin

    SET COMPILE_OPT= ^
        -O0 -march=native -ggdb -Wall -Wextra -Wpedantic -IInclude  ^
        -DJIT_BACKEND_X64_WINDOWS                                     ^
        x64_Windows_Build.c -o bin\Graph.exe                        ^
        -lgdi32 -luser32 -lkernel32 -Wl,--stack,0x1000000"

    REM no compiler specified, use gcc as default
    if "%1"=="" (
        gcc !COMPILE_OPT!
    ) else (
        "%1" !COMPILE_OPT!
    )
)
