@echo off

if "clean"=="%1" (
    if exist bin\ rmdir /q /s bin
) else if "cl"=="%1" (

    if "%VisualStudioVersion%"=="" call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

    if not exist bin\ mkdir bin
    pushd bin
        cl /Zi -I..\Include ..\Win32.c -o Graph.exe ^
            gdi32.lib user32.lib^
            /link /subsystem:windows
    popd
) else (
    if not exist bin\ mkdir bin
    tcc -Wall -Wextra -Wpedantic -municode -IInclude Win32.c -o bin\Graph.exe -lgdi32 -luser32 -lkernel32
)
