@echo off

if "clean"=="%1" (
    if exist bin\ rmdir /q /s bin
) else if "cl"=="%1" (

    if "%VisualStudioVersion%"=="" call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

    if not exist bin\ mkdir bin
    pushd bin
        cl /source-charset:utf-8 /Zi -I..\Include ^
            ..\Build.c /FeGraph.exe gdi32.lib user32.lib^
            /link /subsystem:windows /stack:0x1000000
    popd
) else (
    if not exist bin\ mkdir bin
    gcc -O2 -ggdb -Wall -Wextra -Wpedantic -IInclude ^
        Build.c -o bin\Graph.exe ^
        -lgdi32 -luser32 -lkernel32 -Wl,--stack,0x1000000
)
