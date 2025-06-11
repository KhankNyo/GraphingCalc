# Graphing Caculator
- Written in C without any external dependencies eexcept for platform APIs
- Has a minimal JIT compiler that compiles functions into machine code for the current machine and platform (only x64 Windows for now)

# Build
- Windows, MSVC:
  - Make sure Microsoft Visual Studio 2022 is installed
  - Make sure vcvarsall.bat is in "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\"
    - If not, find where Microsoft Visual Studio is installed on your machine, and copy the path of vcvarsall.bat into build.bat:
      - The line ```call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64```
      - Becomes  ```call "(path to Microsoft Visual Studio)\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64```
  - To build:
    - Open the command line (cmd.exe is fine) to where build.bat resides
    - Run ```build.bat cl```
  - To remove built binaries:
    - Run ```build.bat clean```
- Windows, GCC/non-MSVC compiler:
  - Make sure GCC or the compiler you're planning to use is installed and can be invoked where build.bat resides
  - To build:
    - Open the command line (cmd.exe is fine) where build.bat resides
    - Run ```build.bat <compiler name>```, example: ```build.bat gcc```
  - To remove built binaries:
    - Run ```build.bat clean```
