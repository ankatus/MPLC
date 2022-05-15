# MPLC Usage Instructions
## System Requirements
[.NET 6 Runtime](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) (SDK is fine, too)  
Tested on Ubuntu 22.10, should work on other distros and Windows 10 as well.
## Usage
- Place `Compiler.dll` and `Compiler.runtimeconfig.json` in a location of your choosing.
- Run `dotnet Compiler.dll <input_path> <output_path>`, where `<input_path> and <output_path>` are the path to your input file and your output file, respectively.
## Building From Source
If you have the [.NET 6 SDK](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) installed, you can run `dotnet build` in the `MPLC/Compiler-directory`. The artifacts can then be found somewhere in `MPLC/Compiler/bin/`. You can also "run from source" with `dotnet run`.