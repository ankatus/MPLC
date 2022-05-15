using System.Reflection;

namespace Compiler;

public static class Cli
{
    public static void Run(string[] args)
    {
        if (args.Length != 2)
        {
            Console.WriteLine("Expected two arguments: INPUT_PATH, OUTPUT_PATH");
            Environment.Exit(1);            
        }

        try
        {
            var inputPath = args[0];
            var fileString = File.ReadAllText(inputPath);

            var scannerInput = new ScannerInput(fileString);
            var scanner = new Scanner(scannerInput);
            var tokens = scanner.Scan();

            var parser = new Parser(tokens);
            var parseTree = parser.Parse();

            var analyzer = new Analyzer(parseTree);
            var ast = analyzer.Analyze();

            var generator = new Generator(ast);
            var output = generator.Generate();

            var outputPath = args[1];
            File.WriteAllText(outputPath, output);

            Console.WriteLine($"Compiled program saved to {outputPath}");
        }
        catch (ScanningException e)
        {
            Console.WriteLine($"Error during scanning:\n{e.Message}");    
        }
        
        
    }
}