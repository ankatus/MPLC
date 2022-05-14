using System.IO;
using System.Text;
using Compiler;
using NUnit.Framework;

namespace Tests;

public class EndToEndTests
{
    private const string TEST_DATA_PATH = @"/home/antti/Programming/MPLC/Tests";

    [Test]
    [TestCase($@"{TEST_DATA_PATH}/ProgramA.txt")]
    [TestCase($@"{TEST_DATA_PATH}/ProgramB.txt")]
    [TestCase($@"{TEST_DATA_PATH}/ProgramC.txt")]
    [Parallelizable(ParallelScope.All)]
    public void EndToEndTest(string programFile)
    {
        var program = File.ReadAllText(programFile, Encoding.UTF8);
        var scanner = new Scanner(new ScannerInput(program));
        var tokens = scanner.Scan();
        var parser = new Parser(tokens);
        var pt = parser.Parse();
        var analyzer = new Analyzer(pt);
        var ast = analyzer.Analyze();
        var generator = new Generator(ast);
        var output = generator.Generate();
    }
}