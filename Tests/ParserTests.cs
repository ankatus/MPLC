using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using Compiler;

namespace Tests;

public class ParserTests
{
    private record TestToken(TokenType Type, string Lexeme = "", int Line = 0, int Col = 0) : Token(Type, Lexeme, Line, Col);

    private static bool TreeContains(PtNode tree, PtNode node)
    {
        bool NodesMatch(PtNode actual, PtNode expected)
        {
            switch (actual)
            {
                case PtNonTerminal nonTerminalA when expected is PtNonTerminal nonTerminalB:
                {
                    if (nonTerminalA.Children.Count == nonTerminalB.Children.Count)
                    {
                        var aNonTerminals = nonTerminalA.Children.OfType<PtNonTerminal>().ToList();
                        var bNonTerminals = nonTerminalB.Children.OfType<PtNonTerminal>().ToList();

                        if (aNonTerminals.Count != bNonTerminals.Count)
                            return false;

                        if (aNonTerminals.Where((t, i) => t.Type != bNonTerminals[i].Type).Any())
                            return false;

                        var aTerminals = nonTerminalA.Children.OfType<PtTerminal>().ToList();
                        var bTerminals = nonTerminalB.Children.OfType<PtTerminal>().ToList();

                        if (aTerminals.Count != bTerminals.Count)
                            return false;

                        if (aTerminals.Where((t, i) => t.Token != bTerminals[i].Token).Any())
                        {
                            return false;
                        }
                    }
                    else
                    {
                        return false;
                    }

                    break;
                }
                case PtTerminal terminalA when expected is PtTerminal terminalB:
                    return terminalA.Token == terminalB.Token;
                default:
                    return false;
            }

            return true;
        }

        if (NodesMatch(tree, node))
            return true;

        if (tree is not PtNonTerminal nonTerminal)
            return false;

        return nonTerminal.Children.Any(child => TreeContains(child, node));
    }
    
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public void InvalidProgramTest()
    {
        var tokens = new List<Token>
        {
            new TestToken(TokenType.KW_PROGRAM, "program"),
            new TestToken(TokenType.IDENTIFIER, "abc"),
            // new TestToken(TokenType.SEMICOLON, ";"),
            new TestToken(TokenType.KW_BEGIN, "begin"),
            new TestToken(TokenType.IDENTIFIER, "writeln"),
            new TestToken(TokenType.OPEN_PARENS, "("),
            new TestToken(TokenType.STRING, "Hello World!"),
            new TestToken(TokenType.CLOSE_PARENS, ")"),
            new TestToken(TokenType.KW_END, "end"),
            new TestToken(TokenType.PERIOD, "."),
        };
        
        var parser = new Parser(tokens);
        Assert.Throws<ParseException>(() => parser.Parse());
    }
    
    [Test]
    public void ValidProgramTest()
    {
        var tokens = new List<Token>
        {
            new(TokenType.KW_PROGRAM, "program", 0, 0),
            new(TokenType.IDENTIFIER, "abc", 0, 0),
            new(TokenType.SEMICOLON, ";", 0, 0),
            new(TokenType.KW_BEGIN, "begin", 0, 0),
            new(TokenType.IDENTIFIER, "writeln", 0, 0),
            new(TokenType.OPEN_PARENS, "(", 0, 0),
            new(TokenType.STRING, "Hello World!", 0, 0),
            new(TokenType.CLOSE_PARENS, ")", 0, 0),
            new(TokenType.SEMICOLON, ";", 0, 0),
            new(TokenType.KW_END, "end", 0, 0),
            new(TokenType.PERIOD, ".", 0, 0),
        };
        
        var parsers = new Parser(tokens);
        
        var parseTree = parsers.Parse();

        var expectedProgram = new PtNonTerminal(NonTerminalType.PROGRAM,
            new List<PtNode>
            {
                new PtTerminal(new Token(TokenType.KW_PROGRAM, "program", 0, 0)),
                new PtTerminal(new Token(TokenType.IDENTIFIER, "abc", 0, 0)),
                new PtTerminal(new Token(TokenType.SEMICOLON, ";", 0, 0)),
                new PtNonTerminal(NonTerminalType.BLOCK, new List<PtNode>()),
                new PtTerminal(new Token(TokenType.PERIOD, ".", 0, 0)),
            });

        var expectedWrite = new PtNonTerminal(NonTerminalType.WRITE,
            new List<PtNode>
            {
                new PtTerminal(new Token(TokenType.IDENTIFIER, "writeln", 0, 0)),
                new PtTerminal(new Token(TokenType.OPEN_PARENS, "(", 0, 0)),
                new PtNonTerminal(NonTerminalType.ARGUMENTS, new List<PtNode>()),
                new PtTerminal(new Token(TokenType.CLOSE_PARENS, ")", 0, 0)),
            });

        Assert.IsTrue(TreeContains(parseTree, expectedProgram));
        Assert.IsTrue(TreeContains(parseTree, expectedWrite));
    }

    [Test]
    public void ExampleProgramATest()
    {
        var tokens = new List<Token>()
        {
            // Program
            new TestToken(TokenType.KW_PROGRAM),
            new TestToken(TokenType.IDENTIFIER, "GCD"),
            new TestToken(TokenType.SEMICOLON),

            // Main block start
            new TestToken(TokenType.KW_BEGIN),

            // Declaration
            new TestToken(TokenType.KW_VAR),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.COMMA),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.COLON),
            new TestToken(TokenType.IDENTIFIER, "integer"),
            new TestToken(TokenType.SEMICOLON),

            // Read
            new TestToken(TokenType.IDENTIFIER, "read"),
            new TestToken(TokenType.OPEN_PARENS),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.COMMA),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.CLOSE_PARENS),
            new TestToken(TokenType.SEMICOLON),

            // While
            new TestToken(TokenType.KW_WHILE),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.DIAMOND),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.KW_DO),

            // If
            new TestToken(TokenType.KW_IF),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.GREATER_THAN),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.KW_THEN),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.KW_ASSIGN),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.OP_SUB),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.SEMICOLON),

            // Else
            new TestToken(TokenType.KW_ELSE),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.KW_ASSIGN),
            new TestToken(TokenType.IDENTIFIER, "j"),
            new TestToken(TokenType.OP_SUB),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.SEMICOLON),

            // Write
            new TestToken(TokenType.IDENTIFIER, "writeln"),
            new TestToken(TokenType.OPEN_PARENS),
            new TestToken(TokenType.IDENTIFIER, "i"),
            new TestToken(TokenType.CLOSE_PARENS),
            new TestToken(TokenType.SEMICOLON),

            // Main block end
            new TestToken(TokenType.KW_END),
            new TestToken(TokenType.PERIOD),
        };

        var parser = new Parser(tokens);
        var parseTree = parser.Parse();
    }

    [Test]
    public void EndToEndTest()
    {
        
    }
}