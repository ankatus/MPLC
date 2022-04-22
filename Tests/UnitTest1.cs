using System;
using NUnit.Framework;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Compiler;

namespace Tests;

public class ScannerTests
{
    private const string TEST_DATA_PATH = @"/home/antti/Programming/MPLC/Tests";
    private readonly string _data;
    private Scanner? _scanner;

    public ScannerTests()
    {
        _data = File.ReadAllText($"{TEST_DATA_PATH}/ValidTokens.txt", Encoding.UTF8);
    }

    [SetUp]
    public void Setup()
    {
        var input = new ScannerInput(_data);
        _scanner = new Scanner(input);
    }

    [Test]
    public void ValidTokensTest()
    {
        List<Token> expectedTokens = new()
        {
            new Token(TokenType.KW_WHILE, "while", 1, 1),
            new Token(TokenType.IDENTIFIER, "asdf", 1, 7),
            new Token(TokenType.KW_OR, "or", 1, 12),
            new Token(TokenType.KW_AND, "and", 1, 15),
            new Token(TokenType.KW_RETURN, "return", 2, 1),
            new Token(TokenType.KW_ASSERT, "assert", 2, 8),
            new Token(TokenType.IDENTIFIER, "lhöarthjad", 2, 15),
            new Token(TokenType.NUMBER, "123", 3, 1),
            new Token(TokenType.IDENTIFIER, "asdf", 3, 4),
            new Token(TokenType.NUMBER, "123456789", 3, 9),
            new Token(TokenType.IDENTIFIER, "asdf", 4, 1),
            new Token(TokenType.STRING, "this is a string", 4, 6),
            new Token(TokenType.STRING, "\nthis is a multiline string\n", 5, 1),
            new Token(TokenType.IDENTIFIER, "anIdentifier", 8, 39),
            new Token(TokenType.GREATER_THAN, ">", 9, 1),
            new Token(TokenType.OP_ADD, "+", 9, 2),
            new Token(TokenType.EQUALS, "=", 9, 3),
            new Token(TokenType.GREATER_THAN_OR_EQ, ">=", 9, 4),
            new Token(TokenType.PERIOD, ".", 9, 6),
            new Token(TokenType.PERIOD, ".", 9, 7),
            new Token(TokenType.SEMICOLON, ";", 9, 8),
            new Token(TokenType.COLON, ":", 9, 9),
            new Token(TokenType.LESS_THAN, "<", 9, 10),
            new Token(TokenType.GREATER_THAN, ">", 10, 1),
            new Token(TokenType.DIAMOND, "<>", 10, 2),
            new Token(TokenType.OPEN_PARENS, "(", 10, 4),
            new Token(TokenType.OPEN_SQUARE, "[", 10, 5),
            new Token(TokenType.CLOSE_SQUARE, "]", 10, 6),
            new Token(TokenType.CLOSE_PARENS, ")", 10, 7),
            new Token(TokenType.KW_ASSIGN, ":=", 10, 8),
            new Token(TokenType.COLON, ":", 10, 10),
            new Token(TokenType.PERIOD, ".", 11, 1),
            new Token(TokenType.NUMBER, "123", 11, 2),
            new Token(TokenType.NUMBER, "0.5", 11, 6),
            new Token(TokenType.NUMBER, "5.5", 11, 10),
            new Token(TokenType.PERIOD, ".", 11, 13),
            new Token(TokenType.NUMBER, "5", 11, 14),
            new Token(TokenType.NUMBER, "0", 11, 16),
            new Token(TokenType.PERIOD, ".", 11, 17),
            new Token(TokenType.NUMBER, "123.4", 12, 1),
            new Token(TokenType.NUMBER, "10.123e-500", 12, 7),
            new Token(TokenType.NUMBER, "0.5e+25", 12, 19),
            new Token(TokenType.NUMBER, "1.1e123", 12, 27),

            
            
            Token.Eof,
        };

        var result = _scanner!.Scan();
        
        Assert.AreEqual(expectedTokens.Count, result.Count);
        for (var i = 0; i < result.Count; i++)
        {
            Assert.AreEqual(expectedTokens[i], result[i]);
        }
    }
}