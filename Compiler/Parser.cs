namespace Compiler;

public enum NonTerminalType
{
    PROGRAM,
    PROCEDURE,
    FUNCTION,
    BLOCK,
    EXPRESSION,
    SIMPLE_EXPRESSION,
    TERM,
    FACTOR,
    CALL,
    ARGUMENTS,
    PARAMETERS,
    ASSIGNMENT,
    READ,
    WRITE,
    ASSERT,
    IF,
    WHILE,
    RETURN,
    VARIABLE,
    LITERAL,
    STATEMENT,
    TYPE,
    SIMPLE_TYPE,
    ARRAY_TYPE,
    SIMPLE_STATEMENT,
    STRUCTURED_STATEMENT,
    VAR_DECLARATION
}

public abstract class PtNode
{
}

public class PtNonTerminal : PtNode
{
    public NonTerminalType Type { get; }
    public List<PtNode> Children { get; }

    public PtNonTerminal(NonTerminalType type, List<PtNode> children)
    {
        Type = type;
        Children = children;
    }
}

public class PtTerminal : PtNode
{
    public Token Token { get; }

    public PtTerminal(Token token)
    {
        Token = token;
    }
}

public class Parser
{
    private readonly List<Token> _tokens;
    private int _nextTokenIndex;
    private Token NextToken => _tokens[_nextTokenIndex];

    public Parser(List<Token> tokens)
    {
        if (tokens.Count < 1)
            throw new ArgumentException("Expected at least one token.", nameof(tokens));

        _tokens = tokens;
    }

    public PtNode Parse()
    {
        return ParseProgram();
    }

    private PtNode ParseProgram()
    {
        var children = new List<PtNode>
        {
            new PtTerminal(Consume(TokenType.KW_PROGRAM)),
            new PtTerminal(Consume(TokenType.IDENTIFIER)),
            new PtTerminal(Consume(TokenType.SEMICOLON)),
        };

        while (NextToken.Type is not TokenType.KW_BEGIN)
        {
            children.Add(NextToken switch
            {
                {Type: TokenType.KW_PROCEDURE} => ParseProcedure(),
                {Type: TokenType.KW_FUNCTION} => ParseFunction(),
                _ => throw new ParseException(GetExceptionMessage(NextToken, TokenType.KW_PROCEDURE,
                    TokenType.KW_FUNCTION)),
            });
        }

        children.Add(ParseBlock());
        children.Add(ConsumeToTerminal(TokenType.PERIOD));

        var node = new PtNonTerminal(NonTerminalType.PROGRAM, children);

        return node;
    }

    private PtNode ParseProcedure()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_PROCEDURE),
            ConsumeToTerminal(TokenType.IDENTIFIER),
            ConsumeToTerminal(TokenType.OPEN_PARENS),
            ParseParameters(),
            ConsumeToTerminal(TokenType.CLOSE_PARENS),
            ConsumeToTerminal(TokenType.SEMICOLON),
            ParseBlock(),
            ConsumeToTerminal(TokenType.SEMICOLON),
        };

        var node = new PtNonTerminal(NonTerminalType.PROCEDURE, children);

        return node;
    }

    private PtNode ParseFunction()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_FUNCTION),
            ConsumeToTerminal(TokenType.IDENTIFIER),
            ConsumeToTerminal(TokenType.OPEN_PARENS),
            ParseParameters(),
            ConsumeToTerminal(TokenType.CLOSE_PARENS),
            ConsumeToTerminal(TokenType.COLON),
            ParseType(),
            ConsumeToTerminal(TokenType.SEMICOLON),
            ParseBlock(),
            ConsumeToTerminal(TokenType.SEMICOLON),
        };

        var node = new PtNonTerminal(NonTerminalType.FUNCTION, children);

        return node;
    }

    private PtNode ParseVarDeclaration()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_VAR),
            ConsumeToTerminal(TokenType.IDENTIFIER),
        };

        while (NextToken.Type == TokenType.COMMA)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ConsumeToTerminal(TokenType.IDENTIFIER));
        }

        children.Add(ConsumeToTerminal(TokenType.COLON));
        children.Add(ParseType());

        var node = new PtNonTerminal(NonTerminalType.VAR_DECLARATION, children);

        return node;
    }

    private PtNode ParseParameters()
    {
        var children = new List<PtNode>();

        // Empty parameters
        if (NextToken.Type is not TokenType.KW_VAR and not TokenType.IDENTIFIER)
            return new PtNonTerminal(NonTerminalType.PARAMETERS, children);

        if (NextToken.Type is TokenType.KW_VAR)
            children.Add(ConsumeToTerminal());

        children.Add(ConsumeToTerminal(TokenType.IDENTIFIER));
        children.Add(ConsumeToTerminal(TokenType.COLON));
        children.Add(ParseType());

        while (NextToken.Type is TokenType.COMMA)
        {
            children.Add(ConsumeToTerminal());

            if (NextToken.Type is TokenType.KW_VAR)
                children.Add(ConsumeToTerminal());

            children.Add(ConsumeToTerminal(TokenType.IDENTIFIER));
            children.Add(ConsumeToTerminal(TokenType.COLON));
            children.Add(ParseType());
        }

        var node = new PtNonTerminal(NonTerminalType.PARAMETERS, children);

        return node;
    }

    private PtNode ParseType()
    {
        var children = new List<PtNode>
        {
            NextToken.Type == TokenType.KW_ARRAY ? ParseArrayType() : ParseSimpleType(),
        };

        return new PtNonTerminal(NonTerminalType.TYPE, children);
    }

    private PtNode ParseArrayType()
    {
        var children = new List<PtNode>()
        {
            ConsumeToTerminal(TokenType.KW_ARRAY),
            ConsumeToTerminal(TokenType.OPEN_SQUARE),
        };

        if (NextToken.Type != TokenType.CLOSE_SQUARE)
        {
            children.Add(ParseExpression());
        }

        children.Add(ConsumeToTerminal(TokenType.CLOSE_SQUARE));
        children.Add(ConsumeToTerminal(TokenType.KW_OF));
        children.Add(ParseSimpleType());

        var node = new PtNonTerminal(NonTerminalType.ARRAY_TYPE, children);

        return node;
    }

    private PtNode ParseSimpleType()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.IDENTIFIER),
        };

        var node = new PtNonTerminal(NonTerminalType.SIMPLE_TYPE, children);

        return node;
    }

    private PtNode ParseBlock()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_BEGIN),
            ParseStatement(),
        };

        while (NextToken.Type is not TokenType.KW_END)
        {
            children.Add(ConsumeToTerminal(TokenType.SEMICOLON));

            if (NextToken.Type is TokenType.KW_END)
                break;

            children.Add(ParseStatement());
        }

        children.Add(ConsumeToTerminal(TokenType.KW_END));

        var node = new PtNonTerminal(NonTerminalType.BLOCK, children);

        return node;
    }

    private PtNode ParseStatement()
    {
        var children = new List<PtNode>
        {
            NextToken switch
            {
                {Type: TokenType.IDENTIFIER, Lexeme: "writeln"} => ParseSimpleStatement(),
                {Type: TokenType.IDENTIFIER, Lexeme: "read"} => ParseSimpleStatement(),
                {Type: TokenType.KW_VAR} => ParseSimpleStatement(),
                {Type: TokenType.KW_ASSERT} => ParseSimpleStatement(),
                {Type: TokenType.IDENTIFIER} => ParseSimpleStatement(),
                {Type: TokenType.KW_RETURN} => ParseSimpleStatement(),
                {Type: TokenType.KW_IF} => ParseStructuredStatement(),
                {Type: TokenType.KW_BEGIN} => ParseStructuredStatement(),
                {Type: TokenType.KW_WHILE} => ParseStructuredStatement(),
                _ => throw new ParseException(GetExceptionMessage(NextToken,
                    TokenType.IDENTIFIER,
                    TokenType.KW_VAR,
                    TokenType.KW_ASSERT,
                    TokenType.KW_RETURN,
                    TokenType.KW_IF,
                    TokenType.KW_BEGIN,
                    TokenType.KW_WHILE
                )),
            },
        };

        var node = new PtNonTerminal(NonTerminalType.STATEMENT, children);

        return node;
    }

    private PtNode ParseSimpleStatement()
    {
        var children = new List<PtNode>
        {
            NextToken switch
            {
                {Type: TokenType.IDENTIFIER, Lexeme: "writeln"} => ParseWrite(),
                {Type: TokenType.IDENTIFIER, Lexeme: "read"} => ParseRead(),
                {Type: TokenType.KW_VAR} => ParseVarDeclaration(),
                {Type: TokenType.KW_ASSERT} => ParseAssert(),
                {Type: TokenType.IDENTIFIER} => ParseCallOrAssignment(),
                {Type: TokenType.KW_RETURN} => ParseReturn(),
                _ => throw new InvalidOperationException(""),
            },
        };

        var node = new PtNonTerminal(NonTerminalType.SIMPLE_STATEMENT, children);

        return node;
    }

    private PtNode ParseStructuredStatement()
    {
        var children = new List<PtNode>
        {
            NextToken switch
            {
                {Type: TokenType.KW_IF} => ParseIf(),
                {Type: TokenType.KW_BEGIN} => ParseBlock(),
                {Type: TokenType.KW_WHILE} => ParseWhile(),
                _ => throw new InvalidOperationException(),
            },
        };

        var node = new PtNonTerminal(NonTerminalType.STRUCTURED_STATEMENT, children);

        return node;
    }

    private PtNode ParseCallOrAssignment()
    {
        // Have to cheat a bit here...
        var token = Consume();

        var children = new List<PtNode>();

        switch (NextToken.Type)
        {
            case TokenType.OPEN_PARENS:
            {
                // Call
                children.Add(new PtTerminal(token));
                children.Add(ConsumeToTerminal());
                children.Add(ParseArguments());
                children.Add(ConsumeToTerminal(TokenType.CLOSE_PARENS));

                var node = new PtNonTerminal(NonTerminalType.CALL, children);

                return node;
            }
            case TokenType.KW_ASSIGN:
            {
                // Assignment
                children.Add(new PtNonTerminal(NonTerminalType.VARIABLE,
                    new List<PtNode> {new PtTerminal(token)}));
                children.Add(ConsumeToTerminal());
                children.Add(ParseExpression());

                var node = new PtNonTerminal(NonTerminalType.ASSIGNMENT, children);
                return node;
            }
            default:
                throw new ParseException(GetExceptionMessage(NextToken,
                    TokenType.OPEN_PARENS,
                    TokenType.KW_ASSIGN
                ));
        }
    }

    private PtNode ParseCallOrVariable()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.IDENTIFIER),
        };

        if (NextToken.Type == TokenType.OPEN_PARENS)
        {
            // Call
            children.Add(ConsumeToTerminal());
            children.Add(ParseArguments());
            children.Add(ConsumeToTerminal(TokenType.CLOSE_PARENS));

            var node = new PtNonTerminal(NonTerminalType.CALL, children);

            return node;
        }
        else
        {
            // Variable
            if (NextToken.Type == TokenType.OPEN_SQUARE)
            {
                children.Add(ConsumeToTerminal());
                children.Add(ParseExpression());
                children.Add(ConsumeToTerminal(TokenType.CLOSE_SQUARE));
            }

            var node = new PtNonTerminal(NonTerminalType.VARIABLE, children);
            return node;
        }
    }

    private PtNode ParseArguments()
    {
        var children = new List<PtNode>
        {
            ParseExpression(),
        };

        while (NextToken.Type is TokenType.COMMA)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseExpression());
        }

        var node = new PtNonTerminal(NonTerminalType.ARGUMENTS, children);

        return node;
    }

    private PtNode ParseReturn()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_RETURN),
        };

        if (IsExprStartToken(NextToken))
            children.Add(ParseExpression());

        return new PtNonTerminal(NonTerminalType.RETURN, children);
    }

    private PtNode ParseRead()
    {
        var children = new List<PtNode>()
        {
            ConsumeToTerminal(TokenType.IDENTIFIER, "read"),
            ConsumeToTerminal(TokenType.OPEN_PARENS),
            ParseVariable(),
        };

        while (NextToken.Type is TokenType.COMMA)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseVariable());
        }

        children.Add(ConsumeToTerminal(TokenType.CLOSE_PARENS));

        var node = new PtNonTerminal(NonTerminalType.READ, children);

        return node;
    }

    private PtNode ParseWrite()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.IDENTIFIER, "writeln"),
            ConsumeToTerminal(TokenType.OPEN_PARENS),
            ParseArguments(),
            ConsumeToTerminal(TokenType.CLOSE_PARENS),
        };

        var node = new PtNonTerminal(NonTerminalType.WRITE, children);

        return node;
    }

    private PtNode ParseAssert()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_ASSERT),
            ConsumeToTerminal(TokenType.OPEN_PARENS),
            ParseExpression(),
            ConsumeToTerminal(TokenType.CLOSE_PARENS),
        };

        var node = new PtNonTerminal(NonTerminalType.ASSERT, children);

        return node;
    }

    private PtNode ParseIf()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_IF),
            ParseExpression(),
            ConsumeToTerminal(TokenType.KW_THEN),
            ParseStatement(),
        };

        if (NextToken.Type is TokenType.KW_ELSE)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseStatement());
        }

        var node = new PtNonTerminal(NonTerminalType.IF, children);

        return node;
    }

    private PtNode ParseWhile()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.KW_WHILE),
            ParseExpression(),
            ConsumeToTerminal(TokenType.KW_DO),
            ParseStatement(),
        };

        var node = new PtNonTerminal(NonTerminalType.WHILE, children);

        return node;
    }

    private PtNode ParseExpression()
    {
        var children = new List<PtNode>
        {
            ParseSimpleExpression(),
        };

        if (NextToken.Type
            is TokenType.EQUALS
            or TokenType.DIAMOND
            or TokenType.LESS_THAN
            or TokenType.LESS_THAN_OR_EQ
            or TokenType.GREATER_THAN
            or TokenType.GREATER_THAN_OR_EQ)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseSimpleExpression());
        }

        var node = new PtNonTerminal(NonTerminalType.EXPRESSION, children);

        return node;
    }

    private PtNode ParseSimpleExpression()
    {
        var children = new List<PtNode>();

        if (NextToken.Type is TokenType.OP_ADD or TokenType.OP_SUB)
            children.Add(ConsumeToTerminal());

        children.Add(ParseTerm());

        while (NextToken.Type is TokenType.OP_ADD or TokenType.OP_SUB or TokenType.KW_OR)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseTerm());
        }

        var node = new PtNonTerminal(NonTerminalType.SIMPLE_EXPRESSION, children);

        return node;
    }

    private PtNode ParseTerm()
    {
        var children = new List<PtNode>()
        {
            ParseFactor(),
        };

        while (NextToken.Type
               is TokenType.OP_MUL
               or TokenType.OP_DIV
               or TokenType.OP_MOD
               or TokenType.KW_AND)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseFactor());
        }

        var node = new PtNonTerminal(NonTerminalType.TERM, children);

        return node;
    }

    private PtNode ParseFactor()
    {
        var children = new List<PtNode>();

        if (NextToken.Type is TokenType.IDENTIFIER)
        {
            children.Add(ParseCallOrVariable());
        }
        else if (IsLiteralStartToken(NextToken))
        {
            children.Add(ParseLiteral());
        }
        else if (NextToken.Type is TokenType.OPEN_PARENS)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseExpression());
            children.Add(ConsumeToTerminal(TokenType.CLOSE_PARENS));
        }
        else if (NextToken.Type is TokenType.KW_NOT)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseFactor());
        }

        if (NextToken.Type is TokenType.PERIOD)
        {
            return new PtNonTerminal(NonTerminalType.FACTOR, new List<PtNode>()
            {
                new PtNonTerminal(NonTerminalType.FACTOR, children),
                ConsumeToTerminal(),
                ConsumeToTerminal(TokenType.IDENTIFIER, "size"),
            });
        }

        var node = new PtNonTerminal(NonTerminalType.FACTOR, children);

        return node;
    }

    private PtNode ParseVariable()
    {
        var children = new List<PtNode>
        {
            ConsumeToTerminal(TokenType.IDENTIFIER),
        };

        if (NextToken.Type is TokenType.OPEN_SQUARE)
        {
            children.Add(ConsumeToTerminal());
            children.Add(ParseExpression());
            children.Add(ConsumeToTerminal(TokenType.CLOSE_SQUARE));
        }

        var node = new PtNonTerminal(NonTerminalType.VARIABLE, children);

        return node;
    }

    private PtNode ParseLiteral()
    {
        if (NextToken.Type is TokenType.INTEGER or TokenType.STRING or TokenType.REAL
            or TokenType.KW_TRUE or TokenType.KW_FALSE)
        {
            return new PtNonTerminal(NonTerminalType.LITERAL, new List<PtNode>
            {
                ConsumeToTerminal(),
            });
        }

        throw new InvalidOperationException();
    }

    private Token Consume()
    {
        return _tokens[_nextTokenIndex++];
    }

    private Token Consume(TokenType type) => NextToken.Type == type
        ? Consume()
        : throw new ParseException(GetExceptionMessage(NextToken, type));

    private Token Consume(TokenType type, string lexeme) =>
        NextToken.Lexeme == lexeme
            ? Consume(type)
            : throw new ParseException(GetExceptionMessage(NextToken, type));

    private PtTerminal ConsumeToTerminal() => new(Consume());

    private PtTerminal ConsumeToTerminal(TokenType type) => new(Consume(type));

    private PtTerminal ConsumeToTerminal(TokenType type, string lexeme) =>
        new(Consume(type, lexeme));

    private static string GetExceptionMessage(Token token, params TokenType[] expectedTypes)
    {
        var message =
            $"At line {token.Line}, col {token.Col}: expected one of: \"{string.Join(", ", expectedTypes)}\", found {token.Type}.";
        return message;
    }

    private static bool IsExprStartToken(Token token) => IsSimpleExprStartToken(token);

    private static bool IsSimpleExprStartToken(Token token)
    {
        if (token.Type is TokenType.OP_ADD or TokenType.OP_SUB)
            return true;

        return IsTermStartToken(token);
    }

    private static bool IsTermStartToken(Token token) => IsFactorStartToken(token);

    private static bool IsFactorStartToken(Token token)
    {
        return IsCallStartToken(token)
               || IsVariableStartToken(token)
               || IsLiteralStartToken(token)
               || token.Type is TokenType.OPEN_PARENS
               || token.Type is TokenType.KW_NOT;
    }

    private static bool IsLiteralStartToken(Token token)
    {
        return token.Type is TokenType.INTEGER or TokenType.STRING or TokenType.REAL
            or TokenType.KW_FALSE or TokenType.KW_TRUE;
    }

    private static bool IsVariableStartToken(Token token)
    {
        return token.Type is TokenType.IDENTIFIER;
    }

    private static bool IsCallStartToken(Token token)
    {
        return token.Type is TokenType.IDENTIFIER;
    }
}

public class ParseException : Exception
{
    public ParseException(string message) : base(message)
    {
    }
}