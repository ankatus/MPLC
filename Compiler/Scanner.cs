namespace Compiler;

public enum TokenType
{
    UNKNOWN,
    OPEN_PARENS,
    CLOSE_PARENS,
    OPEN_SQUARE,
    CLOSE_SQUARE,
    LESS_THAN,
    LESS_THAN_OR_EQ,
    GREATER_THAN,
    GREATER_THAN_OR_EQ,
    EQUALS,
    DIAMOND,
    PERIOD,
    COMMA,
    COLON,
    SEMICOLON,
    IDENTIFIER,
    INTEGER,
    REAL,
    STRING,

    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,

    KW_PROGRAM,
    KW_PROCEDURE,
    KW_FUNCTION,
    KW_VAR,
    KW_ARRAY,
    KW_BEGIN,
    KW_END,
    KW_RETURN,
    KW_ASSERT,
    KW_IF,
    KW_THEN,
    KW_ELSE,
    KW_WHILE,
    KW_DO,
    KW_OF,
    KW_OR,
    KW_AND,
    KW_NOT,
    KW_ASSIGN,
    KW_TRUE,
    KW_FALSE,

    EOF,
}

public record Token(TokenType Type, string Lexeme, int Line, int Col)
{
    public static Token Eof => new(TokenType.EOF, "", 0, 0);
}

public class Scanner
{
    private readonly Dictionary<string, TokenType> _keywords = new()
    {
        {":=", TokenType.KW_ASSIGN},
        {"or", TokenType.KW_OR},
        {"and", TokenType.KW_AND},
        {"not", TokenType.KW_NOT},
        {"if", TokenType.KW_IF},
        {"then", TokenType.KW_THEN},
        {"else", TokenType.KW_ELSE},
        {"of", TokenType.KW_OF},
        {"while", TokenType.KW_WHILE},
        {"do", TokenType.KW_DO},
        {"begin", TokenType.KW_BEGIN},
        {"end", TokenType.KW_END},
        {"var", TokenType.KW_VAR},
        {"array", TokenType.KW_ARRAY},
        {"procedure", TokenType.KW_PROCEDURE},
        {"function", TokenType.KW_FUNCTION},
        {"program", TokenType.KW_PROGRAM},
        {"assert", TokenType.KW_ASSERT},
        {"return", TokenType.KW_RETURN},
        {"true", TokenType.KW_TRUE},
        {"false", TokenType.KW_FALSE},
    };

    private readonly ScannerInput _input;
    private readonly List<Token> _tokens = new();

    public Scanner(ScannerInput input)
    {
        _input = input;
    }

    public List<Token> Scan()
    {
        while (!_input.EndReached)
        {
            if (char.IsWhiteSpace(_input.NextChar))
            {
                _input.Advance();
                _input.Commit();
                continue;
            }

            _input.Reset();

            if (TryBuildIdentifier())
            {
                var lexeme = _input.CurrentString;

                // Could be a keyword, check for that
                AddToken(
                    _keywords.ContainsKey(lexeme) ? _keywords[lexeme] : TokenType.IDENTIFIER,
                    lexeme
                );

                _input.Commit();
                continue;
            }

            _input.Reset();

            if (TryBuildRealWithExponent())
            {
                AddToken(TokenType.REAL);
                _input.Commit();
                continue;
            }
            
            _input.Reset();
            
            if (TryBuildReal())
            {
                AddToken(TokenType.REAL);
                _input.Commit();
                continue;
            }
            
            _input.Reset();
            
            if (TryBuildInteger())
            {
                AddToken(TokenType.INTEGER);
                _input.Commit();
                continue;
            }

            _input.Reset();

            if (TryBuildString())
            {
                var lexeme = _input.CurrentString.Replace("\"", "");
                AddToken(TokenType.STRING, lexeme);
                _input.Commit();
                continue;
            }

            _input.Reset();

            if (TryBuildComment())
            {
                // Comments are disregarded
                _input.Commit();
                continue;
            }

            _input.Reset();

            // Match trivial tokens
            switch (_input.NextChar)
            {
                case '+':
                    _input.Advance();
                    AddToken(TokenType.OP_ADD);
                    _input.Commit();
                    break;
                case '-':
                    _input.Advance();
                    AddToken(TokenType.OP_SUB);
                    _input.Commit();
                    break;
                case '*':
                    _input.Advance();
                    AddToken(TokenType.OP_MUL);
                    _input.Commit();
                    break;
                case '/':
                    _input.Advance();
                    AddToken(TokenType.OP_DIV);
                    _input.Commit();
                    break;
                case '%':
                    _input.Advance();
                    AddToken(TokenType.OP_MOD);
                    _input.Commit();
                    break;
                case '=':
                    _input.Advance();
                    AddToken(TokenType.EQUALS);
                    _input.Commit();
                    break;
                case '<':
                    _input.Advance();
                    switch (_input.NextChar, _input.EndReached)
                    {
                        case ('>', false):
                            _input.Advance();
                            AddToken(TokenType.DIAMOND);
                            break;
                        case ('=', false):
                            _input.Advance();
                            AddToken(TokenType.LESS_THAN_OR_EQ);
                            break;
                        default:
                            AddToken(TokenType.LESS_THAN);
                            break;
                    }
                    _input.Commit();
                    break;
                case '>':
                    _input.Advance();
                    if (!_input.EndReached && _input.NextChar == '=')
                    {
                        _input.Advance();
                        AddToken(TokenType.GREATER_THAN_OR_EQ);
                    }
                    else
                    {
                        AddToken(TokenType.GREATER_THAN);
                    }

                    _input.Commit();
                    break;
                case '(':
                    _input.Advance();
                    AddToken(TokenType.OPEN_PARENS);
                    _input.Commit();
                    break;
                case ')':
                    _input.Advance();
                    AddToken(TokenType.CLOSE_PARENS);
                    _input.Commit();
                    break;
                case '[':
                    _input.Advance();
                    AddToken(TokenType.OPEN_SQUARE);
                    _input.Commit();
                    break;
                case ']':
                    _input.Advance();
                    AddToken(TokenType.CLOSE_SQUARE);
                    _input.Commit();
                    break;
                case ':':
                    _input.Advance();
                    if (!_input.EndReached && _input.NextChar == '=')
                    {
                        _input.Advance();
                        AddToken(TokenType.KW_ASSIGN);
                    }
                    else
                    {
                        AddToken(TokenType.COLON);
                    }
                    _input.Commit();
                    break;
                case '.':
                    _input.Advance();
                    AddToken(TokenType.PERIOD);
                    _input.Commit();
                    break;
                case ',':
                    _input.Advance();
                    AddToken(TokenType.COMMA);
                    _input.Commit();
                    break;
                case ';':
                    _input.Advance();
                    AddToken(TokenType.SEMICOLON);
                    _input.Commit();
                    break;
                default:
                    _input.Advance();
                    AddToken(TokenType.UNKNOWN);
                    _input.Commit();
                    break;
            }
        }

        _tokens.Add(Token.Eof);

        var unknowns = _tokens.Where(token => token.Type == TokenType.UNKNOWN).ToList();
        
        if (unknowns.Count > 0)
        {
            var unknownsStr = "";
            foreach (var token in unknowns)
            {
                unknownsStr += $"Unknown token \"{token.Lexeme}\" at {token.Line}, {token.Col}.\n";
            }
        
            throw new ScanningException("Unknown tokens found:\n" + unknownsStr);
        }

        return _tokens;
    }

    private void AddToken(TokenType tokenType) => AddToken(tokenType, _input.CurrentString);

    private void AddToken(TokenType tokenType, string lexeme)
    {
        _tokens.Add(new Token(tokenType, lexeme, _input.BaseLine, _input.BaseCol));
    }

    private bool TryBuildIdentifier()
    {
        if (_input.EndReached)
            throw new InvalidOperationException();

        if (!char.IsLetter(_input.NextChar))
            return false;

        _input.Advance();

        while (!_input.EndReached)
        {
            if (!char.IsLetterOrDigit(_input.NextChar) && _input.NextChar != '_')
                break;

            _input.Advance();
        }

        return true;
    }

    private bool TryBuildRealWithExponent()
    {
        if (!TryBuildReal())
            return false;
        
        if (_input.EndReached || _input.NextChar != 'e')
            return false;

        _input.Advance();
        
        if (_input.EndReached)
            return false;

        if (_input.NextChar is '+' or '-')
            _input.Advance();

        if (!TryBuildDigits())
            return false;

        return true;
    }

    private bool TryBuildReal()
    {
        if (!TryBuildDigits())
            return false;

        if (_input.EndReached || _input.NextChar != '.')
            return false;
        
        _input.Advance();

        if (!TryBuildDigits())
            return false;

        return true;
    }
    
    private bool TryBuildInteger()
    {
        return TryBuildDigits();
    }

    private bool TryBuildDigits()
    {
        if (_input.EndReached)
            throw new InvalidOperationException();

        if (!char.IsDigit(_input.NextChar))
            return false;

        while (true)
        {
            if (_input.EndReached)
                break;

            if (!char.IsDigit(_input.NextChar))
                break;

            _input.Advance();
        }

        return true;
    }

    private bool TryBuildString()
    {
        if (_input.EndReached)
            throw new InvalidOperationException();

        if (_input.NextChar != '"')
            return false;

        do
        {
            _input.Advance();
            if (_input.EndReached)
                return false;
        } while (_input.NextChar != '"');

        // Eat the final double quote
        _input.Advance();

        return true;
    }

    private bool TryBuildComment()
    {
        if (_input.EndReached)
            throw new InvalidOperationException();

        if (_input.NextChar != '{')
            return false;

        _input.Advance();

        if (_input.EndReached)
            return false;

        if (_input.NextChar != '*')
            return false;

        while (true)
        {
            _input.Advance();

            if (_input.EndReached)
                return false;

            if (_input.NextChar != '*') continue;

            _input.Advance();

            if (_input.EndReached)
                return false;

            if (_input.NextChar != '}') continue;

            _input.Advance();

            break;
        }

        return true;
    }
}

public class ScanningException : Exception
{
    public ScanningException(string message) : base(message)
    {
    }
}