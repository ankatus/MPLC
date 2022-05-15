using System.Globalization;
using System.Security;

namespace Compiler;

public class Analyzer
{
    private readonly PtNode _parseTree;
    private Dictionary<string, AstFunction> _functions = new();
    private Dictionary<string, AstProcedure> _procedures = new();
    private readonly List<Dictionary<string, (AstType type, bool isRef)>> _contextStack = new();

    public Analyzer(PtNode parseTree)
    {
        _parseTree = parseTree;
    }

    public AstNode Analyze()
    {
        return AnalyzeProgram(_parseTree);
    }

    private AstProgram AnalyzeProgram(PtNode node)
    {
        if (node is not PtNonTerminal programNode)
            throw new ArgumentException("", nameof(node));

        var nonTerminals =
            programNode.Children.OfType<PtNonTerminal>()
                .ToList();

        // Get function/procedure signatures first
        _procedures = nonTerminals
            .Where(c => c.Type is NonTerminalType.PROCEDURE)
            .Select(ProcedureSignature)
            .ToDictionary(x => x.Identifier);

        _functions = nonTerminals
            .Where(c => c.Type is NonTerminalType.FUNCTION)
            .Select(FunctionSignature)
            .ToDictionary(x => x.Identifier);

        // Actual implementations
        _procedures = nonTerminals
            .Where(c => c.Type is NonTerminalType.PROCEDURE)
            .Select(Procedure)
            .ToDictionary(x => x.Identifier);

        _functions = nonTerminals
            .Where(c => c.Type is NonTerminalType.FUNCTION)
            .Select(Function)
            .ToDictionary(x => x.Identifier);

        var mainBlock = nonTerminals.Single(child =>
            child.Type is NonTerminalType.BLOCK);

        var astProgram = new AstProgram(AnalyzeBlock(mainBlock), _functions.Values.ToList(),
            _procedures.Values.ToList());

        return astProgram;
    }

    private AstFunction FunctionSignature(PtNode node)
    {
        if (node is not PtNonTerminal functionNode)
            throw new InvalidOperationException();

        var id = functionNode
            .Children
            .OfType<PtTerminal>()
            .Single(x => x.Token.Type is TokenType.IDENTIFIER)
            .Token.Lexeme;

        var nonTerminals =
            functionNode.Children.OfType<PtNonTerminal>()
                .ToList();

        var type = AnalyzeType(
            nonTerminals
                .Single(x => x.Type is NonTerminalType.TYPE)
        );

        var parameters = Parameters(
            nonTerminals.Single(x => x.Type is NonTerminalType.PARAMETERS)
        );

        var block = new AstBlock(new(), new());

        return new AstFunction(type, id, parameters, block);
    }

    private AstProcedure ProcedureSignature(PtNode node)
    {
        if (node is not PtNonTerminal procedureNode)
            throw new InvalidOperationException();

        var id = procedureNode
            .Children
            .OfType<PtTerminal>()
            .Single(x => x.Token.Type is TokenType.IDENTIFIER)
            .Token.Lexeme;

        var nonTerminals =
            procedureNode.Children.OfType<PtNonTerminal>()
                .ToList();

        var parameters = Parameters(
            nonTerminals.Single(x => x.Type is NonTerminalType.PARAMETERS)
        );

        var block = new AstBlock(new(), new());

        return new AstProcedure(id, parameters, block);
    }

    private AstFunction Function(PtNode node)
    {
        if (node is not PtNonTerminal functionNode)
            throw new InvalidOperationException();

        var id = functionNode
            .Children
            .OfType<PtTerminal>()
            .Single(x => x.Token.Type is TokenType.IDENTIFIER)
            .Token.Lexeme;

        var nonTerminals =
            functionNode.Children.OfType<PtNonTerminal>()
                .ToList();

        var type = AnalyzeType(
            nonTerminals
                .Single(x => x.Type is NonTerminalType.TYPE)
        );

        var parameters = Parameters(
            nonTerminals.Single(x => x.Type is NonTerminalType.PARAMETERS)
        );

        var context = new Dictionary<string, (AstType type, bool isRef)>();

        if (parameters.Any(parameter =>
                !context.TryAdd(parameter.Name, (parameter.Type, parameter.IsRef))))
        {
            throw new SemanticException("");
        }

        _contextStack.Add(context);

        var block = AnalyzeBlock(
            nonTerminals
                .Single(x => x.Type is NonTerminalType.BLOCK)
        );

        _contextStack.RemoveAt(_contextStack.Count - 1);

        return new AstFunction(type, id, parameters, block);
    }

    private List<Parameter> Parameters(PtNode node)
    {
        if (node is not PtNonTerminal parametersNode)
            throw new InvalidOperationException();

        var parameters = new List<Parameter>();
        
        for (var i = 0; i < parametersNode.Children.Count;)
        {
            var current = parametersNode.Children[i];
            if (current is not PtTerminal terminal)
                throw new InvalidOperationException();

            if (terminal.Token.Type == TokenType.KW_VAR)
            {
                var name = (parametersNode.Children[i + 1] as PtTerminal)!.Token.Lexeme;
                var type = AnalyzeType(parametersNode.Children[i + 3]);
                parameters.Add(new Parameter(name, type, true));
                i += 4;
            }
            else if (terminal.Token.Type == TokenType.IDENTIFIER)
            {
                var name = terminal.Token.Lexeme;
                var type = AnalyzeType(parametersNode.Children[i + 2]);
                parameters.Add(new Parameter(name, type, false));
                i += 3;
            }
            else
            {
                i++;
            }
        }
        
        return parameters;
    }

    private AstProcedure Procedure(PtNode node)
    {
        if (node is not PtNonTerminal procedureNode)
            throw new InvalidOperationException();

        var id = procedureNode
            .Children
            .OfType<PtTerminal>()
            .Single(x => x.Token.Type is TokenType.IDENTIFIER)
            .Token.Lexeme;

        var nonTerminals =
            procedureNode.Children.OfType<PtNonTerminal>()
                .ToList();

        var parameters = Parameters(
            nonTerminals.Single(x => x.Type is NonTerminalType.PARAMETERS)
        );

        var context = new Dictionary<string, (AstType type, bool isRef)>();

        if (parameters.Any(parameter =>
                !context.TryAdd(parameter.Name, (parameter.Type, parameter.IsRef))))
        {
            throw new SemanticException("");
        }

        _contextStack.Add(context);

        var block = AnalyzeBlock(
            nonTerminals
                .Single(x => x.Type is NonTerminalType.BLOCK)
        );

        _contextStack.RemoveAt(_contextStack.Count - 1);

        return new AstProcedure(id, parameters, block);
    }

    private AstBlock AnalyzeBlock(PtNode node)
    {
        if (node is not PtNonTerminal blockNode)
            throw new ArgumentException("", nameof(node));

        var context = new Dictionary<string, (AstType type, bool isRef)>();
        _contextStack.Add(context);

        var statementNodes = blockNode.Children.OfType<PtNonTerminal>()
            .Where(child => child.Type is NonTerminalType.STATEMENT);

        var statements = statementNodes.Select(AnalyzeStatement)
            .ToList();

        _contextStack.RemoveAt(_contextStack.Count - 1);

        return new AstBlock(statements, context);
    }

    private AstStatement AnalyzeStatement(PtNode node)
    {
        if (node is not PtNonTerminal statementNode)
            throw new ArgumentException("", nameof(node));

        var child = statementNode
            .Children
            .OfType<PtNonTerminal>()
            .Single();

        return child.Type switch
        {
            NonTerminalType.SIMPLE_STATEMENT => AnalyzeSimpleStatement(child),
            NonTerminalType.STRUCTURED_STATEMENT => AnalyzeStructuredStatement(child),
            _ => throw new SemanticException(""),
        };
    }

    private AstStatement AnalyzeSimpleStatement(PtNode node)
    {
        if (node is not PtNonTerminal statementNode)
            throw new InvalidOperationException();

        var child = statementNode
            .Children
            .OfType<PtNonTerminal>()
            .Single();

        return child.Type switch
        {
            NonTerminalType.VAR_DECLARATION => AnalyzeVarDeclaration(child),
            NonTerminalType.ASSIGNMENT => AnalyzeAssignment(child),
            NonTerminalType.CALL => AnalyzeCallStatement(child),
            NonTerminalType.RETURN => AnalyzeReturn(child),
            NonTerminalType.READ => AnalyzeRead(child),
            NonTerminalType.WRITE => AnalyzeWrite(child),
            NonTerminalType.ASSERT => AnalyzeAssert(child),
            _ => throw new SemanticException(""),
        };
    }

    private AstVarDeclaration AnalyzeVarDeclaration(PtNode node)
    {
        if (node is not PtNonTerminal varDeclarationNode)
            throw new InvalidOperationException();

        var ids = varDeclarationNode
            .Children
            .OfType<PtTerminal>()
            .Where(x => x.Token.Type is TokenType.IDENTIFIER)
            .Select(x => x.Token.Lexeme)
            .ToList();

        var type = AnalyzeType(
            varDeclarationNode
                .Children
                .OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.TYPE)
        );

        var context = _contextStack[^1];
        if (ids.Any(id => !context.TryAdd(id, (type, false))))
            throw new SecurityException("");

        return new AstVarDeclaration(
            ids
                .Select(x => new AstVariable(type, x, false))
                .ToList()
        );
    }

    private AstAssignment AnalyzeAssignment(PtNode node)
    {
        if (node is not PtNonTerminal assignmentNode)
            throw new InvalidOperationException();

        var variable = Variable(
            assignmentNode
                .Children
                .First()
        );

        var expression = AnalyzeExpression(
            assignmentNode
                .Children
                .OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.EXPRESSION)
        );

        if (variable.Type != expression.Type)
            throw new SemanticException("");

        return new AstAssignment(variable, expression);
    }

    private AstCallStatement AnalyzeCallStatement(PtNode node)
    {
        if (node is not PtNonTerminal callStatementNode)
            throw new InvalidOperationException();

        var id = callStatementNode.Children
            .OfType<PtTerminal>()
            .Single(c => c.Token.Type is TokenType.IDENTIFIER)
            .Token
            .Lexeme;

        var arguments = callStatementNode.Children
            .OfType<PtNonTerminal>()
            .Single(c => c.Type is NonTerminalType.ARGUMENTS)
            .Children
            .OfType<PtNonTerminal>()
            .Where(c => c.Type is NonTerminalType.EXPRESSION)
            .Select(AnalyzeExpression)
            .ToList();

        IAstCallable callee =
            _functions.ContainsKey(id)
                ? _functions[id]
                : _procedures.ContainsKey(id)
                    ? _procedures[id]
                    : throw new SemanticException("");

        if (arguments.Count != callee.Parameters.Count)
            throw new SemanticException("");

        if (arguments.Where((t, i) => t.Type != callee.Parameters[i].Type).Any())
            throw new SemanticException("");

        return new AstCallStatement(callee, arguments);
    }

    private AstReturn AnalyzeReturn(PtNode node)
    {
        if (node is not PtNonTerminal returnNode)
            throw new InvalidOperationException();

        return new AstReturn(
            AnalyzeExpression(
                returnNode
                    .Children
                    .OfType<PtNonTerminal>()
                    .Single(x => x.Type is NonTerminalType.EXPRESSION)
            )
        );
    }

    private AstRead AnalyzeRead(PtNode node)
    {
        if (node is not PtNonTerminal readNode)
            throw new InvalidOperationException();

        return new AstRead(
            readNode
                .Children
                .OfType<PtNonTerminal>()
                .Where(x => x.Type is NonTerminalType.VARIABLE)
                .Select(Variable)
                .ToList()
        );
    }

    private AstWrite AnalyzeWrite(PtNode node)
    {
        if (node is not PtNonTerminal writeNode)
            throw new InvalidOperationException();

        return new AstWrite(
            writeNode.Children.OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.ARGUMENTS)
                .Children
                .OfType<PtNonTerminal>()
                .Where(x => x.Type is NonTerminalType.EXPRESSION)
                .Select(AnalyzeExpression)
                .ToList()
        );
    }

    private AstAssert AnalyzeAssert(PtNode node)
    {
        if (node is not PtNonTerminal assertNode)
            throw new InvalidOperationException();

        var expressionNode = assertNode.Children.OfType<PtNonTerminal>()
            .Single(child => child.Type is NonTerminalType.EXPRESSION);

        var expression = AnalyzeExpression(expressionNode);

        if (expression.Type.Name is not AstTypeName.BOOL ||
            expression.Type is AstArrayType)
            throw new SemanticException("");

        return new AstAssert(expression);
    }

    private AstStatement AnalyzeStructuredStatement(PtNode node)
    {
        if (node is not PtNonTerminal statementNode)
            throw new InvalidOperationException();

        var child = statementNode.Children.OfType<PtNonTerminal>()
            .Single();

        return child.Type switch
        {
            NonTerminalType.BLOCK => AnalyzeBlock(child),
            NonTerminalType.IF => AnalyzeIf(child),
            NonTerminalType.WHILE => AnalyzeWhile(child),
            _ => throw new InvalidOperationException(),
        };
    }

    private AstIf AnalyzeIf(PtNode node)
    {
        if (node is not PtNonTerminal ifNode)
            throw new InvalidOperationException();

        var condition = AnalyzeExpression(
            ifNode
                .Children
                .OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.EXPRESSION)
        );

        var then = AnalyzeStatement(
            ifNode
                .Children
                .OfType<PtNonTerminal>()
                .First(x => x.Type is NonTerminalType.STATEMENT)
        );

        if (ifNode
            .Children
            .OfType<PtTerminal>()
            .Any(x => x.Token.Type is TokenType.KW_ELSE))
        {
            var @else = AnalyzeStatement(
                ifNode
                    .Children
                    .OfType<PtNonTerminal>()
                    .Last(x => x.Type is NonTerminalType.STATEMENT)
            );

            return new AstIfElse(condition, then, @else);
        }

        return new AstIf(condition, then);
    }

    private AstWhile AnalyzeWhile(PtNode node)
    {
        if (node is not PtNonTerminal whileNode)
            throw new InvalidOperationException();

        var condition = AnalyzeExpression(
            whileNode.Children.OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.EXPRESSION)
        );

        if (condition.Type is AstArrayType ||
            condition.Type.Name is not AstTypeName.BOOL)
            throw new SemanticException("");

        var statement = AnalyzeStatement(
            whileNode.Children.OfType<PtNonTerminal>()
                .Single(x => x.Type is NonTerminalType.STATEMENT)
        );

        return new AstWhile(condition, statement);
    }

    private AstType AnalyzeType(PtNode node)
    {
        if (node is not PtNonTerminal typeNode)
            throw new InvalidOperationException();

        var child = typeNode
            .Children
            .OfType<PtNonTerminal>()
            .Single();

        switch (child.Type)
        {
            case NonTerminalType.SIMPLE_TYPE:
                return new AstType
                (
                    GetAstTypeName
                    (
                        child
                            .Children
                            .OfType<PtTerminal>()
                            .Single()
                            .Token
                    )
                );
            case NonTerminalType.ARRAY_TYPE:
                var type = GetAstTypeName(
                    child
                        .Children
                        .OfType<PtNonTerminal>()
                        .Single(x => x.Type is NonTerminalType.SIMPLE_TYPE)
                        .Children.OfType<PtTerminal>()
                        .Single()
                        .Token);

                var sizeExpressionNode = child
                    .Children
                    .OfType<PtNonTerminal>()
                    .SingleOrDefault(x => x.Type is NonTerminalType.EXPRESSION);

                if (sizeExpressionNode is null)
                    return new AstArrayType(type, null);

                var sizeExpression = AnalyzeExpression(sizeExpressionNode);

                if (sizeExpression.Type != AstType.Int)
                    throw new SemanticException("");

                return new AstArrayType(type, sizeExpression);
            default:
                throw new InvalidOperationException();
        }
    }

    private AstExpression AnalyzeExpression(PtNode node)
    {
        if (node is not PtNonTerminal expressionNode)
            throw new InvalidOperationException();

        switch (expressionNode.Type)
        {
            case NonTerminalType.EXPRESSION:
                return expressionNode.Children.Count switch
                {
                    1 => AnalyzeSimpleExpression(expressionNode.Children.First()),
                    3 => AnalyzeRelationExpression(expressionNode),
                    _ => throw new InvalidOperationException(),
                };
            case NonTerminalType.SIMPLE_EXPRESSION:
            case NonTerminalType.TERM:
                return AnalyzeSimpleExpression(expressionNode);
            case NonTerminalType.FACTOR:
                return AnalyzeFactor(expressionNode);
            default:
                throw new InvalidOperationException();
        }
    }

    private AstRelationExpression AnalyzeRelationExpression(
        PtNonTerminal relationExprNode)
    {
        if (relationExprNode.Children.Count != 3)
            throw new ArgumentException("", nameof(relationExprNode));

        var leftNode = relationExprNode.Children[0] as PtNonTerminal
                       ?? throw new InvalidOperationException();
        var opNode = relationExprNode.Children[1] as PtTerminal
                     ?? throw new InvalidOperationException();
        var rightNode = relationExprNode.Children[2] as PtNonTerminal
                        ?? throw new InvalidOperationException();

        var left = AnalyzeSimpleExpression(leftNode);
        var op = AnalyzeRelationalOp(opNode);
        var right = AnalyzeSimpleExpression(rightNode);

        if (left.Type != right.Type)
            throw new SemanticException("");

        return new AstRelationExpression(left, right, op);
    }

    private AstExpression AnalyzeSimpleExpression(PtNode node)
    {
        if (node is not PtNonTerminal simpleExprNode)
            throw new InvalidOperationException();

        if (simpleExprNode.Children.Count is 1)
            return AnalyzeExpression(simpleExprNode.Children.First());

        var sign = AstSign.POSITIVE;
        var termsStartIndex = 1;

        if (simpleExprNode.Children[0] is PtTerminal signNode)
        {
            sign = signNode.Token.Type switch
            {
                TokenType.OP_ADD => AstSign.POSITIVE,
                TokenType.OP_SUB => AstSign.NEGATIVE,
                _ => throw new InvalidOperationException(),
            };

            termsStartIndex = 2;
        }

        var termNode = simpleExprNode.Children
            .OfType<PtNonTerminal>()
            .First(child => child.Type
                is NonTerminalType.TERM
                or NonTerminalType.FACTOR);

        var term = AnalyzeExpression(termNode);

        var terms = new List<(AstOperator op, AstExpression term)>();

        var lastTerm = term;

        for (var i = termsStartIndex;
             i < simpleExprNode.Children.Count - 1;
             i += 2)
        {
            var newOp = AnalyzeOperator(simpleExprNode.Children[i]);
            var newExpr = AnalyzeExpression(simpleExprNode.Children[i + 1]);

            if (lastTerm.Type != newExpr.Type)
                throw new SemanticException("");

            CheckOperatorType(newOp, newExpr.Type);

            terms.Add((newOp, newExpr));

            lastTerm = newExpr;
        }

        return new AstSimpleExpression(term.Type, sign, term, terms);
    }

    private AstExpression AnalyzeFactor(PtNode node)
    {
        if (node is not PtNonTerminal factorNode)
            throw new ArgumentException("", nameof(node));

        var child = factorNode
            .Children
            .OfType<PtNonTerminal>()
            .Single();

        switch (child.Type)
        {
            case NonTerminalType.CALL:
                return Call(child);
            case NonTerminalType.VARIABLE:
                return Variable(child);
            case NonTerminalType.LITERAL:
                return Literal(child);
            case NonTerminalType.EXPRESSION:
                return AnalyzeExpression(child);
            case NonTerminalType.FACTOR:
                if (factorNode.Children.First() is PtTerminal terminal &&
                    terminal.Token.Type is TokenType.KW_NOT)
                    return new AstNegation(AnalyzeFactor(child));
                return new AstSizeExpr(AnalyzeFactor(child));
            default:
                throw new InvalidOperationException();
        }
    }

    private AstCall Call(PtNode node)
    {
        if (node is not PtNonTerminal callNode)
            throw new InvalidOperationException();

        var id = callNode.Children
            .OfType<PtTerminal>()
            .Single(c => c.Token.Type is TokenType.IDENTIFIER)
            .Token
            .Lexeme;

        if (!_functions.TryGetValue(id, out var func))
            throw new SemanticException("");

        var arguments = callNode.Children
            .OfType<PtNonTerminal>()
            .Single(c => c.Type is NonTerminalType.ARGUMENTS)
            .Children
            .OfType<PtNonTerminal>()
            .Where(c => c.Type is NonTerminalType.EXPRESSION)
            .Select(AnalyzeExpression)
            .ToList();

        return new AstCall(func.Type, id, arguments);
    }

    private AstVariable Variable(PtNode node)
    {
        if (node is not PtNonTerminal variableNode)
            throw new InvalidOperationException();

        var idNode = variableNode
            .Children
            .OfType<PtTerminal>()
            .Single(x => x.Token.Type is TokenType.IDENTIFIER);

        var identifier = idNode.Token.Lexeme;

        var foundVar = TryFindVar(identifier);
        if (foundVar is null)
            throw new SemanticException("");

        var expressionNode = variableNode
            .Children
            .OfType<PtNonTerminal>()
            .SingleOrDefault(x => x.Type is NonTerminalType.EXPRESSION);

        if (expressionNode is null)
            return new AstVariable(foundVar.Value.type, identifier, foundVar.Value.isRef);

        var expression = AnalyzeExpression(expressionNode);
        if (expression.Type is AstArrayType || expression.Type.Name is not AstTypeName.INTEGER)
            throw new SemanticException("");

        return new AstArrayElement(new AstType(foundVar.Value.type.Name), identifier, expression);
    }

    private (AstType type, bool isRef)? TryFindVar(string identifier)
    {
        for (var i = _contextStack.Count - 1; i >= 0; i--)
        {
            if (!_contextStack[i].TryGetValue(identifier, out var varInfo)) continue;
            return varInfo;
        }

        return null;
    }

    private static AstOperator AnalyzeOperator(PtNode node)
    {
        if (node is not PtTerminal opNode)
            throw new ArgumentException("", nameof(node));

        return opNode.Token.Type switch
        {
            TokenType.OP_ADD => AstOperator.ADD,
            TokenType.OP_SUB => AstOperator.SUB,
            TokenType.OP_MUL => AstOperator.MUL,
            TokenType.OP_DIV => AstOperator.DIV,
            TokenType.OP_MOD => AstOperator.MOD,
            TokenType.KW_AND => AstOperator.AND,
            TokenType.KW_NOT => AstOperator.NOT,
            _ => throw new InvalidOperationException(),
        };
    }

    private static AstRelationalOp AnalyzeRelationalOp(PtTerminal opNode)
    {
        return opNode.Token.Type switch
        {
            TokenType.LESS_THAN => AstRelationalOp.LESS_THAN,
            TokenType.LESS_THAN_OR_EQ => AstRelationalOp.LESS_THAN_OR_EQ,
            TokenType.GREATER_THAN => AstRelationalOp.GREATER_THAN,
            TokenType.GREATER_THAN_OR_EQ => AstRelationalOp.GREATER_THAN_OR_EQ,
            TokenType.EQUALS => AstRelationalOp.EQUALS,
            TokenType.DIAMOND => AstRelationalOp.DIAMOND,
            _ => throw new InvalidOperationException(),
        };
    }

    private static AstLiteral Literal(PtNode node)
    {
        if (node is not PtNonTerminal literalNode)
            throw new InvalidOperationException();

        var tokenNode = literalNode.Children.Single() as PtTerminal
                        ?? throw new InvalidOperationException();

        var token = tokenNode.Token;

        switch (token.Type)
        {
            case TokenType.STRING:
                return new AstLiteral(new(AstTypeName.STRING), token.Lexeme);
            case TokenType.INTEGER:
                if (!int.TryParse(token.Lexeme, out var intValue))
                    throw new InvalidOperationException();
                return new AstLiteral(new(AstTypeName.INTEGER), intValue);
            case TokenType.REAL:
                if (!float.TryParse(
                        token.Lexeme,
                        NumberStyles.AllowExponent |
                        NumberStyles.AllowDecimalPoint,
                        null,
                        out var floatValue))
                    throw new InvalidOperationException();
                return new AstLiteral(new(AstTypeName.REAL), floatValue);
            default:
                throw new InvalidOperationException();
        }
    }

    private static AstTypeName GetAstTypeName(Token token)
    {
        return token switch
        {
            {Type: TokenType.IDENTIFIER, Lexeme: "integer"} => AstTypeName.INTEGER,
            {Type: TokenType.IDENTIFIER, Lexeme: "string"} => AstTypeName.STRING,
            {Type: TokenType.IDENTIFIER, Lexeme: "real"} => AstTypeName.REAL,
            {Type: TokenType.IDENTIFIER, Lexeme: "boolean"} => AstTypeName.BOOL,
            _ => throw new ArgumentException("", nameof(token)),
        };
    }

    private static void CheckOperatorType(AstOperator op, AstType type)
    {
        switch (op)
        {
            case AstOperator.ADD:
                switch (type.Name)
                {
                    case AstTypeName.INTEGER:
                    case AstTypeName.REAL:
                    case AstTypeName.STRING:
                        break;
                    default:
                        throw new SemanticException("");
                }

                break;
            case AstOperator.DIV or AstOperator.MUL or AstOperator.SUB:
                switch (type.Name)
                {
                    case AstTypeName.INTEGER:
                    case AstTypeName.REAL:
                        break;
                    default:
                        throw new SemanticException("");
                }

                break;
            case AstOperator.MOD:
                switch (type.Name)
                {
                    case AstTypeName.INTEGER:
                        break;
                    default:
                        throw new SemanticException("");
                }

                break;
            case AstOperator.AND or AstOperator.NOT:
                switch (type.Name)
                {
                    case AstTypeName.BOOL:
                        break;
                    default:
                        throw new SemanticException("");
                }

                break;
        }
    }
}

public class SemanticException : Exception
{
    public SemanticException(string message) : base(message)
    {
    }
}