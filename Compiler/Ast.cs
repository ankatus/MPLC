namespace Compiler;

public enum AstTypeName
{
    BOOL,
    STRING,
    INTEGER,
    REAL,
}

public enum AstRelationalOp
{
    LESS_THAN,
    GREATER_THAN,
    DIAMOND,
    EQUALS,
    GREATER_THAN_OR_EQ,
    LESS_THAN_OR_EQ
}

public enum AstOperator
{
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    AND,
    NOT
}

public enum AstSign
{
    POSITIVE,
    NEGATIVE,
}

public abstract class AstNode
{
}

public class AstType : AstNode
{
    public AstType(AstTypeName name)
    {
        Name = name;
    }

    public AstTypeName Name { get; }

    public string DebugName => this is AstArrayType ? $"Array of type {Name}" : $"{Name}";
    
    public static bool operator ==(AstType a, AstType b)
    {
        if (a.Name != b.Name)
            return false;

        if (a is AstArrayType && b is not AstArrayType)
            return false;

        if (b is AstArrayType && a is not AstArrayType)
            return false;

        return true;
    }

    public static bool operator !=(AstType a, AstType b)
    {
        return !(a == b);
    }

    public static AstType Int => new(AstTypeName.INTEGER);
    public static AstType String => new(AstTypeName.STRING);
    public static AstType Bool => new(AstTypeName.BOOL);
}

public class AstArrayType : AstType
{
    public AstArrayType(AstTypeName name, AstExpression? size) : base(name)
    {
        Size = size;
    }

    public AstExpression? Size { get; }
}

public class AstProgram : AstNode
{
    public AstProgram(AstBlock mainBlock, List<AstFunction> functions,
        List<AstProcedure> procedures)
    {
        MainBlock = mainBlock;
        Functions = functions;
        Procedures = procedures;
    }

    public List<AstFunction> Functions { get; }
    public List<AstProcedure> Procedures { get; }
    public AstBlock MainBlock { get; }
}

public class Parameter
{
    public Parameter(string name, AstType type, bool isRef)
    {
        Name = name;
        Type = type;
        IsRef = isRef;
    }

    public string Name { get; }
    public AstType Type { get; }
    public bool IsRef { get; }
}

public class AstProcedure : AstNode, IAstCallable
{
    public AstProcedure(string identifier,
        List<Parameter> parameters, AstBlock block)
    {
        Identifier = identifier;
        Parameters = parameters;
        Block = block;
    }

    public string Identifier { get; }
    public List<Parameter> Parameters { get; }
    public AstBlock Block { get; }
}

public class AstFunction : AstNode, IAstCallable
{
    public AstFunction(AstType type, string identifier,
        List<Parameter> parameters, AstBlock block)
    {
        Type = type;
        Identifier = identifier;
        Parameters = parameters;
        Block = block;
    }

    public AstType Type { get; }
    public string Identifier { get; }
    public List<Parameter> Parameters { get; }
    public AstBlock Block { get; }
}

public class AstBlock : AstStatement
{
    public AstBlock(List<AstStatement> statements,
        Dictionary<string, (AstType type, bool isRef)> context)
    {
        Statements = statements;
        Context = context;
    }

    public Dictionary<string, (AstType type, bool isRef)> Context { get; }
    public List<AstStatement> Statements { get; }
}

public class AstIf : AstStatement
{
    public AstIf(AstExpression condition, AstStatement then)
    {
        Condition = condition;
        Then = then;
    }

    public AstExpression Condition { get; }
    public AstStatement Then { get; }
}

public class AstIfElse : AstIf
{
    public AstIfElse(AstExpression condition, AstStatement then,
        AstStatement @else) : base(condition, then)
    {
        Else = @else;
    }

    public AstStatement Else { get; }
}

public class AstWhile : AstStatement
{
    public AstWhile(AstExpression condition, AstStatement statement)
    {
        Condition = condition;
        Statement = statement;
    }

    public AstExpression Condition { get; }
    public AstStatement Statement { get; }
}

public abstract class AstStatement : AstNode
{
}

public class AstVarDeclaration : AstStatement
{
    public AstVarDeclaration(List<AstVariable> variables)
    {
        Variables = variables;
    }

    public List<AstVariable> Variables { get; }
}

public class AstAssignment : AstStatement
{
    public AstAssignment(AstVariable variable, AstExpression expression)
    {
        Variable = variable;
        Expression = expression;
    }

    public AstVariable Variable { get; }
    public AstExpression Expression { get; }
}

public interface IAstCallable
{
    public string Identifier { get; }
    public List<Parameter> Parameters { get; }
}

public class AstCallStatement : AstStatement
{
    public AstCallStatement(IAstCallable callee, List<AstExpression> arguments)
    {
        Callee = callee;
        Arguments = arguments;
    }

    public IAstCallable Callee { get; }
    public List<AstExpression> Arguments { get; }
}

public class AstReturn : AstStatement
{
    public AstReturn(AstExpression expression)
    {
        Expression = expression;
    }

    public AstExpression Expression { get; }
}

public class AstRead : AstStatement
{
    public AstRead(List<AstVariable> variables)
    {
        Variables = variables;
    }

    public List<AstVariable> Variables { get; }
}

public class AstWrite : AstStatement
{
    public AstWrite(List<AstExpression> arguments)
    {
        Arguments = arguments;
    }

    public List<AstExpression> Arguments { get; }
}

public class AstAssert : AstStatement
{
    public AstAssert(AstExpression expression)
    {
        Expression = expression;
    }

    public AstExpression Expression { get; }
}

public abstract class AstExpression : AstNode
{
    protected AstExpression(AstType type)
    {
        Type = type;
    }

    public AstType Type { get; }
}

public class AstRelationExpression : AstExpression
{
    public AstRelationExpression(AstExpression leftExpr,
        AstExpression rightExpr, AstRelationalOp @operator) : base(
        new AstType(AstTypeName.BOOL))
    {
        LeftExpr = leftExpr;
        RightExpr = rightExpr;
        Operator = @operator;
    }

    public AstExpression LeftExpr { get; }
    public AstExpression RightExpr { get; }
    public AstRelationalOp Operator { get; }
}

public class AstSimpleExpression : AstExpression
{
    public AstSimpleExpression(AstType type, AstSign sign, AstExpression term,
        List<(AstOperator op, AstExpression term)> terms) : base(type)
    {
        Sign = sign;
        Term = term;
        Terms = terms;
    }

    public AstSign Sign { get; }
    public AstExpression Term { get; }
    public List<(AstOperator op, AstExpression term)> Terms { get; }
}

public class AstLiteral : AstExpression
{
    public AstLiteral(AstType type, object value) : base(type)
    {
        Value = value;
    }

    public object Value { get; }
}

public class AstVariable : AstExpression
{
    public AstVariable(AstType type, string identifier, bool isRef) : base(type)
    {
        Identifier = identifier;
        IsRef = isRef;
    }

    public string Identifier { get; }
    public bool IsRef { get; }
}

public class AstArrayElement : AstVariable
{
    public AstArrayElement(AstType type, string identifier, AstExpression index) : base(type,
        identifier, false)
    {
        Index = index;
    }

    public AstExpression Index { get; }
}

public class AstCall : AstExpression
{
    public AstCall(AstType type, string identifier,
        List<AstExpression> arguments) : base(type)
    {
        Identifier = identifier;
        Arguments = arguments;
    }

    public string Identifier { get; }
    public List<AstExpression> Arguments { get; }
}

public class AstNegation : AstExpression
{
    public AstNegation(AstExpression factor) : base(new AstType(AstTypeName.BOOL))
    {
        Factor = factor;
    }

    public AstExpression Factor { get; }
}

public class AstSizeExpr : AstExpression
{
    public AstSizeExpr(AstExpression factor) : base(new AstType(AstTypeName.INTEGER))
    {
        Factor = factor;
    }

    public AstExpression Factor { get; }
}