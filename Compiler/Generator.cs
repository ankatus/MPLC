namespace Compiler;

public class Generator
{
    private const int STRING_MAX_LENGTH = 1024;
    private readonly AstNode _ast;
    private int _varCounter;
    private int _labelCounter;

    public Generator(AstNode ast)
    {
        _ast = ast;
    }

    public string Generate()
    {
        if (_ast is not AstProgram program)
            throw new InvalidOperationException();

        var output = GetIncludes();
        output += GetStructs();
        output += GenerateProgram(program);

        return output;
    }

    private string GenerateProgram(AstProgram programNode)
    {
        var output = GetFuncsAndProcs(programNode);
        output += GetMainBlock(programNode.MainBlock);
        return output;
    }

    private static string GetIncludes()
    {
        const string output = "#include <stdio.h>\n" +
                              "#include <stdbool.h>\n" +
                              "#include <stdlib.h>\n" +
                              "#include <assert.h>\n" +
                              "#include <string.h>\n";

        return output;
    }

    private static string GetStructs()
    {
        var output = "";
        output += "typedef struct" +
                  "{" +
                  "int size;" +
                  "bool *data;" +
                  "} bool_array;";

        output += "typedef struct" +
                  "{" +
                  "int size;" +
                  "int *data;" +
                  "} int_array;";

        output += "typedef struct" +
                  "{" +
                  "int size;" +
                  "int *data;" +
                  "} float_array;";

        return output;
    }

    private string GetFuncsAndProcs(AstProgram programNode)
    {
        var output = "";
        var funcs = programNode.Functions;
        var procs = programNode.Procedures;

        // Prototypes
        foreach (var func in funcs)
        {
            output +=
                $"{GetCType(func.Type)} {func.Identifier}({GetParameterList(func.Parameters)});\n";
        }

        foreach (var proc in procs)
        {
            output += $"void {proc.Identifier}({GetParameterList(proc.Parameters)});\n";
        }

        // Implementations
        foreach (var func in funcs)
        {
            output += GetFunctionImplementation(func);
        }

        foreach (var proc in procs)
        {
            output += GetProcedureImplementation(proc);
        }

        return output;
    }

    private string GetMainBlock(AstBlock mainBlock)
    {
        var output = "int main()\n";
        output += "{\n";

        foreach (var variable in mainBlock.Context)
        {
            output += $"{GetVarDeclaration(variable.Key, variable.Value.type)}\n";
        }

        foreach (var statement in mainBlock.Statements)
        {
            if (statement is AstVarDeclaration)
                continue;

            output += GetStatement(statement);
        }

        output += "return 0;\n";

        output += "}\n";
        return output;
    }

    private string GetFunctionImplementation(AstFunction func)
    {
        var output = "";
        output += $"{GetCType(func.Type)} {func.Identifier}({GetParameterList(func.Parameters)})\n";
        output += $"{GetBlock(func.Block)}";
        return output;
    }

    private string GetProcedureImplementation(AstProcedure proc)
    {
        var output = "";
        output += $"void {proc.Identifier}({GetParameterList(proc.Parameters)})\n";
        output += $"{GetBlock(proc.Block)}";
        return output;
    }

    private string GetBlock(AstBlock block)
    {
        var output = "{\n";

        foreach (var variable in block.Context)
        {
            output += $"{GetVarDeclaration(variable.Key, variable.Value.type)}\n";
        }

        foreach (var statement in block.Statements)
        {
            if (statement is AstVarDeclaration)
                continue;

            output += GetStatement(statement);
        }

        output += "}\n";
        return output;
    }

    private string GetStatement(AstStatement statement)
    {
        return statement switch
        {
            AstBlock block => GetBlock(block),
            AstIf @if => GetIf(@if),
            AstWhile @while => GetWhile(@while),
            AstVarDeclaration _ => "",
            AstAssignment assignment => GetAssignment(assignment),
            AstCallStatement callStatement => GetCallStatement(callStatement),
            AstReturn @return => GetReturn(@return),
            AstRead read => GetRead(read),
            AstWrite write => GetWrite(write),
            AstAssert assert => GetAssert(assert),
            _ => throw new ArgumentOutOfRangeException(nameof(statement), statement, null)
        };
    }

    private string GetIf(AstIf @if)
    {
        var (output, conditionName) = GetExpressionEvaluation(@if.Condition);
        var elseLabel = GetLabelName();
        var endLabel = GetLabelName();

        output += $"if (!{conditionName}) goto {elseLabel};\n";
        output += GetStatement(@if.Then);
        output += $"goto {endLabel};";
        output += $"{elseLabel}:\n";
        if (@if is AstIfElse ifElse)
        {
            output += GetStatement(ifElse.Else);
        }

        output += $"{endLabel}:";

        return output;
    }

    private string GetWhile(AstWhile @while)
    {
        var output = "";
        var startLabel = GetLabelName();
        var endLabel = GetLabelName();

        output += $"{startLabel}:\n";
        var (expressionOutput, conditionName) = GetExpressionEvaluation(@while.Condition);
        output += expressionOutput;
        output += $"if (!{conditionName}) goto {endLabel};\n";
        output += GetStatement(@while.Statement);
        output += $"goto {startLabel};\n";
        output += $"{endLabel}:\n";

        return output;
    }

    private string GetCallStatement(AstCallStatement callStatement)
    {
        var output = "";
        var argumentVars = new List<string>();

        for (var i = 0; i < callStatement.Arguments.Count; i++)
        {
            var arg = callStatement.Arguments[i];
            var (argOutput, varName) = GetExpressionEvaluation(arg);
            output += argOutput;
            argumentVars.Add(callStatement.Callee.Parameters[i].IsRef ? $"&{varName}" : varName);
        }

        output += $"{callStatement.Callee.Identifier}({string.Join(',', argumentVars)});\n";

        return output;
    }

    private string GetReturn(AstReturn @return)
    {
        var (output, valueName) = GetExpressionEvaluation(@return.Expression);
        output += $"return {valueName};\n";
        return output;
    }

    private string GetRead(AstRead read)
    {
        var output = "";

        var indexQueue = new Queue<string>();
        foreach (var variable in read.Variables)
        {
            if (variable.Type.Name is AstTypeName.STRING)
            {
                // Zero string before reading new stuff into it
                output += $"memset({variable.Identifier},{0},{STRING_MAX_LENGTH});";
            }
            
            if (variable is not AstArrayElement arrayElement)
                continue;

            var (expressionOutput, expressionValueName) =
                GetExpressionEvaluation(arrayElement.Index);
            output += expressionOutput;

            indexQueue.Enqueue(expressionValueName);
        }

        output += "scanf(\"";

        for (var i = 0; i < read.Variables.Count; i++)
        {
            output += read.Variables[i] switch
            {
                {Type: AstArrayType} => throw new NotImplementedException(),
                {Type.Name: AstTypeName.INTEGER or AstTypeName.BOOL} => "%d",
                {Type.Name: AstTypeName.REAL} => "%f",
                {Type.Name: AstTypeName.STRING} => "%s",
                _ => throw new ArgumentOutOfRangeException(),
            };
        }

        output += "\",";

        for (var i = 0; i < read.Variables.Count; i++)
        {
            output += read.Variables[i] switch
            {
                {Type: AstArrayType} =>
                    throw new NotImplementedException(),
                AstArrayElement arrayElement =>
                    $"&{arrayElement.Identifier}.data[{indexQueue.Dequeue()}]",
                {Type.Name: AstTypeName.STRING} =>
                    $"{read.Variables[i].Identifier}",
                _ => $"&{read.Variables[i].Identifier}",
            };

            if (i < read.Variables.Count - 1)
                output += ",";
        }

        output += ");";

        return output;
    }

    private string GetWrite(AstWrite write)
    {
        var output = "";
        var varNames = new List<string>();

        foreach (var arg in write.Arguments)
        {
            var (argOutput, varName) = GetExpressionEvaluation(arg);
            output += argOutput;
            varNames.Add(varName);
        }

        output += "printf(\"";

        foreach (var arg in write.Arguments)
        {
            output += arg.Type switch
            {
                AstArrayType => throw new NotImplementedException(),
                {Name: AstTypeName.STRING} => "%s",
                {Name: AstTypeName.REAL} => "%f",
                {Name: AstTypeName.INTEGER or AstTypeName.BOOL} => "%d",
                _ => throw new ArgumentOutOfRangeException(),
            };
        }

        output += "\\n\", ";

        for (var i = 0; i < write.Arguments.Count; i++)
        {
            output += write.Arguments[i].Type switch
            {
                AstArrayType => throw new NotImplementedException(),
                _ => varNames[i],
            };

            if (i < write.Arguments.Count - 1)
                output += ", ";
        }

        output += ");\n";

        return output;
    }

    // TODO: Array element assignment?
    private string GetAssignment(AstAssignment statement)
    {
        var output = "";
        var (expressionOutput, valueName) = GetExpressionEvaluation(statement.Expression);
        output += expressionOutput;
        if (statement.Variable.IsRef)
            output += $"*{statement.Variable.Identifier} = {valueName};\n";
        else
            output += $"{statement.Variable.Identifier} = {valueName};\n";
        return output;
    }

    private string GetAssert(AstAssert assert)
    {
        var (output, valueName) = GetExpressionEvaluation(assert.Expression);
        output += $"assert({valueName});\n";

        return output;
    }

    private (string output, string valueName) GetExpressionEvaluation(AstExpression expression)
    {
        return expression switch
        {
            AstRelationExpression relationExpression => GetRelationExpression(relationExpression),
            AstSimpleExpression simpleExpression => GetSimpleExpression(simpleExpression),
            AstLiteral literal => GetLiteral(literal),
            AstVariable variable => GetVariable(variable),
            AstCall call => GetCall(call),
            AstNegation negation => GetNegation(negation),
            AstSizeExpr sizeExpr => ("", GetSize(sizeExpr)),
            _ => throw new ArgumentOutOfRangeException(nameof(expression)),
        };
    }

    private (string output, string valueName) GetRelationExpression(
        AstRelationExpression relationExpression)
    {
        var (output, leftValueName) = GetExpressionEvaluation(relationExpression.LeftExpr);
        var (rightOutput, rightValueName) = GetExpressionEvaluation(relationExpression.RightExpr);

        output += rightOutput;

        var valueName = GetVarName();
        var @operator = GetCRelationalOperator(relationExpression.Operator);
        var type = GetCType(relationExpression.Type);

        output += $"{type} {valueName} = {leftValueName} {@operator} {rightValueName};\n";

        return (output, valueName);
    }

    private (string output, string valueName) GetSimpleExpression(
        AstSimpleExpression simpleExpression)
    {
        var varNames = new List<string>();
        var operators = new List<string>();

        var (output, termName) = GetExpressionEvaluation(simpleExpression.Term);

        if (simpleExpression.Sign is AstSign.NEGATIVE)
        {
            var negatedTermName = GetVarName();
            output += $"{GetCType(simpleExpression.Type)} {negatedTermName} = -{termName};";
            termName = negatedTermName;
        }

        if (simpleExpression.Terms.Count == 0)
            return (output, termName);

        foreach (var term in simpleExpression.Terms)
        {
            var (termOutput, varName) = GetExpressionEvaluation(term.term);
            output += termOutput;
            varNames.Add(varName);

            operators.Add(GetCOperator(term.op));
        }

        var type = GetCType(simpleExpression.Type);
        var valueName = GetVarName();

        output += $"{type} {valueName} = {termName} {operators[0]} {varNames[0]};\n";

        for (var i = 1; i < varNames.Count; i++)
        {
            output += $"{valueName} = {valueName} {operators[i]} {varNames[i]};\n";
        }

        return (output, valueName);
    }

    private (string output, string valueName) GetLiteral(AstLiteral literal)
    {
        var asString = literal.Value.ToString();
        if (asString is null)
            throw new InvalidOperationException();

        if (literal.Type == AstType.String)
            asString = $"\"{asString}\"";

        return ("", asString);
    }

    private (string output, string valueName) GetVariable(AstVariable variable)
    {
        var id = "";
        if (variable is not AstArrayElement arrayElement)
            id = variable.Identifier;
        else
        {
            var (output, indexVarName) = GetExpressionEvaluation(arrayElement.Index);

            id = $"{variable.Identifier}.data[{indexVarName}]";
        }

        if (variable.IsRef)
            id = id.Insert(0, "*");

        return ("", id);
    }

    private (string output, string valueName) GetCall(AstCall call)
    {
        var output = "";
        var argumentVars = new List<string>();

        foreach (var arg in call.Arguments)
        {
            var (argOutput, varName) = GetExpressionEvaluation(arg);
            output += argOutput;
            argumentVars.Add(varName);
        }

        var valueName = GetVarName();
        output +=
            $"{GetCType(call.Type)} {valueName} = {call.Identifier}({string.Join(',', argumentVars)});\n";

        return (output, valueName);
    }

    private (string output, string valueName) GetNegation(AstNegation negation)
    {
        var (output, factorName) = GetExpressionEvaluation(negation.Factor);

        var valueName = GetVarName();

        output += $"{GetCType(negation.Factor.Type)} {valueName} = !{factorName};\n";

        return (output, valueName);
    }

    private string GetVarDeclaration(string name, AstType type)
    {
        var output = "";

        if (type is not AstArrayType arrayType)
        {
            if (type.Name is AstTypeName.STRING)
                return $"char {name}[{STRING_MAX_LENGTH}] = {{}};";
            
            return $"{GetCType(type)} {name};";
        }

        if (arrayType.Size is null)
            throw new InvalidOperationException();

        var (expressionOutput, expressionValueName) = GetExpressionEvaluation(arrayType.Size);
        output += expressionOutput;
        var cTypeName = GetCType(type);
        output +=
            $"{cTypeName}_array {name}; {name}.size = {expressionValueName}; {name}.data = malloc(sizeof({cTypeName}) * {expressionValueName});";

        return output;
    }

    private string GetParameterList(List<Parameter> parameters)
    {
        var output = "";

        for (var i = 0; i < parameters.Count; i++)
        {
            var current = parameters[i];
            if (current.Type is AstArrayType)
            {
                output += $"{GetCType(current.Type)}_array {current.Name}";
            }
            else
            {
                if (current.IsRef)
                    output += $"{GetCType(current.Type)} *{current.Name}";
                else
                    output += $"{GetCType(current.Type)} {current.Name}";
            }

            if (i < parameters.Count - 1)
                output += ", ";
        }

        return output;
    }

    private static string GetSize(AstSizeExpr sizeExpr)
    {
        if (sizeExpr.Factor is AstVariable arrayVariable)
        {
            return $"{arrayVariable.Identifier}.size";
        }

        return "1;";
    }

    private static string GetCType(AstType type)
    {
        return type switch
        {
            {Name: AstTypeName.INTEGER} => "int",
            {Name: AstTypeName.REAL} => "float",
            {Name: AstTypeName.BOOL} => "bool",
            {Name: AstTypeName.STRING} => throw new NotImplementedException(),
            _ => throw new ArgumentOutOfRangeException(nameof(type)),
        };
    }

    private static string GetCOperator(AstOperator @operator)
    {
        return @operator switch
        {
            AstOperator.ADD => "+",
            AstOperator.SUB => "-",
            AstOperator.MUL => "*",
            AstOperator.DIV => "/",
            AstOperator.MOD => "%",
            AstOperator.AND => "&&",
            AstOperator.NOT => "!",
            _ => throw new ArgumentOutOfRangeException(nameof(@operator), @operator, null)
        };
    }

    private static string GetCRelationalOperator(AstRelationalOp @operator)
    {
        return @operator switch
        {
            AstRelationalOp.LESS_THAN => "<",
            AstRelationalOp.GREATER_THAN => ">",
            AstRelationalOp.DIAMOND => "!=",
            AstRelationalOp.EQUALS => "==",
            AstRelationalOp.GREATER_THAN_OR_EQ => ">=",
            AstRelationalOp.LESS_THAN_OR_EQ => "<=",
            _ => throw new ArgumentOutOfRangeException(nameof(@operator), @operator, null)
        };
    }

    private string GetVarName() => $"var{_varCounter++}";

    private string GetLabelName() => $"label{_labelCounter++}";
}