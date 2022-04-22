namespace Compiler;

public class ScannerInput
{
    private readonly string _input;
    private int _baseIndex;
    private int _nextIndex;

    public string CurrentString => _input.Substring(_baseIndex, _nextIndex - _baseIndex);

    public char NextChar
    {
        get
        {
            if (EndReached)
                throw new InvalidOperationException();
            
            return _input[_nextIndex];
        }
    }
    public bool EndReached => _nextIndex == _input.Length;
    public int BaseCol { get; private set; } = 1;
    public int BaseLine { get; private set; } = 1;
    
    public ScannerInput(string input)
    {
        _input = input;
    }

    public void Advance()
    {
        if (_nextIndex == _input.Length)
            throw new InvalidOperationException();
        
        _nextIndex++;
    }

    public void Reset()
    {
        _nextIndex = _baseIndex;
    }

    public void Commit()
    {
        var charsToCommit = new List<char>(CurrentString);
        var newLines = charsToCommit.Count(c => c == '\n');

        if (newLines > 0)
        {
            BaseLine += newLines;
            var lastNewLine = charsToCommit.LastIndexOf('\n');
            var charsOnLastLine = charsToCommit.Count - lastNewLine - 1;
            BaseCol = charsOnLastLine + 1;
        }
        else
        {
            BaseCol += charsToCommit.Count;
        }
        
        _baseIndex = _nextIndex;
    }
}