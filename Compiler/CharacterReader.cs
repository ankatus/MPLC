namespace Compiler;

public class CharacterReader
{
    private readonly List<char> _chars;
    private readonly List<char> _readChars = new();

    public string ReadString => new string(_readChars.ToArray());
    public int Index { get; private set; }
    public int Col { get; private set; }
    public int NewLines { get; private set; }
    public bool NextIsWhitespace { get; private set; }
    public bool EndFound { get; private set; }
    public char Next { get; private set; }
    
    public CharacterReader(List<char> chars, int index, int col)
    {
        if (index < 0 || index >= chars.Count)
            throw new ArgumentOutOfRangeException(nameof(index));
        
        _chars = chars;
        Index = index;
        Col = col;

        Next = _chars[Index];

        NextIsWhitespace = char.IsWhiteSpace(_chars[Index]);
    }

    public void Advance()
    {
        if (Index == _chars.Count)
            return; // TODO: Throw here instead?
     
        _readChars.Add(Next);
        
        Index++;

        // Check if end of input reached
        if (Index == _chars.Count)
        {
            NextIsWhitespace = true;
            EndFound = true;
            Next = ' '; // Use a space at the end

            return;
        }
        
        // Reset line position if previous character was a newline
        if (Next == '\n')
        {
            Col = 1;
        }
        
        Next = _chars[Index];
        _readChars.Add(_chars[Index]);
        Col++;

        NextIsWhitespace = char.IsWhiteSpace(Next);

        if (Next == '\n')
            NewLines++;
    }
}