module jaster.serial.core.lexer;

import std.format : format;

@safe
struct DebugInfo
{
    string file;
    uint line = 1;
    uint column;

    string toString() const
    {
        return "%s:%s:%s".format(this.file, this.line, this.column);
    }
}

enum TokenType
{
    ERROR,
    STRING,
    INTEGER,
    FLOAT,
    IDENTIFIER,
    BOOLEAN,

    OP_COLON,
    OP_AT,
    OP_BRACKET_L,
    OP_BRACKET_R,
    OP_CURLY_BRACKET_L,
    OP_CURLY_BRACKET_R,
    OP_DIVA,
    OP_EXCLAMATION,
    OP_SQUARE_BRACKET_L,
    OP_SQUARE_BRACKET_R,
    OP_DOT,
    OP_COMMA,

    KW_TYPE,
    KW_MULTI,
    KW_NAMESPACE,
    KW_VALUE,
    KW_ENUM,
    KW_REFERENCE
}

struct Token
{
    TokenType type;
    DebugInfo debug_;
    string    text;
}

@safe
struct Lexer 
{
    private struct OperatorInfo
    {
        char ch;
        TokenType type;
    }
    private static immutable OPERATORS = 
    [
        OperatorInfo(':', TokenType.OP_COLON),
        OperatorInfo('@', TokenType.OP_AT),
        OperatorInfo('(', TokenType.OP_BRACKET_L),
        OperatorInfo(')', TokenType.OP_BRACKET_R),
        OperatorInfo('{', TokenType.OP_CURLY_BRACKET_L),
        OperatorInfo('}', TokenType.OP_CURLY_BRACKET_R),
        OperatorInfo(';', TokenType.OP_DIVA),
        OperatorInfo('!', TokenType.OP_EXCLAMATION),
        OperatorInfo('[', TokenType.OP_SQUARE_BRACKET_L),
        OperatorInfo(']', TokenType.OP_SQUARE_BRACKET_R),
        OperatorInfo('.', TokenType.OP_DOT),
        OperatorInfo(',', TokenType.OP_COMMA),
    ];

    private struct KeywordInfo
    {
        string word;
        TokenType type;
    }
    private static immutable KEYWORDS =
    [
        KeywordInfo("type",         TokenType.KW_TYPE),
        KeywordInfo("multi",        TokenType.KW_MULTI),
        KeywordInfo("namespace",    TokenType.KW_NAMESPACE),
        KeywordInfo("value",        TokenType.KW_VALUE),
        KeywordInfo("enum",         TokenType.KW_ENUM),
        KeywordInfo("reference",    TokenType.KW_REFERENCE),
    ];

    enum COMMENT_CHAR = '#';

    private
    {
        string    _input;
        size_t    _cursor;
        Token     _front;
        DebugInfo _debug;
    }

    this(string input, string file = "no_file")
    {
        this._debug.file = file;
        this._input = input;
        this.popFront();
    }

    @property @nogc
    Token front() nothrow pure { return this._front; }

    @property @nogc
    bool empty() nothrow pure { return this._front.type == TokenType.ERROR; }
    
    void popFront()
    {
        this.eatWhite();
        if(this.isEOF)
        {
            this._front.type = TokenType.ERROR;
            return;
        }

        const ch = this.peekChar();
        if(ch == COMMENT_CHAR)
        {
            this.eatLine();
            this.popFront();
            return;
        }

        // import std.stdio;
        // writeln(ch, " ", this.getOperator(ch), " ", this.isNumericStart(ch), " ", this.isIdentifierChar(ch));

        if(ch == '"')
            this.readString();
        else if(this.getOperator(ch) != TokenType.ERROR)
        {
            this.createTokenFromRange(this.getOperator(ch), this._cursor, this._cursor + 1);
            this.nextChar();
        }
        else if(this.isNumericStart(ch))
            this.readNumber();
        else if(this.isIdentifierChar(ch))
            this.readIdentifier();
        else
            this.enforce(false, "Unexpected character '%s'.", ch);
    }

    private void readString()
    {
        this.enforceNextChar('"');

        const start = this._cursor;
        while(true)
        {
            this.enforceNotEOF();
            if(this.nextChar() == '"')
            {
                this.createTokenFromRange(TokenType.STRING, start, this._cursor - 1); // - 1 since we want the end to be on top of the ending '"'
                return;
            }
        }
    }

    private void readNumber()
    {
        import std.uni : isWhite;
        this.enforceNotEOF();
        assert(this.isNumericStart(this.peekChar()), "This function shouldn't have been called.");

        const start   = this._cursor;
        bool foundDot = false;
        while(!this.isEOF)
        {
            const ch = this.nextChar();
            if(ch == '-')
            {
                this.enforce(this._cursor == start + 1, "Unexpected '-'. Minus signs can only appear at the start of a number.");
                continue;
            }

            if(ch == '.')
            {
                this.enforce(!foundDot, "Unexpected '.'. There can only be one dot ('.') within a number.");
                foundDot = true;
                continue;
            }

            if(this.getOperator(ch) != TokenType.ERROR
            || ch.isWhite)
            {
                this._cursor--; // In case it's an operator.
                break;
            }

            this.enforce(ch >= '0' && ch <= '9', "Expected digit, not '%s'.", ch);
        }

        assert(this._cursor > start, "No characters were read?");
        this.createTokenFromRange((foundDot) ? TokenType.FLOAT : TokenType.INTEGER, start, this._cursor);
    }

    private void readIdentifier()
    {
        assert(this.isIdentifierChar(this.peekChar()), "This function shouldn't have been called.");

        const start = this._cursor;
        while(!this.isEOF && this.isIdentifierChar(this.peekChar()))
            this.nextChar();

        this.createTokenFromRange(TokenType.IDENTIFIER, start, this._cursor);

        if(this._front.text == "yes" || this._front.text == "no"
        || this._front.text == "true" || this._front.text == "false")
            this._front.type = TokenType.BOOLEAN;
        
        const kwType = this.getKeyword(this.front.text);
        if(kwType != TokenType.ERROR)
            this._front.type = kwType;
    }
    
    @nogc
    private void createTokenFromRange(TokenType type, size_t start, size_t end) pure nothrow
    {
        this._front.type   = type;
        this._front.debug_ = this._debug;
        this._front.text   = this._input[start..end];
    }

    private void enforce(Args...)(bool condition, lazy string message, lazy Args args)
    {
        if(!condition)
            throw new JasterialiseLexerException("%s - %s".format(this._debug, message.format(args)));
    }

    private void enforceNotEOF()
    {
        this.enforce(!this.isEOF, "Unexpected End of file.");
    }

    private char enforceNextChar(char expectedChar)
    {
        this.enforceNotEOF();
        auto ch = this.nextChar();

        this.enforce(ch == expectedChar, "Expected '%s' but got '%s'.", expectedChar, ch);

        return ch;
    }

    @nogc
    private TokenType getOperator(char ch) pure nothrow
    {
        switch(ch)
        {
            static foreach(op; OPERATORS)
            {
                case op.ch: return op.type;
            }

            default: return TokenType.ERROR;
        }
    }

    @nogc
    private TokenType getKeyword(string word) pure nothrow
    {
        switch(word)
        {
            static foreach(kw; KEYWORDS)
            {
                case kw.word: return kw.type;
            }

            default: return TokenType.ERROR;
        }
    }

    @nogc
    private bool isNumericStart(char ch) pure nothrow
    {
        return (
            (ch >= '0' && ch <= '9')
         || ch == '-'
        );
    }

    @nogc
    private bool isIdentifierChar(char ch) pure nothrow
    {
        return (
            (ch >= 'a' && ch <= 'z')
         || (ch >= 'A' && ch <= 'Z')
         || (ch >= '0' && ch <= '9')
         || ch == '_'
        );
    }

    @nogc
    private bool isEOF() pure nothrow
    {
        return this._cursor >= this._input.length;
    }

    @nogc
    private char peekChar() nothrow pure
    {
        assert(!this.isEOF, "Check for EOF beforehand please.");
        return this._input[this._cursor];
    }

    @nogc
    private char nextChar() nothrow pure
    {
        assert(!this.isEOF, "Check for EOF beforehand please.");

        const ch = this._input[this._cursor++];
        this._debug.column++;

        if(ch == '\n')
        {
            this._debug.line++;
            this._debug.column = 0;
        }

        return ch;
    }

    @nogc
    private void eatWhite() nothrow pure
    {
        import std.uni : isWhite;

        while(!this.isEOF && this.peekChar().isWhite)
            this.nextChar();
    }

    @nogc
    private void eatLine() nothrow pure
    {
        while(!this.isEOF && this.peekChar() != '\n')
            this.nextChar();
    }
}

class JasterialiseLexerException : Exception
{
    this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) pure nothrow @nogc @safe
    {
        super(msg, file, line, nextInChain);
    }
}

version(unittest)
{
    import std.algorithm : map;
    import std.array : array;
    import fluent.asserts;

    const TEST_INPUT = import("tiled.jser");
    
    auto mapText(R)(R range)
    {
        return range.map!(v => v.text);
    }

    @("The lexer can parse an entire valid file")
    unittest
    {
        Lexer(TEST_INPUT, "tiled.jser").array;
    }

    @("Lexer can handle strings")
    unittest
    {
        Lexer(`"Hello!"`).front.text.should.equal("Hello!");
        ({Lexer(`"Hello!`);}).should.throwException!JasterialiseLexerException.because("Unterminated string");
    }

    @("Lexer can handle operators")
    unittest
    {
        Lexer(`:@ (`).array.mapText.should.equal([":", "@", "("]);
    }

    @("Lexer can handle identifiers/keywords")
    unittest
    {
        Lexer("A:B\nC D").array.mapText.should.equal(["A", ":", "B", "C", "D"]);
        Lexer("A").front.type.should.equal(TokenType.IDENTIFIER);
        Lexer("type").front.type.should.equal(TokenType.KW_TYPE);
    }

    @("Lexer can handle numbers")
    unittest
    {
        Lexer("1").front.type.should.equal(TokenType.INTEGER);
        Lexer("1").front.text.should.equal("1");

        Lexer("-1").front.text.should.equal("-1");

        Lexer("1.0").front.text.should.equal("1.0");
        Lexer("1.0").front.type.should.equal(TokenType.FLOAT);

        Lexer("1 1.0\n-2:4").array.mapText.should.equal(["1", "1.0", "-2", ":", "4"]);

        ({Lexer("1-0");}).should.throwException!JasterialiseLexerException.because("'-' midway through number.");
        ({Lexer("1.0.");}).should.throwException!JasterialiseLexerException.because("Multiple decimal places.");
    }

    @("Lexer can handle comments")
    unittest
    {
        Lexer("ABC\n#Easy as\n123").array.mapText.should.equal(["ABC", "123"]);
    }

    @("Lexer can handle bools")
    unittest
    {
        foreach(word; ["yes", "no", "true", "false"])
            Lexer(word).front.type.should.equal(TokenType.BOOLEAN);
    }
}