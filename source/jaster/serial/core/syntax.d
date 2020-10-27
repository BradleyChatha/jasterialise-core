module jaster.serial.core.syntax;

import std.conv      : to;
import std.format    : format;
import std.exception : enforce;
import std.typecons  : Flag;
import jaster.serial.core;

version(unittest)
{ 
    import core.exception;
    import fluent.asserts;
}

alias KeywordsAreIdentifiers = Flag!"kwai";

enum NodeType
{
    ERROR,
    IDENTIFIER,
    STRING,
    NUMBER,
    BOOLEAN,
    NAMESPACE,
    ATTRIBUTE,
    MEMBER_TYPE,
    OBJECT_TYPE,
    GLOBAL_ATTRIBUTES,
    NAMED_VALUE,
    SYMBOL_FQN,
    MEMBER_DECLARATION,
    TYPE_DECLARATION
}

// START HELPER FUNCS
void enforceFrontType(Lexer lexer, TokenType[] expectedTypes, KeywordsAreIdentifiers keywordsAreIdentifiers = KeywordsAreIdentifiers.no)
{
    import std.algorithm : canFind;
    enforce(
        (keywordsAreIdentifiers)
        ? expectedTypes.canFind(lexer.front.type) || (lexer.front.type.isKeyword && expectedTypes.canFind(TokenType.IDENTIFIER))
        : expectedTypes.canFind(lexer.front.type),
        "%s: Expected any of %s but got a %s.".format(lexer.front.debug_, expectedTypes, lexer.front.type)
    );
}

Token enforceFrontTypeAndPop(ref Lexer lexer, TokenType[] expectedTypes, KeywordsAreIdentifiers keywordsAreIdentifiers = KeywordsAreIdentifiers.no)
{
    lexer.enforceFrontType(expectedTypes, keywordsAreIdentifiers);
    auto value = lexer.front;
    lexer.popFront();

    return value;
}

void enforceHasTokens(Lexer lexer)
{
    enforce(!lexer.empty, "Unexpected end of file.");
}

void onUnexpectedToken(Token unexpected, string message)
{
    throw new Exception(
        "%s: Unexpected token '%s' of type %s - %s".format(unexpected.debug_, unexpected.text, unexpected.type, message)
    );
}

void onUnexpectedEof(Token roughlyAround, string message)
{
    throw new Exception(
        "%s: Unexpected end of file near token '%s' of type %s - %s".format(roughlyAround.debug_, roughlyAround.text, roughlyAround.type, message)
    );
}

bool isKeyword(TokenType type)
{
    import std.algorithm : startsWith;
    return type.to!string().startsWith("KW_");
}

bool isOperator(TokenType type)
{
    import std.algorithm : startsWith;
    return type.to!string().startsWith("OP_");
}

enum AllowValueType
{
    NONE,
    BASIC      = 1 << 0,
    NAMED      = 1 << 1,
    SYMBOL_FQN = 1 << 2
}

AstNode nextValue(AllowValueType AllowedTypes)(ref Lexer lexer)
{
    switch(lexer.front.type) with(TokenType)
    {
        static if(AllowedTypes & AllowValueType.BASIC)
        {
            case STRING: return String.fromDefault(lexer);
            case FLOAT:
            case INTEGER: return Number.fromDefault(lexer);
            case BOOLEAN: return Boolean.fromDefault(lexer);
        }
        
        static if(AllowedTypes & AllowValueType.SYMBOL_FQN)
        {
            case OP_DOT:
                return SymbolFqn.fromDefault(lexer);
        }

        case IDENTIFIER:
            static if(AllowedTypes & AllowValueType.NAMED && AllowedTypes & AllowValueType.SYMBOL_FQN)
            {
                auto copy = lexer;
                copy.popFront();

                if(copy.front.type == TokenType.OP_COLON)
                    return NamedValue.fromDefault!(AllowedTypes & ~AllowValueType.NAMED)(lexer);
                else
                    return SymbolFqn.fromDefault(lexer);
            }
            else static if(AllowedTypes & AllowValueType.NAMED)
                return NamedValue.fromDefault!(AllowedTypes & ~AllowValueType.NAMED)(lexer);
            else static if(AllowedTypes & AllowValueType.SYMBOL_FQN)
                return SymbolFqn.fromDefault(lexer);
            else
                goto default;

        default:
            lexer.front.onUnexpectedToken("Expected a value");
            assert(false);
    }
}
// END HELPER FUNCS

final class JasterialiseFile
{
    private
    {
        AstNode[] _nodes;
    }

    static JasterialiseFile fromString(string code)
    {
        return JasterialiseFile.fromLexer(Lexer(code));
    }

    static JasterialiseFile fromLexer(Lexer lexer)
    {
        auto file = new JasterialiseFile();

        while(!lexer.empty)
        {
            auto node = file.nextNode(lexer);
            file._nodes ~= node;
        }

        return file;
    }

    private AstNode nextNode(ref Lexer lexer)
    {
        lexer.enforceHasTokens();
        switch(lexer.front.type)
        {
            case TokenType.KW_TYPE:
            case TokenType.KW_MULTI:
            case TokenType.KW_VALUE:
            case TokenType.KW_REFERENCE:
            case TokenType.KW_ENUM:
                return this.nextTypeDeclarationOrStatement(lexer);

            case TokenType.KW_NAMESPACE:
                return NamespaceStatement.fromDefault(lexer);

            default:
                lexer.front.onUnexpectedToken("Expected a keyword in top-level scope.");
                assert(0); // Above function is noreturn.
        }
    }

    private AstNode nextTypeDeclarationOrStatement(ref Lexer lexer)
    {
        auto copy = lexer;

        while(!copy.empty)
        {
            if(copy.front.type == TokenType.OP_COLON)
                return GlobalAttributesStatement.fromDefault(lexer);
            else if(copy.front.type == TokenType.OP_CURLY_BRACKET_L)
                return TypeDeclaration.fromDefault(lexer);
            else if(copy.front.type == TokenType.IDENTIFIER)
            {
                auto copyCopy = copy;
                copyCopy.popFront();

                if(copyCopy.front.type == TokenType.OP_COLON)
                    return TypeDeclaration.fromDefault(lexer);
            }
            else if(!copy.front.type.isKeyword && copy.front.type != TokenType.IDENTIFIER)
            {
                copy.front.onUnexpectedToken("Expected a keyword, identifier, a colon ':', or a left curly bracket '{'.");
                assert(0);
            }

            copy.popFront();
        }

        lexer.front.onUnexpectedEof("When determining whether the next token is a type declaration or statement.");
        assert(false);
    }
}

// START NODES
abstract class AstNode
{
    private
    {
        NodeType _type;
    }

    DebugInfo debug_;

    this(NodeType type)
    {
        this._type = type;
    }

    T as(T)()
    out(obj; obj !is null, "Cannot convert this node into a "~T.stringof)
    {
        return cast(T)this;
    }

    @property @safe @nogc
    final NodeType type() nothrow
    {
        return this._type;
    }
}

final class String : AstNode
{
    string value;

    this()
    {
        super(NodeType.STRING);
    }

    static String fromDefault(ref Lexer lexer)
    {
        auto node = new String();
        node.value = lexer.enforceFrontTypeAndPop([TokenType.STRING]).text;
        return node;
    }
    @("String.fromDefault")
    unittest
    {
        auto lexer = Lexer(`"Hello""World"`);
        String.fromDefault(lexer).value.should.equal("Hello");
        String.fromDefault(lexer).value.should.equal("World");
        ({String.fromDefault(lexer);}).should.throwAnyException.because("No more tokens");
    }
}

final class Number : AstNode
{
    private union
    {
        double _float;
        long   _integer;
    }
    private TokenType _type;
    
    this()
    {
        super(NodeType.NUMBER);
    }

    static Number fromDefault(ref Lexer lexer)
    {
        const token = lexer.enforceFrontTypeAndPop([TokenType.INTEGER, TokenType.FLOAT]);
        auto node   = new Number();
        node._type  = token.type;

        if(token.type == TokenType.INTEGER)
            node._integer = token.text.to!long;
        else
            node._float = token.text.to!double;

        return node;
    }
    @("Number.fromDefault")
    unittest
    {
        auto lexer = Lexer("1 -1 1.0 -1.0 2 2.0");
        Number.fromDefault(lexer).asInteger.should.equal(1);
        Number.fromDefault(lexer).asInteger.should.equal(-1);
        Number.fromDefault(lexer).asFloat.should.equal(1.0);
        Number.fromDefault(lexer).asFloat.should.equal(-1.0);

        ({Number.fromDefault(lexer).asFloat;}).should.throwException!AssertError.because("It's not a float");
        ({Number.fromDefault(lexer).asInteger;}).should.throwException!AssertError.because("It's not an integer");
        ({Number.fromDefault(lexer);}).should.throwAnyException.because("No more tokens");
    }

    @property @safe @nogc nothrow:

    bool isFloat()   { return this._type == TokenType.FLOAT; }
    bool isInteger() { return this._type == TokenType.INTEGER; }
    double asFloat() { assert(this.isFloat, "I'm not a float"); return this._float; }
    long asInteger() { assert(this.isInteger, "I'm not an integer"); return this._integer; }
}

final class Boolean : AstNode
{
    bool value;

    this()
    {
        super(NodeType.BOOLEAN);
    }

    static Boolean fromDefault(ref Lexer lexer)
    {
        auto node = new Boolean();

        const token = lexer.enforceFrontTypeAndPop([TokenType.BOOLEAN]);
        switch(token.text)
        {
            case "yes":
            case "true":
                node.value = true;
                break;

            case "no":
            case "false":
                node.value = false;
                break;

            default: assert(false, "This is not a boolean: " ~ token.text);
        }

        return node;
    }
    @("Boolean.fromDefault")
    unittest
    {
        auto lexer = Lexer(`yes no true false`);
        Boolean.fromDefault(lexer).value.should.equal(true);
        Boolean.fromDefault(lexer).value.should.equal(false);
        Boolean.fromDefault(lexer).value.should.equal(true);
        Boolean.fromDefault(lexer).value.should.equal(false);
        ({Boolean.fromDefault(lexer);}).should.throwAnyException.because("No more tokens");
    }
}

final class NamedValue : AstNode
{
    string  name;
    AstNode value;

    this()
    {
        super(NodeType.NAMED_VALUE);
    }

    static NamedValue fromDefault(AllowValueType AllowedTypes)(ref Lexer lexer)
    {
        auto node = new NamedValue();

        node.name = lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;
        lexer.enforceFrontTypeAndPop([TokenType.OP_COLON]);
        node.value = lexer.nextValue!AllowedTypes();

        return node;
    }
    @("NamedValue.fromDefault")
    unittest
    {
        auto lexer = Lexer(`named: "value"`);
        auto value = NamedValue.fromDefault!(AllowValueType.BASIC)(lexer);
        value.name.should.equal("named");
        value.value.type.should.equal(NodeType.STRING);
        value.value.as!String.value.should.equal("value");
    }
}

final class NamespaceStatement : AstNode
{
    string name;

    this()
    {
        super(NodeType.NAMESPACE);
    }

    static NamespaceStatement fromDefault(ref Lexer lexer)
    {
        auto node = new NamespaceStatement();
        lexer.enforceFrontTypeAndPop([TokenType.KW_NAMESPACE]);
        node.name = lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;
        lexer.enforceFrontTypeAndPop([TokenType.OP_DIVA]);
        return node;
    }
    @("NamespaceStatement.fromDefault")
    unittest
    {
        auto lexer = Lexer("namespace lol;");
        NamespaceStatement.fromDefault(lexer).name.should.equal("lol");
    }
}

final class Attribute : AstNode
{
    string    name;
    AstNode[] values;

    this()
    {
        super(NodeType.ATTRIBUTE);
    }

    static Attribute fromDefault(ref Lexer lexer)
    {
        auto node = new Attribute();

        lexer.enforceFrontTypeAndPop([TokenType.OP_AT]);
        node.name = lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;
        auto nearToken = lexer.enforceFrontTypeAndPop([TokenType.OP_BRACKET_L]);

        // Valueless attribute.
        if(lexer.front.type == TokenType.OP_BRACKET_R)
        {
            lexer.popFront();
            return node;
        }

        while(!lexer.empty)
        {
            node.values ~= lexer.nextValue!(AllowValueType.BASIC | AllowValueType.NAMED | AllowValueType.SYMBOL_FQN)();

            const operator = lexer.enforceFrontTypeAndPop([TokenType.OP_COMMA, TokenType.OP_BRACKET_R]).type;
            if(operator == TokenType.OP_BRACKET_R)
                return node;
        }

        nearToken.onUnexpectedEof("When parsing attribute called "~node.name);
        assert(false);
    }
    @("Attribute.fromDefault")
    unittest
    {
        auto lexer = Lexer(`@NoValue() @SingleValue(20) @MultiValue(nope: yes, -2.0, "Lala", Type.member)`);

        auto attr = Attribute.fromDefault(lexer);
        attr.name.should.equal("NoValue");
        attr.values.length.should.equal(0);
        
        attr = Attribute.fromDefault(lexer);
        attr.name.should.equal("SingleValue");
        attr.values.length.should.equal(1);
        attr.values[0].type.should.equal(NodeType.NUMBER);
        attr.values[0].as!Number.asInteger.should.equal(20);

        attr = Attribute.fromDefault(lexer);
        attr.name.should.equal("MultiValue");
        attr.values.length.should.equal(4);
        attr.values[0].type.should.equal(NodeType.NAMED_VALUE);
        attr.values[0].as!NamedValue.name.should.equal("nope");
        attr.values[0].as!NamedValue.value.as!Boolean.value.should.equal(true);
        attr.values[1].type.should.equal(NodeType.NUMBER);
        attr.values[1].as!Number.asFloat.should.be.lessThan(0);
        attr.values[2].type.should.equal(NodeType.STRING);
        attr.values[2].as!String.value.should.equal("Lala");
        attr.values[3].type.should.equal(NodeType.SYMBOL_FQN);
        attr.values[3].as!SymbolFqn.fqn.should.equal("Type.member");
    }
}

final class MemberType : AstNode
{
    string name;
    bool isRequired;
    bool isArray;

    this()
    {
        super(NodeType.MEMBER_TYPE);
    }

    static MemberType fromDefault(ref Lexer lexer)
    {
        auto node = new MemberType();
        node.name = lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;
        
        if(lexer.front.type == TokenType.OP_SQUARE_BRACKET_L)
        {
            node.isArray = true;
            lexer.popFront();
            lexer.enforceFrontTypeAndPop([TokenType.OP_SQUARE_BRACKET_R]);
        }

        if(lexer.front.type == TokenType.OP_EXCLAMATION)
        {
            node.isRequired = true;
            lexer.popFront();
        }

        return node;
    }
    @("MemberType.fromDefault")
    unittest
    {
        auto lexer = Lexer("int32 string_utf8! bool[]");

        auto type = MemberType.fromDefault(lexer);
        type.name.should.equal("int32");
        type.isRequired.should.equal(false);

        type = MemberType.fromDefault(lexer);
        type.name.should.equal("string_utf8");
        type.isRequired.should.equal(true);

        type = MemberType.fromDefault(lexer);
        type.name.should.equal("bool");
        type.isArray.should.equal(true);
    }
}

enum ObjectTypeClass
{
    NONE,
    VALUE,
    REFERENCE,
    ENUM,
    MULTI
}

final class ObjectType : AstNode
{
    string          name;
    ObjectTypeClass class_;
    MemberType      inheritType;

    this()
    {
        super(NodeType.OBJECT_TYPE);
    }

    static ObjectType fromDefault(ref Lexer lexer)
    {
        return ObjectType.from(lexer, false);
    }

    static ObjectType fromNameless(ref Lexer lexer)
    {
        return ObjectType.from(lexer, true);
    }

    private static ObjectType from(ref Lexer lexer, bool nameless)
    {
        auto node = new ObjectType();

        void setClass(ObjectTypeClass class_)
        {
            enforce(
                node.class_ == ObjectTypeClass.NONE, 
                "%s: Incompatible object classes '%s' and '%s'".format(lexer.front.debug_, node.class_, class_)
            );
            node.class_ = class_;
        }

        auto nearToken = lexer.front;

        bool foundType = false;
        scope(success) enforce(foundType, "%s: ObjectType does not contain the keyword 'type'.".format(nearToken.debug_));

        while(!lexer.empty)
        {
            switch(lexer.front.type) with(TokenType) with(ObjectTypeClass)
            {
                case KW_VALUE:     setClass(VALUE); break;
                case KW_REFERENCE: setClass(REFERENCE); break;
                case KW_ENUM:      setClass(ENUM); break;
                case KW_MULTI:     setClass(MULTI); break;
                case KW_TYPE:      foundType = true; break;

                case IDENTIFIER:
                    if(!nameless)
                    {
                        node.name = lexer.front.text;
                        lexer.popFront();

                        if(lexer.front.type == TokenType.OP_COLON)
                        {
                            lexer.popFront();
                            node.inheritType = MemberType.fromDefault(lexer);
                        }

                        return node;
                    }
                    lexer.front.onUnexpectedToken("Unexpected identifier for nameless object type.");
                    assert(false);

                default:
                    if(lexer.front.type.isOperator)
                    {
                        if(!nameless)
                            lexer.front.onUnexpectedToken("Unexpected operator when expecting identifier or object class keyword.");

                        return node;
                    }
                    lexer.front.onUnexpectedToken("When reading object type");
                    assert(false);
            }

            lexer.popFront();
        }

        nearToken.onUnexpectedEof("When reading object type.");
        assert(false);
    }
    @("ObjectType.from")
    unittest
    {
        void testType(ObjectType type, string name, ObjectTypeClass class_)
        {
            type.name.should.equal(name);
            type.class_.should.equal(class_);
        }

        // Passing (Named)
        auto lexer = Lexer("type Bacon multi type Apples enum type TheNew : boston");

        testType(ObjectType.fromDefault(lexer), "Bacon", ObjectTypeClass.NONE);
        testType(ObjectType.fromDefault(lexer), "Apples", ObjectTypeClass.MULTI);

        auto type = ObjectType.fromDefault(lexer);
        testType(type, "TheNew", ObjectTypeClass.ENUM);
        type.inheritType.should.not.equal(null);
        type.inheritType.name.should.equal("boston");

        // Passing (Nameless)
        lexer = Lexer("type: multi type:");

        testType(ObjectType.fromNameless(lexer), null, ObjectTypeClass.NONE);
        lexer.popFront();
        testType(ObjectType.fromNameless(lexer), null, ObjectTypeClass.MULTI);

        // Failing (Mixed)
        lexer = Lexer("type {\nmulti value\nvalue Tuna\ntype ABC\ntype");

        ({ObjectType.fromDefault(lexer);}).should.throwAnyException.because("No name");
        ({ObjectType.fromDefault(lexer);}).should.throwAnyException.because("Multiple class types");
        ({ObjectType.fromDefault(lexer);}).should.throwAnyException.because("No 'type' keyword");
        ({ObjectType.fromNameless(lexer);}).should.throwAnyException.because("Nameless type was given a name");
        ({ObjectType.fromNameless(lexer);}).should.throwAnyException.because("EOF");
    }

    @property @safe @nogc nothrow:
    
    bool isNameless() { return this.name is null; }
    bool isValueType() { return (this.class_ & ObjectTypeClass.VALUE) > 0; }
    bool isEnumType() { return (this.class_ & ObjectTypeClass.ENUM) > 0; }
    bool isMultiType() { return (this.class_ & ObjectTypeClass.MULTI) > 0; }
    bool isReferenceType() { return (this.class_ & ObjectTypeClass.REFERENCE) > 0; }
}

final class GlobalAttributesStatement : AstNode
{
    ObjectType forType;
    Attribute[] attributes;

    this()
    {
        super(NodeType.GLOBAL_ATTRIBUTES);
    }

    static GlobalAttributesStatement fromDefault(ref Lexer lexer)
    {
        auto node = new GlobalAttributesStatement();

        node.forType = ObjectType.fromNameless(lexer);
        auto nearToken = lexer.enforceFrontTypeAndPop([TokenType.OP_COLON]);

        while(!lexer.empty)
        {
            if(lexer.front.type == TokenType.OP_DIVA)
            {
                lexer.popFront();
                return node;
            }

            lexer.enforceFrontType([TokenType.OP_AT]);
            node.attributes ~= Attribute.fromDefault(lexer);
        }

        nearToken.onUnexpectedEof("When reading global attributes statement");
        assert(false);
    }
    @("GlobalAttributesStatement.fromDefault")
    unittest
    {
        auto lexer = Lexer(`multi type: @Some() @Attributes(1, "value");`);
        auto node  = GlobalAttributesStatement.fromDefault(lexer);

        node.forType.class_.should.equal(ObjectTypeClass.MULTI);
        node.attributes.length.should.equal(2);
        node.attributes[0].name.should.equal("Some");
        node.attributes[1].name.should.equal("Attributes");
        node.attributes[1].values.length.should.equal(2);
    }
}

final class SymbolFqn : AstNode
{
    string fqn;
    
    bool isRelative() { return fqn.length > 0 && fqn[0] == '.'; }

    this()
    {
        super(NodeType.SYMBOL_FQN);
    }

    static SymbolFqn fromDefault(ref Lexer lexer)
    {
        import std.exception : assumeUnique;

        auto node = new SymbolFqn();
        lexer.enforceFrontType([TokenType.IDENTIFIER, TokenType.OP_DOT]);

        char[] fqn;
        fqn.reserve(64);
        
        if(lexer.front.type == TokenType.OP_DOT)
        {
            auto nearToken = lexer.front;

            lexer.popFront();
            if(lexer.empty)
                nearToken.onUnexpectedEof("Expected identifier after dot.");

            fqn ~= '.';
        }

        fqn ~= lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;

        while(!lexer.empty)
        {
            if(lexer.front.type != TokenType.OP_DOT)
                break;

            lexer.enforceFrontTypeAndPop([TokenType.OP_DOT]);
            
            fqn ~= '.';
            fqn ~= lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER]).text;
        }

        node.fqn = fqn.assumeUnique;
        return node;
    }
    @("SymbolFqn.fromDefault")
    unittest
    {
        auto lexer = Lexer("Tile Tile.x.y.z : .x.y : .x : .");

        SymbolFqn.fromDefault(lexer).fqn.should.equal("Tile");
        SymbolFqn.fromDefault(lexer).fqn.should.equal("Tile.x.y.z");
        lexer.popFront();
        SymbolFqn.fromDefault(lexer).fqn.should.equal(".x.y");
        lexer.popFront();
        SymbolFqn.fromDefault(lexer).isRelative.should.equal(true);
        lexer.popFront();
        ({SymbolFqn.fromDefault(lexer);}).should.throwAnyException.because("No identifier after dot.");
    }
}

final class MemberDeclaration : AstNode
{
    string name;
    MemberType type;
    Attribute[] attributes;
    
    this()
    {
        super(NodeType.MEMBER_DECLARATION);
    }

    static MemberDeclaration fromDefault(ref Lexer lexer)
    {
        auto node = new MemberDeclaration();

        node.name = lexer.enforceFrontTypeAndPop([TokenType.IDENTIFIER], KeywordsAreIdentifiers.yes).text;
        lexer.enforceFrontTypeAndPop([TokenType.OP_COLON]);
        node.type = MemberType.fromDefault(lexer);

        auto nearToken = lexer.front;
        while(!lexer.empty)
        {
            if(lexer.front.type == TokenType.OP_DIVA)
            {
                lexer.popFront();
                return node;
            }
            else if(lexer.front.type == TokenType.OP_AT)
                node.attributes ~= Attribute.fromDefault(lexer);
            else
                lexer.enforceFrontType([TokenType.OP_DIVA, TokenType.OP_AT]); // Since it already has a good error message.
        }

        nearToken.onUnexpectedEof("When parsing member declaration");
        assert(0);
    }
    @("MemberDeclaration.fromDefault")
    unittest
    {
        auto lexer = Lexer("tuna: bacon!; apples: justin @Beaver();");

        auto node = MemberDeclaration.fromDefault(lexer);
        node.name.should.equal("tuna");
        node.type.name.should.equal("bacon");
        node.type.isRequired.should.equal(true);
        node.attributes.length.should.equal(0);

        node = MemberDeclaration.fromDefault(lexer);
        node.name.should.equal("apples");
        node.type.name.should.equal("justin");
        node.attributes.length.should.equal(1);
        node.attributes[0].name.should.equal("Beaver");
    }
}

final class TypeDeclaration : AstNode
{
    ObjectType          type;
    Attribute[]         attributes;
    MemberDeclaration[] members;      // Only for basic types NONE VALUE REFERENCE
    MemberType[]        multiMembers; // Only for MULTI
    NamedValue[]        enumMembers;  // Only for ENUM

    this()
    {
        super(NodeType.TYPE_DECLARATION);
    }

    static TypeDeclaration fromDefault(ref Lexer lexer)
    {
        auto node = new TypeDeclaration();

        node.type = ObjectType.fromDefault(lexer);
        lexer.enforceFrontTypeAndPop([TokenType.OP_CURLY_BRACKET_L]);

        final switch(node.type.class_) with(ObjectTypeClass)
        {
            case NONE:
            case VALUE:
            case REFERENCE:
                return TypeDeclaration.parseBasicType(node, lexer);

            case MULTI:
                return TypeDeclaration.parseMultiType(node, lexer);

            case ENUM:
                return TypeDeclaration.parseEnumType(node, lexer);
        }
    }
    ///
    @("TypeDeclaration.fromDefault")
    unittest
    {
        auto lexer = Lexer("value type Apples { @IsBacon(really: yes, who: .tuna) tuna: bacon!; boobs: fantasy @LolImAVirgin(); }");

        auto node = TypeDeclaration.fromDefault(lexer);
        node.type.name.should.equal("Apples");
        node.type.isValueType.should.equal(true);
        node.attributes.length.should.equal(1);
        node.attributes[0].name.should.equal("IsBacon");
        node.members.length.should.equal(2);
        node.members[0].name.should.equal("tuna");
        node.members[0].type.name.should.equal("bacon");
        node.members[1].name.should.equal("boobs");
        node.members[1].type.name.should.equal("fantasy");
        node.members[1].attributes.length.should.equal(1);
        node.members[1].attributes[0].name.should.equal("LolImAVirgin");

        lexer = Lexer("multi type MyTypes { string; int32; bool; }");
        node = TypeDeclaration.fromDefault(lexer);
        node.multiMembers.length.should.equal(3);
        node.multiMembers[0].name.should.equal("string");
        node.multiMembers[1].name.should.equal("int32");
        node.multiMembers[2].name.should.equal("bool");

        lexer = Lexer(`enum type Names : string { me: "Bradley"; you: "Unfortunate Coder"; }`);
        node = TypeDeclaration.fromDefault(lexer);
        node.enumMembers.length.should.equal(2);
        node.enumMembers[0].name.should.equal("me");
        node.enumMembers[0].value.as!String.value.should.equal("Bradley");
    }

    private static TypeDeclaration parseBasicType(TypeDeclaration node, ref Lexer lexer)
    {
        Token nearToken;
        while(!lexer.empty)
        {
            nearToken = lexer.front;
            lexer.enforceFrontType([TokenType.OP_AT, TokenType.IDENTIFIER, TokenType.OP_CURLY_BRACKET_R], KeywordsAreIdentifiers.yes);

            if(lexer.front.type == TokenType.OP_AT)
                node.attributes ~= Attribute.fromDefault(lexer);
            else if(lexer.front.type == TokenType.IDENTIFIER || lexer.front.type.isKeyword)
                node.members ~= MemberDeclaration.fromDefault(lexer);
            else if(lexer.front.type == TokenType.OP_CURLY_BRACKET_R)
            {
                lexer.popFront();
                return node;
            }
            else
                assert(false);
        }

        nearToken.onUnexpectedEof("When parsing type declaration");
        assert(false);
    }

    private static TypeDeclaration parseMultiType(TypeDeclaration node, ref Lexer lexer)
    {
        Token nearToken;
        while(!lexer.empty)
        {
            nearToken = lexer.front;
            lexer.enforceFrontType([TokenType.IDENTIFIER, TokenType.OP_CURLY_BRACKET_R]);
            
            if(lexer.front.type == TokenType.IDENTIFIER)
            {
                node.multiMembers ~= MemberType.fromDefault(lexer);
                lexer.enforceFrontTypeAndPop([TokenType.OP_DIVA]);
            }
            else if(lexer.front.type == TokenType.OP_CURLY_BRACKET_R)
            {
                lexer.popFront();
                return node;
            }
            else
                assert(false);
        }

        nearToken.onUnexpectedEof("When parsing type declaration");
        assert(false);
    }
    
    private static TypeDeclaration parseEnumType(TypeDeclaration node, ref Lexer lexer)
    {
        Token nearToken;
        while(!lexer.empty)
        {
            nearToken = lexer.front;
            lexer.enforceFrontType([TokenType.IDENTIFIER, TokenType.OP_CURLY_BRACKET_R]);
            
            if(lexer.front.type == TokenType.IDENTIFIER)
            {
                node.enumMembers ~= NamedValue.fromDefault!(AllowValueType.BASIC)(lexer);
                lexer.enforceFrontTypeAndPop([TokenType.OP_DIVA]);
            }
            else if(lexer.front.type == TokenType.OP_CURLY_BRACKET_R)
            {
                lexer.popFront();
                return node;
            }
            else
                assert(false);
        }

        nearToken.onUnexpectedEof("When parsing type declaration");
        assert(false);
    }
}
// END NODES

version(unittest)
{
    // TEST_INPUT is from lexer.d

    @("Can successfully parse the AST of a valid file.")
    unittest
    {
        auto file = JasterialiseFile.fromString(TEST_INPUT);
    }
}