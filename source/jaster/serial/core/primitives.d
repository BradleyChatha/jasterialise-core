module jaster.serial.core.primitives;

import std.typecons : Flag;

alias IsUnsigned = Flag!"isUnsigned";
alias IsFloating = Flag!"isFloating";

private PrimitiveInfo[string] _byName;

PrimitiveInfo[string] PRIMITIVE_TYPES_BY_NAME()
{
    if(_byName is null)
    {
        _byName = 
        [
            "string": PrimitiveInfo.isString(StringEncoding.DEFAULT, StringEncoding.DEFAULT),
            "bool": PrimitiveInfo.isBoolean(),

            "int8": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.no, 8),
            "int16": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.no, 16),
            "int32": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.no, 32),
            "int64": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.no, 64),

            "uint8": PrimitiveInfo.isNumber(IsUnsigned.yes, IsFloating.no, 8),
            "uint16": PrimitiveInfo.isNumber(IsUnsigned.yes, IsFloating.no, 16),
            "uint32": PrimitiveInfo.isNumber(IsUnsigned.yes, IsFloating.no, 32),
            "uint64": PrimitiveInfo.isNumber(IsUnsigned.yes, IsFloating.no, 64),
            
            "float32": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.yes, 32),
            "float64": PrimitiveInfo.isNumber(IsUnsigned.no, IsFloating.yes, 64),
        ];
    }

    return _byName;
}

bool isPrimitiveTypeName(string typeName)
{
    import std.algorithm : canFind;
    return PRIMITIVE_TYPES_BY_NAME.byKey.canFind(typeName);
}
unittest
{
    assert(isPrimitiveTypeName("int32"));
    assert(!isPrimitiveTypeName("aboba"));
}

enum PrimitiveType
{
    ERROR,
    STRING,
    NUMBER,
    BOOLEAN
}

enum StringEncoding
{
    DEFAULT // Language and generator specific.
}

struct PrimitiveInfo
{
    PrimitiveType type;

    // STRING
    StringEncoding stringSerialisedEncoding;
    StringEncoding stringNativeEncoding;

    // NUMBER
    bool numberIsFloating;
    bool numberIsUnsigned;
    uint numberBitCount;
    uint numberByteCount() { return this.numberBitCount / 8; }

    static PrimitiveInfo isString(StringEncoding serialisedEncoding, StringEncoding nativeEncoding)
    {
        auto info = PrimitiveInfo(PrimitiveType.STRING);
        info.stringSerialisedEncoding = serialisedEncoding;
        info.stringNativeEncoding = nativeEncoding;

        return info;
    }

    static PrimitiveInfo isBoolean()
    {
        return PrimitiveInfo(PrimitiveType.BOOLEAN);
    }

    static PrimitiveInfo isNumber(
        IsUnsigned isUnsigned, 
        IsFloating isFloating,
        uint bitCount
    )
    {
        auto info = PrimitiveInfo(PrimitiveType.NUMBER);
        info.numberIsFloating = isFloating;
        info.numberIsUnsigned = isUnsigned;
        info.numberBitCount   = bitCount;

        return info;
    }
}