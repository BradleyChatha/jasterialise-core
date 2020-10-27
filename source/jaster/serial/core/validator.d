module jaster.serial.core.validator;

import std.format : format;
import jaster.serial.core;

version(unittest)
{
    import fluent.asserts;
    
    ValidationResult validateFirst(string code)
    {
        auto ast = JasterialiseFileRaw.fromString(code);
        ValidationResult result;

        ast.visitAllTopLevel((node) 
        {
            if(result != ValidationResult.init)
                return;

            result = AstValidator.instance.validate(node);
        });

        return result;
    }
}

alias ValidatorFunc = ValidationResult delegate(AstNode node);

struct ValidationResult
{
    bool success;
    string message;

    static ValidationResult ok()
    {
        return ValidationResult(true);
    }

    static ValidationResult fail(string message)
    {
        return ValidationResult(false, message);
    }
}

final class AstValidator
{
    private
    {
        static AstValidator _instance;
        ValidatorFunc[]     _validators;
    }

    this()
    {
        import std.functional : toDelegate;
        this.add((&enumHasValidBaseType).toDelegate);
        this.add((&enumMembersAreSameAsBaseType).toDelegate);
        this.add((&noBaseTypeRedefine).toDelegate);
    }

    void add(ValidatorFunc func)
    {
        assert(func !is null);
        this._validators ~= func;
    }

    ValidationResult validate(AstNode node)
    {
        assert(node !is null);

        foreach(validator; this._validators)
        {
            auto result = validator(node);
            if(!result.success)
                return result;
        }

        return ValidationResult.ok();
    }

    @property
    static AstValidator instance()
    {
        if(this._instance is null)
            this._instance = new AstValidator();

        return this._instance;
    }
}

private:

ValidationResult enumHasValidBaseType(AstNode node)
{
    if(node.type != NodeType.TYPE_DECLARATION || !node.as!TypeDeclaration.type.isEnumType)
        return ValidationResult.ok();

    auto decl = node.as!TypeDeclaration;
    if(decl.type.inheritType is null)
        return ValidationResult.fail("Enum %s requires an explicit base type.".format(decl.type.name));

    if(!decl.type.inheritType.name.isPrimitiveTypeName)
    {
        return ValidationResult.fail(
            "Enum %s has a non-primitive base type %s. Currently only primitive types are supported on enums."
            .format(decl.type.name, decl.type.inheritType.name)
        );
    }

    return ValidationResult.ok();
}
@("enumMebersMatchBaseType")
unittest
{
    validateFirst("enum type NoBaseType {}").success.should.equal(false);
    validateFirst("enum type BadBaseType : Class {}").success.should.equal(false);
    validateFirst("enum type Good : string {}").success.should.equal(true);
}

ValidationResult noBaseTypeRedefine(AstNode node)
{
    if(node.type != NodeType.TYPE_DECLARATION)
        return ValidationResult.ok();

    const name = node.as!TypeDeclaration.type.name;
    if(name.isPrimitiveTypeName)
        return ValidationResult.fail("Cannot redefine base type '%s'.".format(name));

    return ValidationResult.ok();
}
@("noBaseTypeRedefine")
unittest
{
    validateFirst("value type string {}").success.should.equal(false);
    validateFirst("value type String {}").success.should.equal(true);
}

ValidationResult enumMembersAreSameAsBaseType(AstNode node)
{
    if(node.type != NodeType.TYPE_DECLARATION || !node.as!TypeDeclaration.type.isEnumType)
        return ValidationResult.ok();

    // Since for now, it's safe to assume that enums are only primitive types.
    auto decl = node.as!TypeDeclaration;
    auto type = PRIMITIVE_TYPES_BY_NAME[decl.type.inheritType.name];
    foreach(member; decl.enumMembers)
    {
        bool fail = false;
        final switch(type.type) with(PrimitiveType)
        {
            case NUMBER:
                auto number = member.value.as!Number;
                fail = (
                    (type.numberIsFloating && !number.isFloat)
                 || (!type.numberIsFloating && !number.isInteger)
                );
                break;

            case STRING:
                fail = member.value.type != NodeType.STRING;
                break;

            case BOOLEAN:
                fail = member.value.type != NodeType.BOOLEAN;
                break;

            case ERROR:
                assert(false);
        }

        if(fail)
        {
            return ValidationResult.fail(
                "Enum %s uses base type %s but member %s is of type %s."
                .format(decl.type.name, decl.type.inheritType.name, member.name, member.value.type)
            );
        }
    }

    return ValidationResult.ok();
}
@("enumMembersAreSameAsBaseType")
unittest
{
    validateFirst(`enum type StringWithString : string { a: "b"; }`).success.should.equal(true);
    validateFirst(`enum type BoolWithBool : bool { a: yes; }`).success.should.equal(true);
    validateFirst(`enum type IntWithInt : int32 { a: 20; }`).success.should.equal(true);
    validateFirst(`enum type FloatWithFloat : float32 { a: 20.0; }`).success.should.equal(true);
}