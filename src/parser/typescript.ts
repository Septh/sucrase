import { Parser, StopState } from './parser'
import { TokenType } from './types.generated'
import { ContextualKeyword } from './keywords'
import { IdentifierRole } from './token'

const enum FunctionType {
    TSFunctionType,
    TSConstructorType,
    TSAbstractConstructorType,
}

export class ParserTypescript extends Parser {

    private tsIsIdentifier(): boolean {
        // TODO: actually a bit more complex in TypeScript, but shouldn't matter.
        // See https://github.com/Microsoft/TypeScript/issues/15008
        return this.state.match(TokenType.name)
    }

    private isLiteralPropertyName(): boolean {
        return (
            this.state.match(TokenType.name) ||
            Boolean(this.state.type & TokenType.IS_KEYWORD) ||
            this.state.match(TokenType.string) ||
            this.state.match(TokenType.num) ||
            this.state.match(TokenType.bigint) ||
            this.state.match(TokenType.decimal)
        )
    }

    private tsNextTokenCanFollowModifier(): boolean {
        // Note: TypeScript's implementation is much more complicated because
        // more things are considered modifiers there.
        // This implementation only handles modifiers not handled by babylon itself. And "static".
        // TODO: Would be nice to avoid lookahead. Want a hasLineBreakUpNext() method...
        const snapshot = this.state.snapshot()

        this.state.next()
        const canFollowModifier =
            (this.state.match(TokenType.bracketL) ||
                this.state.match(TokenType.braceL) ||
                this.state.match(TokenType.star) ||
                this.state.match(TokenType.ellipsis) ||
                this.state.match(TokenType.hash) ||
                this.isLiteralPropertyName()) &&
            !this.state.hasPrecedingLineBreak()

        if (canFollowModifier) {
            return true
        } else {
            this.state.restoreFromSnapshot(snapshot)
            return false
        }
    }

    private tsParseModifiers(allowedModifiers: Array<ContextualKeyword>): void {
        while (true) {
            const modifier = this.tsParseModifier(allowedModifiers)
            if (modifier === null) {
                break
            }
        }
    }

    /** Parses a modifier matching one the given modifier names. */
    private tsParseModifier(
        allowedModifiers: Array<ContextualKeyword>,
    ): ContextualKeyword | null {
        if (!this.state.match(TokenType.name)) {
            return null
        }

        const modifier = this.state.contextualKeyword
        if (allowedModifiers.indexOf(modifier) !== -1 && this.tsNextTokenCanFollowModifier()) {
            switch (modifier) {
                case ContextualKeyword._readonly:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._readonly
                    break
                case ContextualKeyword._abstract:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._abstract
                    break
                case ContextualKeyword._static:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._static
                    break
                case ContextualKeyword._public:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._public
                    break
                case ContextualKeyword._private:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._private
                    break
                case ContextualKeyword._protected:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._protected
                    break
                case ContextualKeyword._override:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._override
                    break
                case ContextualKeyword._declare:
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._declare
                    break
                default:
                    break
            }
            return modifier
        }
        return null
    }

    private tsParseEntityName(): void {
        this.parseIdentifier()
        while (this.state.eat(TokenType.dot)) {
            this.parseIdentifier()
        }
    }

    private tsParseTypeReference(): void {
        this.tsParseEntityName()
        if (!this.state.hasPrecedingLineBreak() && this.state.match(TokenType.lessThan)) {
            this.tsParseTypeArguments()
        }
    }

    private tsParseThisTypePredicate(): void {
        this.state.next()
        this.tsParseTypeAnnotation()
    }

    private tsParseThisTypeNode(): void {
        this.state.next()
    }

    private tsParseTypeQuery(): void {
        this.state.expect(TokenType._typeof)
        if (this.state.match(TokenType._import)) {
            this.tsParseImportType()
        } else {
            this.tsParseEntityName()
        }
        if (!this.state.hasPrecedingLineBreak() && this.state.match(TokenType.lessThan)) {
            this.tsParseTypeArguments()
        }
    }

    private tsParseImportType(): void {
        this.state.expect(TokenType._import)
        this.state.expect(TokenType.parenL)
        this.state.expect(TokenType.string)
        this.state.expect(TokenType.parenR)
        if (this.state.eat(TokenType.dot)) {
            this.tsParseEntityName()
        }
        if (this.state.match(TokenType.lessThan)) {
            this.tsParseTypeArguments()
        }
    }

    private tsParseTypeParameter(): void {
        this.state.eat(TokenType._const)
        const hadIn = this.state.eat(TokenType._in)
        const hadOut = this.state.eatContextual(ContextualKeyword._out)
        this.state.eat(TokenType._const)
        if ((hadIn || hadOut) && !this.state.match(TokenType.name)) {
            // The "in" or "out" keyword must have actually been the type parameter
            // name, so set it as the name.
            this.state.tokens[this.state.tokens.length - 1].type = TokenType.name
        } else {
            this.parseIdentifier()
        }

        if (this.state.eat(TokenType._extends)) {
            this.tsParseType()
        }
        if (this.state.eat(TokenType.eq)) {
            this.tsParseType()
        }
    }

    private tsTryParseTypeParameters(): void {
        if (this.state.match(TokenType.lessThan)) {
            this.tsParseTypeParameters()
        }
    }

    private tsParseTypeParameters(): void {
        const oldIsType = this.state.pushTypeContext(0)
        if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.typeParameterStart)) {
            this.state.next()
        } else {
            this.state.unexpected()
        }

        while (!this.state.eat(TokenType.greaterThan) && !this.state.error) {
            this.tsParseTypeParameter()
            this.state.eat(TokenType.comma)
        }
        this.state.popTypeContext(oldIsType)
    }

    // Note: In TypeScript implementation we must provide `yieldContext` and `awaitContext`,
    // but here it's always false, because this is only used for types.
    private tsFillSignature(returnToken: TokenType): void {
        // Arrow fns *must* have return token (`=>`). Normal functions can omit it.
        const returnTokenRequired = returnToken === TokenType.arrow
        this.tsTryParseTypeParameters()
        this.state.expect(TokenType.parenL)
        // Create a scope even though we're doing type parsing so we don't accidentally
        // treat params as top-level bindings.
        this.state.scopeDepth++
        this.tsParseBindingListForSignature(false /* isBlockScope */)
        this.state.scopeDepth--
        if (returnTokenRequired) {
            this.tsParseTypeOrTypePredicateAnnotation(returnToken)
        } else if (this.state.match(returnToken)) {
            this.tsParseTypeOrTypePredicateAnnotation(returnToken)
        }
    }

    private tsParseBindingListForSignature(isBlockScope: boolean): void {
        this.parseBindingList(TokenType.parenR, isBlockScope)
    }

    private tsParseTypeMemberSemicolon(): void {
        if (!this.state.eat(TokenType.comma)) {
            this.state.semicolon()
        }
    }

    private tsParseSignatureMember(): void {
        this.tsFillSignature(TokenType.colon)
        this.tsParseTypeMemberSemicolon()
    }

    private tsIsUnambiguouslyIndexSignature(): boolean {
        const snapshot = this.state.snapshot()
        this.state.next() // Skip '{'
        const isIndexSignature = this.state.eat(TokenType.name) && this.state.match(TokenType.colon)
        this.state.restoreFromSnapshot(snapshot)
        return isIndexSignature
    }

    private tsTryParseIndexSignature(): boolean {
        if (!(this.state.match(TokenType.bracketL) && this.tsIsUnambiguouslyIndexSignature())) {
            return false
        }

        const oldIsType = this.state.pushTypeContext(0)

        this.state.expect(TokenType.bracketL)
        this.parseIdentifier()
        this.tsParseTypeAnnotation()
        this.state.expect(TokenType.bracketR)

        this.tsTryParseTypeAnnotation()
        this.tsParseTypeMemberSemicolon()

        this.state.popTypeContext(oldIsType)
        return true
    }

    private tsParsePropertyOrMethodSignature(isReadonly: boolean): void {
        this.state.eat(TokenType.question)

        if (!isReadonly && (this.state.match(TokenType.parenL) || this.state.match(TokenType.lessThan))) {
            this.tsFillSignature(TokenType.colon)
            this.tsParseTypeMemberSemicolon()
        } else {
            this.tsTryParseTypeAnnotation()
            this.tsParseTypeMemberSemicolon()
        }
    }

    private tsParseTypeMember(): void {
        if (this.state.match(TokenType.parenL) || this.state.match(TokenType.lessThan)) {
            // call signature
            this.tsParseSignatureMember()
            return
        }
        if (this.state.match(TokenType._new)) {
            this.state.next()
            if (this.state.match(TokenType.parenL) || this.state.match(TokenType.lessThan)) {
                // constructor signature
                this.tsParseSignatureMember()
            } else {
                this.tsParsePropertyOrMethodSignature(false)
            }
            return
        }
        const readonly = !!this.tsParseModifier([ContextualKeyword._readonly])

        const found = this.tsTryParseIndexSignature()
        if (found) {
            return
        }
        if (
            (this.state.isContextual(ContextualKeyword._get) || this.state.isContextual(ContextualKeyword._set)) &&
            this.tsNextTokenCanFollowModifier()
        ) {
            // This is a getter/setter on a type. The tsNextTokenCanFollowModifier
            // already called next() for us, so continue parsing the name.
        }
        this.parsePropertyName(-1 /* Types don't need context IDs. */)
        this.tsParsePropertyOrMethodSignature(readonly)
    }

    private tsParseTypeLiteral(): void {
        this.tsParseObjectTypeMembers()
    }

    private tsParseObjectTypeMembers(): void {
        this.state.expect(TokenType.braceL)
        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            this.tsParseTypeMember()
        }
    }

    private tsLookaheadIsStartOfMappedType(): boolean {
        const snapshot = this.state.snapshot()
        const isStartOfMappedType = this.tsIsStartOfMappedType()
        this.state.restoreFromSnapshot(snapshot)
        return isStartOfMappedType
    }

    private tsIsStartOfMappedType(): boolean {
        this.state.next()
        if (this.state.eat(TokenType.plus) || this.state.eat(TokenType.minus)) {
            return this.state.isContextual(ContextualKeyword._readonly)
        }
        if (this.state.isContextual(ContextualKeyword._readonly)) {
            this.state.next()
        }
        if (!this.state.match(TokenType.bracketL)) {
            return false
        }
        this.state.next()
        if (!this.tsIsIdentifier()) {
            return false
        }
        this.state.next()
        return this.state.match(TokenType._in)
    }

    private tsParseMappedTypeParameter(): void {
        this.parseIdentifier()
        this.state.expect(TokenType._in)
        this.tsParseType()
    }

    private tsParseMappedType(): void {
        this.state.expect(TokenType.braceL)
        if (this.state.match(TokenType.plus) || this.state.match(TokenType.minus)) {
            this.state.next()
            this.state.expectContextual(ContextualKeyword._readonly)
        } else {
            this.state.eatContextual(ContextualKeyword._readonly)
        }
        this.state.expect(TokenType.bracketL)
        this.tsParseMappedTypeParameter()
        if (this.state.eatContextual(ContextualKeyword._as)) {
            this.tsParseType()
        }
        this.state.expect(TokenType.bracketR)
        if (this.state.match(TokenType.plus) || this.state.match(TokenType.minus)) {
            this.state.next()
            this.state.expect(TokenType.question)
        } else {
            this.state.eat(TokenType.question)
        }
        this.tsTryParseType()
        this.state.semicolon()
        this.state.expect(TokenType.braceR)
    }

    private tsParseTupleType(): void {
        this.state.expect(TokenType.bracketL)
        while (!this.state.eat(TokenType.bracketR) && !this.state.error) {
            // Do not validate presence of either none or only labeled elements
            this.tsParseTupleElementType()
            this.state.eat(TokenType.comma)
        }
    }

    private tsParseTupleElementType(): void {
        // parses `...TsType[]`
        if (this.state.eat(TokenType.ellipsis)) {
            this.tsParseType()
        } else {
            // parses `TsType?`
            this.tsParseType()
            this.state.eat(TokenType.question)
        }

        // The type we parsed above was actually a label
        if (this.state.eat(TokenType.colon)) {
            // Labeled tuple types must affix the label with `...` or `?`, so no need to handle those here
            this.tsParseType()
        }
    }

    private tsParseParenthesizedType(): void {
        this.state.expect(TokenType.parenL)
        this.tsParseType()
        this.state.expect(TokenType.parenR)
    }

    private tsParseTemplateLiteralType(): void {
        // Finish `, read quasi
        this.state.nextTemplateToken()
        // Finish quasi, read ${
        this.state.nextTemplateToken()
        while (!this.state.match(TokenType.backQuote) && !this.state.error) {
            this.state.expect(TokenType.dollarBraceL)
            this.tsParseType()
            // Finish }, read quasi
            this.state.nextTemplateToken()
            // Finish quasi, read either ${ or `
            this.state.nextTemplateToken()
        }
        this.state.next()
    }

    private tsParseFunctionOrConstructorType(type: FunctionType): void {
        if (type === FunctionType.TSAbstractConstructorType) {
            this.state.expectContextual(ContextualKeyword._abstract)
        }
        if (type === FunctionType.TSConstructorType || type === FunctionType.TSAbstractConstructorType) {
            this.state.expect(TokenType._new)
        }
        const oldInDisallowConditionalTypesContext = this.state.inDisallowConditionalTypesContext
        this.state.inDisallowConditionalTypesContext = false
        this.tsFillSignature(TokenType.arrow)
        this.state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
    }

    private tsParseNonArrayType(): void {
        switch (this.state.type) {
            case TokenType.name:
                this.tsParseTypeReference()
                return
            case TokenType._void:
            case TokenType._null:
                this.state.next()
                return
            case TokenType.string:
            case TokenType.num:
            case TokenType.bigint:
            case TokenType.decimal:
            case TokenType._true:
            case TokenType._false:
                this.parseLiteral()
                return
            case TokenType.minus:
                this.state.next()
                this.parseLiteral()
                return
            case TokenType._this: {
                this.tsParseThisTypeNode()
                if (this.state.isContextual(ContextualKeyword._is) && !this.state.hasPrecedingLineBreak()) {
                    this.tsParseThisTypePredicate()
                }
                return
            }
            case TokenType._typeof:
                this.tsParseTypeQuery()
                return
            case TokenType._import:
                this.tsParseImportType()
                return
            case TokenType.braceL:
                if (this.tsLookaheadIsStartOfMappedType()) {
                    this.tsParseMappedType()
                } else {
                    this.tsParseTypeLiteral()
                }
                return
            case TokenType.bracketL:
                this.tsParseTupleType()
                return
            case TokenType.parenL:
                this.tsParseParenthesizedType()
                return
            case TokenType.backQuote:
                this.tsParseTemplateLiteralType()
                return
            default:
                if (this.state.type & TokenType.IS_KEYWORD) {
                    this.state.next()
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType.name
                    return
                }
                break
        }

        this.state.unexpected()
    }

    private tsParseArrayTypeOrHigher(): void {
        this.tsParseNonArrayType()
        while (!this.state.hasPrecedingLineBreak() && this.state.eat(TokenType.bracketL)) {
            if (!this.state.eat(TokenType.bracketR)) {
                // If we hit ] immediately, this is an array type, otherwise it's an indexed access type.
                this.tsParseType()
                this.state.expect(TokenType.bracketR)
            }
        }
    }

    private tsParseInferType(): void {
        this.state.expectContextual(ContextualKeyword._infer)
        this.parseIdentifier()
        if (this.state.match(TokenType._extends)) {
            // Infer type constraints introduce an ambiguity about whether the "extends"
            // is a constraint for this infer type or is another conditional type.
            const snapshot = this.state.snapshot()
            this.state.expect(TokenType._extends)
            const oldInDisallowConditionalTypesContext = this.state.inDisallowConditionalTypesContext
            this.state.inDisallowConditionalTypesContext = true
            this.tsParseType()
            this.state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
            if (this.state.error || (!this.state.inDisallowConditionalTypesContext && this.state.match(TokenType.question))) {
                this.state.restoreFromSnapshot(snapshot)
            }
        }
    }

    private tsParseTypeOperatorOrHigher(): void {
        if (
            this.state.isContextual(ContextualKeyword._keyof) ||
            this.state.isContextual(ContextualKeyword._unique) ||
            this.state.isContextual(ContextualKeyword._readonly)
        ) {
            this.state.next()
            this.tsParseTypeOperatorOrHigher()
        } else if (this.state.isContextual(ContextualKeyword._infer)) {
            this.tsParseInferType()
        } else {
            const oldInDisallowConditionalTypesContext = this.state.inDisallowConditionalTypesContext
            this.state.inDisallowConditionalTypesContext = false
            this.tsParseArrayTypeOrHigher()
            this.state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
        }
    }

    private tsParseIntersectionTypeOrHigher(): void {
        this.state.eat(TokenType.bitwiseAND)
        this.tsParseTypeOperatorOrHigher()
        if (this.state.match(TokenType.bitwiseAND)) {
            while (this.state.eat(TokenType.bitwiseAND)) {
                this.tsParseTypeOperatorOrHigher()
            }
        }
    }

    private tsParseUnionTypeOrHigher(): void {
        this.state.eat(TokenType.bitwiseOR)
        this.tsParseIntersectionTypeOrHigher()
        if (this.state.match(TokenType.bitwiseOR)) {
            while (this.state.eat(TokenType.bitwiseOR)) {
                this.tsParseIntersectionTypeOrHigher()
            }
        }
    }

    private tsIsStartOfFunctionType(): boolean {
        if (this.state.match(TokenType.lessThan)) {
            return true
        }
        return this.state.match(TokenType.parenL) && this.tsLookaheadIsUnambiguouslyStartOfFunctionType()
    }

    private tsSkipParameterStart(): boolean {
        if (this.state.match(TokenType.name) || this.state.match(TokenType._this)) {
            this.state.next()
            return true
        }
        // If this is a possible array/object destructure, walk to the matching bracket/brace.
        // The next token after will tell us definitively whether this is a param.
        if (this.state.match(TokenType.braceL) || this.state.match(TokenType.bracketL)) {
            let depth = 1
            this.state.next()
            while (depth > 0 && !this.state.error) {
                if (this.state.match(TokenType.braceL) || this.state.match(TokenType.bracketL)) {
                    depth++
                } else if (this.state.match(TokenType.braceR) || this.state.match(TokenType.bracketR)) {
                    depth--
                }
                this.state.next()
            }
            return true
        }
        return false
    }

    private tsLookaheadIsUnambiguouslyStartOfFunctionType(): boolean {
        const snapshot = this.state.snapshot()
        const isUnambiguouslyStartOfFunctionType = this.tsIsUnambiguouslyStartOfFunctionType()
        this.state.restoreFromSnapshot(snapshot)
        return isUnambiguouslyStartOfFunctionType
    }

    private tsIsUnambiguouslyStartOfFunctionType(): boolean {
        this.state.next()
        if (this.state.match(TokenType.parenR) || this.state.match(TokenType.ellipsis)) {
            // ( )
            // ( ...
            return true
        }
        if (this.tsSkipParameterStart()) {
            if (this.state.match(TokenType.colon) || this.state.match(TokenType.comma) || this.state.match(TokenType.question) || this.state.match(TokenType.eq)) {
                // ( xxx :
                // ( xxx ,
                // ( xxx ?
                // ( xxx =
                return true
            }
            if (this.state.match(TokenType.parenR)) {
                this.state.next()
                if (this.state.match(TokenType.arrow)) {
                    // ( xxx ) =>
                    return true
                }
            }
        }
        return false
    }

    private tsParseTypeOrTypePredicateAnnotation(returnToken: TokenType): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(returnToken)
        const finishedReturn = this.tsParseTypePredicateOrAssertsPrefix()
        if (!finishedReturn) {
            this.tsParseType()
        }
        this.state.popTypeContext(oldIsType)
    }

    private tsTryParseTypeOrTypePredicateAnnotation(): void {
        if (this.state.match(TokenType.colon)) {
            this.tsParseTypeOrTypePredicateAnnotation(TokenType.colon)
        }
    }

    private tsTryParseTypeAnnotation(): void {
        if (this.state.match(TokenType.colon)) {
            this.tsParseTypeAnnotation()
        }
    }

    private tsTryParseType(): void {
        if (this.state.eat(TokenType.colon)) {
            this.tsParseType()
        }
    }

    /**
     * Detect a few special return syntax cases: `x is T`, `asserts x`, `asserts x is T`,
     * `asserts this is T`.
     *
     * Returns true if we parsed the return type, false if there's still a type to be parsed.
     */
    private tsParseTypePredicateOrAssertsPrefix(): boolean {
        const snapshot = this.state.snapshot()
        if (this.state.isContextual(ContextualKeyword._asserts)) {
            // Normally this is `asserts x is T`, but at this point, it might be `asserts is T` (a user-
            // defined type guard on the `asserts` variable) or just a type called `asserts`.
            this.state.next()
            if (this.state.eatContextual(ContextualKeyword._is)) {
                // If we see `asserts is`, then this must be of the form `asserts is T`, since
                // `asserts is is T` isn't valid.
                this.tsParseType()
                return true
            } else if (this.tsIsIdentifier() || this.state.match(TokenType._this)) {
                this.state.next()
                if (this.state.eatContextual(ContextualKeyword._is)) {
                    // If we see `is`, then this is `asserts x is T`. Otherwise, it's `asserts x`.
                    this.tsParseType()
                }
                return true
            } else {
                // Regular type, so bail out and start type parsing from scratch.
                this.state.restoreFromSnapshot(snapshot)
                return false
            }
        } else if (this.tsIsIdentifier() || this.state.match(TokenType._this)) {
            // This is a regular identifier, which may or may not have "is" after it.
            this.state.next()
            if (this.state.isContextual(ContextualKeyword._is) && !this.state.hasPrecedingLineBreak()) {
                this.state.next()
                this.tsParseType()
                return true
            } else {
                // Regular type, so bail out and start type parsing from scratch.
                this.state.restoreFromSnapshot(snapshot)
                return false
            }
        }
        return false
    }

    private tsParseTypeAnnotation(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(TokenType.colon)
        this.tsParseType()
        this.state.popTypeContext(oldIsType)
    }

    private tsParseType(): void {
        this.tsParseNonConditionalType()
        if (this.state.inDisallowConditionalTypesContext || this.state.hasPrecedingLineBreak() || !this.state.eat(TokenType._extends)) {
            return
        }
        // extends type
        const oldInDisallowConditionalTypesContext = this.state.inDisallowConditionalTypesContext
        this.state.inDisallowConditionalTypesContext = true
        this.tsParseNonConditionalType()
        this.state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext

        this.state.expect(TokenType.question)
        // true type
        this.tsParseType()
        this.state.expect(TokenType.colon)
        // false type
        this.tsParseType()
    }

    private isAbstractConstructorSignature(): boolean {
        return this.state.isContextual(ContextualKeyword._abstract) && this.state.lookaheadType() === TokenType._new
    }

    private tsParseNonConditionalType(): void {
        if (this.tsIsStartOfFunctionType()) {
            this.tsParseFunctionOrConstructorType(FunctionType.TSFunctionType)
            return
        }
        if (this.state.match(TokenType._new)) {
            // As in `new () => Date`
            this.tsParseFunctionOrConstructorType(FunctionType.TSConstructorType)
            return
        } else if (this.isAbstractConstructorSignature()) {
            // As in `abstract new () => Date`
            this.tsParseFunctionOrConstructorType(FunctionType.TSAbstractConstructorType)
            return
        }
        this.tsParseUnionTypeOrHigher()
    }

    private tsParseTypeAssertion(): void {
        const oldIsType = this.state.pushTypeContext(1)
        this.tsParseType()
        this.state.expect(TokenType.greaterThan)
        this.state.popTypeContext(oldIsType)
        this.parseMaybeUnary()
    }

    private tsTryParseJSXTypeArgument(): void {
        if (this.state.eat(TokenType.jsxTagStart)) {
            this.state.tokens[this.state.tokens.length - 1].type = TokenType.typeParameterStart
            const oldIsType = this.state.pushTypeContext(1)
            while (!this.state.match(TokenType.greaterThan) && !this.state.error) {
                this.tsParseType()
                this.state.eat(TokenType.comma)
            }
            // Process >, but the one after needs to be parsed JSX-style.
            this.state.nextJSXTagToken()
            this.state.popTypeContext(oldIsType)
        }
    }

    private tsParseHeritageClause(): void {
        while (!this.state.match(TokenType.braceL) && !this.state.error) {
            this.tsParseExpressionWithTypeArguments()
            this.state.eat(TokenType.comma)
        }
    }

    private tsParseExpressionWithTypeArguments(): void {
        // Note: TS uses parseLeftHandSideExpressionOrHigher,
        // then has grammar errors later if it's not an EntityName.
        this.tsParseEntityName()
        if (this.state.match(TokenType.lessThan)) {
            this.tsParseTypeArguments()
        }
    }

    private tsParseInterfaceDeclaration(): void {
        this.parseBindingIdentifier(false)
        this.tsTryParseTypeParameters()
        if (this.state.eat(TokenType._extends)) {
            this.tsParseHeritageClause()
        }
        this.tsParseObjectTypeMembers()
    }

    private tsParseTypeAliasDeclaration(): void {
        this.parseBindingIdentifier(false)
        this.tsTryParseTypeParameters()
        this.state.expect(TokenType.eq)
        this.tsParseType()
        this.state.semicolon()
    }

    private tsParseEnumMember(): void {
        // Computed property names are grammar errors in an enum, so accept just string literal or identifier.
        if (this.state.match(TokenType.string)) {
            this.parseLiteral()
        } else {
            this.parseIdentifier()
        }
        if (this.state.eat(TokenType.eq)) {
            const eqIndex = this.state.tokens.length - 1
            this.parseMaybeAssign()
            this.state.tokens[eqIndex].rhsEndIndex = this.state.tokens.length
        }
    }

    private tsParseEnumDeclaration(): void {
        this.parseBindingIdentifier(false)
        this.state.expect(TokenType.braceL)
        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            this.tsParseEnumMember()
            this.state.eat(TokenType.comma)
        }
    }

    private tsParseModuleBlock(): void {
        this.state.expect(TokenType.braceL)
        this.parseBlockBody(/* end */ TokenType.braceR)
    }

    private tsParseModuleOrNamespaceDeclaration(): void {
        this.parseBindingIdentifier(false)
        if (this.state.eat(TokenType.dot)) {
            this.tsParseModuleOrNamespaceDeclaration()
        } else {
            this.tsParseModuleBlock()
        }
    }

    private tsParseAmbientExternalModuleDeclaration(): void {
        if (this.state.isContextual(ContextualKeyword._global)) {
            this.parseIdentifier()
        } else if (this.state.match(TokenType.string)) {
            this.parseExprAtom()
        } else {
            this.state.unexpected()
        }

        if (this.state.match(TokenType.braceL)) {
            this.tsParseModuleBlock()
        } else {
            this.state.semicolon()
        }
    }

    private tsParseImportEqualsDeclaration(): void {
        this.parseImportedIdentifier()
        this.state.expect(TokenType.eq)
        this.tsParseModuleReference()
        this.state.semicolon()
    }

    private tsIsExternalModuleReference(): boolean {
        return this.state.isContextual(ContextualKeyword._require) && this.state.lookaheadType() === TokenType.parenL
    }

    private tsParseModuleReference(): void {
        if (this.tsIsExternalModuleReference()) {
            this.tsParseExternalModuleReference()
        } else {
            this.tsParseEntityName()
        }
    }

    private tsParseExternalModuleReference(): void {
        this.state.expectContextual(ContextualKeyword._require)
        this.state.expect(TokenType.parenL)
        if (!this.state.match(TokenType.string)) {
            this.state.unexpected()
        }
        this.parseLiteral()
        this.state.expect(TokenType.parenR)
    }

    // Utilities

    // Returns true if a statement matched.
    private tsTryParseDeclare(): boolean {
        if (this.state.isLineTerminator()) {
            return false
        }
        switch (this.state.type) {
            case TokenType._function: {
                const oldIsType = this.state.pushTypeContext(1)
                this.state.next()
                // We don't need to precisely get the start here, since it's only used to mark
                // the as a type if it's bodiless, and it's already a type here.
                const functionStart = this.state.start
                this.parseFunction(functionStart, /* isStatement */ true)
                this.state.popTypeContext(oldIsType)
                return true
            }
            case TokenType._class: {
                const oldIsType = this.state.pushTypeContext(1)
                this.parseClass(/* isStatement */ true, /* optionalId */ false)
                this.state.popTypeContext(oldIsType)
                return true
            }
            case TokenType._const: {
                if (this.state.match(TokenType._const) && this.state.isLookaheadContextual(ContextualKeyword._enum)) {
                    const oldIsType = this.state.pushTypeContext(1)
                    // `const enum = 0;` not allowed because "enum" is a strict mode reserved word.
                    this.state.expect(TokenType._const)
                    this.state.expectContextual(ContextualKeyword._enum)
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._enum
                    this.tsParseEnumDeclaration()
                    this.state.popTypeContext(oldIsType)
                    return true
                }
            }
            // falls through
            case TokenType._var:
            case TokenType._let: {
                const oldIsType = this.state.pushTypeContext(1)
                this.parseVarStatement(this.state.type !== TokenType._var)
                this.state.popTypeContext(oldIsType)
                return true
            }
            case TokenType.name: {
                const oldIsType = this.state.pushTypeContext(1)
                const contextualKeyword = this.state.contextualKeyword
                let matched = false
                if (contextualKeyword === ContextualKeyword._global) {
                    this.tsParseAmbientExternalModuleDeclaration()
                    matched = true
                } else {
                    matched = this.tsParseDeclaration(contextualKeyword, /* isBeforeToken */ true)
                }
                this.state.popTypeContext(oldIsType)
                return matched
            }
            default:
                return false
        }
    }

    // Note: this won't be called unless the keyword is allowed in `shouldParseExportDeclaration`.
    // Returns true if it matched a declaration.
    private tsTryParseExportDeclaration(): boolean {
        return this.tsParseDeclaration(this.state.contextualKeyword, /* isBeforeToken */ true)
    }

    // Returns true if it matched a statement.
    private tsParseExpressionStatement(contextualKeyword: ContextualKeyword): boolean {
        switch (contextualKeyword) {
            case ContextualKeyword._declare: {
                const declareTokenIndex = this.state.tokens.length - 1
                const matched = this.tsTryParseDeclare()
                if (matched) {
                    this.state.tokens[declareTokenIndex].type = TokenType._declare
                    return true
                }
                break
            }
            case ContextualKeyword._global:
                // `global { }` (with no `declare`) may appear inside an ambient module declaration.
                // Would like to use tsParseAmbientExternalModuleDeclaration here, but already ran past "global".
                if (this.state.match(TokenType.braceL)) {
                    this.tsParseModuleBlock()
                    return true
                }
                break

            default:
                return this.tsParseDeclaration(contextualKeyword, /* isBeforeToken */ false)
        }
        return false
    }

    /**
     * Common code for parsing a declaration.
     *
     * isBeforeToken indicates that the current parser state is at the contextual
     * keyword (and that it is not yet emitted) rather than reading the token after
     * it. When isBeforeToken is true, we may be preceded by an `export` token and
     * should include that token in a type context we create, e.g. to handle
     * `export interface` or `export type`. (This is a bit of a hack and should be
     * cleaned up at some point.)
     *
     * Returns true if it matched a declaration.
     */
    private tsParseDeclaration(contextualKeyword: ContextualKeyword, isBeforeToken: boolean): boolean {
        switch (contextualKeyword) {
            case ContextualKeyword._abstract:
                if (this.tsCheckLineTerminator(isBeforeToken) && this.state.match(TokenType._class)) {
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._abstract
                    this.parseClass(/* isStatement */ true, /* optionalId */ false)
                    return true
                }
                break

            case ContextualKeyword._enum:
                if (this.tsCheckLineTerminator(isBeforeToken) && this.state.match(TokenType.name)) {
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType._enum
                    this.tsParseEnumDeclaration()
                    return true
                }
                break

            case ContextualKeyword._interface:
                if (this.tsCheckLineTerminator(isBeforeToken) && this.state.match(TokenType.name)) {
                    // `next` is true in "export" and "declare" contexts, so we want to remove that token
                    // as well.
                    const oldIsType = this.state.pushTypeContext(isBeforeToken ? 2 : 1)
                    this.tsParseInterfaceDeclaration()
                    this.state.popTypeContext(oldIsType)
                    return true
                }
                break

            case ContextualKeyword._module:
                if (this.tsCheckLineTerminator(isBeforeToken)) {
                    if (this.state.match(TokenType.string)) {
                        const oldIsType = this.state.pushTypeContext(isBeforeToken ? 2 : 1)
                        this.tsParseAmbientExternalModuleDeclaration()
                        this.state.popTypeContext(oldIsType)
                        return true
                    } else if (this.state.match(TokenType.name)) {
                        const oldIsType = this.state.pushTypeContext(isBeforeToken ? 2 : 1)
                        this.tsParseModuleOrNamespaceDeclaration()
                        this.state.popTypeContext(oldIsType)
                        return true
                    }
                }
                break

            case ContextualKeyword._namespace:
                if (this.tsCheckLineTerminator(isBeforeToken) && this.state.match(TokenType.name)) {
                    const oldIsType = this.state.pushTypeContext(isBeforeToken ? 2 : 1)
                    this.tsParseModuleOrNamespaceDeclaration()
                    this.state.popTypeContext(oldIsType)
                    return true
                }
                break

            case ContextualKeyword._type:
                if (this.tsCheckLineTerminator(isBeforeToken) && this.state.match(TokenType.name)) {
                    const oldIsType = this.state.pushTypeContext(isBeforeToken ? 2 : 1)
                    this.tsParseTypeAliasDeclaration()
                    this.state.popTypeContext(oldIsType)
                    return true
                }
                break

            default:
                break
        }
        return false
    }

    private tsCheckLineTerminator(isBeforeToken: boolean): boolean {
        if (isBeforeToken) {
            // Babel checks hasFollowingLineBreak here and returns false, but this
            // doesn't actually come up, e.g. `export interface` can never be on its own
            // line in valid code.
            this.state.next()
            return true
        } else {
            return !this.state.isLineTerminator()
        }
    }

    // Returns true if there was a generic async arrow function.
    private tsTryParseGenericAsyncArrowFunction(): boolean {
        const snapshot = this.state.snapshot()

        this.tsParseTypeParameters()
        this.parseFunctionParams()
        this.tsTryParseTypeOrTypePredicateAnnotation()
        this.state.expect(TokenType.arrow)

        if (this.state.error) {
            this.state.restoreFromSnapshot(snapshot)
            return false
        }

        this.parseFunctionBody(true)
        return true
    }

    /**
     * If necessary, hack the tokenizer state so that this bitshift was actually a
     * less-than token, then keep parsing. This should only be used in situations
     * where we restore from snapshot on error (which reverts this change) or
     * where bitshift would be illegal anyway (e.g. in a class "extends" clause).
     *
     * This hack is useful to handle situations like foo<<T>() => void>() where
     * there can legitimately be two open-angle-brackets in a row in TS.
     */
    private tsParseTypeArgumentsWithPossibleBitshift(): void {
        if (this.state.type === TokenType.bitShiftL) {
            this.state.pos -= 1
            this.state.scanner.finishToken(TokenType.lessThan)
        }
        this.tsParseTypeArguments()
    }

    private tsParseTypeArguments(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(TokenType.lessThan)
        while (!this.state.match(TokenType.greaterThan) && !this.state.error) {
            this.tsParseType()
            this.state.eat(TokenType.comma)
        }
        if (!oldIsType) {
            // If the type arguments are present in an expression context, e.g.
            // f<number>(), then the > sign should be tokenized as a non-type token.
            // In particular, f(a < b, c >= d) should parse the >= as a single token,
            // resulting in a syntax error and fallback to the non-type-args
            // interpretation. In the success case, even though the > is tokenized as a
            // non-type token, it still must be marked as a type token so that it is
            // erased.
            this.state.popTypeContext(oldIsType)
            this.state.scanner.rescan_gt()
            this.state.expect(TokenType.greaterThan)
            this.state.tokens[this.state.tokens.length - 1].isType = true
        } else {
            this.state.expect(TokenType.greaterThan)
            this.state.popTypeContext(oldIsType)
        }
    }

    private tsIsDeclarationStart(): boolean {
        if (this.state.match(TokenType.name)) {
            switch (this.state.contextualKeyword) {
                case ContextualKeyword._abstract:
                case ContextualKeyword._declare:
                case ContextualKeyword._enum:
                case ContextualKeyword._interface:
                case ContextualKeyword._module:
                case ContextualKeyword._namespace:
                case ContextualKeyword._type:
                    return true
                default:
                    break
            }
        }

        return false
    }

    // ======================================================
    // #region OVERRIDES
    // ======================================================

    protected override parseFunctionBodyAndFinish(functionStart: number, funcContextId: number): void {
        // For arrow functions, `parseArrow` handles the return type itself.
        if (this.state.match(TokenType.colon)) {
            this.tsParseTypeOrTypePredicateAnnotation(TokenType.colon)
        }

        // The original code checked the node type to make sure this type allows a missing
        // body, but we skip that to avoid sending around the node type. We instead just use the
        // allowExpressionBody boolean to make sure it's not an arrow function.
        if (!this.state.match(TokenType.braceL) && this.state.isLineTerminator()) {
            // Retroactively mark the declaration as a type.
            let i = this.state.tokens.length - 1
            while (
                i >= 0 &&
                (this.state.tokens[i].start >= functionStart ||
                    this.state.tokens[i].type === TokenType._default ||
                    this.state.tokens[i].type === TokenType._export)
            ) {
                this.state.tokens[i].isType = true
                i--
            }
            return
        }

        this.parseFunctionBody(false, funcContextId)
    }

    protected override parseSubscript(
        startTokenIndex: number,
        noCalls: boolean,
        stopState: StopState,
    ): void {
        if (!this.state.hasPrecedingLineBreak() && this.state.eat(TokenType.bang)) {
            this.state.tokens[this.state.tokens.length - 1].type = TokenType.nonNullAssertion
            return
        }

        if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.bitShiftL)) {
            // There are number of things we are going to "maybe" parse, like type arguments on
            // tagged template expressions. If any of them fail, walk it back and continue.
            const snapshot = this.state.snapshot()

            if (!noCalls && this.atPossibleAsync()) {
                // Almost certainly this is a generic async `async <T>() => ...
                // But it might be a call with a type argument `async<T>();`
                const asyncArrowFn = this.tsTryParseGenericAsyncArrowFunction()
                if (asyncArrowFn) {
                    return
                }
            }
            this.tsParseTypeArgumentsWithPossibleBitshift()
            if (!noCalls && this.state.eat(TokenType.parenL)) {
                // With f<T>(), the subscriptStartIndex marker is on the ( token.
                this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex
                this.parseCallExpressionArguments()
            } else if (this.state.match(TokenType.backQuote)) {
                // Tagged template with a type argument.
                this.parseTemplate()
            } else if (
                // The remaining possible case is an instantiation expression, e.g.
                // Array<number> . Check for a few cases that would disqualify it and
                // cause us to bail out.
                // a<b>>c is not (a<b>)>c, but a<(b>>c)
                this.state.type === TokenType.greaterThan ||
                // a<b>c is (a<b)>c
                (this.state.type !== TokenType.parenL &&
                    Boolean(this.state.type & TokenType.IS_EXPRESSION_START) &&
                    !this.state.hasPrecedingLineBreak())
            ) {
                // Bail out. We have something like a<b>c, which is not an expression with
                // type arguments but an (a < b) > c comparison.
                this.state.unexpected()
            }

            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
            } else {
                return
            }
        } else if (!noCalls && this.state.match(TokenType.questionDot) && this.state.lookaheadType() === TokenType.lessThan) {
            // If we see f?.<, then this must be an optional call with a type argument.
            this.state.next()
            this.state.tokens[startTokenIndex].isOptionalChainStart = true
            // With f?.<T>(), the subscriptStartIndex marker is on the ?. token.
            this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex

            this.tsParseTypeArguments()
            this.state.expect(TokenType.parenL)
            this.parseCallExpressionArguments()
        }
        this.baseParseSubscript(startTokenIndex, noCalls, stopState)
    }

    protected override tryParseExport(): boolean {
        if (this.state.eat(TokenType._import)) {
            // One of these cases:
            // export import A = B;
            // export import type A = require("A");
            if (this.state.isContextual(ContextualKeyword._type) && this.state.lookaheadType() !== TokenType.eq) {
                // Eat a `type` token, unless it's actually an identifier name.
                this.state.expectContextual(ContextualKeyword._type)
            }
            this.tsParseImportEqualsDeclaration()
            return true
        } else if (this.state.eat(TokenType.eq)) {
            // `export = x;`
            this.parseExpression()
            this.state.semicolon()
            return true
        } else if (this.state.eatContextual(ContextualKeyword._as)) {
            // `export as namespace A;`
            // See `parseNamespaceExportDeclaration` in TypeScript's own parser
            this.state.expectContextual(ContextualKeyword._namespace)
            this.parseIdentifier()
            this.state.semicolon()
            return true
        } else {
            if (this.state.isContextual(ContextualKeyword._type)) {
                const nextType = this.state.lookaheadType()
                // export type {foo} from 'a';
                // export type * from 'a';'
                // export type * as ns from 'a';'
                if (nextType === TokenType.braceL || nextType === TokenType.star) {
                    this.state.next()
                }
            }
            return false
        }
    }

    /**
     * Parse a TS import specifier, which may be prefixed with "type" and may be of
     * the form `foo as bar`.
     *
     * The number of identifier-like tokens we see happens to be enough to uniquely
     * identify the form, so simply count the number of identifiers rather than
     * matching the words `type` or `as`. This is particularly important because
     * `type` and `as` could each actually be plain identifiers rather than
     * keywords.
     */
    protected override parseImportSpecifier(): void {
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // import {foo}
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
            return
        }
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // import {type foo}
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
            this.state.tokens[this.state.tokens.length - 2].isType = true
            this.state.tokens[this.state.tokens.length - 1].isType = true
            return
        }
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // import {foo as bar}
            this.state.tokens[this.state.tokens.length - 3].identifierRole = IdentifierRole.ImportAccess
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
            return
        }
        this.parseIdentifier()
        // import {type foo as bar}
        this.state.tokens[this.state.tokens.length - 3].identifierRole = IdentifierRole.ImportAccess
        this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
        this.state.tokens[this.state.tokens.length - 4].isType = true
        this.state.tokens[this.state.tokens.length - 3].isType = true
        this.state.tokens[this.state.tokens.length - 2].isType = true
        this.state.tokens[this.state.tokens.length - 1].isType = true
    }

    /**
     * Just like named import specifiers, export specifiers can have from 1 to 4
     * tokens, inclusive, and the number of tokens determines the role of each token.
     */
    protected override parseExportSpecifier(): void {
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // export {foo}
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
            return
        }
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // export {type foo}
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
            this.state.tokens[this.state.tokens.length - 2].isType = true
            this.state.tokens[this.state.tokens.length - 1].isType = true
            return
        }
        this.parseIdentifier()
        if (this.state.match(TokenType.comma) || this.state.match(TokenType.braceR)) {
            // export {foo as bar}
            this.state.tokens[this.state.tokens.length - 3].identifierRole = IdentifierRole.ExportAccess
            return
        }
        this.parseIdentifier()
        // export {type foo as bar}
        this.state.tokens[this.state.tokens.length - 3].identifierRole = IdentifierRole.ExportAccess
        this.state.tokens[this.state.tokens.length - 4].isType = true
        this.state.tokens[this.state.tokens.length - 3].isType = true
        this.state.tokens[this.state.tokens.length - 2].isType = true
        this.state.tokens[this.state.tokens.length - 1].isType = true
    }

    protected override tryParseExportDefaultExpression(): boolean {
        if (this.state.isContextual(ContextualKeyword._abstract) && this.state.lookaheadType() === TokenType._class) {
            this.state.type = TokenType._abstract
            this.state.next() // Skip "abstract"
            this.parseClass(true, true)
            return true
        }
        if (this.state.isContextual(ContextualKeyword._interface)) {
            // Make sure "export default" are considered type tokens so the whole thing is removed.
            const oldIsType = this.state.pushTypeContext(2)
            this.tsParseDeclaration(ContextualKeyword._interface, true)
            this.state.popTypeContext(oldIsType)
            return true
        }
        return false
    }

    private tsTryParseStatementContent(): boolean {
        if (this.state.type === TokenType._const) {
            const ahead = this.state.lookaheadTypeAndKeyword()
            if (ahead.type === TokenType.name && ahead.contextualKeyword === ContextualKeyword._enum) {
                this.state.expect(TokenType._const)
                this.state.expectContextual(ContextualKeyword._enum)
                this.state.tokens[this.state.tokens.length - 1].type = TokenType._enum
                this.tsParseEnumDeclaration()
                return true
            }
        }
        return false
    }

    protected override parseStatementContent(declaration: boolean): void {
        if (this.tsTryParseStatementContent()) {
            return
        }
        super.parseStatementContent(declaration)
    }

    private tsTryParseClassMemberWithIsStatic(isStatic: boolean): boolean {
        const memberStartIndexAfterStatic = this.state.tokens.length
        this.tsParseModifiers([
            ContextualKeyword._abstract,
            ContextualKeyword._readonly,
            ContextualKeyword._declare,
            ContextualKeyword._static,
            ContextualKeyword._override,
        ])

        const modifiersEndIndex = this.state.tokens.length
        const found = this.tsTryParseIndexSignature()
        if (found) {
            // Index signatures are type declarations, so set the modifier tokens as
            // type tokens. Most tokens could be assumed to be type tokens, but `static`
            // is ambiguous unless we set it explicitly here.
            const memberStartIndex = isStatic
                ? memberStartIndexAfterStatic - 1
                : memberStartIndexAfterStatic
            for (let i = memberStartIndex; i < modifiersEndIndex; i++) {
                this.state.tokens[i].isType = true
            }
            return true
        }
        return false
    }

    // Note: The reason we do this in `parseIdentifierStatement` and not `parseStatement`
    // is that e.g. `type()` is valid JS, so we must try parsing that first.
    // If it's really a type, we will parse `type` as the statement, and can correct it here
    // by parsing the rest.
    protected override parseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
        const matched = this.tsParseExpressionStatement(contextualKeyword)
        if (!matched) {
            this.state.semicolon()
        }
    }

    protected override parseExportDeclaration(): void {
        // "export declare" is equivalent to just "export".
        const isDeclare = this.state.eatContextual(ContextualKeyword._declare)
        if (isDeclare) {
            this.state.tokens[this.state.tokens.length - 1].type = TokenType._declare
        }

        let matchedDeclaration = false
        if (this.state.match(TokenType.name)) {
            if (isDeclare) {
                const oldIsType = this.state.pushTypeContext(2)
                matchedDeclaration = this.tsTryParseExportDeclaration()
                this.state.popTypeContext(oldIsType)
            } else {
                matchedDeclaration = this.tsTryParseExportDeclaration()
            }
        }
        if (!matchedDeclaration) {
            if (isDeclare) {
                const oldIsType = this.state.pushTypeContext(2)
                this.parseStatement(true)
                this.state.popTypeContext(oldIsType)
            } else {
                this.parseStatement(true)
            }
        }
    }

    private tsAfterParseClassSuper(hasSuper: boolean): void {
        if (hasSuper && (this.state.match(TokenType.lessThan) || this.state.match(TokenType.bitShiftL))) {
            this.tsParseTypeArgumentsWithPossibleBitshift()
        }
        if (this.state.eatContextual(ContextualKeyword._implements)) {
            this.state.tokens[this.state.tokens.length - 1].type = TokenType._implements
            const oldIsType = this.state.pushTypeContext(1)
            this.tsParseHeritageClause()
            this.state.popTypeContext(oldIsType)
        }
    }

    protected override parseObjPropValue(isPattern: boolean, isBlockScope: boolean, objectContextId: number): void {
        this.tsTryParseTypeParameters()
        this.baseParseObjPropValue(isPattern, isBlockScope, objectContextId)
    }

    private tsStartParseFunctionParams(): void {
        this.tsTryParseTypeParameters()
    }

    protected override parseFunctionParams(allowModifiers: boolean = false, funcContextId: number = 0): void {
        this.tsStartParseFunctionParams()
        super.parseFunctionParams(allowModifiers, funcContextId)
    }

    // `let x: number;`
    private tsAfterParseVarHead(): void {
        const oldIsType = this.state.pushTypeContext(0)
        if (!this.state.hasPrecedingLineBreak()) {
            this.state.eat(TokenType.bang)
        }
        this.tsTryParseTypeAnnotation()
        this.state.popTypeContext(oldIsType)
    }

    protected override parseVarHead(isBlockScope: boolean): void {
        this.parseBindingAtom(isBlockScope)
        this.tsAfterParseVarHead()
    }

    // parse the return type of an async arrow - let foo = (async (): number => {});
    protected override parseAsyncArrowFromCallExpression(startTokenIndex: number): void {
        if (this.state.match(TokenType.colon)) {
            this.tsParseTypeAnnotation()
        }
        this.baseParseAsyncArrowFromCallExpression(startTokenIndex)
    }

    // Returns true if the expression was an arrow function.
    protected override parseMaybeAssign(noIn: boolean = false, isWithinParens: boolean = false): boolean {
        // Note: When the JSX plugin is on, type assertions (`<T> x`) aren't valid syntax.
        if (this.state.isJSXEnabled) {
            return this.tsParseMaybeAssignWithJSX(noIn, isWithinParens)
        } else {
            return this.tsParseMaybeAssignWithoutJSX(noIn, isWithinParens)
        }
    }

    private tsParseMaybeAssignWithJSX(noIn: boolean, isWithinParens: boolean): boolean {
        if (!this.state.match(TokenType.lessThan)) {
            return this.baseParseMaybeAssign(noIn, isWithinParens)
        }

        // Prefer to parse JSX if possible. But may be an arrow fn.
        const snapshot = this.state.snapshot()
        let wasArrow = this.baseParseMaybeAssign(noIn, isWithinParens)
        if (this.state.error) {
            this.state.restoreFromSnapshot(snapshot)
        } else {
            return wasArrow
        }

        // Otherwise, try as type-parameterized arrow function.
        this.state.type = TokenType.typeParameterStart
        // This is similar to TypeScript's `tryParseParenthesizedArrowFunctionExpression`.
        this.tsParseTypeParameters()
        wasArrow = this.baseParseMaybeAssign(noIn, isWithinParens)
        if (!wasArrow) {
            this.state.unexpected()
        }

        return wasArrow
    }

    private tsParseMaybeAssignWithoutJSX(noIn: boolean, isWithinParens: boolean): boolean {
        if (!this.state.match(TokenType.lessThan)) {
            return this.baseParseMaybeAssign(noIn, isWithinParens)
        }

        const snapshot = this.state.snapshot()
        // This is similar to TypeScript's `tryParseParenthesizedArrowFunctionExpression`.
        this.tsParseTypeParameters()
        const wasArrow = this.baseParseMaybeAssign(noIn, isWithinParens)
        if (!wasArrow) {
            this.state.unexpected()
        }
        if (this.state.error) {
            this.state.restoreFromSnapshot(snapshot)
        } else {
            return wasArrow
        }

        // Try parsing a type cast instead of an arrow function.
        // This will start with a type assertion (via parseMaybeUnary).
        // But don't directly call `tsParseTypeAssertion` because we want to handle any binary after it.
        return this.baseParseMaybeAssign(noIn, isWithinParens)
    }

    protected override parseArrow(): boolean {
        if (this.state.match(TokenType.colon)) {
            // This is different from how the TS parser does it.
            // TS uses lookahead. Babylon parses it as a parenthesized expression and converts.
            const snapshot = this.state.snapshot()

            this.tsParseTypeOrTypePredicateAnnotation(TokenType.colon)
            if (this.state.canInsertSemicolon()) this.state.unexpected()
            if (!this.state.match(TokenType.arrow)) this.state.unexpected()

            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
            }
        }
        return this.state.eat(TokenType.arrow)
    }

    protected override parseAssignableListItem(allowModifiers: boolean, isBlockScope: boolean): void {
        if (allowModifiers) {
            this.tsParseModifiers([
                ContextualKeyword._public,
                ContextualKeyword._protected,
                ContextualKeyword._private,
                ContextualKeyword._readonly,
                ContextualKeyword._override,
            ])
        }

        this.parseMaybeDefault(isBlockScope)
        this.parseAssignableListItemTypes()
        this.parseMaybeDefault(isBlockScope, true /* leftAlreadyParsed */)
    }

    // Allow type annotations inside of a parameter list.
    protected override parseAssignableListItemTypes(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.eat(TokenType.question)
        this.tsTryParseTypeAnnotation()
        this.state.popTypeContext(oldIsType)
    }

    protected override parseMaybeDecoratorArguments(): void {
        if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.bitShiftL)) {
            this.tsParseTypeArgumentsWithPossibleBitshift()
        }
        this.baseParseMaybeDecoratorArguments()
    }

    protected override parseExprOp(startTokenIndex: number, minPrec: number, noIn: boolean): void {
        if (
            (TokenType._in & TokenType.PRECEDENCE_MASK) > minPrec &&
            !this.state.hasPrecedingLineBreak() &&
            (this.state.eatContextual(ContextualKeyword._as) || this.state.eatContextual(ContextualKeyword._satisfies))
        ) {
            const oldIsType = this.state.pushTypeContext(1)
            this.tsParseType()
            this.state.popTypeContext(oldIsType)
            this.state.scanner.rescan_gt()
        }
        super.parseExprOp(startTokenIndex, minPrec, noIn)
    }

    protected override parseMaybeUnary(): boolean {
        if (!this.state.isJSXEnabled && this.state.eat(TokenType.lessThan)) {
            this.tsParseTypeAssertion()
            return false
        }
        return super.parseMaybeUnary()
    }

    protected override parseCatchClauseParam(): void {
        this.parseBindingAtom(true /* isBlockScope */)
        this.tsTryParseTypeAnnotation()
    }

    protected override parseClassMember(memberStart: number, classContextId: number): void {
        this.tsParseModifiers([
            ContextualKeyword._declare,
            ContextualKeyword._public,
            ContextualKeyword._protected,
            ContextualKeyword._private,
            ContextualKeyword._override,
        ])
        super.parseClassMember(memberStart, classContextId)
    }

    protected override parseClassMemberWithIsStatic(memberStart: number, isStatic: boolean, classContextId: number): void {
        if (this.tsTryParseClassMemberWithIsStatic(isStatic)) {
            return
        }
        super.parseClassMemberWithIsStatic(memberStart, isStatic, classContextId)
    }

    protected override parseClassMethod(functionStart: number, isConstructor: boolean): void {
        this.tsTryParseTypeParameters()
        super.parseClassMethod(functionStart, isConstructor)
    }

    protected override parsePostMemberNameModifiers(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.eat(TokenType.question)
        this.state.popTypeContext(oldIsType)
    }

    protected override parseClassProperty(): void {
        this.state.eatTypeToken(TokenType.bang)
        this.tsTryParseTypeAnnotation()
        super.parseClassProperty()
    }

    protected override parseClassId(isStatement: boolean, optionalId: boolean = false): void {
        if (
            (!isStatement || optionalId) &&
            this.state.isContextual(ContextualKeyword._implements)
        ) {
            return
        }
        super.parseClassId(isStatement, optionalId)
        this.tsTryParseTypeParameters()
    }

    protected override afterParseClassSuper(hasSuper: boolean): void {
        this.tsAfterParseClassSuper(hasSuper)
    }

    protected override isExportDefaultSpecifier(): boolean {
        if (this.tsIsDeclarationStart()) {
            return false
        }
        return super.isExportDefaultSpecifier()
    }

    protected override shouldParseExportDeclaration(): boolean {
        return this.tsIsDeclarationStart() || super.shouldParseExportDeclaration()
    }

    protected override parseImport(): void {
        if (this.state.match(TokenType.name) && this.state.lookaheadType() === TokenType.eq) {
            this.tsParseImportEqualsDeclaration()
            return
        }
        if (this.state.isContextual(ContextualKeyword._type)) {
            const lookahead = this.state.lookaheadTypeAndKeyword()
            if (lookahead.type === TokenType.name && lookahead.contextualKeyword !== ContextualKeyword._from) {
                // One of these `import type` cases:
                // import type T = require('T');
                // import type A from 'A';
                this.state.expectContextual(ContextualKeyword._type)
                if (this.state.lookaheadType() === TokenType.eq) {
                    this.tsParseImportEqualsDeclaration()
                    return
                }
                // If this is an `import type...from` statement, then we already ate the
                // type token, so proceed to the regular import parser.
            } else if (lookahead.type === TokenType.star || lookahead.type === TokenType.braceL) {
                // One of these `import type` cases, in which case we can eat the type token
                // and proceed as normal:
                // import type * as A from 'A';
                // import type {a} from 'A';
                this.state.expectContextual(ContextualKeyword._type)
            }
            // Otherwise, we are importing the name "type".
        }
        super.parseImport()
    }

    // An apparent conditional expression could actually be an optional parameter in an arrow function.
    protected override parseConditional(noIn: boolean): void {
        // If we see ?:, this can't possibly be a valid conditional. typedParseParenItem will be called
        // later to finish off the arrow parameter. We also need to handle bare ? tokens for optional
        // parameters without type annotations, i.e. ?, and ?) .
        if (this.state.match(TokenType.question)) {
            const nextType = this.state.lookaheadType()
            if (nextType === TokenType.colon || nextType === TokenType.comma || nextType === TokenType.parenR) {
                return
            }
        }
        this.baseParseConditional(noIn)
    }

    protected override parseParenItem(): void {
        this.state.eatTypeToken(TokenType.question)
        if (this.state.match(TokenType.colon)) {
            this.tsParseTypeAnnotation()
        }
    }

    // Parses element name in any form - namespaced, member
    // or single identifier.
    protected override jsxParseElementName(): void {
        super.jsxParseElementName()
        this.tsTryParseJSXTypeArgument()
    }
}
