import {
    IdentifierRole,
} from "../token"
import { ContextualKeyword } from "../keywords"
import { TokenType, TokenType as tt } from "../generated/types"
import {
    atPossibleAsync, baseParseMaybeAssign, baseParseMaybeDecoratorArguments, baseParseSubscript, 
    parseBindingIdentifier, parseBindingList, parseBlockBody, parseCallExpressionArguments, parseClass, parseExprAtom,
    parseExpression, parseFunction, parseFunctionBody, parseFunctionParams, parseIdentifier, parseImportedIdentifier, parseLiteral,
    parseMaybeAssign,
    parseMaybeUnary,
    parsePropertyName, parseStatement, parseTemplate, parseVarStatement, type StopState
} from "../traverser"
import { nextJSXTagToken } from "./jsx/index"
import { state } from '../state'

// #region typescript.ts -------------------------------------------------------
function tsIsIdentifier(): boolean {
    // TODO: actually a bit more complex in TypeScript, but shouldn't matter.
    // See https://github.com/Microsoft/TypeScript/issues/15008
    return state.match(tt.name)
}

function isLiteralPropertyName(): boolean {
    return (
        state.match(tt.name) ||
        Boolean(state.type & TokenType.IS_KEYWORD) ||
        state.match(tt.string) ||
        state.match(tt.num) ||
        state.match(tt.bigint) ||
        state.match(tt.decimal)
    )
}

function tsNextTokenCanFollowModifier(): boolean {
    // Note: TypeScript's implementation is much more complicated because
    // more things are considered modifiers there.
    // This implementation only handles modifiers not handled by babylon itself. And "static".
    // TODO: Would be nice to avoid lookahead. Want a hasLineBreakUpNext() method...
    const snapshot = state.snapshot()

    state.next()
    const canFollowModifier =
        (state.match(tt.bracketL) ||
            state.match(tt.braceL) ||
            state.match(tt.star) ||
            state.match(tt.ellipsis) ||
            state.match(tt.hash) ||
            isLiteralPropertyName()) &&
        !state.hasPrecedingLineBreak()

    if (canFollowModifier) {
        return true
    } else {
        state.restoreFromSnapshot(snapshot)
        return false
    }
}

export function tsParseModifiers(allowedModifiers: Array<ContextualKeyword>): void {
    while (true) {
        const modifier = tsParseModifier(allowedModifiers)
        if (modifier === null) {
            break
        }
    }
}

/** Parses a modifier matching one the given modifier names. */
export function tsParseModifier(
    allowedModifiers: Array<ContextualKeyword>,
): ContextualKeyword | null {
    if (!state.match(tt.name)) {
        return null
    }

    const modifier = state.contextualKeyword
    if (allowedModifiers.indexOf(modifier) !== -1 && tsNextTokenCanFollowModifier()) {
        switch (modifier) {
            case ContextualKeyword._readonly:
                state.tokens[state.tokens.length - 1].type = tt._readonly
                break
            case ContextualKeyword._abstract:
                state.tokens[state.tokens.length - 1].type = tt._abstract
                break
            case ContextualKeyword._static:
                state.tokens[state.tokens.length - 1].type = tt._static
                break
            case ContextualKeyword._public:
                state.tokens[state.tokens.length - 1].type = tt._public
                break
            case ContextualKeyword._private:
                state.tokens[state.tokens.length - 1].type = tt._private
                break
            case ContextualKeyword._protected:
                state.tokens[state.tokens.length - 1].type = tt._protected
                break
            case ContextualKeyword._override:
                state.tokens[state.tokens.length - 1].type = tt._override
                break
            case ContextualKeyword._declare:
                state.tokens[state.tokens.length - 1].type = tt._declare
                break
            default:
                break
        }
        return modifier
    }
    return null
}

function tsParseEntityName(): void {
    parseIdentifier()
    while (state.eat(tt.dot)) {
        parseIdentifier()
    }
}

function tsParseTypeReference(): void {
    tsParseEntityName()
    if (!state.hasPrecedingLineBreak() && state.match(tt.lessThan)) {
        tsParseTypeArguments()
    }
}

function tsParseThisTypePredicate(): void {
    state.next()
    tsParseTypeAnnotation()
}

function tsParseThisTypeNode(): void {
    state.next()
}

function tsParseTypeQuery(): void {
    state.expect(tt._typeof)
    if (state.match(tt._import)) {
        tsParseImportType()
    } else {
        tsParseEntityName()
    }
    if (!state.hasPrecedingLineBreak() && state.match(tt.lessThan)) {
        tsParseTypeArguments()
    }
}

function tsParseImportType(): void {
    state.expect(tt._import)
    state.expect(tt.parenL)
    state.expect(tt.string)
    state.expect(tt.parenR)
    if (state.eat(tt.dot)) {
        tsParseEntityName()
    }
    if (state.match(tt.lessThan)) {
        tsParseTypeArguments()
    }
}

function tsParseTypeParameter(): void {
    state.eat(tt._const)
    const hadIn = state.eat(tt._in)
    const hadOut = state.eatContextual(ContextualKeyword._out)
    state.eat(tt._const)
    if ((hadIn || hadOut) && !state.match(tt.name)) {
        // The "in" or "out" keyword must have actually been the type parameter
        // name, so set it as the name.
        state.tokens[state.tokens.length - 1].type = tt.name
    } else {
        parseIdentifier()
    }

    if (state.eat(tt._extends)) {
        tsParseType()
    }
    if (state.eat(tt.eq)) {
        tsParseType()
    }
}

export function tsTryParseTypeParameters(): void {
    if (state.match(tt.lessThan)) {
        tsParseTypeParameters()
    }
}

function tsParseTypeParameters(): void {
    const oldIsType = state.pushTypeContext(0)
    if (state.match(tt.lessThan) || state.match(tt.typeParameterStart)) {
        state.next()
    } else {
        state.unexpected()
    }

    while (!state.eat(tt.greaterThan) && !state.error) {
        tsParseTypeParameter()
        state.eat(tt.comma)
    }
    state.popTypeContext(oldIsType)
}

// Note: In TypeScript implementation we must provide `yieldContext` and `awaitContext`,
// but here it's always false, because this is only used for types.
function tsFillSignature(returnToken: TokenType): void {
    // Arrow fns *must* have return token (`=>`). Normal functions can omit it.
    const returnTokenRequired = returnToken === tt.arrow
    tsTryParseTypeParameters()
    state.expect(tt.parenL)
    // Create a scope even though we're doing type parsing so we don't accidentally
    // treat params as top-level bindings.
    state.scopeDepth++
    tsParseBindingListForSignature(false /* isBlockScope */)
    state.scopeDepth--
    if (returnTokenRequired) {
        tsParseTypeOrTypePredicateAnnotation(returnToken)
    } else if (state.match(returnToken)) {
        tsParseTypeOrTypePredicateAnnotation(returnToken)
    }
}

function tsParseBindingListForSignature(isBlockScope: boolean): void {
    parseBindingList(tt.parenR, isBlockScope)
}

function tsParseTypeMemberSemicolon(): void {
    if (!state.eat(tt.comma)) {
        state.semicolon()
    }
}

function tsParseSignatureMember(): void {
    tsFillSignature(tt.colon)
    tsParseTypeMemberSemicolon()
}

function tsIsUnambiguouslyIndexSignature(): boolean {
    const snapshot = state.snapshot()
    state.next() // Skip '{'
    const isIndexSignature = state.eat(tt.name) && state.match(tt.colon)
    state.restoreFromSnapshot(snapshot)
    return isIndexSignature
}

function tsTryParseIndexSignature(): boolean {
    if (!(state.match(tt.bracketL) && tsIsUnambiguouslyIndexSignature())) {
        return false
    }

    const oldIsType = state.pushTypeContext(0)

    state.expect(tt.bracketL)
    parseIdentifier()
    tsParseTypeAnnotation()
    state.expect(tt.bracketR)

    tsTryParseTypeAnnotation()
    tsParseTypeMemberSemicolon()

    state.popTypeContext(oldIsType)
    return true
}

function tsParsePropertyOrMethodSignature(isReadonly: boolean): void {
    state.eat(tt.question)

    if (!isReadonly && (state.match(tt.parenL) || state.match(tt.lessThan))) {
        tsFillSignature(tt.colon)
        tsParseTypeMemberSemicolon()
    } else {
        tsTryParseTypeAnnotation()
        tsParseTypeMemberSemicolon()
    }
}

function tsParseTypeMember(): void {
    if (state.match(tt.parenL) || state.match(tt.lessThan)) {
        // call signature
        tsParseSignatureMember()
        return
    }
    if (state.match(tt._new)) {
        state.next()
        if (state.match(tt.parenL) || state.match(tt.lessThan)) {
            // constructor signature
            tsParseSignatureMember()
        } else {
            tsParsePropertyOrMethodSignature(false)
        }
        return
    }
    const readonly = !!tsParseModifier([ContextualKeyword._readonly])

    const found = tsTryParseIndexSignature()
    if (found) {
        return
    }
    if (
        (state.isContextual(ContextualKeyword._get) || state.isContextual(ContextualKeyword._set)) &&
        tsNextTokenCanFollowModifier()
    ) {
        // This is a getter/setter on a type. The tsNextTokenCanFollowModifier
        // function already called next() for us, so continue parsing the name.
    }
    parsePropertyName(-1 /* Types don't need context IDs. */)
    tsParsePropertyOrMethodSignature(readonly)
}

function tsParseTypeLiteral(): void {
    tsParseObjectTypeMembers()
}

function tsParseObjectTypeMembers(): void {
    state.expect(tt.braceL)
    while (!state.eat(tt.braceR) && !state.error) {
        tsParseTypeMember()
    }
}

function tsLookaheadIsStartOfMappedType(): boolean {
    const snapshot = state.snapshot()
    const isStartOfMappedType = tsIsStartOfMappedType()
    state.restoreFromSnapshot(snapshot)
    return isStartOfMappedType
}

function tsIsStartOfMappedType(): boolean {
    state.next()
    if (state.eat(tt.plus) || state.eat(tt.minus)) {
        return state.isContextual(ContextualKeyword._readonly)
    }
    if (state.isContextual(ContextualKeyword._readonly)) {
        state.next()
    }
    if (!state.match(tt.bracketL)) {
        return false
    }
    state.next()
    if (!tsIsIdentifier()) {
        return false
    }
    state.next()
    return state.match(tt._in)
}

function tsParseMappedTypeParameter(): void {
    parseIdentifier()
    state.expect(tt._in)
    tsParseType()
}

function tsParseMappedType(): void {
    state.expect(tt.braceL)
    if (state.match(tt.plus) || state.match(tt.minus)) {
        state.next()
        state.expectContextual(ContextualKeyword._readonly)
    } else {
        state.eatContextual(ContextualKeyword._readonly)
    }
    state.expect(tt.bracketL)
    tsParseMappedTypeParameter()
    if (state.eatContextual(ContextualKeyword._as)) {
        tsParseType()
    }
    state.expect(tt.bracketR)
    if (state.match(tt.plus) || state.match(tt.minus)) {
        state.next()
        state.expect(tt.question)
    } else {
        state.eat(tt.question)
    }
    tsTryParseType()
    state.semicolon()
    state.expect(tt.braceR)
}

function tsParseTupleType(): void {
    state.expect(tt.bracketL)
    while (!state.eat(tt.bracketR) && !state.error) {
        // Do not validate presence of either none or only labeled elements
        tsParseTupleElementType()
        state.eat(tt.comma)
    }
}

function tsParseTupleElementType(): void {
    // parses `...TsType[]`
    if (state.eat(tt.ellipsis)) {
        tsParseType()
    } else {
        // parses `TsType?`
        tsParseType()
        state.eat(tt.question)
    }

    // The type we parsed above was actually a label
    if (state.eat(tt.colon)) {
        // Labeled tuple types must affix the label with `...` or `?`, so no need to handle those here
        tsParseType()
    }
}

function tsParseParenthesizedType(): void {
    state.expect(tt.parenL)
    tsParseType()
    state.expect(tt.parenR)
}

function tsParseTemplateLiteralType(): void {
    // Finish `, read quasi
    state.nextTemplateToken()
    // Finish quasi, read ${
    state.nextTemplateToken()
    while (!state.match(tt.backQuote) && !state.error) {
        state.expect(tt.dollarBraceL)
        tsParseType()
        // Finish }, read quasi
        state.nextTemplateToken()
        // Finish quasi, read either ${ or `
        state.nextTemplateToken()
    }
    state.next()
}

enum FunctionType {
    TSFunctionType,
    TSConstructorType,
    TSAbstractConstructorType,
}

function tsParseFunctionOrConstructorType(type: FunctionType): void {
    if (type === FunctionType.TSAbstractConstructorType) {
        state.expectContextual(ContextualKeyword._abstract)
    }
    if (type === FunctionType.TSConstructorType || type === FunctionType.TSAbstractConstructorType) {
        state.expect(tt._new)
    }
    const oldInDisallowConditionalTypesContext = state.inDisallowConditionalTypesContext
    state.inDisallowConditionalTypesContext = false
    tsFillSignature(tt.arrow)
    state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
}

function tsParseNonArrayType(): void {
    switch (state.type) {
        case tt.name:
            tsParseTypeReference()
            return
        case tt._void:
        case tt._null:
            state.next()
            return
        case tt.string:
        case tt.num:
        case tt.bigint:
        case tt.decimal:
        case tt._true:
        case tt._false:
            parseLiteral()
            return
        case tt.minus:
            state.next()
            parseLiteral()
            return
        case tt._this: {
            tsParseThisTypeNode()
            if (state.isContextual(ContextualKeyword._is) && !state.hasPrecedingLineBreak()) {
                tsParseThisTypePredicate()
            }
            return
        }
        case tt._typeof:
            tsParseTypeQuery()
            return
        case tt._import:
            tsParseImportType()
            return
        case tt.braceL:
            if (tsLookaheadIsStartOfMappedType()) {
                tsParseMappedType()
            } else {
                tsParseTypeLiteral()
            }
            return
        case tt.bracketL:
            tsParseTupleType()
            return
        case tt.parenL:
            tsParseParenthesizedType()
            return
        case tt.backQuote:
            tsParseTemplateLiteralType()
            return
        default:
            if (state.type & TokenType.IS_KEYWORD) {
                state.next()
                state.tokens[state.tokens.length - 1].type = tt.name
                return
            }
            break
    }

    state.unexpected()
}

function tsParseArrayTypeOrHigher(): void {
    tsParseNonArrayType()
    while (!state.hasPrecedingLineBreak() && state.eat(tt.bracketL)) {
        if (!state.eat(tt.bracketR)) {
            // If we hit ] immediately, this is an array type, otherwise it's an indexed access type.
            tsParseType()
            state.expect(tt.bracketR)
        }
    }
}

function tsParseInferType(): void {
    state.expectContextual(ContextualKeyword._infer)
    parseIdentifier()
    if (state.match(tt._extends)) {
        // Infer type constraints introduce an ambiguity about whether the "extends"
        // is a constraint for this infer type or is another conditional type.
        const snapshot = state.snapshot()
        state.expect(tt._extends)
        const oldInDisallowConditionalTypesContext = state.inDisallowConditionalTypesContext
        state.inDisallowConditionalTypesContext = true
        tsParseType()
        state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
        if (state.error || (!state.inDisallowConditionalTypesContext && state.match(tt.question))) {
            state.restoreFromSnapshot(snapshot)
        }
    }
}

function tsParseTypeOperatorOrHigher(): void {
    if (
        state.isContextual(ContextualKeyword._keyof) ||
        state.isContextual(ContextualKeyword._unique) ||
        state.isContextual(ContextualKeyword._readonly)
    ) {
        state.next()
        tsParseTypeOperatorOrHigher()
    } else if (state.isContextual(ContextualKeyword._infer)) {
        tsParseInferType()
    } else {
        const oldInDisallowConditionalTypesContext = state.inDisallowConditionalTypesContext
        state.inDisallowConditionalTypesContext = false
        tsParseArrayTypeOrHigher()
        state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext
    }
}

function tsParseIntersectionTypeOrHigher(): void {
    state.eat(tt.bitwiseAND)
    tsParseTypeOperatorOrHigher()
    if (state.match(tt.bitwiseAND)) {
        while (state.eat(tt.bitwiseAND)) {
            tsParseTypeOperatorOrHigher()
        }
    }
}

function tsParseUnionTypeOrHigher(): void {
    state.eat(tt.bitwiseOR)
    tsParseIntersectionTypeOrHigher()
    if (state.match(tt.bitwiseOR)) {
        while (state.eat(tt.bitwiseOR)) {
            tsParseIntersectionTypeOrHigher()
        }
    }
}

function tsIsStartOfFunctionType(): boolean {
    if (state.match(tt.lessThan)) {
        return true
    }
    return state.match(tt.parenL) && tsLookaheadIsUnambiguouslyStartOfFunctionType()
}

function tsSkipParameterStart(): boolean {
    if (state.match(tt.name) || state.match(tt._this)) {
        state.next()
        return true
    }
    // If this is a possible array/object destructure, walk to the matching bracket/brace.
    // The next token after will tell us definitively whether this is a function param.
    if (state.match(tt.braceL) || state.match(tt.bracketL)) {
        let depth = 1
        state.next()
        while (depth > 0 && !state.error) {
            if (state.match(tt.braceL) || state.match(tt.bracketL)) {
                depth++
            } else if (state.match(tt.braceR) || state.match(tt.bracketR)) {
                depth--
            }
            state.next()
        }
        return true
    }
    return false
}

function tsLookaheadIsUnambiguouslyStartOfFunctionType(): boolean {
    const snapshot = state.snapshot()
    const isUnambiguouslyStartOfFunctionType = tsIsUnambiguouslyStartOfFunctionType()
    state.restoreFromSnapshot(snapshot)
    return isUnambiguouslyStartOfFunctionType
}

function tsIsUnambiguouslyStartOfFunctionType(): boolean {
    state.next()
    if (state.match(tt.parenR) || state.match(tt.ellipsis)) {
        // ( )
        // ( ...
        return true
    }
    if (tsSkipParameterStart()) {
        if (state.match(tt.colon) || state.match(tt.comma) || state.match(tt.question) || state.match(tt.eq)) {
            // ( xxx :
            // ( xxx ,
            // ( xxx ?
            // ( xxx =
            return true
        }
        if (state.match(tt.parenR)) {
            state.next()
            if (state.match(tt.arrow)) {
                // ( xxx ) =>
                return true
            }
        }
    }
    return false
}

function tsParseTypeOrTypePredicateAnnotation(returnToken: TokenType): void {
    const oldIsType = state.pushTypeContext(0)
    state.expect(returnToken)
    const finishedReturn = tsParseTypePredicateOrAssertsPrefix()
    if (!finishedReturn) {
        tsParseType()
    }
    state.popTypeContext(oldIsType)
}

function tsTryParseTypeOrTypePredicateAnnotation(): void {
    if (state.match(tt.colon)) {
        tsParseTypeOrTypePredicateAnnotation(tt.colon)
    }
}

export function tsTryParseTypeAnnotation(): void {
    if (state.match(tt.colon)) {
        tsParseTypeAnnotation()
    }
}

function tsTryParseType(): void {
    if (state.eat(tt.colon)) {
        tsParseType()
    }
}

/**
 * Detect a few special return syntax cases: `x is T`, `asserts x`, `asserts x is T`,
 * `asserts this is T`.
 *
 * Returns true if we parsed the return type, false if there's still a type to be parsed.
 */
function tsParseTypePredicateOrAssertsPrefix(): boolean {
    const snapshot = state.snapshot()
    if (state.isContextual(ContextualKeyword._asserts)) {
        // Normally this is `asserts x is T`, but at this point, it might be `asserts is T` (a user-
        // defined type guard on the `asserts` variable) or just a type called `asserts`.
        state.next()
        if (state.eatContextual(ContextualKeyword._is)) {
            // If we see `asserts is`, then this must be of the form `asserts is T`, since
            // `asserts is is T` isn't valid.
            tsParseType()
            return true
        } else if (tsIsIdentifier() || state.match(tt._this)) {
            state.next()
            if (state.eatContextual(ContextualKeyword._is)) {
                // If we see `is`, then this is `asserts x is T`. Otherwise, it's `asserts x`.
                tsParseType()
            }
            return true
        } else {
            // Regular type, so bail out and start type parsing from scratch.
            state.restoreFromSnapshot(snapshot)
            return false
        }
    } else if (tsIsIdentifier() || state.match(tt._this)) {
        // This is a regular identifier, which may or may not have "is" after it.
        state.next()
        if (state.isContextual(ContextualKeyword._is) && !state.hasPrecedingLineBreak()) {
            state.next()
            tsParseType()
            return true
        } else {
            // Regular type, so bail out and start type parsing from scratch.
            state.restoreFromSnapshot(snapshot)
            return false
        }
    }
    return false
}

export function tsParseTypeAnnotation(): void {
    const oldIsType = state.pushTypeContext(0)
    state.expect(tt.colon)
    tsParseType()
    state.popTypeContext(oldIsType)
}

export function tsParseType(): void {
    tsParseNonConditionalType()
    if (state.inDisallowConditionalTypesContext || state.hasPrecedingLineBreak() || !state.eat(tt._extends)) {
        return
    }
    // extends type
    const oldInDisallowConditionalTypesContext = state.inDisallowConditionalTypesContext
    state.inDisallowConditionalTypesContext = true
    tsParseNonConditionalType()
    state.inDisallowConditionalTypesContext = oldInDisallowConditionalTypesContext

    state.expect(tt.question)
    // true type
    tsParseType()
    state.expect(tt.colon)
    // false type
    tsParseType()
}

function isAbstractConstructorSignature(): boolean {
    return state.isContextual(ContextualKeyword._abstract) && state.lookaheadType() === tt._new
}

export function tsParseNonConditionalType(): void {
    if (tsIsStartOfFunctionType()) {
        tsParseFunctionOrConstructorType(FunctionType.TSFunctionType)
        return
    }
    if (state.match(tt._new)) {
        // As in `new () => Date`
        tsParseFunctionOrConstructorType(FunctionType.TSConstructorType)
        return
    } else if (isAbstractConstructorSignature()) {
        // As in `abstract new () => Date`
        tsParseFunctionOrConstructorType(FunctionType.TSAbstractConstructorType)
        return
    }
    tsParseUnionTypeOrHigher()
}

export function tsParseTypeAssertion(): void {
    const oldIsType = state.pushTypeContext(1)
    tsParseType()
    state.expect(tt.greaterThan)
    state.popTypeContext(oldIsType)
    parseMaybeUnary()
}

export function tsTryParseJSXTypeArgument(): void {
    if (state.eat(tt.jsxTagStart)) {
        state.tokens[state.tokens.length - 1].type = tt.typeParameterStart
        const oldIsType = state.pushTypeContext(1)
        while (!state.match(tt.greaterThan) && !state.error) {
            tsParseType()
            state.eat(tt.comma)
        }
        // Process >, but the one after needs to be parsed JSX-style.
        nextJSXTagToken()
        state.popTypeContext(oldIsType)
    }
}

function tsParseHeritageClause(): void {
    while (!state.match(tt.braceL) && !state.error) {
        tsParseExpressionWithTypeArguments()
        state.eat(tt.comma)
    }
}

function tsParseExpressionWithTypeArguments(): void {
    // Note: TS uses parseLeftHandSideExpressionOrHigher,
    // then has grammar errors later if it's not an EntityName.
    tsParseEntityName()
    if (state.match(tt.lessThan)) {
        tsParseTypeArguments()
    }
}

function tsParseInterfaceDeclaration(): void {
    parseBindingIdentifier(false)
    tsTryParseTypeParameters()
    if (state.eat(tt._extends)) {
        tsParseHeritageClause()
    }
    tsParseObjectTypeMembers()
}

function tsParseTypeAliasDeclaration(): void {
    parseBindingIdentifier(false)
    tsTryParseTypeParameters()
    state.expect(tt.eq)
    tsParseType()
    state.semicolon()
}

function tsParseEnumMember(): void {
    // Computed property names are grammar errors in an enum, so accept just string literal or identifier.
    if (state.match(tt.string)) {
        parseLiteral()
    } else {
        parseIdentifier()
    }
    if (state.eat(tt.eq)) {
        const eqIndex = state.tokens.length - 1
        parseMaybeAssign()
        state.tokens[eqIndex].rhsEndIndex = state.tokens.length
    }
}

function tsParseEnumDeclaration(): void {
    parseBindingIdentifier(false)
    state.expect(tt.braceL)
    while (!state.eat(tt.braceR) && !state.error) {
        tsParseEnumMember()
        state.eat(tt.comma)
    }
}

function tsParseModuleBlock(): void {
    state.expect(tt.braceL)
    parseBlockBody(/* end */ tt.braceR)
}

function tsParseModuleOrNamespaceDeclaration(): void {
    parseBindingIdentifier(false)
    if (state.eat(tt.dot)) {
        tsParseModuleOrNamespaceDeclaration()
    } else {
        tsParseModuleBlock()
    }
}

function tsParseAmbientExternalModuleDeclaration(): void {
    if (state.isContextual(ContextualKeyword._global)) {
        parseIdentifier()
    } else if (state.match(tt.string)) {
        parseExprAtom()
    } else {
        state.unexpected()
    }

    if (state.match(tt.braceL)) {
        tsParseModuleBlock()
    } else {
        state.semicolon()
    }
}

export function tsParseImportEqualsDeclaration(): void {
    parseImportedIdentifier()
    state.expect(tt.eq)
    tsParseModuleReference()
    state.semicolon()
}

function tsIsExternalModuleReference(): boolean {
    return state.isContextual(ContextualKeyword._require) && state.lookaheadType() === tt.parenL
}

function tsParseModuleReference(): void {
    if (tsIsExternalModuleReference()) {
        tsParseExternalModuleReference()
    } else {
        tsParseEntityName()
    }
}

function tsParseExternalModuleReference(): void {
    state.expectContextual(ContextualKeyword._require)
    state.expect(tt.parenL)
    if (!state.match(tt.string)) {
        state.unexpected()
    }
    parseLiteral()
    state.expect(tt.parenR)
}

// Utilities

// Returns true if a statement matched.
function tsTryParseDeclare(): boolean {
    if (state.isLineTerminator()) {
        return false
    }
    switch (state.type) {
        case tt._function: {
            const oldIsType = state.pushTypeContext(1)
            state.next()
            // We don't need to precisely get the function start here, since it's only used to mark
            // the function as a type if it's bodiless, and it's already a type here.
            const functionStart = state.start
            parseFunction(functionStart, /* isStatement */ true)
            state.popTypeContext(oldIsType)
            return true
        }
        case tt._class: {
            const oldIsType = state.pushTypeContext(1)
            parseClass(/* isStatement */ true, /* optionalId */ false)
            state.popTypeContext(oldIsType)
            return true
        }
        case tt._const: {
            if (state.match(tt._const) && state.isLookaheadContextual(ContextualKeyword._enum)) {
                const oldIsType = state.pushTypeContext(1)
                // `const enum = 0;` not allowed because "enum" is a strict mode reserved word.
                state.expect(tt._const)
                state.expectContextual(ContextualKeyword._enum)
                state.tokens[state.tokens.length - 1].type = tt._enum
                tsParseEnumDeclaration()
                state.popTypeContext(oldIsType)
                return true
            }
        }
        // falls through
        case tt._var:
        case tt._let: {
            const oldIsType = state.pushTypeContext(1)
            parseVarStatement(state.type !== tt._var)
            state.popTypeContext(oldIsType)
            return true
        }
        case tt.name: {
            const oldIsType = state.pushTypeContext(1)
            const contextualKeyword = state.contextualKeyword
            let matched = false
            if (contextualKeyword === ContextualKeyword._global) {
                tsParseAmbientExternalModuleDeclaration()
                matched = true
            } else {
                matched = tsParseDeclaration(contextualKeyword, /* isBeforeToken */ true)
            }
            state.popTypeContext(oldIsType)
            return matched
        }
        default:
            return false
    }
}

// Note: this won't be called unless the keyword is allowed in `shouldParseExportDeclaration`.
// Returns true if it matched a declaration.
function tsTryParseExportDeclaration(): boolean {
    return tsParseDeclaration(state.contextualKeyword, /* isBeforeToken */ true)
}

// Returns true if it matched a statement.
function tsParseExpressionStatement(contextualKeyword: ContextualKeyword): boolean {
    switch (contextualKeyword) {
        case ContextualKeyword._declare: {
            const declareTokenIndex = state.tokens.length - 1
            const matched = tsTryParseDeclare()
            if (matched) {
                state.tokens[declareTokenIndex].type = tt._declare
                return true
            }
            break
        }
        case ContextualKeyword._global:
            // `global { }` (with no `declare`) may appear inside an ambient module declaration.
            // Would like to use tsParseAmbientExternalModuleDeclaration here, but already ran past "global".
            if (state.match(tt.braceL)) {
                tsParseModuleBlock()
                return true
            }
            break

        default:
            return tsParseDeclaration(contextualKeyword, /* isBeforeToken */ false)
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
function tsParseDeclaration(contextualKeyword: ContextualKeyword, isBeforeToken: boolean): boolean {
    switch (contextualKeyword) {
        case ContextualKeyword._abstract:
            if (tsCheckLineTerminator(isBeforeToken) && state.match(tt._class)) {
                state.tokens[state.tokens.length - 1].type = tt._abstract
                parseClass(/* isStatement */ true, /* optionalId */ false)
                return true
            }
            break

        case ContextualKeyword._enum:
            if (tsCheckLineTerminator(isBeforeToken) && state.match(tt.name)) {
                state.tokens[state.tokens.length - 1].type = tt._enum
                tsParseEnumDeclaration()
                return true
            }
            break

        case ContextualKeyword._interface:
            if (tsCheckLineTerminator(isBeforeToken) && state.match(tt.name)) {
                // `next` is true in "export" and "declare" contexts, so we want to remove that token
                // as well.
                const oldIsType = state.pushTypeContext(isBeforeToken ? 2 : 1)
                tsParseInterfaceDeclaration()
                state.popTypeContext(oldIsType)
                return true
            }
            break

        case ContextualKeyword._module:
            if (tsCheckLineTerminator(isBeforeToken)) {
                if (state.match(tt.string)) {
                    const oldIsType = state.pushTypeContext(isBeforeToken ? 2 : 1)
                    tsParseAmbientExternalModuleDeclaration()
                    state.popTypeContext(oldIsType)
                    return true
                } else if (state.match(tt.name)) {
                    const oldIsType = state.pushTypeContext(isBeforeToken ? 2 : 1)
                    tsParseModuleOrNamespaceDeclaration()
                    state.popTypeContext(oldIsType)
                    return true
                }
            }
            break

        case ContextualKeyword._namespace:
            if (tsCheckLineTerminator(isBeforeToken) && state.match(tt.name)) {
                const oldIsType = state.pushTypeContext(isBeforeToken ? 2 : 1)
                tsParseModuleOrNamespaceDeclaration()
                state.popTypeContext(oldIsType)
                return true
            }
            break

        case ContextualKeyword._type:
            if (tsCheckLineTerminator(isBeforeToken) && state.match(tt.name)) {
                const oldIsType = state.pushTypeContext(isBeforeToken ? 2 : 1)
                tsParseTypeAliasDeclaration()
                state.popTypeContext(oldIsType)
                return true
            }
            break

        default:
            break
    }
    return false
}

function tsCheckLineTerminator(isBeforeToken: boolean): boolean {
    if (isBeforeToken) {
        // Babel checks hasFollowingLineBreak here and returns false, but this
        // doesn't actually come up, e.g. `export interface` can never be on its own
        // line in valid code.
        state.next()
        return true
    } else {
        return !state.isLineTerminator()
    }
}

// Returns true if there was a generic async arrow function.
function tsTryParseGenericAsyncArrowFunction(): boolean {
    const snapshot = state.snapshot()

    tsParseTypeParameters()
    parseFunctionParams()
    tsTryParseTypeOrTypePredicateAnnotation()
    state.expect(tt.arrow)

    if (state.error) {
        state.restoreFromSnapshot(snapshot)
        return false
    }

    parseFunctionBody(true)
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
function tsParseTypeArgumentsWithPossibleBitshift(): void {
    if (state.type === tt.bitShiftL) {
        state.pos -= 1
        state.scanner.finishToken(tt.lessThan)
    }
    tsParseTypeArguments()
}

function tsParseTypeArguments(): void {
    const oldIsType = state.pushTypeContext(0)
    state.expect(tt.lessThan)
    while (!state.match(tt.greaterThan) && !state.error) {
        tsParseType()
        state.eat(tt.comma)
    }
    if (!oldIsType) {
        // If the type arguments are present in an expression context, e.g.
        // f<number>(), then the > sign should be tokenized as a non-type token.
        // In particular, f(a < b, c >= d) should parse the >= as a single token,
        // resulting in a syntax error and fallback to the non-type-args
        // interpretation. In the success case, even though the > is tokenized as a
        // non-type token, it still must be marked as a type token so that it is
        // erased.
        state.popTypeContext(oldIsType)
        state.scanner.rescan_gt()
        state.expect(tt.greaterThan)
        state.tokens[state.tokens.length - 1].isType = true
    } else {
        state.expect(tt.greaterThan)
        state.popTypeContext(oldIsType)
    }
}

export function tsIsDeclarationStart(): boolean {
    if (state.match(tt.name)) {
        switch (state.contextualKeyword) {
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
// OVERRIDES
// ======================================================

export function tsParseFunctionBodyAndFinish(functionStart: number, funcContextId: number): void {
    // For arrow functions, `parseArrow` handles the return type itself.
    if (state.match(tt.colon)) {
        tsParseTypeOrTypePredicateAnnotation(tt.colon)
    }

    // The original code checked the node type to make sure this function type allows a missing
    // body, but we skip that to avoid sending around the node type. We instead just use the
    // allowExpressionBody boolean to make sure it's not an arrow function.
    if (!state.match(tt.braceL) && state.isLineTerminator()) {
        // Retroactively mark the function declaration as a type.
        let i = state.tokens.length - 1
        while (
            i >= 0 &&
            (state.tokens[i].start >= functionStart ||
                state.tokens[i].type === tt._default ||
                state.tokens[i].type === tt._export)
        ) {
            state.tokens[i].isType = true
            i--
        }
        return
    }

    parseFunctionBody(false, funcContextId)
}

export function tsParseSubscript(
    startTokenIndex: number,
    noCalls: boolean,
    stopState: StopState,
): void {
    if (!state.hasPrecedingLineBreak() && state.eat(tt.bang)) {
        state.tokens[state.tokens.length - 1].type = tt.nonNullAssertion
        return
    }

    if (state.match(tt.lessThan) || state.match(tt.bitShiftL)) {
        // There are number of things we are going to "maybe" parse, like type arguments on
        // tagged template expressions. If any of them fail, walk it back and continue.
        const snapshot = state.snapshot()

        if (!noCalls && atPossibleAsync()) {
            // Almost certainly this is a generic async function `async <T>() => ...
            // But it might be a call with a type argument `async<T>();`
            const asyncArrowFn = tsTryParseGenericAsyncArrowFunction()
            if (asyncArrowFn) {
                return
            }
        }
        tsParseTypeArgumentsWithPossibleBitshift()
        if (!noCalls && state.eat(tt.parenL)) {
            // With f<T>(), the subscriptStartIndex marker is on the ( token.
            state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex
            parseCallExpressionArguments()
        } else if (state.match(tt.backQuote)) {
            // Tagged template with a type argument.
            parseTemplate()
        } else if (
            // The remaining possible case is an instantiation expression, e.g.
            // Array<number> . Check for a few cases that would disqualify it and
            // cause us to bail out.
            // a<b>>c is not (a<b>)>c, but a<(b>>c)
            state.type === tt.greaterThan ||
            // a<b>c is (a<b)>c
            (state.type !== tt.parenL &&
                Boolean(state.type & TokenType.IS_EXPRESSION_START) &&
                !state.hasPrecedingLineBreak())
        ) {
            // Bail out. We have something like a<b>c, which is not an expression with
            // type arguments but an (a < b) > c comparison.
            state.unexpected()
        }

        if (state.error) {
            state.restoreFromSnapshot(snapshot)
        } else {
            return
        }
    } else if (!noCalls && state.match(tt.questionDot) && state.lookaheadType() === tt.lessThan) {
        // If we see f?.<, then this must be an optional call with a type argument.
        state.next()
        state.tokens[startTokenIndex].isOptionalChainStart = true
        // With f?.<T>(), the subscriptStartIndex marker is on the ?. token.
        state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex

        tsParseTypeArguments()
        state.expect(tt.parenL)
        parseCallExpressionArguments()
    }
    baseParseSubscript(startTokenIndex, noCalls, stopState)
}

export function tsTryParseExport(): boolean {
    if (state.eat(tt._import)) {
        // One of these cases:
        // export import A = B;
        // export import type A = require("A");
        if (state.isContextual(ContextualKeyword._type) && state.lookaheadType() !== tt.eq) {
            // Eat a `type` token, unless it's actually an identifier name.
            state.expectContextual(ContextualKeyword._type)
        }
        tsParseImportEqualsDeclaration()
        return true
    } else if (state.eat(tt.eq)) {
        // `export = x;`
        parseExpression()
        state.semicolon()
        return true
    } else if (state.eatContextual(ContextualKeyword._as)) {
        // `export as namespace A;`
        // See `parseNamespaceExportDeclaration` in TypeScript's own parser
        state.expectContextual(ContextualKeyword._namespace)
        parseIdentifier()
        state.semicolon()
        return true
    } else {
        if (state.isContextual(ContextualKeyword._type)) {
            const nextType = state.lookaheadType()
            // export type {foo} from 'a';
            // export type * from 'a';'
            // export type * as ns from 'a';'
            if (nextType === tt.braceL || nextType === tt.star) {
                state.next()
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
export function tsParseImportSpecifier(): void {
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // import {foo}
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
        return
    }
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // import {type foo}
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
        state.tokens[state.tokens.length - 2].isType = true
        state.tokens[state.tokens.length - 1].isType = true
        return
    }
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // import {foo as bar}
        state.tokens[state.tokens.length - 3].identifierRole = IdentifierRole.ImportAccess
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
        return
    }
    parseIdentifier()
    // import {type foo as bar}
    state.tokens[state.tokens.length - 3].identifierRole = IdentifierRole.ImportAccess
    state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
    state.tokens[state.tokens.length - 4].isType = true
    state.tokens[state.tokens.length - 3].isType = true
    state.tokens[state.tokens.length - 2].isType = true
    state.tokens[state.tokens.length - 1].isType = true
}

/**
 * Just like named import specifiers, export specifiers can have from 1 to 4
 * tokens, inclusive, and the number of tokens determines the role of each token.
 */
export function tsParseExportSpecifier(): void {
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // export {foo}
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
        return
    }
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // export {type foo}
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
        state.tokens[state.tokens.length - 2].isType = true
        state.tokens[state.tokens.length - 1].isType = true
        return
    }
    parseIdentifier()
    if (state.match(tt.comma) || state.match(tt.braceR)) {
        // export {foo as bar}
        state.tokens[state.tokens.length - 3].identifierRole = IdentifierRole.ExportAccess
        return
    }
    parseIdentifier()
    // export {type foo as bar}
    state.tokens[state.tokens.length - 3].identifierRole = IdentifierRole.ExportAccess
    state.tokens[state.tokens.length - 4].isType = true
    state.tokens[state.tokens.length - 3].isType = true
    state.tokens[state.tokens.length - 2].isType = true
    state.tokens[state.tokens.length - 1].isType = true
}

export function tsTryParseExportDefaultExpression(): boolean {
    if (state.isContextual(ContextualKeyword._abstract) && state.lookaheadType() === tt._class) {
        state.type = tt._abstract
        state.next() // Skip "abstract"
        parseClass(true, true)
        return true
    }
    if (state.isContextual(ContextualKeyword._interface)) {
        // Make sure "export default" are considered type tokens so the whole thing is removed.
        const oldIsType = state.pushTypeContext(2)
        tsParseDeclaration(ContextualKeyword._interface, true)
        state.popTypeContext(oldIsType)
        return true
    }
    return false
}

export function tsTryParseStatementContent(): boolean {
    if (state.type === tt._const) {
        const ahead = state.lookaheadTypeAndKeyword()
        if (ahead.type === tt.name && ahead.contextualKeyword === ContextualKeyword._enum) {
            state.expect(tt._const)
            state.expectContextual(ContextualKeyword._enum)
            state.tokens[state.tokens.length - 1].type = tt._enum
            tsParseEnumDeclaration()
            return true
        }
    }
    return false
}

export function tsTryParseClassMemberWithIsStatic(isStatic: boolean): boolean {
    const memberStartIndexAfterStatic = state.tokens.length
    tsParseModifiers([
        ContextualKeyword._abstract,
        ContextualKeyword._readonly,
        ContextualKeyword._declare,
        ContextualKeyword._static,
        ContextualKeyword._override,
    ])

    const modifiersEndIndex = state.tokens.length
    const found = tsTryParseIndexSignature()
    if (found) {
        // Index signatures are type declarations, so set the modifier tokens as
        // type tokens. Most tokens could be assumed to be type tokens, but `static`
        // is ambiguous unless we set it explicitly here.
        const memberStartIndex = isStatic
            ? memberStartIndexAfterStatic - 1
            : memberStartIndexAfterStatic
        for (let i = memberStartIndex; i < modifiersEndIndex; i++) {
            state.tokens[i].isType = true
        }
        return true
    }
    return false
}

// Note: The reason we do this in `parseIdentifierStatement` and not `parseStatement`
// is that e.g. `type()` is valid JS, so we must try parsing that first.
// If it's really a type, we will parse `type` as the statement, and can correct it here
// by parsing the rest.
export function tsParseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
    const matched = tsParseExpressionStatement(contextualKeyword)
    if (!matched) {
        state.semicolon()
    }
}

export function tsParseExportDeclaration(): void {
    // "export declare" is equivalent to just "export".
    const isDeclare = state.eatContextual(ContextualKeyword._declare)
    if (isDeclare) {
        state.tokens[state.tokens.length - 1].type = tt._declare
    }

    let matchedDeclaration = false
    if (state.match(tt.name)) {
        if (isDeclare) {
            const oldIsType = state.pushTypeContext(2)
            matchedDeclaration = tsTryParseExportDeclaration()
            state.popTypeContext(oldIsType)
        } else {
            matchedDeclaration = tsTryParseExportDeclaration()
        }
    }
    if (!matchedDeclaration) {
        if (isDeclare) {
            const oldIsType = state.pushTypeContext(2)
            parseStatement(true)
            state.popTypeContext(oldIsType)
        } else {
            parseStatement(true)
        }
    }
}

export function tsAfterParseClassSuper(hasSuper: boolean): void {
    if (hasSuper && (state.match(tt.lessThan) || state.match(tt.bitShiftL))) {
        tsParseTypeArgumentsWithPossibleBitshift()
    }
    if (state.eatContextual(ContextualKeyword._implements)) {
        state.tokens[state.tokens.length - 1].type = tt._implements
        const oldIsType = state.pushTypeContext(1)
        tsParseHeritageClause()
        state.popTypeContext(oldIsType)
    }
}

export function tsStartParseObjPropValue(): void {
    tsTryParseTypeParameters()
}

export function tsStartParseFunctionParams(): void {
    tsTryParseTypeParameters()
}

// `let x: number;`
export function tsAfterParseVarHead(): void {
    const oldIsType = state.pushTypeContext(0)
    if (!state.hasPrecedingLineBreak()) {
        state.eat(tt.bang)
    }
    tsTryParseTypeAnnotation()
    state.popTypeContext(oldIsType)
}

// parse the return type of an async arrow function - let foo = (async (): number => {});
export function tsStartParseAsyncArrowFromCallExpression(): void {
    if (state.match(tt.colon)) {
        tsParseTypeAnnotation()
    }
}

// Returns true if the expression was an arrow function.
export function tsParseMaybeAssign(noIn: boolean, isWithinParens: boolean): boolean {
    // Note: When the JSX plugin is on, type assertions (`<T> x`) aren't valid syntax.
    if (state.isJSXEnabled) {
        return tsParseMaybeAssignWithJSX(noIn, isWithinParens)
    } else {
        return tsParseMaybeAssignWithoutJSX(noIn, isWithinParens)
    }
}

export function tsParseMaybeAssignWithJSX(noIn: boolean, isWithinParens: boolean): boolean {
    if (!state.match(tt.lessThan)) {
        return baseParseMaybeAssign(noIn, isWithinParens)
    }

    // Prefer to parse JSX if possible. But may be an arrow fn.
    const snapshot = state.snapshot()
    let wasArrow = baseParseMaybeAssign(noIn, isWithinParens)
    if (state.error) {
        state.restoreFromSnapshot(snapshot)
    } else {
        return wasArrow
    }

    // Otherwise, try as type-parameterized arrow function.
    state.type = tt.typeParameterStart
    // This is similar to TypeScript's `tryParseParenthesizedArrowFunctionExpression`.
    tsParseTypeParameters()
    wasArrow = baseParseMaybeAssign(noIn, isWithinParens)
    if (!wasArrow) {
        state.unexpected()
    }

    return wasArrow
}

export function tsParseMaybeAssignWithoutJSX(noIn: boolean, isWithinParens: boolean): boolean {
    if (!state.match(tt.lessThan)) {
        return baseParseMaybeAssign(noIn, isWithinParens)
    }

    const snapshot = state.snapshot()
    // This is similar to TypeScript's `tryParseParenthesizedArrowFunctionExpression`.
    tsParseTypeParameters()
    const wasArrow = baseParseMaybeAssign(noIn, isWithinParens)
    if (!wasArrow) {
        state.unexpected()
    }
    if (state.error) {
        state.restoreFromSnapshot(snapshot)
    } else {
        return wasArrow
    }

    // Try parsing a type cast instead of an arrow function.
    // This will start with a type assertion (via parseMaybeUnary).
    // But don't directly call `tsParseTypeAssertion` because we want to handle any binary after it.
    return baseParseMaybeAssign(noIn, isWithinParens)
}

export function tsParseArrow(): boolean {
    if (state.match(tt.colon)) {
        // This is different from how the TS parser does it.
        // TS uses lookahead. Babylon parses it as a parenthesized expression and converts.
        const snapshot = state.snapshot()

        tsParseTypeOrTypePredicateAnnotation(tt.colon)
        if (state.canInsertSemicolon()) state.unexpected()
        if (!state.match(tt.arrow)) state.unexpected()

        if (state.error) {
            state.restoreFromSnapshot(snapshot)
        }
    }
    return state.eat(tt.arrow)
}

// Allow type annotations inside of a parameter list.
export function tsParseAssignableListItemTypes(): void {
    const oldIsType = state.pushTypeContext(0)
    state.eat(tt.question)
    tsTryParseTypeAnnotation()
    state.popTypeContext(oldIsType)
}

export function tsParseMaybeDecoratorArguments(): void {
    if (state.match(tt.lessThan) || state.match(tt.bitShiftL)) {
        tsParseTypeArgumentsWithPossibleBitshift()
    }
    baseParseMaybeDecoratorArguments()
}
// #endregion
