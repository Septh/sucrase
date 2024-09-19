import { File } from "./index"
import {
    flowAfterParseClassSuper,
    flowAfterParseVarHead,
    flowParseArrow,
    flowParseAssignableListItemTypes,
    flowParseExportDeclaration,
    flowParseExportStar,
    flowParseFunctionBodyAndFinish,
    flowParseIdentifierStatement,
    flowParseImportSpecifier,
    flowParseMaybeAssign,
    flowParseSubscript,
    flowParseSubscripts,
    flowParseTypeAnnotation,
    flowParseTypeParameterDeclaration,
    flowParseVariance,
    flowShouldDisallowExportDefaultSpecifier,
    flowShouldParseExportDeclaration,
    flowShouldParseExportStar,
    flowStartParseAsyncArrowFromCallExpression,
    flowStartParseFunctionParams,
    flowStartParseImportSpecifiers,
    flowStartParseNewArguments,
    flowStartParseObjPropValue,
    flowTryParseExportDefaultExpression,
    flowTryParseStatement,
} from "./plugins/flow"
import { jsxParseElement } from './plugins/jsx/index'
import { typedParseConditional, typedParseParenItem } from './plugins/types'
import {
    tsAfterParseClassSuper,
    tsAfterParseVarHead,
    tsIsDeclarationStart,
    tsParseArrow,
    tsParseAssignableListItemTypes,
    tsParseExportDeclaration,
    tsParseExportSpecifier,
    tsParseFunctionBodyAndFinish,
    tsParseIdentifierStatement,
    tsParseImportEqualsDeclaration,
    tsParseImportSpecifier,
    tsParseMaybeAssign,
    tsParseMaybeDecoratorArguments,
    tsParseModifiers,
    tsParseSubscript,
    tsParseType,
    tsParseTypeAssertion,
    tsStartParseAsyncArrowFromCallExpression,
    tsStartParseFunctionParams,
    tsStartParseObjPropValue,
    tsTryParseClassMemberWithIsStatic,
    tsTryParseExport,
    tsTryParseExportDefaultExpression,
    tsTryParseStatementContent,
    tsTryParseTypeAnnotation,
    tsTryParseTypeParameters,
} from './plugins/typescript'
import State, {
    IdentifierRole, Scope, eat,
    eatTypeToken, finishToken, lookaheadCharCode, lookaheadType,
    lookaheadTypeAndKeyword,
    match,
    next, nextTemplateToken, nextToken, nextTokenStart,
    nextTokenStartSince,
    popTypeContext,
    pushTypeContext, rescan_gt, retokenizeSlashAsRegex, skipLineComment
} from "./tokenizer"
import { ContextualKeyword } from './tokenizer/keywords'
import { TokenType, formatTokenType, TokenType as tt } from "./tokenizer/types"
import { IS_IDENTIFIER_START, charCodes } from "./util"

export let isJSXEnabled: boolean
export let isTypeScriptEnabled: boolean
export let isFlowEnabled: boolean
export let state: State
export let input: string
export let nextContextId: number

// #region util.ts -------------------------------------------------------------
// Tests whether parsed token is a contextual keyword.
export function isContextual(contextualKeyword: ContextualKeyword): boolean {
    return state.contextualKeyword === contextualKeyword
}

export function isLookaheadContextual(contextualKeyword: ContextualKeyword): boolean {
    const l = lookaheadTypeAndKeyword()
    return l.type === tt.name && l.contextualKeyword === contextualKeyword
}

// Consumes contextual keyword if possible.
export function eatContextual(contextualKeyword: ContextualKeyword): boolean {
    return state.contextualKeyword === contextualKeyword && eat(tt.name)
}

// Asserts that following token is given contextual keyword.
export function expectContextual(contextualKeyword: ContextualKeyword): void {
    if (!eatContextual(contextualKeyword)) {
        unexpected()
    }
}

// Test whether a semicolon can be inserted at the current position.
export function canInsertSemicolon(): boolean {
    return match(tt.eof) || match(tt.braceR) || hasPrecedingLineBreak()
}

export function hasPrecedingLineBreak(): boolean {
    const prevToken = state.tokens[state.tokens.length - 1]
    const lastTokEnd = prevToken ? prevToken.end : 0
    for (let i = lastTokEnd; i < state.start; i++) {
        const code = input.charCodeAt(i)
        if (
            code === charCodes.lineFeed ||
            code === charCodes.carriageReturn ||
            code === 0x2028 ||
            code === 0x2029
        ) {
            return true
        }
    }
    return false
}

export function hasFollowingLineBreak(): boolean {
    const nextStart = nextTokenStart()
    for (let i = state.end; i < nextStart; i++) {
        const code = input.charCodeAt(i)
        if (
            code === charCodes.lineFeed ||
            code === charCodes.carriageReturn ||
            code === 0x2028 ||
            code === 0x2029
        ) {
            return true
        }
    }
    return false
}

export function isLineTerminator(): boolean {
    return eat(tt.semi) || canInsertSemicolon()
}

// Consume a semicolon, or, failing that, see if we are allowed to
// pretend that there is a semicolon at this position.
export function semicolon(): void {
    if (!isLineTerminator()) {
        unexpected('Unexpected token, expected ";"')
    }
}

// Expect a token of a given type. If found, consume it, otherwise,
// raise an unexpected token error at given pos.
export function expect(type: TokenType): void {
    const matched = eat(type)
    if (!matched) {
        unexpected(`Unexpected token, expected "${formatTokenType(type)}"`)
    }
}

/**
 * Transition the parser to an error state. All code needs to be written to naturally unwind in this
 * state, which allows us to backtrack without exceptions and without error plumbing everywhere.
 */
export function unexpected(message: string = "Unexpected token", pos: number = state.start): void {
    if (state.error) {
        return
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const err: any = new SyntaxError(message)
    err.pos = pos
    state.error = err
    state.pos = input.length
    finishToken(tt.eof)
}
// #endregion

// #region lval.ts -------------------------------------------------------------
export function parseSpread(): void {
    next()
    parseMaybeAssign(false)
}

export function parseRest(isBlockScope: boolean): void {
    next()
    parseBindingAtom(isBlockScope)
}

export function parseBindingIdentifier(isBlockScope: boolean): void {
    parseIdentifier()
    markPriorBindingIdentifier(isBlockScope)
}

export function parseImportedIdentifier(): void {
    parseIdentifier()
    state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
}

export function markPriorBindingIdentifier(isBlockScope: boolean): void {
    let identifierRole
    if (state.scopeDepth === 0) {
        identifierRole = IdentifierRole.TopLevelDeclaration
    } else if (isBlockScope) {
        identifierRole = IdentifierRole.BlockScopedDeclaration
    } else {
        identifierRole = IdentifierRole.FunctionScopedDeclaration
    }
    state.tokens[state.tokens.length - 1].identifierRole = identifierRole
}

// Parses lvalue (assignable) atom.
export function parseBindingAtom(isBlockScope: boolean): void {
    switch (state.type) {
        case tt._this: {
            // In TypeScript, "this" may be the name of a parameter, so allow it.
            const oldIsType = pushTypeContext(0)
            next()
            popTypeContext(oldIsType)
            return
        }

        case tt._yield:
        case tt.name: {
            state.type = tt.name
            parseBindingIdentifier(isBlockScope)
            return
        }

        case tt.bracketL: {
            next()
            parseBindingList(tt.bracketR, isBlockScope, true /* allowEmpty */)
            return
        }

        case tt.braceL:
            parseObj(true, isBlockScope)
            return

        default:
            unexpected()
    }
}

export function parseBindingList(
    close: TokenType,
    isBlockScope: boolean,
    allowEmpty: boolean = false,
    allowModifiers: boolean = false,
    contextId: number = 0,
): void {
    let first = true

    let hasRemovedComma = false
    const firstItemTokenIndex = state.tokens.length

    while (!eat(close) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            state.tokens[state.tokens.length - 1].contextId = contextId
            // After a "this" type in TypeScript, we need to set the following comma (if any) to also be
            // a type token so that it will be removed.
            if (!hasRemovedComma && state.tokens[firstItemTokenIndex].isType) {
                state.tokens[state.tokens.length - 1].isType = true
                hasRemovedComma = true
            }
        }
        if (allowEmpty && match(tt.comma)) {
            // Empty item; nothing further to parse for this item.
        } else if (eat(close)) {
            break
        } else if (match(tt.ellipsis)) {
            parseRest(isBlockScope)
            parseAssignableListItemTypes()
            // Support rest element trailing commas allowed by TypeScript <2.9.
            eat(TokenType.comma)
            expect(close)
            break
        } else {
            parseAssignableListItem(allowModifiers, isBlockScope)
        }
    }
}

function parseAssignableListItem(allowModifiers: boolean, isBlockScope: boolean): void {
    if (allowModifiers) {
        tsParseModifiers([
            ContextualKeyword._public,
            ContextualKeyword._protected,
            ContextualKeyword._private,
            ContextualKeyword._readonly,
            ContextualKeyword._override,
        ])
    }

    parseMaybeDefault(isBlockScope)
    parseAssignableListItemTypes()
    parseMaybeDefault(isBlockScope, true /* leftAlreadyParsed */)
}

function parseAssignableListItemTypes(): void {
    if (isFlowEnabled) {
        flowParseAssignableListItemTypes()
    } else if (isTypeScriptEnabled) {
        tsParseAssignableListItemTypes()
    }
}

// Parses assignment pattern around given atom if possible.
export function parseMaybeDefault(isBlockScope: boolean, leftAlreadyParsed: boolean = false): void {
    if (!leftAlreadyParsed) {
        parseBindingAtom(isBlockScope)
    }
    if (!eat(tt.eq)) {
        return
    }
    const eqIndex = state.tokens.length - 1
    parseMaybeAssign()
    state.tokens[eqIndex].rhsEndIndex = state.tokens.length
}
// #endregion

// #region expression.ts -------------------------------------------------------
export class StopState {
    stop: boolean
    constructor(stop: boolean) {
        this.stop = stop
    }
}

// ### Expression parsing

// These nest, from the most general expression type at the top to
// 'atomic', nondivisible expression types at the bottom. Most of
// the functions will simply let the function (s) below them parse,
// and, *if* the syntactic construct they handle is present, wrap
// the AST node that the inner parser gave them in another node.
export function parseExpression(noIn: boolean = false): void {
    parseMaybeAssign(noIn)
    if (match(tt.comma)) {
        while (eat(tt.comma)) {
            parseMaybeAssign(noIn)
        }
    }
}

/**
 * noIn is used when parsing a for loop so that we don't interpret a following "in" as the binary
 * operatior.
 * isWithinParens is used to indicate that we're parsing something that might be a comma expression
 * or might be an arrow function or might be a Flow type assertion (which requires explicit parens).
 * In these cases, we should allow : and ?: after the initial "left" part.
 */
export function parseMaybeAssign(noIn: boolean = false, isWithinParens: boolean = false): boolean {
    if (isTypeScriptEnabled) {
        return tsParseMaybeAssign(noIn, isWithinParens)
    } else if (isFlowEnabled) {
        return flowParseMaybeAssign(noIn, isWithinParens)
    } else {
        return baseParseMaybeAssign(noIn, isWithinParens)
    }
}

// Parse an assignment expression. This includes applications of
// operators like `+=`.
// Returns true if the expression was an arrow function.
export function baseParseMaybeAssign(noIn: boolean, isWithinParens: boolean): boolean {
    if (match(tt._yield)) {
        parseYield()
        return false
    }

    if (match(tt.parenL) || match(tt.name) || match(tt._yield)) {
        state.potentialArrowAt = state.start
    }

    const wasArrow = parseMaybeConditional(noIn)
    if (isWithinParens) {
        parseParenItem()
    }
    if (state.type & TokenType.IS_ASSIGN) {
        next()
        parseMaybeAssign(noIn)
        return false
    }
    return wasArrow
}

// Parse a ternary conditional (`?:`) operator.
// Returns true if the expression was an arrow function.
function parseMaybeConditional(noIn: boolean): boolean {
    const wasArrow = parseExprOps(noIn)
    if (wasArrow) {
        return true
    }
    parseConditional(noIn)
    return false
}

function parseConditional(noIn: boolean): void {
    if (isTypeScriptEnabled || isFlowEnabled) {
        typedParseConditional(noIn)
    } else {
        baseParseConditional(noIn)
    }
}

export function baseParseConditional(noIn: boolean): void {
    if (eat(tt.question)) {
        parseMaybeAssign()
        expect(tt.colon)
        parseMaybeAssign(noIn)
    }
}

// Start the precedence parser.
// Returns true if this was an arrow function
function parseExprOps(noIn: boolean): boolean {
    const startTokenIndex = state.tokens.length
    const wasArrow = parseMaybeUnary()
    if (wasArrow) {
        return true
    }
    parseExprOp(startTokenIndex, -1, noIn)
    return false
}

// Parse binary operators with the operator precedence parsing
// algorithm. `left` is the left-hand side of the operator.
// `minPrec` provides context that allows the function to stop and
// defer further parser to one of its callers when it encounters an
// operator that has a lower precedence than the set it is parsing.
function parseExprOp(startTokenIndex: number, minPrec: number, noIn: boolean): void {
    if (
        isTypeScriptEnabled &&
        (tt._in & TokenType.PRECEDENCE_MASK) > minPrec &&
        !hasPrecedingLineBreak() &&
        (eatContextual(ContextualKeyword._as) || eatContextual(ContextualKeyword._satisfies))
    ) {
        const oldIsType = pushTypeContext(1)
        tsParseType()
        popTypeContext(oldIsType)
        rescan_gt()
        parseExprOp(startTokenIndex, minPrec, noIn)
        return
    }

    const prec = state.type & TokenType.PRECEDENCE_MASK
    if (prec > 0 && (!noIn || !match(tt._in))) {
        if (prec > minPrec) {
            const op = state.type
            next()
            if (op === tt.nullishCoalescing) {
                state.tokens[state.tokens.length - 1].nullishStartIndex = startTokenIndex
            }

            const rhsStartTokenIndex = state.tokens.length
            parseMaybeUnary()
            // Extend the right operand of this operator if possible.
            parseExprOp(rhsStartTokenIndex, op & TokenType.IS_RIGHT_ASSOCIATIVE ? prec - 1 : prec, noIn)
            if (op === tt.nullishCoalescing) {
                state.tokens[startTokenIndex].numNullishCoalesceStarts++
                state.tokens[state.tokens.length - 1].numNullishCoalesceEnds++
            }
            // Continue with any future operator holding this expression as the left operand.
            parseExprOp(startTokenIndex, minPrec, noIn)
        }
    }
}

// Parse unary operators, both prefix and postfix.
// Returns true if this was an arrow function.
export function parseMaybeUnary(): boolean {
    if (isTypeScriptEnabled && !isJSXEnabled && eat(tt.lessThan)) {
        tsParseTypeAssertion()
        return false
    }
    if (
        isContextual(ContextualKeyword._module) &&
        lookaheadCharCode() === charCodes.leftCurlyBrace &&
        !hasFollowingLineBreak()
    ) {
        parseModuleExpression()
        return false
    }
    if (state.type & TokenType.IS_PREFIX) {
        next()
        parseMaybeUnary()
        return false
    }

    const wasArrow = parseExprSubscripts()
    if (wasArrow) {
        return true
    }
    while (state.type & TokenType.IS_POSTFIX && !canInsertSemicolon()) {
        // The tokenizer calls everything a preincrement, so make it a postincrement when
        // we see it in that context.
        if (state.type === tt.preIncDec) {
            state.type = tt.postIncDec
        }
        next()
    }
    return false
}

// Parse call, dot, and `[]`-subscript expressions.
// Returns true if this was an arrow function.
export function parseExprSubscripts(): boolean {
    const startTokenIndex = state.tokens.length
    const wasArrow = parseExprAtom()
    if (wasArrow) {
        return true
    }
    parseSubscripts(startTokenIndex)
    // If there was any optional chain operation, the start token would be marked
    // as such, so also mark the end now.
    if (state.tokens.length > startTokenIndex && state.tokens[startTokenIndex].isOptionalChainStart) {
        state.tokens[state.tokens.length - 1].isOptionalChainEnd = true
    }
    return false
}

function parseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
    if (isFlowEnabled) {
        flowParseSubscripts(startTokenIndex, noCalls)
    } else {
        baseParseSubscripts(startTokenIndex, noCalls)
    }
}

export function baseParseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
    const stopState = new StopState(false)
    do {
        parseSubscript(startTokenIndex, noCalls, stopState)
    } while (!stopState.stop && !state.error)
}

function parseSubscript(startTokenIndex: number, noCalls: boolean, stopState: StopState): void {
    if (isTypeScriptEnabled) {
        tsParseSubscript(startTokenIndex, noCalls, stopState)
    } else if (isFlowEnabled) {
        flowParseSubscript(startTokenIndex, noCalls, stopState)
    } else {
        baseParseSubscript(startTokenIndex, noCalls, stopState)
    }
}

/** Set 'state.stop = true' to indicate that we should stop parsing subscripts. */
export function baseParseSubscript(
    startTokenIndex: number,
    noCalls: boolean,
    stopState: StopState,
): void {
    if (!noCalls && eat(tt.doubleColon)) {
        parseNoCallExpr()
        stopState.stop = true
        // Propagate startTokenIndex so that `a::b?.()` will keep `a` as the first token. We may want
        // to revisit this in the future when fully supporting bind syntax.
        parseSubscripts(startTokenIndex, noCalls)
    } else if (match(tt.questionDot)) {
        state.tokens[startTokenIndex].isOptionalChainStart = true
        if (noCalls && lookaheadType() === tt.parenL) {
            stopState.stop = true
            return
        }
        next()
        state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex

        if (eat(tt.bracketL)) {
            parseExpression()
            expect(tt.bracketR)
        } else if (eat(tt.parenL)) {
            parseCallExpressionArguments()
        } else {
            parseMaybePrivateName()
        }
    } else if (eat(tt.dot)) {
        state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex
        parseMaybePrivateName()
    } else if (eat(tt.bracketL)) {
        state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex
        parseExpression()
        expect(tt.bracketR)
    } else if (!noCalls && match(tt.parenL)) {
        if (atPossibleAsync()) {
            // We see "async", but it's possible it's a usage of the name "async". Parse as if it's a
            // function call, and if we see an arrow later, backtrack and re-parse as a parameter list.
            const snapshot = state.snapshot()
            const asyncStartTokenIndex = state.tokens.length
            next()
            state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex

            const callContextId = getNextContextId()

            state.tokens[state.tokens.length - 1].contextId = callContextId
            parseCallExpressionArguments()
            state.tokens[state.tokens.length - 1].contextId = callContextId

            if (shouldParseAsyncArrow()) {
                // We hit an arrow, so backtrack and start again parsing function parameters.
                state.restoreFromSnapshot(snapshot)
                stopState.stop = true
                state.scopeDepth++

                parseFunctionParams()
                parseAsyncArrowFromCallExpression(asyncStartTokenIndex)
            }
        } else {
            next()
            state.tokens[state.tokens.length - 1].subscriptStartIndex = startTokenIndex
            const callContextId = getNextContextId()
            state.tokens[state.tokens.length - 1].contextId = callContextId
            parseCallExpressionArguments()
            state.tokens[state.tokens.length - 1].contextId = callContextId
        }
    } else if (match(tt.backQuote)) {
        // Tagged template expression.
        parseTemplate()
    } else {
        stopState.stop = true
    }
}

export function atPossibleAsync(): boolean {
    // This was made less strict than the original version to avoid passing around nodes, but it
    // should be safe to have rare false positives here.
    return (
        state.tokens[state.tokens.length - 1].contextualKeyword === ContextualKeyword._async &&
        !canInsertSemicolon()
    )
}

export function parseCallExpressionArguments(): void {
    let first = true
    while (!eat(tt.parenR) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            if (eat(tt.parenR)) {
                break
            }
        }

        parseExprListItem(false)
    }
}

function shouldParseAsyncArrow(): boolean {
    return match(tt.colon) || match(tt.arrow)
}

function parseAsyncArrowFromCallExpression(startTokenIndex: number): void {
    if (isTypeScriptEnabled) {
        tsStartParseAsyncArrowFromCallExpression()
    } else if (isFlowEnabled) {
        flowStartParseAsyncArrowFromCallExpression()
    }
    expect(tt.arrow)
    parseArrowExpression(startTokenIndex)
}

// Parse a no-call expression (like argument of `new` or `::` operators).

function parseNoCallExpr(): void {
    const startTokenIndex = state.tokens.length
    parseExprAtom()
    parseSubscripts(startTokenIndex, true)
}

// Parse an atomic expression — either a single token that is an
// expression, an expression started by a keyword like `function` or
// `new`, or an expression wrapped in punctuation like `()`, `[]`,
// or `{}`.
// Returns true if the parsed expression was an arrow function.
export function parseExprAtom(): boolean {
    if (eat(tt.modulo)) {
        // V8 intrinsic expression. Just parse the identifier, and the function invocation is parsed
        // naturally.
        parseIdentifier()
        return false
    }

    if (match(tt.jsxText) || match(tt.jsxEmptyText)) {
        parseLiteral()
        return false
    } else if (match(tt.lessThan) && isJSXEnabled) {
        state.type = tt.jsxTagStart
        jsxParseElement()
        next()
        return false
    }

    const canBeArrow = state.potentialArrowAt === state.start
    switch (state.type) {
        case tt.slash:
        case tt.assign:
            retokenizeSlashAsRegex()
        // Fall through.

        case tt._super:
        case tt._this:
        case tt.regexp:
        case tt.num:
        case tt.bigint:
        case tt.decimal:
        case tt.string:
        case tt._null:
        case tt._true:
        case tt._false:
            next()
            return false

        case tt._import:
            next()
            if (match(tt.dot)) {
                // import.meta
                state.tokens[state.tokens.length - 1].type = tt.name
                next()
                parseIdentifier()
            }
            return false

        case tt.name: {
            const startTokenIndex = state.tokens.length
            const functionStart = state.start
            const contextualKeyword = state.contextualKeyword
            parseIdentifier()
            if (contextualKeyword === ContextualKeyword._await) {
                parseAwait()
                return false
            } else if (
                contextualKeyword === ContextualKeyword._async &&
                match(tt._function) &&
                !canInsertSemicolon()
            ) {
                next()
                parseFunction(functionStart, false)
                return false
            } else if (
                canBeArrow &&
                contextualKeyword === ContextualKeyword._async &&
                !canInsertSemicolon() &&
                match(tt.name)
            ) {
                state.scopeDepth++
                parseBindingIdentifier(false)
                expect(tt.arrow)
                // let foo = async bar => {};
                parseArrowExpression(startTokenIndex)
                return true
            } else if (match(tt._do) && !canInsertSemicolon()) {
                next()
                parseBlock()
                return false
            }

            if (canBeArrow && !canInsertSemicolon() && match(tt.arrow)) {
                state.scopeDepth++
                markPriorBindingIdentifier(false)
                expect(tt.arrow)
                parseArrowExpression(startTokenIndex)
                return true
            }

            state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.Access
            return false
        }

        case tt._do: {
            next()
            parseBlock()
            return false
        }

        case tt.parenL: {
            const wasArrow = parseParenAndDistinguishExpression(canBeArrow)
            return wasArrow
        }

        case tt.bracketL:
            next()
            parseExprList(tt.bracketR, true)
            return false

        case tt.braceL:
            parseObj(false, false)
            return false

        case tt._function:
            parseFunctionExpression()
            return false

        case tt.at:
            parseDecorators()
        // Fall through.

        case tt._class:
            parseClass(false)
            return false

        case tt._new:
            parseNew()
            return false

        case tt.backQuote:
            parseTemplate()
            return false

        case tt.doubleColon: {
            next()
            parseNoCallExpr()
            return false
        }

        case tt.hash: {
            const code = lookaheadCharCode()
            if (IS_IDENTIFIER_START[code] || code === charCodes.backslash) {
                parseMaybePrivateName()
            } else {
                next()
            }
            // Smart pipeline topic reference.
            return false
        }

        default:
            unexpected()
            return false
    }
}

function parseMaybePrivateName(): void {
    eat(tt.hash)
    parseIdentifier()
}

function parseFunctionExpression(): void {
    const functionStart = state.start
    parseIdentifier()
    if (eat(tt.dot)) {
        // function.sent
        parseIdentifier()
    }
    parseFunction(functionStart, false)
}

export function parseLiteral(): void {
    next()
}

export function parseParenExpression(): void {
    expect(tt.parenL)
    parseExpression()
    expect(tt.parenR)
}

// Returns true if this was an arrow expression.
function parseParenAndDistinguishExpression(canBeArrow: boolean): boolean {
    // Assume this is a normal parenthesized expression, but if we see an arrow, we'll bail and
    // start over as a parameter list.
    const snapshot = state.snapshot()

    const startTokenIndex = state.tokens.length
    expect(tt.parenL)

    let first = true

    while (!match(tt.parenR) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            if (match(tt.parenR)) {
                break
            }
        }

        if (match(tt.ellipsis)) {
            parseRest(false /* isBlockScope */)
            parseParenItem()
            break
        } else {
            parseMaybeAssign(false, true)
        }
    }

    expect(tt.parenR)

    if (canBeArrow && shouldParseArrow()) {
        const wasArrow = parseArrow()
        if (wasArrow) {
            // It was an arrow function this whole time, so start over and parse it as params so that we
            // get proper token annotations.
            state.restoreFromSnapshot(snapshot)
            state.scopeDepth++
            // Don't specify a context ID because arrow functions don't need a context ID.
            parseFunctionParams()
            parseArrow()
            parseArrowExpression(startTokenIndex)
            if (state.error) {
                // Nevermind! This must have been something that looks very much like an
                // arrow function but where its "parameter list" isn't actually a valid
                // parameter list. Force non-arrow parsing.
                // See https://github.com/alangpierce/sucrase/issues/666 for an example.
                state.restoreFromSnapshot(snapshot)
                parseParenAndDistinguishExpression(false)
                return false
            }
            return true
        }
    }

    return false
}

function shouldParseArrow(): boolean {
    return match(tt.colon) || !canInsertSemicolon()
}

// Returns whether there was an arrow token.
export function parseArrow(): boolean {
    if (isTypeScriptEnabled) {
        return tsParseArrow()
    } else if (isFlowEnabled) {
        return flowParseArrow()
    } else {
        return eat(tt.arrow)
    }
}

function parseParenItem(): void {
    if (isTypeScriptEnabled || isFlowEnabled) {
        typedParseParenItem()
    }
}

// New's precedence is slightly tricky. It must allow its argument to
// be a `[]` or dot subscript expression, but not a call — at least,
// not without wrapping it in parentheses. Thus, it uses the noCalls
// argument to parseSubscripts to prevent it from consuming the
// argument list.
function parseNew(): void {
    expect(tt._new)
    if (eat(tt.dot)) {
        // new.target
        parseIdentifier()
        return
    }
    parseNewCallee()
    if (isFlowEnabled) {
        flowStartParseNewArguments()
    }
    if (eat(tt.parenL)) {
        parseExprList(tt.parenR)
    }
}

function parseNewCallee(): void {
    parseNoCallExpr()
    eat(tt.questionDot)
}

export function parseTemplate(): void {
    // Finish `, read quasi
    nextTemplateToken()
    // Finish quasi, read ${
    nextTemplateToken()
    while (!match(tt.backQuote) && !state.error) {
        expect(tt.dollarBraceL)
        parseExpression()
        // Finish }, read quasi
        nextTemplateToken()
        // Finish quasi, read either ${ or `
        nextTemplateToken()
    }
    next()
}

// Parse an object literal or binding pattern.
export function parseObj(isPattern: boolean, isBlockScope: boolean): void {
    // Attach a context ID to the object open and close brace and each object key.
    const contextId = getNextContextId()
    let first = true

    next()
    state.tokens[state.tokens.length - 1].contextId = contextId

    while (!eat(tt.braceR) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            if (eat(tt.braceR)) {
                break
            }
        }

        let isGenerator = false
        if (match(tt.ellipsis)) {
            const previousIndex = state.tokens.length
            parseSpread()
            if (isPattern) {
                // Mark role when the only thing being spread over is an identifier.
                if (state.tokens.length === previousIndex + 2) {
                    markPriorBindingIdentifier(isBlockScope)
                }
                if (eat(tt.braceR)) {
                    break
                }
            }
            continue
        }

        if (!isPattern) {
            isGenerator = eat(tt.star)
        }

        if (!isPattern && isContextual(ContextualKeyword._async)) {
            if (isGenerator) unexpected()

            parseIdentifier()
            if (
                match(tt.colon) ||
                match(tt.parenL) ||
                match(tt.braceR) ||
                match(tt.eq) ||
                match(tt.comma)
            ) {
                // This is a key called "async" rather than an async function.
            } else {
                if (match(tt.star)) {
                    next()
                    isGenerator = true
                }
                parsePropertyName(contextId)
            }
        } else {
            parsePropertyName(contextId)
        }

        parseObjPropValue(isPattern, isBlockScope, contextId)
    }

    state.tokens[state.tokens.length - 1].contextId = contextId
}

function isGetterOrSetterMethod(isPattern: boolean): boolean {
    // We go off of the next and don't bother checking if the node key is actually "get" or "set".
    // This lets us avoid generating a node, and should only make the validation worse.
    return (
        !isPattern &&
        (match(tt.string) || // get "string"() {}
            match(tt.num) || // get 1() {}
            match(tt.bracketL) || // get ["string"]() {}
            match(tt.name) || // get foo() {}
            !!(state.type & TokenType.IS_KEYWORD)) // get debugger() {}
    )
}

// Returns true if this was a method.
function parseObjectMethod(isPattern: boolean, objectContextId: number): boolean {
    // We don't need to worry about modifiers because object methods can't have optional bodies, so
    // the start will never be used.
    const functionStart = state.start
    if (match(tt.parenL)) {
        if (isPattern) unexpected()
        parseMethod(functionStart, /* isConstructor */ false)
        return true
    }

    if (isGetterOrSetterMethod(isPattern)) {
        parsePropertyName(objectContextId)
        parseMethod(functionStart, /* isConstructor */ false)
        return true
    }
    return false
}

function parseObjectProperty(isPattern: boolean, isBlockScope: boolean): void {
    if (eat(tt.colon)) {
        if (isPattern) {
            parseMaybeDefault(isBlockScope)
        } else {
            parseMaybeAssign(false)
        }
        return
    }

    // Since there's no colon, we assume this is an object shorthand.

    // If we're in a destructuring, we've now discovered that the key was actually an assignee, so
    // we need to tag it as a declaration with the appropriate scope. Otherwise, we might need to
    // transform it on access, so mark it as a normal object shorthand.
    let identifierRole
    if (isPattern) {
        if (state.scopeDepth === 0) {
            identifierRole = IdentifierRole.ObjectShorthandTopLevelDeclaration
        } else if (isBlockScope) {
            identifierRole = IdentifierRole.ObjectShorthandBlockScopedDeclaration
        } else {
            identifierRole = IdentifierRole.ObjectShorthandFunctionScopedDeclaration
        }
    } else {
        identifierRole = IdentifierRole.ObjectShorthand
    }
    state.tokens[state.tokens.length - 1].identifierRole = identifierRole

    // Regardless of whether we know this to be a pattern or if we're in an ambiguous context, allow
    // parsing as if there's a default value.
    parseMaybeDefault(isBlockScope, true)
}

function parseObjPropValue(
    isPattern: boolean,
    isBlockScope: boolean,
    objectContextId: number,
): void {
    if (isTypeScriptEnabled) {
        tsStartParseObjPropValue()
    } else if (isFlowEnabled) {
        flowStartParseObjPropValue()
    }
    const wasMethod = parseObjectMethod(isPattern, objectContextId)
    if (!wasMethod) {
        parseObjectProperty(isPattern, isBlockScope)
    }
}

export function parsePropertyName(objectContextId: number): void {
    if (isFlowEnabled) {
        flowParseVariance()
    }
    if (eat(tt.bracketL)) {
        state.tokens[state.tokens.length - 1].contextId = objectContextId
        parseMaybeAssign()
        expect(tt.bracketR)
        state.tokens[state.tokens.length - 1].contextId = objectContextId
    } else {
        if (match(tt.num) || match(tt.string) || match(tt.bigint) || match(tt.decimal)) {
            parseExprAtom()
        } else {
            parseMaybePrivateName()
        }

        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ObjectKey
        state.tokens[state.tokens.length - 1].contextId = objectContextId
    }
}

// Parse object or class method.
export function parseMethod(functionStart: number, isConstructor: boolean): void {
    const funcContextId = getNextContextId()

    state.scopeDepth++
    const startTokenIndex = state.tokens.length
    const allowModifiers = isConstructor // For TypeScript parameter properties
    parseFunctionParams(allowModifiers, funcContextId)
    parseFunctionBodyAndFinish(functionStart, funcContextId)
    const endTokenIndex = state.tokens.length
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
    state.scopeDepth--
}

// Parse arrow function expression.
// If the parameters are provided, they will be converted to an
// assignable list.
export function parseArrowExpression(startTokenIndex: number): void {
    parseFunctionBody(true)
    const endTokenIndex = state.tokens.length
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
    state.scopeDepth--
}

export function parseFunctionBodyAndFinish(functionStart: number, funcContextId: number = 0): void {
    if (isTypeScriptEnabled) {
        tsParseFunctionBodyAndFinish(functionStart, funcContextId)
    } else if (isFlowEnabled) {
        flowParseFunctionBodyAndFinish(funcContextId)
    } else {
        parseFunctionBody(false, funcContextId)
    }
}

export function parseFunctionBody(allowExpression: boolean, funcContextId: number = 0): void {
    const isExpression = allowExpression && !match(tt.braceL)

    if (isExpression) {
        parseMaybeAssign()
    } else {
        parseBlock(true /* isFunctionScope */, funcContextId)
    }
}

// Parses a comma-separated list of expressions, and returns them as
// an array. `close` is the token type that ends the list, and
// `allowEmpty` can be turned on to allow subsequent commas with
// nothing in between them to be parsed as `null` (which is needed
// for array literals).

function parseExprList(close: TokenType, allowEmpty: boolean = false): void {
    let first = true
    while (!eat(close) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            if (eat(close)) break
        }
        parseExprListItem(allowEmpty)
    }
}

function parseExprListItem(allowEmpty: boolean): void {
    if (allowEmpty && match(tt.comma)) {
        // Empty item; nothing more to parse for this item.
    } else if (match(tt.ellipsis)) {
        parseSpread()
        parseParenItem()
    } else if (match(tt.question)) {
        // Partial function application proposal.
        next()
    } else {
        parseMaybeAssign(false, true)
    }
}

// Parse the next token as an identifier.
export function parseIdentifier(): void {
    next()
    state.tokens[state.tokens.length - 1].type = tt.name
}

// Parses await expression inside async function.
function parseAwait(): void {
    parseMaybeUnary()
}

// Parses yield expression inside generator.
function parseYield(): void {
    next()
    if (!match(tt.semi) && !canInsertSemicolon()) {
        eat(tt.star)
        parseMaybeAssign()
    }
}

// https://github.com/tc39/proposal-js-module-blocks
function parseModuleExpression(): void {
    expectContextual(ContextualKeyword._module)
    expect(tt.braceL)
    // For now, just call parseBlockBody to parse the block. In the future when we
    // implement full support, we'll want to emit scopes and possibly other
    // information.
    parseBlockBody(tt.braceR)
}
// #endregion

// #region statement.ts --------------------------------------------------------
export function parseTopLevel(): File {
    parseBlockBody(tt.eof)
    state.scopes.push(new Scope(0, state.tokens.length, true))
    if (state.scopeDepth !== 0) {
        throw new Error(`Invalid scope depth at end of file: ${state.scopeDepth}`)
    }
    return new File(state.tokens, state.scopes)
}

// Parse a single statement.
//
// If expecting a statement and finding a slash operator, parse a
// regular expression literal. This is to handle cases like
// `if (foo) /blah/.exec(foo)`, where looking at the previous token
// does not help.

export function parseStatement(declaration: boolean): void {
    if (isFlowEnabled) {
        if (flowTryParseStatement()) {
            return
        }
    }
    if (match(tt.at)) {
        parseDecorators()
    }
    parseStatementContent(declaration)
}

function parseStatementContent(declaration: boolean): void {
    if (isTypeScriptEnabled) {
        if (tsTryParseStatementContent()) {
            return
        }
    }

    const starttype = state.type

    // Most types of statements are recognized by the keyword they
    // start with. Many are trivial to parse, some require a bit of
    // complexity.

    switch (starttype) {
        case tt._break:
        case tt._continue:
            parseBreakContinueStatement()
            return
        case tt._debugger:
            parseDebuggerStatement()
            return
        case tt._do:
            parseDoStatement()
            return
        case tt._for:
            parseForStatement()
            return
        case tt._function:
            if (lookaheadType() === tt.dot) break
            if (!declaration) unexpected()
            parseFunctionStatement()
            return

        case tt._class:
            if (!declaration) unexpected()
            parseClass(true)
            return

        case tt._if:
            parseIfStatement()
            return
        case tt._return:
            parseReturnStatement()
            return
        case tt._switch:
            parseSwitchStatement()
            return
        case tt._throw:
            parseThrowStatement()
            return
        case tt._try:
            parseTryStatement()
            return

        case tt._let:
        case tt._const:
            if (!declaration) unexpected() // NOTE: falls through to _var

        case tt._var:
            parseVarStatement(starttype !== tt._var)
            return

        case tt._while:
            parseWhileStatement()
            return
        case tt.braceL:
            parseBlock()
            return
        case tt.semi:
            parseEmptyStatement()
            return
        case tt._export:
        case tt._import: {
            const nextType = lookaheadType()
            if (nextType === tt.parenL || nextType === tt.dot) {
                break
            }
            next()
            if (starttype === tt._import) {
                parseImport()
            } else {
                parseExport()
            }
            return
        }
        case tt.name:
            if (state.contextualKeyword === ContextualKeyword._async) {
                const functionStart = state.start
                // peek ahead and see if next token is a function
                const snapshot = state.snapshot()
                next()
                if (match(tt._function) && !canInsertSemicolon()) {
                    expect(tt._function)
                    parseFunction(functionStart, true)
                    return
                } else {
                    state.restoreFromSnapshot(snapshot)
                }
            } else if (
                state.contextualKeyword === ContextualKeyword._using &&
                !hasFollowingLineBreak() &&
                // Statements like `using[0]` and `using in foo` aren't actual using
                // declarations.
                lookaheadType() === tt.name
            ) {
                parseVarStatement(true)
                return
            } else if (startsAwaitUsing()) {
                expectContextual(ContextualKeyword._await)
                parseVarStatement(true)
                return
            }
        default:
            // Do nothing.
            break
    }

    // If the statement does not start with a statement keyword or a
    // brace, it's an ExpressionStatement or LabeledStatement. We
    // simply start parsing an expression, and afterwards, if the
    // next token is a colon and the expression was a simple
    // Identifier node, we switch to interpreting it as a label.
    const initialTokensLength = state.tokens.length
    parseExpression()
    let simpleName = null
    if (state.tokens.length === initialTokensLength + 1) {
        const token = state.tokens[state.tokens.length - 1]
        if (token.type === tt.name) {
            simpleName = token.contextualKeyword
        }
    }
    if (simpleName == null) {
        semicolon()
        return
    }
    if (eat(tt.colon)) {
        parseLabeledStatement()
    } else {
        // This was an identifier, so we might want to handle flow/typescript-specific cases.
        parseIdentifierStatement(simpleName)
    }
}

/**
 * Determine if we're positioned at an `await using` declaration.
 *
 * Note that this can happen either in place of a regular variable declaration
 * or in a loop body, and in both places, there are similar-looking cases where
 * we need to return false.
 *
 * Examples returning true:
 * await using foo = bar();
 * for (await using a of b) {}
 *
 * Examples returning false:
 * await using
 * await using + 1
 * await using instanceof T
 * for (await using;;) {}
 *
 * For now, we early return if we don't see `await`, then do a simple
 * backtracking-based lookahead for the `using` and identifier tokens. In the
 * future, this could be optimized with a character-based approach.
 */
function startsAwaitUsing(): boolean {
    if (!isContextual(ContextualKeyword._await)) {
        return false
    }
    const snapshot = state.snapshot()
    // await
    next()
    if (!isContextual(ContextualKeyword._using) || hasPrecedingLineBreak()) {
        state.restoreFromSnapshot(snapshot)
        return false
    }
    // using
    next()
    if (!match(tt.name) || hasPrecedingLineBreak()) {
        state.restoreFromSnapshot(snapshot)
        return false
    }
    state.restoreFromSnapshot(snapshot)
    return true
}

export function parseDecorators(): void {
    while (match(tt.at)) {
        parseDecorator()
    }
}

function parseDecorator(): void {
    next()
    if (eat(tt.parenL)) {
        parseExpression()
        expect(tt.parenR)
    } else {
        parseIdentifier()
        while (eat(tt.dot)) {
            parseIdentifier()
        }
        parseMaybeDecoratorArguments()
    }
}

function parseMaybeDecoratorArguments(): void {
    if (isTypeScriptEnabled) {
        tsParseMaybeDecoratorArguments()
    } else {
        baseParseMaybeDecoratorArguments()
    }
}

export function baseParseMaybeDecoratorArguments(): void {
    if (eat(tt.parenL)) {
        parseCallExpressionArguments()
    }
}

function parseBreakContinueStatement(): void {
    next()
    if (!isLineTerminator()) {
        parseIdentifier()
        semicolon()
    }
}

function parseDebuggerStatement(): void {
    next()
    semicolon()
}

function parseDoStatement(): void {
    next()
    parseStatement(false)
    expect(tt._while)
    parseParenExpression()
    eat(tt.semi)
}

function parseForStatement(): void {
    state.scopeDepth++
    const startTokenIndex = state.tokens.length
    parseAmbiguousForStatement()
    const endTokenIndex = state.tokens.length
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, false))
    state.scopeDepth--
}

/**
 * Determine if this token is a `using` declaration (explicit resource
 * management) as part of a loop.
 * https://github.com/tc39/proposal-explicit-resource-management
 */
function isUsingInLoop(): boolean {
    if (!isContextual(ContextualKeyword._using)) {
        return false
    }
    // This must be `for (using of`, where `using` is the name of the loop
    // variable.
    if (isLookaheadContextual(ContextualKeyword._of)) {
        return false
    }
    return true
}

// Disambiguating between a `for` and a `for`/`in` or `for`/`of`
// loop is non-trivial. Basically, we have to parse the init `var`
// statement or expression, disallowing the `in` operator (see
// the second parameter to `parseExpression`), and then check
// whether the next token is `in` or `of`. When there is no init
// part (semicolon immediately after the opening parenthesis), it
// is a regular `for` loop.
function parseAmbiguousForStatement(): void {
    next()

    let forAwait = false
    if (isContextual(ContextualKeyword._await)) {
        forAwait = true
        next()
    }
    expect(tt.parenL)

    if (match(tt.semi)) {
        if (forAwait) {
            unexpected()
        }
        parseFor()
        return
    }

    const isAwaitUsing = startsAwaitUsing()
    if (isAwaitUsing || match(tt._var) || match(tt._let) || match(tt._const) || isUsingInLoop()) {
        if (isAwaitUsing) {
            expectContextual(ContextualKeyword._await)
        }
        next()
        parseVar(true, state.type !== tt._var)
        if (match(tt._in) || isContextual(ContextualKeyword._of)) {
            parseForIn(forAwait)
            return
        }
        parseFor()
        return
    }

    parseExpression(true)
    if (match(tt._in) || isContextual(ContextualKeyword._of)) {
        parseForIn(forAwait)
        return
    }
    if (forAwait) {
        unexpected()
    }
    parseFor()
}

function parseFunctionStatement(): void {
    const functionStart = state.start
    next()
    parseFunction(functionStart, true)
}

function parseIfStatement(): void {
    next()
    parseParenExpression()
    parseStatement(false)
    if (eat(tt._else)) {
        parseStatement(false)
    }
}

function parseReturnStatement(): void {
    next()

    // In `return` (and `break`/`continue`), the keywords with
    // optional arguments, we eagerly look for a semicolon or the
    // possibility to insert one.

    if (!isLineTerminator()) {
        parseExpression()
        semicolon()
    }
}

function parseSwitchStatement(): void {
    next()
    parseParenExpression()
    state.scopeDepth++
    const startTokenIndex = state.tokens.length
    expect(tt.braceL)

    // Don't bother validation; just go through any sequence of cases, defaults, and statements.
    while (!match(tt.braceR) && !state.error) {
        if (match(tt._case) || match(tt._default)) {
            const isCase = match(tt._case)
            next()
            if (isCase) {
                parseExpression()
            }
            expect(tt.colon)
        } else {
            parseStatement(true)
        }
    }
    next() // Closing brace
    const endTokenIndex = state.tokens.length
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, false))
    state.scopeDepth--
}

function parseThrowStatement(): void {
    next()
    parseExpression()
    semicolon()
}

function parseCatchClauseParam(): void {
    parseBindingAtom(true /* isBlockScope */)

    if (isTypeScriptEnabled) {
        tsTryParseTypeAnnotation()
    }
}

function parseTryStatement(): void {
    next()

    parseBlock()

    if (match(tt._catch)) {
        next()
        let catchBindingStartTokenIndex = null
        if (match(tt.parenL)) {
            state.scopeDepth++
            catchBindingStartTokenIndex = state.tokens.length
            expect(tt.parenL)
            parseCatchClauseParam()
            expect(tt.parenR)
        }
        parseBlock()
        if (catchBindingStartTokenIndex != null) {
            // We need a special scope for the catch binding which includes the binding itself and the
            // catch block.
            const endTokenIndex = state.tokens.length
            state.scopes.push(new Scope(catchBindingStartTokenIndex, endTokenIndex, false))
            state.scopeDepth--
        }
    }
    if (eat(tt._finally)) {
        parseBlock()
    }
}

export function parseVarStatement(isBlockScope: boolean): void {
    next()
    parseVar(false, isBlockScope)
    semicolon()
}

function parseWhileStatement(): void {
    next()
    parseParenExpression()
    parseStatement(false)
}

function parseEmptyStatement(): void {
    next()
}

function parseLabeledStatement(): void {
    parseStatement(true)
}

/**
 * Parse a statement starting with an identifier of the given name. Subclasses match on the name
 * to handle statements like "declare".
 */
function parseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
    if (isTypeScriptEnabled) {
        tsParseIdentifierStatement(contextualKeyword)
    } else if (isFlowEnabled) {
        flowParseIdentifierStatement(contextualKeyword)
    } else {
        semicolon()
    }
}

// Parse a semicolon-enclosed block of statements.
export function parseBlock(isFunctionScope: boolean = false, contextId: number = 0): void {
    const startTokenIndex = state.tokens.length
    state.scopeDepth++
    expect(tt.braceL)
    if (contextId) {
        state.tokens[state.tokens.length - 1].contextId = contextId
    }
    parseBlockBody(tt.braceR)
    if (contextId) {
        state.tokens[state.tokens.length - 1].contextId = contextId
    }
    const endTokenIndex = state.tokens.length
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, isFunctionScope))
    state.scopeDepth--
}

export function parseBlockBody(end: TokenType): void {
    while (!eat(end) && !state.error) {
        parseStatement(true)
    }
}

// Parse a regular `for` loop. The disambiguation code in
// `parseStatement` will already have parsed the init statement or
// expression.

function parseFor(): void {
    expect(tt.semi)
    if (!match(tt.semi)) {
        parseExpression()
    }
    expect(tt.semi)
    if (!match(tt.parenR)) {
        parseExpression()
    }
    expect(tt.parenR)
    parseStatement(false)
}

// Parse a `for`/`in` and `for`/`of` loop, which are almost
// same from parser's perspective.

function parseForIn(forAwait: boolean): void {
    if (forAwait) {
        eatContextual(ContextualKeyword._of)
    } else {
        next()
    }
    parseExpression()
    expect(tt.parenR)
    parseStatement(false)
}

// Parse a list of variable declarations.

function parseVar(isFor: boolean, isBlockScope: boolean): void {
    while (true) {
        parseVarHead(isBlockScope)
        if (eat(tt.eq)) {
            const eqIndex = state.tokens.length - 1
            parseMaybeAssign(isFor)
            state.tokens[eqIndex].rhsEndIndex = state.tokens.length
        }
        if (!eat(tt.comma)) {
            break
        }
    }
}

function parseVarHead(isBlockScope: boolean): void {
    parseBindingAtom(isBlockScope)
    if (isTypeScriptEnabled) {
        tsAfterParseVarHead()
    } else if (isFlowEnabled) {
        flowAfterParseVarHead()
    }
}

// Parse a function declaration or literal (depending on the
// `isStatement` parameter).

export function parseFunction(
    functionStart: number,
    isStatement: boolean,
    optionalId: boolean = false,
): void {
    if (match(tt.star)) {
        next()
    }

    if (isStatement && !optionalId && !match(tt.name) && !match(tt._yield)) {
        unexpected()
    }

    let nameScopeStartTokenIndex = null

    if (match(tt.name)) {
        // Expression-style functions should limit their name's scope to the function body, so we make
        // a new function scope to enforce that.
        if (!isStatement) {
            nameScopeStartTokenIndex = state.tokens.length
            state.scopeDepth++
        }
        parseBindingIdentifier(false)
    }

    const startTokenIndex = state.tokens.length
    state.scopeDepth++
    parseFunctionParams()
    parseFunctionBodyAndFinish(functionStart)
    const endTokenIndex = state.tokens.length
    // In addition to the block scope of the function body, we need a separate function-style scope
    // that includes the params.
    state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
    state.scopeDepth--
    if (nameScopeStartTokenIndex !== null) {
        state.scopes.push(new Scope(nameScopeStartTokenIndex, endTokenIndex, true))
        state.scopeDepth--
    }
}

export function parseFunctionParams(
    allowModifiers: boolean = false,
    funcContextId: number = 0,
): void {
    if (isTypeScriptEnabled) {
        tsStartParseFunctionParams()
    } else if (isFlowEnabled) {
        flowStartParseFunctionParams()
    }

    expect(tt.parenL)
    if (funcContextId) {
        state.tokens[state.tokens.length - 1].contextId = funcContextId
    }
    parseBindingList(
        tt.parenR,
        false /* isBlockScope */,
        false /* allowEmpty */,
        allowModifiers,
        funcContextId,
    )
    if (funcContextId) {
        state.tokens[state.tokens.length - 1].contextId = funcContextId
    }
}

// Parse a class declaration or literal (depending on the
// `isStatement` parameter).

export function parseClass(isStatement: boolean, optionalId: boolean = false): void {
    // Put a context ID on the class keyword, the open-brace, and the close-brace, so that later
    // code can easily navigate to meaningful points on the class.
    const contextId = getNextContextId()

    next()
    state.tokens[state.tokens.length - 1].contextId = contextId
    state.tokens[state.tokens.length - 1].isExpression = !isStatement
    // Like with functions, we declare a special "name scope" from the start of the name to the end
    // of the class, but only with expression-style classes, to represent the fact that the name is
    // available to the body of the class but not an outer declaration.
    let nameScopeStartTokenIndex = null
    if (!isStatement) {
        nameScopeStartTokenIndex = state.tokens.length
        state.scopeDepth++
    }
    parseClassId(isStatement, optionalId)
    parseClassSuper()
    const openBraceIndex = state.tokens.length
    parseClassBody(contextId)
    if (state.error) {
        return
    }
    state.tokens[openBraceIndex].contextId = contextId
    state.tokens[state.tokens.length - 1].contextId = contextId
    if (nameScopeStartTokenIndex !== null) {
        const endTokenIndex = state.tokens.length
        state.scopes.push(new Scope(nameScopeStartTokenIndex, endTokenIndex, false))
        state.scopeDepth--
    }
}

function isClassProperty(): boolean {
    return match(tt.eq) || match(tt.semi) || match(tt.braceR) || match(tt.bang) || match(tt.colon)
}

function isClassMethod(): boolean {
    return match(tt.parenL) || match(tt.lessThan)
}

function parseClassBody(classContextId: number): void {
    expect(tt.braceL)

    while (!eat(tt.braceR) && !state.error) {
        if (eat(tt.semi)) {
            continue
        }

        if (match(tt.at)) {
            parseDecorator()
            continue
        }
        const memberStart = state.start
        parseClassMember(memberStart, classContextId)
    }
}

function parseClassMember(memberStart: number, classContextId: number): void {
    if (isTypeScriptEnabled) {
        tsParseModifiers([
            ContextualKeyword._declare,
            ContextualKeyword._public,
            ContextualKeyword._protected,
            ContextualKeyword._private,
            ContextualKeyword._override,
        ])
    }
    let isStatic = false
    if (match(tt.name) && state.contextualKeyword === ContextualKeyword._static) {
        parseIdentifier() // eats 'static'
        if (isClassMethod()) {
            parseClassMethod(memberStart, /* isConstructor */ false)
            return
        } else if (isClassProperty()) {
            parseClassProperty()
            return
        }
        // otherwise something static
        state.tokens[state.tokens.length - 1].type = tt._static
        isStatic = true

        if (match(tt.braceL)) {
            // This is a static block. Mark the word "static" with the class context ID for class element
            // detection and parse as a regular block.
            state.tokens[state.tokens.length - 1].contextId = classContextId
            parseBlock()
            return
        }
    }

    parseClassMemberWithIsStatic(memberStart, isStatic, classContextId)
}

function parseClassMemberWithIsStatic(
    memberStart: number,
    isStatic: boolean,
    classContextId: number,
): void {
    if (isTypeScriptEnabled) {
        if (tsTryParseClassMemberWithIsStatic(isStatic)) {
            return
        }
    }
    if (eat(tt.star)) {
        // a generator
        parseClassPropertyName(classContextId)
        parseClassMethod(memberStart, /* isConstructor */ false)
        return
    }

    // Get the identifier name so we can tell if it's actually a keyword like "async", "get", or
    // "set".
    parseClassPropertyName(classContextId)
    let isConstructor = false
    const token = state.tokens[state.tokens.length - 1]
    // We allow "constructor" as either an identifier or a string.
    if (token.contextualKeyword === ContextualKeyword._constructor) {
        isConstructor = true
    }
    parsePostMemberNameModifiers()

    if (isClassMethod()) {
        parseClassMethod(memberStart, isConstructor)
    } else if (isClassProperty()) {
        parseClassProperty()
    } else if (token.contextualKeyword === ContextualKeyword._async && !isLineTerminator()) {
        state.tokens[state.tokens.length - 1].type = tt._async
        // an async method
        const isGenerator = match(tt.star)
        if (isGenerator) {
            next()
        }

        // The so-called parsed name would have been "async": get the real name.
        parseClassPropertyName(classContextId)
        parsePostMemberNameModifiers()
        parseClassMethod(memberStart, false /* isConstructor */)
    } else if (
        (token.contextualKeyword === ContextualKeyword._get ||
            token.contextualKeyword === ContextualKeyword._set) &&
        !(isLineTerminator() && match(tt.star))
    ) {
        if (token.contextualKeyword === ContextualKeyword._get) {
            state.tokens[state.tokens.length - 1].type = tt._get
        } else {
            state.tokens[state.tokens.length - 1].type = tt._set
        }
        // `get\n*` is an uninitialized property named 'get' followed by a generator.
        // a getter or setter
        // The so-called parsed name would have been "get/set": get the real name.
        parseClassPropertyName(classContextId)
        parseClassMethod(memberStart, /* isConstructor */ false)
    } else if (token.contextualKeyword === ContextualKeyword._accessor && !isLineTerminator()) {
        parseClassPropertyName(classContextId)
        parseClassProperty()
    } else if (isLineTerminator()) {
        // an uninitialized class property (due to ASI, since we don't otherwise recognize the next token)
        parseClassProperty()
    } else {
        unexpected()
    }
}

function parseClassMethod(functionStart: number, isConstructor: boolean): void {
    if (isTypeScriptEnabled) {
        tsTryParseTypeParameters()
    } else if (isFlowEnabled) {
        if (match(tt.lessThan)) {
            flowParseTypeParameterDeclaration()
        }
    }
    parseMethod(functionStart, isConstructor)
}

// Return the name of the class property, if it is a simple identifier.
export function parseClassPropertyName(classContextId: number): void {
    parsePropertyName(classContextId)
}

export function parsePostMemberNameModifiers(): void {
    if (isTypeScriptEnabled) {
        const oldIsType = pushTypeContext(0)
        eat(tt.question)
        popTypeContext(oldIsType)
    }
}

export function parseClassProperty(): void {
    if (isTypeScriptEnabled) {
        eatTypeToken(tt.bang)
        tsTryParseTypeAnnotation()
    } else if (isFlowEnabled) {
        if (match(tt.colon)) {
            flowParseTypeAnnotation()
        }
    }

    if (match(tt.eq)) {
        const equalsTokenIndex = state.tokens.length
        next()
        parseMaybeAssign()
        state.tokens[equalsTokenIndex].rhsEndIndex = state.tokens.length
    }
    semicolon()
}

function parseClassId(isStatement: boolean, optionalId: boolean = false): void {
    if (
        isTypeScriptEnabled &&
        (!isStatement || optionalId) &&
        isContextual(ContextualKeyword._implements)
    ) {
        return
    }

    if (match(tt.name)) {
        parseBindingIdentifier(true)
    }

    if (isTypeScriptEnabled) {
        tsTryParseTypeParameters()
    } else if (isFlowEnabled) {
        if (match(tt.lessThan)) {
            flowParseTypeParameterDeclaration()
        }
    }
}

// Returns true if there was a superclass.
function parseClassSuper(): void {
    let hasSuper = false
    if (eat(tt._extends)) {
        parseExprSubscripts()
        hasSuper = true
    } else {
        hasSuper = false
    }
    if (isTypeScriptEnabled) {
        tsAfterParseClassSuper(hasSuper)
    } else if (isFlowEnabled) {
        flowAfterParseClassSuper(hasSuper)
    }
}

// Parses module export declaration.

export function parseExport(): void {
    const exportIndex = state.tokens.length - 1
    if (isTypeScriptEnabled) {
        if (tsTryParseExport()) {
            return
        }
    }
    // export * from '...'
    if (shouldParseExportStar()) {
        parseExportStar()
    } else if (isExportDefaultSpecifier()) {
        // export default from
        parseIdentifier()
        if (match(tt.comma) && lookaheadType() === tt.star) {
            expect(tt.comma)
            expect(tt.star)
            expectContextual(ContextualKeyword._as)
            parseIdentifier()
        } else {
            parseExportSpecifiersMaybe()
        }
        parseExportFrom()
    } else if (eat(tt._default)) {
        // export default ...
        parseExportDefaultExpression()
    } else if (shouldParseExportDeclaration()) {
        parseExportDeclaration()
    } else {
        // export { x, y as z } [from '...']
        parseExportSpecifiers()
        parseExportFrom()
    }
    state.tokens[exportIndex].rhsEndIndex = state.tokens.length
}

function parseExportDefaultExpression(): void {
    if (isTypeScriptEnabled) {
        if (tsTryParseExportDefaultExpression()) {
            return
        }
    }
    if (isFlowEnabled) {
        if (flowTryParseExportDefaultExpression()) {
            return
        }
    }
    const functionStart = state.start
    if (eat(tt._function)) {
        parseFunction(functionStart, true, true)
    } else if (isContextual(ContextualKeyword._async) && lookaheadType() === tt._function) {
        // async function declaration
        eatContextual(ContextualKeyword._async)
        eat(tt._function)
        parseFunction(functionStart, true, true)
    } else if (match(tt._class)) {
        parseClass(true, true)
    } else if (match(tt.at)) {
        parseDecorators()
        parseClass(true, true)
    } else {
        parseMaybeAssign()
        semicolon()
    }
}

function parseExportDeclaration(): void {
    if (isTypeScriptEnabled) {
        tsParseExportDeclaration()
    } else if (isFlowEnabled) {
        flowParseExportDeclaration()
    } else {
        parseStatement(true)
    }
}

function isExportDefaultSpecifier(): boolean {
    if (isTypeScriptEnabled && tsIsDeclarationStart()) {
        return false
    } else if (isFlowEnabled && flowShouldDisallowExportDefaultSpecifier()) {
        return false
    }
    if (match(tt.name)) {
        return state.contextualKeyword !== ContextualKeyword._async
    }

    if (!match(tt._default)) {
        return false
    }

    const _next = nextTokenStart()
    const lookahead = lookaheadTypeAndKeyword()
    const hasFrom =
        lookahead.type === tt.name && lookahead.contextualKeyword === ContextualKeyword._from
    if (lookahead.type === tt.comma) {
        return true
    }
    // lookahead again when `export default from` is seen
    if (hasFrom) {
        const nextAfterFrom = input.charCodeAt(nextTokenStartSince(_next + 4))
        return nextAfterFrom === charCodes.quotationMark || nextAfterFrom === charCodes.apostrophe
    }
    return false
}

function parseExportSpecifiersMaybe(): void {
    if (eat(tt.comma)) {
        parseExportSpecifiers()
    }
}

export function parseExportFrom(): void {
    if (eatContextual(ContextualKeyword._from)) {
        parseExprAtom()
        maybeParseImportAttributes()
    }
    semicolon()
}

function shouldParseExportStar(): boolean {
    if (isFlowEnabled) {
        return flowShouldParseExportStar()
    } else {
        return match(tt.star)
    }
}

function parseExportStar(): void {
    if (isFlowEnabled) {
        flowParseExportStar()
    } else {
        baseParseExportStar()
    }
}

export function baseParseExportStar(): void {
    expect(tt.star)

    if (isContextual(ContextualKeyword._as)) {
        parseExportNamespace()
    } else {
        parseExportFrom()
    }
}

function parseExportNamespace(): void {
    next()
    state.tokens[state.tokens.length - 1].type = tt._as
    parseIdentifier()
    parseExportSpecifiersMaybe()
    parseExportFrom()
}

function shouldParseExportDeclaration(): boolean {
    return (
        (isTypeScriptEnabled && tsIsDeclarationStart()) ||
        (isFlowEnabled && flowShouldParseExportDeclaration()) ||
        state.type === tt._var ||
        state.type === tt._const ||
        state.type === tt._let ||
        state.type === tt._function ||
        state.type === tt._class ||
        isContextual(ContextualKeyword._async) ||
        match(tt.at)
    )
}

// Parses a comma-separated list of module exports.
export function parseExportSpecifiers(): void {
    let first = true

    // export { x, y as z } [from '...']
    expect(tt.braceL)

    while (!eat(tt.braceR) && !state.error) {
        if (first) {
            first = false
        } else {
            expect(tt.comma)
            if (eat(tt.braceR)) {
                break
            }
        }
        parseExportSpecifier()
    }
}

function parseExportSpecifier(): void {
    if (isTypeScriptEnabled) {
        tsParseExportSpecifier()
        return
    }
    parseIdentifier()
    state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
    if (eatContextual(ContextualKeyword._as)) {
        parseIdentifier()
    }
}

/**
 * Starting at the `module` token in an import, determine if it was truly an
 * import reflection token or just looks like one.
 *
 * Returns true for:
 * import module foo from "foo";
 * import module from from "foo";
 *
 * Returns false for:
 * import module from "foo";
 * import module, {bar} from "foo";
 */
function isImportReflection(): boolean {
    const snapshot = state.snapshot()
    expectContextual(ContextualKeyword._module)
    if (eatContextual(ContextualKeyword._from)) {
        if (isContextual(ContextualKeyword._from)) {
            state.restoreFromSnapshot(snapshot)
            return true
        } else {
            state.restoreFromSnapshot(snapshot)
            return false
        }
    } else if (match(tt.comma)) {
        state.restoreFromSnapshot(snapshot)
        return false
    } else {
        state.restoreFromSnapshot(snapshot)
        return true
    }
}

/**
 * Eat the "module" token from the import reflection proposal.
 * https://github.com/tc39/proposal-import-reflection
 */
function parseMaybeImportReflection(): void {
    // isImportReflection does snapshot/restore, so only run it if we see the word
    // "module".
    if (isContextual(ContextualKeyword._module) && isImportReflection()) {
        next()
    }
}

// Parses import declaration.

export function parseImport(): void {
    if (isTypeScriptEnabled && match(tt.name) && lookaheadType() === tt.eq) {
        tsParseImportEqualsDeclaration()
        return
    }
    if (isTypeScriptEnabled && isContextual(ContextualKeyword._type)) {
        const lookahead = lookaheadTypeAndKeyword()
        if (lookahead.type === tt.name && lookahead.contextualKeyword !== ContextualKeyword._from) {
            // One of these `import type` cases:
            // import type T = require('T');
            // import type A from 'A';
            expectContextual(ContextualKeyword._type)
            if (lookaheadType() === tt.eq) {
                tsParseImportEqualsDeclaration()
                return
            }
            // If this is an `import type...from` statement, then we already ate the
            // type token, so proceed to the regular import parser.
        } else if (lookahead.type === tt.star || lookahead.type === tt.braceL) {
            // One of these `import type` cases, in which case we can eat the type token
            // and proceed as normal:
            // import type * as A from 'A';
            // import type {a} from 'A';
            expectContextual(ContextualKeyword._type)
        }
        // Otherwise, we are importing the name "type".
    }

    // import '...'
    if (match(tt.string)) {
        parseExprAtom()
    } else {
        parseMaybeImportReflection()
        parseImportSpecifiers()
        expectContextual(ContextualKeyword._from)
        parseExprAtom()
    }
    maybeParseImportAttributes()
    semicolon()
}

// eslint-disable-next-line no-unused-vars
function shouldParseDefaultImport(): boolean {
    return match(tt.name)
}

function parseImportSpecifierLocal(): void {
    parseImportedIdentifier()
}

// Parses a comma-separated list of module imports.
function parseImportSpecifiers(): void {
    if (isFlowEnabled) {
        flowStartParseImportSpecifiers()
    }

    let first = true
    if (shouldParseDefaultImport()) {
        // import defaultObj, { x, y as z } from '...'
        parseImportSpecifierLocal()

        if (!eat(tt.comma)) return
    }

    if (match(tt.star)) {
        next()
        expectContextual(ContextualKeyword._as)

        parseImportSpecifierLocal()

        return
    }

    expect(tt.braceL)
    while (!eat(tt.braceR) && !state.error) {
        if (first) {
            first = false
        } else {
            // Detect an attempt to deep destructure
            if (eat(tt.colon)) {
                unexpected(
                    "ES2015 named imports do not destructure. Use another statement for destructuring after the import.",
                )
            }

            expect(tt.comma)
            if (eat(tt.braceR)) {
                break
            }
        }

        parseImportSpecifier()
    }
}

function parseImportSpecifier(): void {
    if (isTypeScriptEnabled) {
        tsParseImportSpecifier()
        return
    }
    if (isFlowEnabled) {
        flowParseImportSpecifier()
        return
    }
    parseImportedIdentifier()
    if (isContextual(ContextualKeyword._as)) {
        state.tokens[state.tokens.length - 1].identifierRole = IdentifierRole.ImportAccess
        next()
        parseImportedIdentifier()
    }
}

/**
 * Parse import attributes like `with {type: "json"}`, or the legacy form
 * `assert {type: "json"}`.
 *
 * Import attributes technically have their own syntax, but are always parseable
 * as a plain JS object, so just do that for simplicity.
 */
function maybeParseImportAttributes(): void {
    if (match(tt._with) || (isContextual(ContextualKeyword._assert) && !hasPrecedingLineBreak())) {
        next()
        parseObj(false, false)
    }
}
// #endregion

// #region base.ts -------------------------------------------------------------
export function getNextContextId(): number {
    return nextContextId++
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function augmentError(error: any): any {
    if ("pos" in error) {
        const loc = locationForIndex(error.pos)
        error.message += ` (${loc.line}:${loc.column})`
        error.loc = loc
    }
    return error
}

export class Loc {
    line: number
    column: number
    constructor(line: number, column: number) {
        this.line = line
        this.column = column
    }
}

export function locationForIndex(pos: number): Loc {
    let line = 1
    let column = 1
    for (let i = 0; i < pos; i++) {
        if (input.charCodeAt(i) === charCodes.lineFeed) {
            line++
            column = 1
        } else {
            column++
        }
    }
    return new Loc(line, column)
}

export function initParser(
    inputCode: string,
    isJSXEnabledArg: boolean,
    isTypeScriptEnabledArg: boolean,
    isFlowEnabledArg: boolean,
): void {
    input = inputCode
    state = new State()
    nextContextId = 1
    isJSXEnabled = isJSXEnabledArg
    isTypeScriptEnabled = isTypeScriptEnabledArg
    isFlowEnabled = isFlowEnabledArg
}
// #endregion

// #region index.ts -------------------------------------------------------------
export function parseFile(): File {
    // If enabled, skip leading hashbang line.
    if (
        state.pos === 0 &&
        input.charCodeAt(0) === charCodes.numberSign &&
        input.charCodeAt(1) === charCodes.exclamationMark
    ) {
        skipLineComment(2)
    }
    nextToken()
    return parseTopLevel()
}
// #endregion
