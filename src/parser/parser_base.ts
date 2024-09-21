import { Scope, State } from './state'
import { TokenType } from './generated/types'
import { ContextualKeyword } from './keywords'
import { IdentifierRole, JSXRole, type Token } from './token'
import { Charcode, IS_IDENTIFIER_START } from './charcode'

export class File {
    tokens: Array<Token>
    scopes: Array<Scope>

    constructor(tokens: Array<Token>, scopes: Array<Scope>) {
        this.tokens = tokens
        this.scopes = scopes
    }
}

export class StopState {
    stop: boolean
    constructor(stop: boolean) {
        this.stop = stop
    }
}

export class Parser {
    constructor(protected state: State) { }

    // #region jsx.ts ----------------------------------------------------------

    // Parse next token as JSX identifier
    protected jsxParseIdentifier(): void {
        this.state.nextJSXTagToken()
    }

    // Parse namespaced identifier.
    protected jsxParseNamespacedName(identifierRole: IdentifierRole): void {
        this.jsxParseIdentifier()
        if (!this.state.eat(TokenType.colon)) {
            // Plain identifier, so this is an access.
            this.state.tokens[this.state.tokens.length - 1].identifierRole = identifierRole
            return
        }
        // Process the second half of the namespaced name.
        this.jsxParseIdentifier()
    }

    // Parses element name in any form - namespaced, member
    // or single identifier.
    protected jsxParseElementName(): void {
        const firstTokenIndex = this.state.tokens.length
        this.jsxParseNamespacedName(IdentifierRole.Access)
        let hadDot = false
        while (this.state.match(TokenType.dot)) {
            hadDot = true
            this.state.nextJSXTagToken()
            this.jsxParseIdentifier()
        }
        // For tags like <div> with a lowercase letter and no dots, the name is
        // actually *not* an identifier access, since it's referring to a built-in
        // tag name. Remove the identifier role in this case so that it's not
        // accidentally transformed by the imports transform when preserving JSX.
        if (!hadDot) {
            const firstToken = this.state.tokens[firstTokenIndex]
            const firstChar = this.state.input.charCodeAt(firstToken.start)
            if (firstChar >= Charcode.lowercaseA && firstChar <= Charcode.lowercaseZ) {
                firstToken.identifierRole = null
            }
        }
    }

    // Parses any type of JSX attribute value.
    protected jsxParseAttributeValue(): void {
        switch (this.state.type) {
            case TokenType.braceL:
                this.state.next()
                this.parseExpression()
                this.state.nextJSXTagToken()
                return

            case TokenType.jsxTagStart:
                this.jsxParseElement()
                this.state.nextJSXTagToken()
                return

            case TokenType.string:
                this.state.nextJSXTagToken()
                return

            default:
                this.state.unexpected("JSX value should be either an expression or a quoted JSX text")
        }
    }

    // Parse JSX spread child, after already processing the {
    // Does not parse the closing }
    protected jsxParseSpreadChild(): void {
        this.state.expect(TokenType.ellipsis)
        this.parseExpression()
    }

    // Parses JSX opening tag starting after "<".
    // Returns true if the tag was self-closing.
    // Does not parse the last token.
    protected jsxParseOpeningElement(initialTokenIndex: number): boolean {
        if (this.state.match(TokenType.jsxTagEnd)) {
            // This is an open-fragment.
            return false
        }
        this.jsxParseElementName()
        let hasSeenPropSpread = false
        while (!this.state.match(TokenType.slash) && !this.state.match(TokenType.jsxTagEnd) && !this.state.error) {
            if (this.state.eat(TokenType.braceL)) {
                hasSeenPropSpread = true
                this.state.expect(TokenType.ellipsis)
                this.parseMaybeAssign()
                // }
                this.state.nextJSXTagToken()
                continue
            }
            if (
                hasSeenPropSpread &&
                this.state.end - this.state.start === 3 &&
                this.state.input.charCodeAt(this.state.start) === Charcode.lowercaseK &&
                this.state.input.charCodeAt(this.state.start + 1) === Charcode.lowercaseE &&
                this.state.input.charCodeAt(this.state.start + 2) === Charcode.lowercaseY
            ) {
                this.state.tokens[initialTokenIndex].jsxRole = JSXRole.KeyAfterPropSpread
            }
            this.jsxParseNamespacedName(IdentifierRole.ObjectKey)
            if (this.state.match(TokenType.eq)) {
                this.state.nextJSXTagToken()
                this.jsxParseAttributeValue()
            }
        }
        const isSelfClosing = this.state.match(TokenType.slash)
        if (isSelfClosing) {
            // /
            this.state.nextJSXTagToken()
        }
        return isSelfClosing
    }

    // Parses JSX closing tag starting after "</".
    // Does not parse the last token.
    protected jsxParseClosingElement(): void {
        if (this.state.match(TokenType.jsxTagEnd)) {
            // Fragment syntax, so we immediately have a tag end.
            return
        }
        this.jsxParseElementName()
    }

    // Parses entire JSX element, including its opening tag
    // (starting after "<"), attributes, contents and closing tag.
    // Does not parse the last token.
    protected jsxParseElementAt(): void {
        const initialTokenIndex = this.state.tokens.length - 1
        this.state.tokens[initialTokenIndex].jsxRole = JSXRole.NoChildren
        let numExplicitChildren = 0
        const isSelfClosing = this.jsxParseOpeningElement(initialTokenIndex)
        if (!isSelfClosing) {
            this.state.nextJSXExprToken()
            while (true) {
                switch (this.state.type) {
                    case TokenType.jsxTagStart:
                        this.state.nextJSXTagToken()
                        if (this.state.match(TokenType.slash)) {
                            this.state.nextJSXTagToken()
                            this.jsxParseClosingElement()
                            // Key after prop spread takes precedence over number of children,
                            // since it means we switch to createElement, which doesn't care
                            // about number of children.
                            if (this.state.tokens[initialTokenIndex].jsxRole !== JSXRole.KeyAfterPropSpread) {
                                if (numExplicitChildren === 1) {
                                    this.state.tokens[initialTokenIndex].jsxRole = JSXRole.OneChild
                                } else if (numExplicitChildren > 1) {
                                    this.state.tokens[initialTokenIndex].jsxRole = JSXRole.StaticChildren
                                }
                            }
                            return
                        }
                        numExplicitChildren++
                        this.jsxParseElementAt()
                        this.state.nextJSXExprToken()
                        break

                    case TokenType.jsxText:
                        numExplicitChildren++
                        this.state.nextJSXExprToken()
                        break

                    case TokenType.jsxEmptyText:
                        this.state.nextJSXExprToken()
                        break

                    case TokenType.braceL:
                        this.state.next()
                        if (this.state.match(TokenType.ellipsis)) {
                            this.jsxParseSpreadChild()
                            this.state.nextJSXExprToken()
                            // Spread children are a mechanism to explicitly mark children as
                            // static, so count it as 2 children to satisfy the "more than one
                            // child" condition.
                            numExplicitChildren += 2
                        } else {
                            // If we see {}, this is an empty pseudo-expression that doesn't
                            // count as a child.
                            if (!this.state.match(TokenType.braceR)) {
                                numExplicitChildren++
                                this.parseExpression()
                            }
                            this.state.nextJSXExprToken()
                        }

                        break

                    // istanbul ignore next - should never happen
                    default:
                        this.state.unexpected()
                        return
                }
            }
        }
    }

    // Parses entire JSX element from current position.
    // Does not parse the last token.
    protected jsxParseElement(): void {
        this.state.nextJSXTagToken()
        this.jsxParseElementAt()
    }

    // #endregion

    // #region lval.ts ---------------------------------------------------------
    protected parseSpread(): void {
        this.state.next()
        this.parseMaybeAssign(false)
    }

    protected parseRest(isBlockScope: boolean): void {
        this.state.next()
        this.parseBindingAtom(isBlockScope)
    }

    protected parseBindingIdentifier(isBlockScope: boolean): void {
        this.parseIdentifier()
        this.markPriorBindingIdentifier(isBlockScope)
    }

    protected parseImportedIdentifier(): void {
        this.parseIdentifier()
        this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportDeclaration
    }

    protected markPriorBindingIdentifier(isBlockScope: boolean): void {
        let identifierRole
        if (this.state.scopeDepth === 0) {
            identifierRole = IdentifierRole.TopLevelDeclaration
        } else if (isBlockScope) {
            identifierRole = IdentifierRole.BlockScopedDeclaration
        } else {
            identifierRole = IdentifierRole.FunctionScopedDeclaration
        }
        this.state.tokens[this.state.tokens.length - 1].identifierRole = identifierRole
    }

    // Parses lvalue (assignable) atom.
    protected parseBindingAtom(isBlockScope: boolean): void {
        switch (this.state.type) {
            case TokenType._this: {
                // In TypeScript, "this" may be the name of a parameter, so allow it.
                const oldIsType = this.state.pushTypeContext(0)
                this.state.next()
                this.state.popTypeContext(oldIsType)
                return
            }

            case TokenType._yield:
            case TokenType.name: {
                this.state.type = TokenType.name
                this.parseBindingIdentifier(isBlockScope)
                return
            }

            case TokenType.bracketL: {
                this.state.next()
                this.parseBindingList(TokenType.bracketR, isBlockScope, true /* allowEmpty */)
                return
            }

            case TokenType.braceL:
                this.parseObj(true, isBlockScope)
                return

            default:
                this.state.unexpected()
        }
    }

    protected parseBindingList(close: TokenType, isBlockScope: boolean, allowEmpty: boolean = false, allowModifiers: boolean = false, contextId: number = 0): void {
        let first = true

        let hasRemovedComma = false
        const firstItemTokenIndex = this.state.tokens.length

        while (!this.state.eat(close) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                this.state.tokens[this.state.tokens.length - 1].contextId = contextId
                // After a "this" type in TypeScript, we need to set the following comma (if any) to also be
                // a type token so that it will be removed.
                if (!hasRemovedComma && this.state.tokens[firstItemTokenIndex].isType) {
                    this.state.tokens[this.state.tokens.length - 1].isType = true
                    hasRemovedComma = true
                }
            }
            if (allowEmpty && this.state.match(TokenType.comma)) {
                // Empty item; nothing further to parse for this item.
            } else if (this.state.eat(close)) {
                break
            } else if (this.state.match(TokenType.ellipsis)) {
                this.parseRest(isBlockScope)
                this.parseAssignableListItemTypes()
                // Support rest element trailing commas allowed by TypeScript <2.9.
                this.state.eat(TokenType.comma)
                this.state.expect(close)
                break
            } else {
                this.parseAssignableListItem(allowModifiers, isBlockScope)
            }
        }
    }

    protected parseAssignableListItem(allowModifiers: boolean, isBlockScope: boolean): void {
        this.parseMaybeDefault(isBlockScope)
        this.parseAssignableListItemTypes()
        this.parseMaybeDefault(isBlockScope, true /* leftAlreadyParsed */)
    }

    protected parseAssignableListItemTypes(): void {
    }

    // Parses assignment pattern around given atom if possible.
    protected parseMaybeDefault(isBlockScope: boolean, leftAlreadyParsed: boolean = false): void {
        if (!leftAlreadyParsed) {
            this.parseBindingAtom(isBlockScope)
        }
        if (!this.state.eat(TokenType.eq)) {
            return
        }
        const eqIndex = this.state.tokens.length - 1
        this.parseMaybeAssign()
        this.state.tokens[eqIndex].rhsEndIndex = this.state.tokens.length
    }
    // #endregion

    // #region expression.ts -------------------------------------------------------

    // These nest, from the most general expression type at the top to
    // 'atomic', nondivisible expression types at the bottom. Most of
    // the functions will simply let the protected (s) below them parse,
    // and, *if* the syntactic construct they handle is present, wrap
    // the AST node that the inner parser gave them in another node.
    protected parseExpression(noIn: boolean = false): void {
        this.parseMaybeAssign(noIn)
        if (this.state.match(TokenType.comma)) {
            while (this.state.eat(TokenType.comma)) {
                this.parseMaybeAssign(noIn)
            }
        }
    }

    /**
     * noIn is used when parsing a for loop so that we don't interpret a following "in" as the binary
     * operatior.
     * isWithinParens is used to indicate that we're parsing something that might be a comma expression
     * or might be an arrow protected or might be a Flow type assertion (which requires explicit parens).
     * In these cases, we should allow : and ?: after the initial "left" part.
     */
    protected parseMaybeAssign(noIn: boolean = false, isWithinParens: boolean = false): boolean {
        return this.baseParseMaybeAssign(noIn, isWithinParens)
    }

    // Parse an assignment expression. This includes applications of
    // operators like `+=`.
    // Returns true if the expression was an arrow function.
    protected baseParseMaybeAssign(noIn: boolean, isWithinParens: boolean): boolean {
        if (this.state.match(TokenType._yield)) {
            this.parseYield()
            return false
        }

        if (this.state.match(TokenType.parenL) || this.state.match(TokenType.name) || this.state.match(TokenType._yield)) {
            this.state.potentialArrowAt = this.state.start
        }

        const wasArrow = this.parseMaybeConditional(noIn)
        if (isWithinParens) {
            this.parseParenItem()
        }
        if (this.state.type & TokenType.IS_ASSIGN) {
            this.state.next()
            this.parseMaybeAssign(noIn)
            return false
        }
        return wasArrow
    }

    // Parse a ternary conditional (`?:`) operator.
    // Returns true if the expression was an arrow function.
    protected parseMaybeConditional(noIn: boolean): boolean {
        const wasArrow = this.parseExprOps(noIn)
        if (wasArrow) {
            return true
        }
        this.parseConditional(noIn)
        return false
    }

    protected parseConditional(noIn: boolean): void {
        this.baseParseConditional(noIn)
    }

    protected baseParseConditional(noIn: boolean): void {
        if (this.state.eat(TokenType.question)) {
            this.parseMaybeAssign()
            this.state.expect(TokenType.colon)
            this.parseMaybeAssign(noIn)
        }
    }

    // Start the precedence parser.
    // Returns true if this was an arrow function
    protected parseExprOps(noIn: boolean): boolean {
        const startTokenIndex = this.state.tokens.length
        const wasArrow = this.parseMaybeUnary()
        if (wasArrow) {
            return true
        }
        this.parseExprOp(startTokenIndex, -1, noIn)
        return false
    }

    // Parse binary operators with the operator precedence parsing
    // algorithm. `left` is the left-hand side of the operator.
    // `minPrec` provides context that allows the protected to stop and
    // defer further parser to one of its callers when it encounters an
    // operator that has a lower precedence than the set it is parsing.
    protected parseExprOp(startTokenIndex: number, minPrec: number, noIn: boolean): void {
        const prec = this.state.type & TokenType.PRECEDENCE_MASK
        if (prec > 0 && (!noIn || !this.state.match(TokenType._in))) {
            if (prec > minPrec) {
                const op = this.state.type
                this.state.next()
                if (op === TokenType.nullishCoalescing) {
                    this.state.tokens[this.state.tokens.length - 1].nullishStartIndex = startTokenIndex
                }

                const rhsStartTokenIndex = this.state.tokens.length
                this.parseMaybeUnary()
                // Extend the right operand of this operator if possible.
                this.parseExprOp(rhsStartTokenIndex, op & TokenType.IS_RIGHT_ASSOCIATIVE ? prec - 1 : prec, noIn)
                if (op === TokenType.nullishCoalescing) {
                    this.state.tokens[startTokenIndex].numNullishCoalesceStarts++
                    this.state.tokens[this.state.tokens.length - 1].numNullishCoalesceEnds++
                }
                // Continue with any future operator holding this expression as the left operand.
                this.parseExprOp(startTokenIndex, minPrec, noIn)
            }
        }
    }

    // Parse unary operators, both prefix and postfix.
    // Returns true if this was an arrow function.
    protected parseMaybeUnary(): boolean {
        if (
            this.state.isContextual(ContextualKeyword._module) &&
            this.state.lookaheadCharCode() === Charcode.leftCurlyBrace &&
            !this.state.hasFollowingLineBreak()
        ) {
            this.parseModuleExpression()
            return false
        }
        if (this.state.type & TokenType.IS_PREFIX) {
            this.state.next()
            this.parseMaybeUnary()
            return false
        }

        const wasArrow = this.parseExprSubscripts()
        if (wasArrow) {
            return true
        }
        while (this.state.type & TokenType.IS_POSTFIX && !this.state.canInsertSemicolon()) {
            // The tokenizer calls everything a preincrement, so make it a postincrement when
            // we see it in that context.
            if (this.state.type === TokenType.preIncDec) {
                this.state.type = TokenType.postIncDec
            }
            this.state.next()
        }
        return false
    }

    // Parse call, dot, and `[]`-subscript expressions.
    // Returns true if this was an arrow function.
    protected parseExprSubscripts(): boolean {
        const startTokenIndex = this.state.tokens.length
        const wasArrow = this.parseExprAtom()
        if (wasArrow) {
            return true
        }
        this.parseSubscripts(startTokenIndex)
        // If there was any optional chain operation, the start token would be marked
        // as such, so also mark the end now.
        if (this.state.tokens.length > startTokenIndex && this.state.tokens[startTokenIndex].isOptionalChainStart) {
            this.state.tokens[this.state.tokens.length - 1].isOptionalChainEnd = true
        }
        return false
    }

    protected parseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
        this.baseParseSubscripts(startTokenIndex, noCalls)
    }

    protected baseParseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
        const stopState = new StopState(false)
        do {
            this.parseSubscript(startTokenIndex, noCalls, stopState)
        } while (!stopState.stop && !this.state.error)
    }

    protected parseSubscript(startTokenIndex: number, noCalls: boolean, stopState: StopState): void {
        this.baseParseSubscript(startTokenIndex, noCalls, stopState)
    }

    /** Set 'this.state.stop = true' to indicate that we should stop parsing subscripts. */
    protected baseParseSubscript(
        startTokenIndex: number,
        noCalls: boolean,
        stopState: StopState,
    ): void {
        if (!noCalls && this.state.eat(TokenType.doubleColon)) {
            this.parseNoCallExpr()
            stopState.stop = true
            // Propagate startTokenIndex so that `a::b?.()` will keep `a` as the first token. We may want
            // to revisit this in the future when fully supporting bind syntax.
            this.parseSubscripts(startTokenIndex, noCalls)
        } else if (this.state.match(TokenType.questionDot)) {
            this.state.tokens[startTokenIndex].isOptionalChainStart = true
            if (noCalls && this.state.lookaheadType() === TokenType.parenL) {
                stopState.stop = true
                return
            }
            this.state.next()
            this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex

            if (this.state.eat(TokenType.bracketL)) {
                this.parseExpression()
                this.state.expect(TokenType.bracketR)
            } else if (this.state.eat(TokenType.parenL)) {
                this.parseCallExpressionArguments()
            } else {
                this.parseMaybePrivateName()
            }
        } else if (this.state.eat(TokenType.dot)) {
            this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex
            this.parseMaybePrivateName()
        } else if (this.state.eat(TokenType.bracketL)) {
            this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex
            this.parseExpression()
            this.state.expect(TokenType.bracketR)
        } else if (!noCalls && this.state.match(TokenType.parenL)) {
            if (this.atPossibleAsync()) {
                // We see "async", but it's possible it's a usage of the name "async". Parse as if it's a
                // protected call, and if we see an arrow later, backtrack and re-parse as a parameter list.
                const snapshot = this.state.snapshot()
                const asyncStartTokenIndex = this.state.tokens.length
                this.state.next()
                this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex

                const callContextId = this.state.getNextContextId()

                this.state.tokens[this.state.tokens.length - 1].contextId = callContextId
                this.parseCallExpressionArguments()
                this.state.tokens[this.state.tokens.length - 1].contextId = callContextId

                if (this.shouldParseAsyncArrow()) {
                    // We hit an arrow, so backtrack and start again parsing protected parameters.
                    this.state.restoreFromSnapshot(snapshot)
                    stopState.stop = true
                    this.state.scopeDepth++

                    this.parseFunctionParams()
                    this.parseAsyncArrowFromCallExpression(asyncStartTokenIndex)
                }
            } else {
                this.state.next()
                this.state.tokens[this.state.tokens.length - 1].subscriptStartIndex = startTokenIndex
                const callContextId = this.state.getNextContextId()
                this.state.tokens[this.state.tokens.length - 1].contextId = callContextId
                this.parseCallExpressionArguments()
                this.state.tokens[this.state.tokens.length - 1].contextId = callContextId
            }
        } else if (this.state.match(TokenType.backQuote)) {
            // Tagged template expression.
            this.parseTemplate()
        } else {
            stopState.stop = true
        }
    }

    protected atPossibleAsync(): boolean {
        // This was made less strict than the original version to avoid passing around nodes, but it
        // should be safe to have rare false positives here.
        return (
            this.state.tokens[this.state.tokens.length - 1].contextualKeyword === ContextualKeyword._async &&
            !this.state.canInsertSemicolon()
        )
    }

    protected parseCallExpressionArguments(): void {
        let first = true
        while (!this.state.eat(TokenType.parenR) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                if (this.state.eat(TokenType.parenR)) {
                    break
                }
            }

            this.parseExprListItem(false)
        }
    }

    protected shouldParseAsyncArrow(): boolean {
        return this.state.match(TokenType.colon) || this.state.match(TokenType.arrow)
    }

    protected parseAsyncArrowFromCallExpression(startTokenIndex: number): void {
        this.baseParseAsyncArrowFromCallExpression(startTokenIndex)
    }

    protected baseParseAsyncArrowFromCallExpression(startTokenIndex: number): void {
        this.state.expect(TokenType.arrow)
        this.parseArrowExpression(startTokenIndex)
    }

    // Parse a no-call expression (like argument of `new` or `::` operators).

    protected parseNoCallExpr(): void {
        const startTokenIndex = this.state.tokens.length
        this.parseExprAtom()
        this.parseSubscripts(startTokenIndex, true)
    }

    // Parse an atomic expression — either a single token that is an
    // expression, an expression started by a keyword like `function` or
    // `new`, or an expression wrapped in punctuation like `()`, `[]`,
    // or `{}`.
    // Returns true if the parsed expression was an arrow function.
    protected parseExprAtom(): boolean {
        if (this.state.eat(TokenType.modulo)) {
            // V8 intrinsic expression. Just parse the identifier, and the protected invocation is parsed
            // naturally.
            this.parseIdentifier()
            return false
        }

        if (this.state.match(TokenType.jsxText) || this.state.match(TokenType.jsxEmptyText)) {
            this.parseLiteral()
            return false
        } else if (this.state.match(TokenType.lessThan) && this.state.isJSXEnabled) {
            this.state.type = TokenType.jsxTagStart
            this.jsxParseElement()
            this.state.next()
            return false
        }

        const canBeArrow = this.state.potentialArrowAt === this.state.start
        switch (this.state.type) {
            case TokenType.slash:
            case TokenType.assign:
                this.state.retokenizeSlashAsRegex()
            // Fall through.

            case TokenType._super:
            case TokenType._this:
            case TokenType.regexp:
            case TokenType.num:
            case TokenType.bigint:
            case TokenType.decimal:
            case TokenType.string:
            case TokenType._null:
            case TokenType._true:
            case TokenType._false:
                this.state.next()
                return false

            case TokenType._import:
                this.state.next()
                if (this.state.match(TokenType.dot)) {
                    // import.meta
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType.name
                    this.state.next()
                    this.parseIdentifier()
                }
                return false

            case TokenType.name: {
                const startTokenIndex = this.state.tokens.length
                const functionStart = this.state.start
                const contextualKeyword = this.state.contextualKeyword
                this.parseIdentifier()
                if (contextualKeyword === ContextualKeyword._await) {
                    this.parseAwait()
                    return false
                } else if (
                    contextualKeyword === ContextualKeyword._async &&
                    this.state.match(TokenType._function) &&
                    !this.state.canInsertSemicolon()
                ) {
                    this.state.next()
                    this.parseFunction(functionStart, false)
                    return false
                } else if (
                    canBeArrow &&
                    contextualKeyword === ContextualKeyword._async &&
                    !this.state.canInsertSemicolon() &&
                    this.state.match(TokenType.name)
                ) {
                    this.state.scopeDepth++
                    this.parseBindingIdentifier(false)
                    this.state.expect(TokenType.arrow)
                    // let foo = async bar => {};
                    this.parseArrowExpression(startTokenIndex)
                    return true
                } else if (this.state.match(TokenType._do) && !this.state.canInsertSemicolon()) {
                    this.state.next()
                    this.parseBlock()
                    return false
                }

                if (canBeArrow && !this.state.canInsertSemicolon() && this.state.match(TokenType.arrow)) {
                    this.state.scopeDepth++
                    this.markPriorBindingIdentifier(false)
                    this.state.expect(TokenType.arrow)
                    this.parseArrowExpression(startTokenIndex)
                    return true
                }

                this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.Access
                return false
            }

            case TokenType._do: {
                this.state.next()
                this.parseBlock()
                return false
            }

            case TokenType.parenL: {
                const wasArrow = this.parseParenAndDistinguishExpression(canBeArrow)
                return wasArrow
            }

            case TokenType.bracketL:
                this.state.next()
                this.parseExprList(TokenType.bracketR, true)
                return false

            case TokenType.braceL:
                this.parseObj(false, false)
                return false

            case TokenType._function:
                this.parseFunctionExpression()
                return false

            case TokenType.at:
                this.parseDecorators()
            // Fall through.

            case TokenType._class:
                this.parseClass(false)
                return false

            case TokenType._new:
                this.parseNew()
                return false

            case TokenType.backQuote:
                this.parseTemplate()
                return false

            case TokenType.doubleColon: {
                this.state.next()
                this.parseNoCallExpr()
                return false
            }

            case TokenType.hash: {
                const code = this.state.lookaheadCharCode()
                if (IS_IDENTIFIER_START[code] || code === Charcode.backslash) {
                    this.parseMaybePrivateName()
                } else {
                    this.state.next()
                }
                // Smart pipeline topic reference.
                return false
            }

            default:
                this.state.unexpected()
                return false
        }
    }

    protected parseMaybePrivateName(): void {
        this.state.eat(TokenType.hash)
        this.parseIdentifier()
    }

    protected parseFunctionExpression(): void {
        const functionStart = this.state.start
        this.parseIdentifier()
        if (this.state.eat(TokenType.dot)) {
            // function.sent
            this.parseIdentifier()
        }
        this.parseFunction(functionStart, false)
    }

    protected parseLiteral(): void {
        this.state.next()
    }

    protected parseParenExpression(): void {
        this.state.expect(TokenType.parenL)
        this.parseExpression()
        this.state.expect(TokenType.parenR)
    }

    // Returns true if this was an arrow expression.
    protected parseParenAndDistinguishExpression(canBeArrow: boolean): boolean {
        // Assume this is a normal parenthesized expression, but if we see an arrow, we'll bail and
        // start over as a parameter list.
        const snapshot = this.state.snapshot()

        const startTokenIndex = this.state.tokens.length
        this.state.expect(TokenType.parenL)

        let first = true

        while (!this.state.match(TokenType.parenR) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                if (this.state.match(TokenType.parenR)) {
                    break
                }
            }

            if (this.state.match(TokenType.ellipsis)) {
                this.parseRest(false /* isBlockScope */)
                this.parseParenItem()
                break
            } else {
                this.parseMaybeAssign(false, true)
            }
        }

        this.state.expect(TokenType.parenR)

        if (canBeArrow && this.shouldParseArrow()) {
            const wasArrow = this.parseArrow()
            if (wasArrow) {
                // It was an arrow protected this whole time, so start over and parse it as params so that we
                // get proper token annotations.
                this.state.restoreFromSnapshot(snapshot)
                this.state.scopeDepth++
                // Don't specify a context ID because arrow functions don't need a context ID.
                this.parseFunctionParams()
                this.parseArrow()
                this.parseArrowExpression(startTokenIndex)
                if (this.state.error) {
                    // Nevermind! This must have been something that looks very much like an
                    // arrow protected but where its "parameter list" isn't actually a valid
                    // parameter list. Force non-arrow parsing.
                    // See https://github.com/alangpierce/sucrase/issues/666 for an example.
                    this.state.restoreFromSnapshot(snapshot)
                    this.parseParenAndDistinguishExpression(false)
                    return false
                }
                return true
            }
        }

        return false
    }

    protected shouldParseArrow(): boolean {
        return this.state.match(TokenType.colon) || !this.state.canInsertSemicolon()
    }

    // Returns whether there was an arrow token.
    protected parseArrow(): boolean {
        return this.state.eat(TokenType.arrow)
    }

    protected parseParenItem(): void {
    }

    // New's precedence is slightly tricky. It must allow its argument to
    // be a `[]` or dot subscript expression, but not a call — at least,
    // not without wrapping it in parentheses. Thus, it uses the noCalls
    // argument to parseSubscripts to prevent it from consuming the
    // argument list.
    protected parseNew(): void {
        this.state.expect(TokenType._new)
        if (this.state.eat(TokenType.dot)) {
            // new.target
            this.parseIdentifier()
            return
        }
        this.parseNewCallee()
        this.parseNewArguments()
        if (this.state.eat(TokenType.parenL)) {
            this.parseExprList(TokenType.parenR)
        }
    }

    protected parseNewArguments() {
    }

    protected parseNewCallee(): void {
        this.parseNoCallExpr()
        this.state.eat(TokenType.questionDot)
    }

    protected parseTemplate(): void {
        // Finish `, read quasi
        this.state.nextTemplateToken()
        // Finish quasi, read ${
        this.state.nextTemplateToken()
        while (!this.state.match(TokenType.backQuote) && !this.state.error) {
            this.state.expect(TokenType.dollarBraceL)
            this.parseExpression()
            // Finish }, read quasi
            this.state.nextTemplateToken()
            // Finish quasi, read either ${ or `
            this.state.nextTemplateToken()
        }
        this.state.next()
    }

    // Parse an object literal or binding pattern.
    protected parseObj(isPattern: boolean, isBlockScope: boolean): void {
        // Attach a context ID to the object open and close brace and each object key.
        const contextId = this.state.getNextContextId()
        let first = true

        this.state.next()
        this.state.tokens[this.state.tokens.length - 1].contextId = contextId

        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                if (this.state.eat(TokenType.braceR)) {
                    break
                }
            }

            let isGenerator = false
            if (this.state.match(TokenType.ellipsis)) {
                const previousIndex = this.state.tokens.length
                this.parseSpread()
                if (isPattern) {
                    // Mark role when the only thing being spread over is an identifier.
                    if (this.state.tokens.length === previousIndex + 2) {
                        this.markPriorBindingIdentifier(isBlockScope)
                    }
                    if (this.state.eat(TokenType.braceR)) {
                        break
                    }
                }
                continue
            }

            if (!isPattern) {
                isGenerator = this.state.eat(TokenType.star)
            }

            if (!isPattern && this.state.isContextual(ContextualKeyword._async)) {
                if (isGenerator) this.state.unexpected()

                this.parseIdentifier()
                if (
                    this.state.match(TokenType.colon) ||
                    this.state.match(TokenType.parenL) ||
                    this.state.match(TokenType.braceR) ||
                    this.state.match(TokenType.eq) ||
                    this.state.match(TokenType.comma)
                ) {
                    // This is a key called "async" rather than an async function.
                } else {
                    if (this.state.match(TokenType.star)) {
                        this.state.next()
                        isGenerator = true
                    }
                    this.parsePropertyName(contextId)
                }
            } else {
                this.parsePropertyName(contextId)
            }

            this.parseObjPropValue(isPattern, isBlockScope, contextId)
        }

        this.state.tokens[this.state.tokens.length - 1].contextId = contextId
    }

    protected isGetterOrSetterMethod(isPattern: boolean): boolean {
        // We go off of the next and don't bother checking if the node key is actually "get" or "set".
        // This lets us avoid generating a node, and should only make the validation worse.
        return (
            !isPattern &&
            (this.state.match(TokenType.string) || // get "string"() {}
                this.state.match(TokenType.num) || // get 1() {}
                this.state.match(TokenType.bracketL) || // get ["string"]() {}
                this.state.match(TokenType.name) || // get foo() {}
                !!(this.state.type & TokenType.IS_KEYWORD)) // get debugger() {}
        )
    }

    // Returns true if this was a method.
    protected parseObjectMethod(isPattern: boolean, objectContextId: number): boolean {
        // We don't need to worry about modifiers because object methods can't have optional bodies, so
        // the start will never be used.
        const functionStart = this.state.start
        if (this.state.match(TokenType.parenL)) {
            if (isPattern) this.state.unexpected()
            this.parseMethod(functionStart, /* isConstructor */ false)
            return true
        }

        if (this.isGetterOrSetterMethod(isPattern)) {
            this.parsePropertyName(objectContextId)
            this.parseMethod(functionStart, /* isConstructor */ false)
            return true
        }
        return false
    }

    protected parseObjectProperty(isPattern: boolean, isBlockScope: boolean): void {
        if (this.state.eat(TokenType.colon)) {
            if (isPattern) {
                this.parseMaybeDefault(isBlockScope)
            } else {
                this.parseMaybeAssign(false)
            }
            return
        }

        // Since there's no colon, we assume this is an object shorthand.

        // If we're in a destructuring, we've now discovered that the key was actually an assignee, so
        // we need to tag it as a declaration with the appropriate scope. Otherwise, we might need to
        // transform it on access, so mark it as a normal object shorthand.
        let identifierRole
        if (isPattern) {
            if (this.state.scopeDepth === 0) {
                identifierRole = IdentifierRole.ObjectShorthandTopLevelDeclaration
            } else if (isBlockScope) {
                identifierRole = IdentifierRole.ObjectShorthandBlockScopedDeclaration
            } else {
                identifierRole = IdentifierRole.ObjectShorthandFunctionScopedDeclaration
            }
        } else {
            identifierRole = IdentifierRole.ObjectShorthand
        }
        this.state.tokens[this.state.tokens.length - 1].identifierRole = identifierRole

        // Regardless of whether we know this to be a pattern or if we're in an ambiguous context, allow
        // parsing as if there's a default value.
        this.parseMaybeDefault(isBlockScope, true)
    }

    protected parseObjPropValue(isPattern: boolean, isBlockScope: boolean, objectContextId: number): void {
        this.baseParseObjPropValue(isPattern, isBlockScope, objectContextId)
    }

    protected baseParseObjPropValue(
        isPattern: boolean,
        isBlockScope: boolean,
        objectContextId: number,
    ): void {
        const wasMethod = this.parseObjectMethod(isPattern, objectContextId)
        if (!wasMethod) {
            this.parseObjectProperty(isPattern, isBlockScope)
        }
    }

    protected parsePropertyName(objectContextId: number): void {
        if (this.state.eat(TokenType.bracketL)) {
            this.state.tokens[this.state.tokens.length - 1].contextId = objectContextId
            this.parseMaybeAssign()
            this.state.expect(TokenType.bracketR)
            this.state.tokens[this.state.tokens.length - 1].contextId = objectContextId
        } else {
            if (this.state.match(TokenType.num) || this.state.match(TokenType.string) || this.state.match(TokenType.bigint) || this.state.match(TokenType.decimal)) {
                this.parseExprAtom()
            } else {
                this.parseMaybePrivateName()
            }

            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ObjectKey
            this.state.tokens[this.state.tokens.length - 1].contextId = objectContextId
        }
    }

    // Parse object or class method.
    protected parseMethod(functionStart: number, isConstructor: boolean): void {
        const funcContextId = this.state.getNextContextId()

        this.state.scopeDepth++
        const startTokenIndex = this.state.tokens.length
        const allowModifiers = isConstructor // For TypeScript parameter properties
        this.parseFunctionParams(allowModifiers, funcContextId)
        this.parseFunctionBodyAndFinish(functionStart, funcContextId)
        const endTokenIndex = this.state.tokens.length
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
        this.state.scopeDepth--
    }

    // Parse arrow protected expression.
    // If the parameters are provided, they will be converted to an
    // assignable list.
    protected parseArrowExpression(startTokenIndex: number): void {
        this.parseFunctionBody(true)
        const endTokenIndex = this.state.tokens.length
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
        this.state.scopeDepth--
    }

    protected parseFunctionBodyAndFinish(functionStart: number, funcContextId: number = 0): void {
        this.parseFunctionBody(false, funcContextId)
    }

    protected parseFunctionBody(allowExpression: boolean, funcContextId: number = 0): void {
        const isExpression = allowExpression && !this.state.match(TokenType.braceL)

        if (isExpression) {
            this.parseMaybeAssign()
        } else {
            this.parseBlock(true /* isFunctionScope */, funcContextId)
        }
    }

    // Parses a comma-separated list of expressions, and returns them as
    // an array. `close` is the token type that ends the list, and
    // `allowEmpty` can be turned on to allow subsequent commas with
    // nothing in between them to be parsed as `null` (which is needed
    // for array literals).

    protected parseExprList(close: TokenType, allowEmpty: boolean = false): void {
        let first = true
        while (!this.state.eat(close) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                if (this.state.eat(close)) break
            }
            this.parseExprListItem(allowEmpty)
        }
    }

    protected parseExprListItem(allowEmpty: boolean): void {
        if (allowEmpty && this.state.match(TokenType.comma)) {
            // Empty item; nothing more to parse for this item.
        } else if (this.state.match(TokenType.ellipsis)) {
            this.parseSpread()
            this.parseParenItem()
        } else if (this.state.match(TokenType.question)) {
            // Partial protected application proposal.
            this.state.next()
        } else {
            this.parseMaybeAssign(false, true)
        }
    }

    // Parse the next token as an identifier.
    protected parseIdentifier(): void {
        this.state.next()
        this.state.tokens[this.state.tokens.length - 1].type = TokenType.name
    }

    // Parses await expression inside async function.
    protected parseAwait(): void {
        this.parseMaybeUnary()
    }

    // Parses yield expression inside generator.
    protected parseYield(): void {
        this.state.next()
        if (!this.state.match(TokenType.semi) && !this.state.canInsertSemicolon()) {
            this.state.eat(TokenType.star)
            this.parseMaybeAssign()
        }
    }

    // https://github.com/tc39/proposal-js-module-blocks
    protected parseModuleExpression(): void {
        this.state.expectContextual(ContextualKeyword._module)
        this.state.expect(TokenType.braceL)
        // For now, just call parseBlockBody to parse the block. In the future when we
        // implement full support, we'll want to emit scopes and possibly other
        // information.
        this.parseBlockBody(TokenType.braceR)
    }
    // #endregion

    // #region statement.ts --------------------------------------------------------

    // Parse a single statement.
    //
    // If expecting a statement and finding a slash operator, parse a
    // regular expression literal. This is to handle cases like
    // `if (foo) /blah/.exec(foo)`, where looking at the previous token
    // does not help.
    protected parseStatement(declaration: boolean): void {
        if (this.state.match(TokenType.at)) {
            this.parseDecorators()
        }
        this.parseStatementContent(declaration)
    }

    protected parseStatementContent(declaration: boolean): void {
        const starttype = this.state.type

        // Most types of statements are recognized by the keyword they
        // start with. Many are trivial to parse, some require a bit of
        // complexity.

        switch (starttype) {
            case TokenType._break:
            case TokenType._continue:
                this.parseBreakContinueStatement()
                return
            case TokenType._debugger:
                this.parseDebuggerStatement()
                return
            case TokenType._do:
                this.parseDoStatement()
                return
            case TokenType._for:
                this.parseForStatement()
                return
            case TokenType._function:
                if (this.state.lookaheadType() === TokenType.dot) break
                if (!declaration) this.state.unexpected()
                this.parseFunctionStatement()
                return

            case TokenType._class:
                if (!declaration) this.state.unexpected()
                this.parseClass(true)
                return

            case TokenType._if:
                this.parseIfStatement()
                return
            case TokenType._return:
                this.parseReturnStatement()
                return
            case TokenType._switch:
                this.parseSwitchStatement()
                return
            case TokenType._throw:
                this.parseThrowStatement()
                return
            case TokenType._try:
                this.parseTryStatement()
                return

            case TokenType._let:
            case TokenType._const:
                if (!declaration) this.state.unexpected() // NOTE: falls through to _var

            case TokenType._var:
                this.parseVarStatement(starttype !== TokenType._var)
                return

            case TokenType._while:
                this.parseWhileStatement()
                return
            case TokenType.braceL:
                this.parseBlock()
                return
            case TokenType.semi:
                this.parseEmptyStatement()
                return
            case TokenType._export:
            case TokenType._import: {
                const nextType = this.state.lookaheadType()
                if (nextType === TokenType.parenL || nextType === TokenType.dot) {
                    break
                }
                this.state.next()
                if (starttype === TokenType._import) {
                    this.parseImport()
                } else {
                    this.parseExport()
                }
                return
            }
            case TokenType.name:
                if (this.state.contextualKeyword === ContextualKeyword._async) {
                    const functionStart = this.state.start
                    // peek ahead and see if next token is a function
                    const snapshot = this.state.snapshot()
                    this.state.next()
                    if (this.state.match(TokenType._function) && !this.state.canInsertSemicolon()) {
                        this.state.expect(TokenType._function)
                        this.parseFunction(functionStart, true)
                        return
                    } else {
                        this.state.restoreFromSnapshot(snapshot)
                    }
                } else if (
                    this.state.contextualKeyword === ContextualKeyword._using &&
                    !this.state.hasFollowingLineBreak() &&
                    // Statements like `using[0]` and `using in foo` aren't actual using
                    // declarations.
                    this.state.lookaheadType() === TokenType.name
                ) {
                    this.parseVarStatement(true)
                    return
                } else if (this.startsAwaitUsing()) {
                    this.state.expectContextual(ContextualKeyword._await)
                    this.parseVarStatement(true)
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
        const initialTokensLength = this.state.tokens.length
        this.parseExpression()
        let simpleName = null
        if (this.state.tokens.length === initialTokensLength + 1) {
            const token = this.state.tokens[this.state.tokens.length - 1]
            if (token.type === TokenType.name) {
                simpleName = token.contextualKeyword
            }
        }
        if (simpleName == null) {
            this.state.semicolon()
            return
        }
        if (this.state.eat(TokenType.colon)) {
            this.parseLabeledStatement()
        } else {
            // This was an identifier, so we might want to handle flow/typescript-specific cases.
            this.parseIdentifierStatement(simpleName)
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
    protected startsAwaitUsing(): boolean {
        if (!this.state.isContextual(ContextualKeyword._await)) {
            return false
        }
        const snapshot = this.state.snapshot()
        // await
        this.state.next()
        if (!this.state.isContextual(ContextualKeyword._using) || this.state.hasPrecedingLineBreak()) {
            this.state.restoreFromSnapshot(snapshot)
            return false
        }
        // using
        this.state.next()
        if (!this.state.match(TokenType.name) || this.state.hasPrecedingLineBreak()) {
            this.state.restoreFromSnapshot(snapshot)
            return false
        }
        this.state.restoreFromSnapshot(snapshot)
        return true
    }

    protected parseDecorators(): void {
        while (this.state.match(TokenType.at)) {
            this.parseDecorator()
        }
    }

    protected parseDecorator(): void {
        this.state.next()
        if (this.state.eat(TokenType.parenL)) {
            this.parseExpression()
            this.state.expect(TokenType.parenR)
        } else {
            this.parseIdentifier()
            while (this.state.eat(TokenType.dot)) {
                this.parseIdentifier()
            }
            this.parseMaybeDecoratorArguments()
        }
    }

    protected parseMaybeDecoratorArguments(): void {
        this.baseParseMaybeDecoratorArguments()
    }

    protected baseParseMaybeDecoratorArguments(): void {
        if (this.state.eat(TokenType.parenL)) {
            this.parseCallExpressionArguments()
        }
    }

    protected parseBreakContinueStatement(): void {
        this.state.next()
        if (!this.state.isLineTerminator()) {
            this.parseIdentifier()
            this.state.semicolon()
        }
    }

    protected parseDebuggerStatement(): void {
        this.state.next()
        this.state.semicolon()
    }

    protected parseDoStatement(): void {
        this.state.next()
        this.parseStatement(false)
        this.state.expect(TokenType._while)
        this.parseParenExpression()
        this.state.eat(TokenType.semi)
    }

    protected parseForStatement(): void {
        this.state.scopeDepth++
        const startTokenIndex = this.state.tokens.length
        this.parseAmbiguousForStatement()
        const endTokenIndex = this.state.tokens.length
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, false))
        this.state.scopeDepth--
    }

    /**
     * Determine if this token is a `using` declaration (explicit resource
     * management) as part of a loop.
     * https://github.com/tc39/proposal-explicit-resource-management
     */
    protected isUsingInLoop(): boolean {
        if (!this.state.isContextual(ContextualKeyword._using)) {
            return false
        }
        // This must be `for (using of`, where `using` is the name of the loop
        // variable.
        if (this.state.isLookaheadContextual(ContextualKeyword._of)) {
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
    protected parseAmbiguousForStatement(): void {
        this.state.next()

        let forAwait = false
        if (this.state.isContextual(ContextualKeyword._await)) {
            forAwait = true
            this.state.next()
        }
        this.state.expect(TokenType.parenL)

        if (this.state.match(TokenType.semi)) {
            if (forAwait) {
                this.state.unexpected()
            }
            this.parseFor()
            return
        }

        const isAwaitUsing = this.startsAwaitUsing()
        if (isAwaitUsing || this.state.match(TokenType._var) || this.state.match(TokenType._let) || this.state.match(TokenType._const) || this.isUsingInLoop()) {
            if (isAwaitUsing) {
                this.state.expectContextual(ContextualKeyword._await)
            }
            this.state.next()
            this.parseVar(true, this.state.type !== TokenType._var)
            if (this.state.match(TokenType._in) || this.state.isContextual(ContextualKeyword._of)) {
                this.parseForIn(forAwait)
                return
            }
            this.parseFor()
            return
        }

        this.parseExpression(true)
        if (this.state.match(TokenType._in) || this.state.isContextual(ContextualKeyword._of)) {
            this.parseForIn(forAwait)
            return
        }
        if (forAwait) {
            this.state.unexpected()
        }
        this.parseFor()
    }

    protected parseFunctionStatement(): void {
        const functionStart = this.state.start
        this.state.next()
        this.parseFunction(functionStart, true)
    }

    protected parseIfStatement(): void {
        this.state.next()
        this.parseParenExpression()
        this.parseStatement(false)
        if (this.state.eat(TokenType._else)) {
            this.parseStatement(false)
        }
    }

    protected parseReturnStatement(): void {
        this.state.next()

        // In `return` (and `break`/`continue`), the keywords with
        // optional arguments, we eagerly look for a semicolon or the
        // possibility to insert one.

        if (!this.state.isLineTerminator()) {
            this.parseExpression()
            this.state.semicolon()
        }
    }

    protected parseSwitchStatement(): void {
        this.state.next()
        this.parseParenExpression()
        this.state.scopeDepth++
        const startTokenIndex = this.state.tokens.length
        this.state.expect(TokenType.braceL)

        // Don't bother validation; just go through any sequence of cases, defaults, and statements.
        while (!this.state.match(TokenType.braceR) && !this.state.error) {
            if (this.state.match(TokenType._case) || this.state.match(TokenType._default)) {
                const isCase = this.state.match(TokenType._case)
                this.state.next()
                if (isCase) {
                    this.parseExpression()
                }
                this.state.expect(TokenType.colon)
            } else {
                this.parseStatement(true)
            }
        }
        this.state.next() // Closing brace
        const endTokenIndex = this.state.tokens.length
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, false))
        this.state.scopeDepth--
    }

    protected parseThrowStatement(): void {
        this.state.next()
        this.parseExpression()
        this.state.semicolon()
    }

    protected parseCatchClauseParam(): void {
        this.parseBindingAtom(true /* isBlockScope */)
    }

    protected parseTryStatement(): void {
        this.state.next()

        this.parseBlock()

        if (this.state.match(TokenType._catch)) {
            this.state.next()
            let catchBindingStartTokenIndex = null
            if (this.state.match(TokenType.parenL)) {
                this.state.scopeDepth++
                catchBindingStartTokenIndex = this.state.tokens.length
                this.state.expect(TokenType.parenL)
                this.parseCatchClauseParam()
                this.state.expect(TokenType.parenR)
            }
            this.parseBlock()
            if (catchBindingStartTokenIndex != null) {
                // We need a special scope for the catch binding which includes the binding itself and the
                // catch block.
                const endTokenIndex = this.state.tokens.length
                this.state.scopes.push(new Scope(catchBindingStartTokenIndex, endTokenIndex, false))
                this.state.scopeDepth--
            }
        }
        if (this.state.eat(TokenType._finally)) {
            this.parseBlock()
        }
    }

    protected parseVarStatement(isBlockScope: boolean): void {
        this.state.next()
        this.parseVar(false, isBlockScope)
        this.state.semicolon()
    }

    protected parseWhileStatement(): void {
        this.state.next()
        this.parseParenExpression()
        this.parseStatement(false)
    }

    protected parseEmptyStatement(): void {
        this.state.next()
    }

    protected parseLabeledStatement(): void {
        this.parseStatement(true)
    }

    /**
     * Parse a statement starting with an identifier of the given name. Subclasses match on the name
     * to handle statements like "declare".
     */
    protected parseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
        this.state.semicolon()
    }

    // Parse a semicolon-enclosed block of statements.
    protected parseBlock(isFunctionScope: boolean = false, contextId: number = 0): void {
        const startTokenIndex = this.state.tokens.length
        this.state.scopeDepth++
        this.state.expect(TokenType.braceL)
        if (contextId) {
            this.state.tokens[this.state.tokens.length - 1].contextId = contextId
        }
        this.parseBlockBody(TokenType.braceR)
        if (contextId) {
            this.state.tokens[this.state.tokens.length - 1].contextId = contextId
        }
        const endTokenIndex = this.state.tokens.length
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, isFunctionScope))
        this.state.scopeDepth--
    }

    protected parseBlockBody(end: TokenType): void {
        while (!this.state.eat(end) && !this.state.error) {
            this.parseStatement(true)
        }
    }

    // Parse a regular `for` loop. The disambiguation code in
    // `parseStatement` will already have parsed the init statement or
    // expression.
    protected parseFor(): void {
        this.state.expect(TokenType.semi)
        if (!this.state.match(TokenType.semi)) {
            this.parseExpression()
        }
        this.state.expect(TokenType.semi)
        if (!this.state.match(TokenType.parenR)) {
            this.parseExpression()
        }
        this.state.expect(TokenType.parenR)
        this.parseStatement(false)
    }

    // Parse a `for`/`in` and `for`/`of` loop, which are almost
    // same from parser's perspective.
    protected parseForIn(forAwait: boolean): void {
        if (forAwait) {
            this.state.eatContextual(ContextualKeyword._of)
        } else {
            this.state.next()
        }
        this.parseExpression()
        this.state.expect(TokenType.parenR)
        this.parseStatement(false)
    }

    // Parse a list of variable declarations.
    protected parseVar(isFor: boolean, isBlockScope: boolean): void {
        while (true) {
            this.parseVarHead(isBlockScope)
            if (this.state.eat(TokenType.eq)) {
                const eqIndex = this.state.tokens.length - 1
                this.parseMaybeAssign(isFor)
                this.state.tokens[eqIndex].rhsEndIndex = this.state.tokens.length
            }
            if (!this.state.eat(TokenType.comma)) {
                break
            }
        }
    }

    protected parseVarHead(isBlockScope: boolean): void {
        this.parseBindingAtom(isBlockScope)
    }

    // Parse a protected declaration or literal (depending on the
    // `isStatement` parameter).
    protected parseFunction(functionStart: number, isStatement: boolean, optionalId: boolean = false): void {
        if (this.state.match(TokenType.star)) {
            this.state.next()
        }

        if (isStatement && !optionalId && !this.state.match(TokenType.name) && !this.state.match(TokenType._yield)) {
            this.state.unexpected()
        }

        let nameScopeStartTokenIndex = null

        if (this.state.match(TokenType.name)) {
            // Expression-style functions should limit their name's scope to the protected body, so we make
            // a new protected scope to enforce that.
            if (!isStatement) {
                nameScopeStartTokenIndex = this.state.tokens.length
                this.state.scopeDepth++
            }
            this.parseBindingIdentifier(false)
        }

        const startTokenIndex = this.state.tokens.length
        this.state.scopeDepth++
        this.parseFunctionParams()
        this.parseFunctionBodyAndFinish(functionStart)
        const endTokenIndex = this.state.tokens.length
        // In addition to the block scope of the protected body, we need a separate function-style scope
        // that includes the params.
        this.state.scopes.push(new Scope(startTokenIndex, endTokenIndex, true))
        this.state.scopeDepth--
        if (nameScopeStartTokenIndex !== null) {
            this.state.scopes.push(new Scope(nameScopeStartTokenIndex, endTokenIndex, true))
            this.state.scopeDepth--
        }
    }

    protected parseFunctionParams(allowModifiers: boolean = false, funcContextId: number = 0): void {
        this.state.expect(TokenType.parenL)
        if (funcContextId) {
            this.state.tokens[this.state.tokens.length - 1].contextId = funcContextId
        }
        this.parseBindingList(
            TokenType.parenR,
            false /* isBlockScope */,
            false /* allowEmpty */,
            allowModifiers,
            funcContextId,
        )
        if (funcContextId) {
            this.state.tokens[this.state.tokens.length - 1].contextId = funcContextId
        }
    }

    // Parse a class declaration or literal (depending on the
    // `isStatement` parameter).

    protected parseClass(isStatement: boolean, optionalId: boolean = false): void {
        // Put a context ID on the class keyword, the open-brace, and the close-brace, so that later
        // code can easily navigate to meaningful points on the class.
        const contextId = this.state.getNextContextId()

        this.state.next()
        this.state.tokens[this.state.tokens.length - 1].contextId = contextId
        this.state.tokens[this.state.tokens.length - 1].isExpression = !isStatement
        // Like with functions, we declare a special "name scope" from the start of the name to the end
        // of the class, but only with expression-style classes, to represent the fact that the name is
        // available to the body of the class but not an outer declaration.
        let nameScopeStartTokenIndex = null
        if (!isStatement) {
            nameScopeStartTokenIndex = this.state.tokens.length
            this.state.scopeDepth++
        }
        this.parseClassId(isStatement, optionalId)
        this.parseClassSuper()
        const openBraceIndex = this.state.tokens.length
        this.parseClassBody(contextId)
        if (this.state.error) {
            return
        }
        this.state.tokens[openBraceIndex].contextId = contextId
        this.state.tokens[this.state.tokens.length - 1].contextId = contextId
        if (nameScopeStartTokenIndex !== null) {
            const endTokenIndex = this.state.tokens.length
            this.state.scopes.push(new Scope(nameScopeStartTokenIndex, endTokenIndex, false))
            this.state.scopeDepth--
        }
    }

    protected isClassProperty(): boolean {
        return this.state.match(TokenType.eq) || this.state.match(TokenType.semi) || this.state.match(TokenType.braceR) || this.state.match(TokenType.bang) || this.state.match(TokenType.colon)
    }

    protected isClassMethod(): boolean {
        return this.state.match(TokenType.parenL) || this.state.match(TokenType.lessThan)
    }

    protected parseClassBody(classContextId: number): void {
        this.state.expect(TokenType.braceL)

        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            if (this.state.eat(TokenType.semi)) {
                continue
            }

            if (this.state.match(TokenType.at)) {
                this.parseDecorator()
                continue
            }
            const memberStart = this.state.start
            this.parseClassMember(memberStart, classContextId)
        }
    }

    protected parseClassMember(memberStart: number, classContextId: number): void {
        let isStatic = false
        if (this.state.match(TokenType.name) && this.state.contextualKeyword === ContextualKeyword._static) {
            this.parseIdentifier() // eats 'static'
            if (this.isClassMethod()) {
                this.parseClassMethod(memberStart, /* isConstructor */ false)
                return
            } else if (this.isClassProperty()) {
                this.parseClassProperty()
                return
            }
            // otherwise something static
            this.state.tokens[this.state.tokens.length - 1].type = TokenType._static
            isStatic = true

            if (this.state.match(TokenType.braceL)) {
                // This is a static block. Mark the word "static" with the class context ID for class element
                // detection and parse as a regular block.
                this.state.tokens[this.state.tokens.length - 1].contextId = classContextId
                this.parseBlock()
                return
            }
        }

        this.parseClassMemberWithIsStatic(memberStart, isStatic, classContextId)
    }

    protected parseClassMemberWithIsStatic(memberStart: number, isStatic: boolean, classContextId: number): void {
        if (this.state.eat(TokenType.star)) {
            // a generator
            this.parseClassPropertyName(classContextId)
            this.parseClassMethod(memberStart, /* isConstructor */ false)
            return
        }

        // Get the identifier name so we can tell if it's actually a keyword like "async", "get", or
        // "set".
        this.parseClassPropertyName(classContextId)
        let isConstructor = false
        const token = this.state.tokens[this.state.tokens.length - 1]
        // We allow "constructor" as either an identifier or a string.
        if (token.contextualKeyword === ContextualKeyword._constructor) {
            isConstructor = true
        }
        this.parsePostMemberNameModifiers()

        if (this.isClassMethod()) {
            this.parseClassMethod(memberStart, isConstructor)
        } else if (this.isClassProperty()) {
            this.parseClassProperty()
        } else if (token.contextualKeyword === ContextualKeyword._async && !this.state.isLineTerminator()) {
            this.state.tokens[this.state.tokens.length - 1].type = TokenType._async
            // an async method
            const isGenerator = this.state.match(TokenType.star)
            if (isGenerator) {
                this.state.next()
            }

            // The so-called parsed name would have been "async": get the real name.
            this.parseClassPropertyName(classContextId)
            this.parsePostMemberNameModifiers()
            this.parseClassMethod(memberStart, false /* isConstructor */)
        } else if (
            (token.contextualKeyword === ContextualKeyword._get ||
                token.contextualKeyword === ContextualKeyword._set) &&
            !(this.state.isLineTerminator() && this.state.match(TokenType.star))
        ) {
            if (token.contextualKeyword === ContextualKeyword._get) {
                this.state.tokens[this.state.tokens.length - 1].type = TokenType._get
            } else {
                this.state.tokens[this.state.tokens.length - 1].type = TokenType._set
            }
            // `get\n*` is an uninitialized property named 'get' followed by a generator.
            // a getter or setter
            // The so-called parsed name would have been "get/set": get the real name.
            this.parseClassPropertyName(classContextId)
            this.parseClassMethod(memberStart, /* isConstructor */ false)
        } else if (token.contextualKeyword === ContextualKeyword._accessor && !this.state.isLineTerminator()) {
            this.parseClassPropertyName(classContextId)
            this.parseClassProperty()
        } else if (this.state.isLineTerminator()) {
            // an uninitialized class property (due to ASI, since we don't otherwise recognize the next token)
            this.parseClassProperty()
        } else {
            this.state.unexpected()
        }
    }

    protected parseClassMethod(functionStart: number, isConstructor: boolean): void {
        this.parseMethod(functionStart, isConstructor)
    }

    // Return the name of the class property, if it is a simple identifier.
    protected parseClassPropertyName(classContextId: number): void {
        this.parsePropertyName(classContextId)
    }

    protected parsePostMemberNameModifiers(): void {
    }

    protected parseClassProperty(): void {
        if (this.state.match(TokenType.eq)) {
            const equalsTokenIndex = this.state.tokens.length
            this.state.next()
            this.parseMaybeAssign()
            this.state.tokens[equalsTokenIndex].rhsEndIndex = this.state.tokens.length
        }
        this.state.semicolon()
    }

    protected parseClassId(isStatement: boolean, optionalId: boolean = false): void {
        if (this.state.match(TokenType.name)) {
            this.parseBindingIdentifier(true)
        }
    }

    // Returns true if there was a superclass.
    protected parseClassSuper(): void {
        let hasSuper = false
        if (this.state.eat(TokenType._extends)) {
            this.parseExprSubscripts()
            hasSuper = true
        } else {
            hasSuper = false
        }
        this.afterParseClassSuper(hasSuper)
    }

    protected afterParseClassSuper(hasSuper: boolean): void {
    }

    // Parses module export declaration.

    protected parseExport(): void {
        const exportIndex = this.state.tokens.length - 1
        if (this.tryParseExport()) {
            return
        }

        // export * from '...'
        if (this.shouldParseExportStar()) {
            this.parseExportStar()
        } else if (this.isExportDefaultSpecifier()) {
            // export default from
            this.parseIdentifier()
            if (this.state.match(TokenType.comma) && this.state.lookaheadType() === TokenType.star) {
                this.state.expect(TokenType.comma)
                this.state.expect(TokenType.star)
                this.state.expectContextual(ContextualKeyword._as)
                this.parseIdentifier()
            } else {
                this.parseExportSpecifiersMaybe()
            }
            this.parseExportFrom()
        } else if (this.state.eat(TokenType._default)) {
            // export default ...
            this.parseExportDefaultExpression()
        } else if (this.shouldParseExportDeclaration()) {
            this.parseExportDeclaration()
        } else {
            // export { x, y as z } [from '...']
            this.parseExportSpecifiers()
            this.parseExportFrom()
        }
        this.state.tokens[exportIndex].rhsEndIndex = this.state.tokens.length
    }

    protected tryParseExport() {
        return false
    }

    protected parseExportDefaultExpression(): void {
        if (this.tryParseExportDefaultExpression())
            return

        const functionStart = this.state.start
        if (this.state.eat(TokenType._function)) {
            this.parseFunction(functionStart, true, true)
        } else if (this.state.isContextual(ContextualKeyword._async) && this.state.lookaheadType() === TokenType._function) {
            // async protected declaration
            this.state.eatContextual(ContextualKeyword._async)
            this.state.eat(TokenType._function)
            this.parseFunction(functionStart, true, true)
        } else if (this.state.match(TokenType._class)) {
            this.parseClass(true, true)
        } else if (this.state.match(TokenType.at)) {
            this.parseDecorators()
            this.parseClass(true, true)
        } else {
            this.parseMaybeAssign()
            this.state.semicolon()
        }
    }

    protected tryParseExportDefaultExpression() {
        return false
    }

    protected parseExportDeclaration(): void {
        this.parseStatement(true)
    }

    protected isExportDefaultSpecifier(): boolean {
        if (this.state.match(TokenType.name)) {
            return this.state.contextualKeyword !== ContextualKeyword._async
        }

        if (!this.state.match(TokenType._default)) {
            return false
        }

        const _next = this.state.nextTokenStart()
        const lookahead = this.state.lookaheadTypeAndKeyword()
        const hasFrom =
            lookahead.type === TokenType.name && lookahead.contextualKeyword === ContextualKeyword._from
        if (lookahead.type === TokenType.comma) {
            return true
        }
        // lookahead again when `export default from` is seen
        if (hasFrom) {
            const nextAfterFrom = this.state.input.charCodeAt(this.state.nextTokenStartSince(_next + 4))
            return nextAfterFrom === Charcode.quotationMark || nextAfterFrom === Charcode.apostrophe
        }
        return false
    }

    protected parseExportSpecifiersMaybe(): void {
        if (this.state.eat(TokenType.comma)) {
            this.parseExportSpecifiers()
        }
    }

    protected parseExportFrom(): void {
        if (this.state.eatContextual(ContextualKeyword._from)) {
            this.parseExprAtom()
            this.maybeParseImportAttributes()
        }
        this.state.semicolon()
    }

    protected shouldParseExportStar(): boolean {
        return this.state.match(TokenType.star)
    }

    protected parseExportStar(): void {
        this.baseParseExportStar()
    }

    protected baseParseExportStar(): void {
        this.state.expect(TokenType.star)

        if (this.state.isContextual(ContextualKeyword._as)) {
            this.parseExportNamespace()
        } else {
            this.parseExportFrom()
        }
    }

    protected parseExportNamespace(): void {
        this.state.next()
        this.state.tokens[this.state.tokens.length - 1].type = TokenType._as
        this.parseIdentifier()
        this.parseExportSpecifiersMaybe()
        this.parseExportFrom()
    }

    protected shouldParseExportDeclaration(): boolean {
        return (
            this.state.type === TokenType._var ||
            this.state.type === TokenType._const ||
            this.state.type === TokenType._let ||
            this.state.type === TokenType._function ||
            this.state.type === TokenType._class ||
            this.state.isContextual(ContextualKeyword._async) ||
            this.state.match(TokenType.at)
        )
    }

    // Parses a comma-separated list of module exports.
    protected parseExportSpecifiers(): void {
        let first = true

        // export { x, y as z } [from '...']
        this.state.expect(TokenType.braceL)

        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            if (first) {
                first = false
            } else {
                this.state.expect(TokenType.comma)
                if (this.state.eat(TokenType.braceR)) {
                    break
                }
            }
            this.parseExportSpecifier()
        }
    }

    protected parseExportSpecifier(): void {
        this.parseIdentifier()
        this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ExportAccess
        if (this.state.eatContextual(ContextualKeyword._as)) {
            this.parseIdentifier()
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
    protected isImportReflection(): boolean {
        const snapshot = this.state.snapshot()
        this.state.expectContextual(ContextualKeyword._module)
        if (this.state.eatContextual(ContextualKeyword._from)) {
            if (this.state.isContextual(ContextualKeyword._from)) {
                this.state.restoreFromSnapshot(snapshot)
                return true
            } else {
                this.state.restoreFromSnapshot(snapshot)
                return false
            }
        } else if (this.state.match(TokenType.comma)) {
            this.state.restoreFromSnapshot(snapshot)
            return false
        } else {
            this.state.restoreFromSnapshot(snapshot)
            return true
        }
    }

    /**
     * Eat the "module" token from the import reflection proposal.
     * https://github.com/tc39/proposal-import-reflection
     */
    protected parseMaybeImportReflection(): void {
        // isImportReflection does snapshot/restore, so only run it if we see the word
        // "module".
        if (this.state.isContextual(ContextualKeyword._module) && this.isImportReflection()) {
            this.state.next()
        }
    }

    // Parses import declaration.
    protected parseImport(): void {

        // import '...'
        if (this.state.match(TokenType.string)) {
            this.parseExprAtom()
        } else {
            this.parseMaybeImportReflection()
            this.parseImportSpecifiers()
            this.state.expectContextual(ContextualKeyword._from)
            this.parseExprAtom()
        }
        this.maybeParseImportAttributes()
        this.state.semicolon()
    }

    // eslint-disable-next-line no-unused-vars
    protected shouldParseDefaultImport(): boolean {
        return this.state.match(TokenType.name)
    }

    protected parseImportSpecifierLocal(): void {
        this.parseImportedIdentifier()
    }

    // Parses a comma-separated list of module imports.
    protected parseImportSpecifiers(): void {
        let first = true
        if (this.shouldParseDefaultImport()) {
            // import defaultObj, { x, y as z } from '...'
            this.parseImportSpecifierLocal()

            if (!this.state.eat(TokenType.comma)) return
        }

        if (this.state.match(TokenType.star)) {
            this.state.next()
            this.state.expectContextual(ContextualKeyword._as)

            this.parseImportSpecifierLocal()

            return
        }

        this.state.expect(TokenType.braceL)
        while (!this.state.eat(TokenType.braceR) && !this.state.error) {
            if (first) {
                first = false
            } else {
                // Detect an attempt to deep destructure
                if (this.state.eat(TokenType.colon)) {
                    this.state.unexpected(
                        "ES2015 named imports do not destructure. Use another statement for destructuring after the import.",
                    )
                }

                this.state.expect(TokenType.comma)
                if (this.state.eat(TokenType.braceR)) {
                    break
                }
            }

            this.parseImportSpecifier()
        }
    }

    protected parseImportSpecifier(): void {
        this.parseImportedIdentifier()
        if (this.state.isContextual(ContextualKeyword._as)) {
            this.state.tokens[this.state.tokens.length - 1].identifierRole = IdentifierRole.ImportAccess
            this.state.next()
            this.parseImportedIdentifier()
        }
    }

    /**
     * Parse import attributes like `with {type: "json"}`, or the legacy form
     * `assert {type: "json"}`.
     *
     * Import attributes technically have their own syntax, but are always parseable
     * as a plain JS object, so just do that for simplicity.
     */
    protected maybeParseImportAttributes(): void {
        if (this.state.match(TokenType._with) || (this.state.isContextual(ContextualKeyword._assert) && !this.state.hasPrecedingLineBreak())) {
            this.state.next()
            this.parseObj(false, false)
        }
    }
    // #endregion

    public parseTopLevel(): File {
        this.parseBlockBody(TokenType.eof)
        this.state.scopes.push(new Scope(0, this.state.tokens.length, true))
        if (this.state.scopeDepth !== 0) {
            throw new Error(`Invalid scope depth at end of file: ${this.state.scopeDepth}`)
        }
        return new File(this.state.tokens, this.state.scopes)
    }
}
