import { TokenType, formatTokenType } from './types.generated'
import { ContextualKeyword } from './keywords'
import { Token, TypeAndKeyword } from './token'
import { createScanner, type Scanner } from './scanner'
import { Charcode, skipWhiteSpace } from './charcode'
import { Parser } from './parser_base'
import { ParserFlow } from './parser_flow'
import { ParserTypescript } from './parser_typescript'

export let parser: Parser

export class Loc {
    line: number
    column: number
    constructor(line: number, column: number) {
        this.line = line
        this.column = column
    }
}

export function initParser(
    inputCode: string,
    isJSXEnabledArg: boolean,
    isTypeScriptEnabledArg: boolean,
    isFlowEnabledArg: boolean,
) {
    const state = new State(inputCode, isTypeScriptEnabledArg, isFlowEnabledArg, isJSXEnabledArg)
    if (isFlowEnabledArg)
        parser = new ParserFlow(state)
    else if (isTypeScriptEnabledArg)
        parser = new ParserTypescript(state)
    else
        parser = new Parser(state)

    return { state, parser }
}

export class Scope {
    startTokenIndex: number
    endTokenIndex: number
    isFunctionScope: boolean

    constructor(startTokenIndex: number, endTokenIndex: number, isFunctionScope: boolean) {
        this.startTokenIndex = startTokenIndex
        this.endTokenIndex = endTokenIndex
        this.isFunctionScope = isFunctionScope
    }
}

export class StateSnapshot {
    constructor(
        readonly potentialArrowAt: number,
        readonly noAnonFunctionType: boolean,
        readonly inDisallowConditionalTypesContext: boolean,
        readonly tokensLength: number,
        readonly scopesLength: number,
        readonly pos: number,
        readonly type: TokenType,
        readonly contextualKeyword: ContextualKeyword,
        readonly start: number,
        readonly end: number,
        readonly isType: boolean,
        readonly scopeDepth: number,
        readonly error: Error | null,
    ) { }
}

export class State {
    input: string
    isTypeScriptEnabled: boolean
    isFlowEnabled: boolean
    isJSXEnabled: boolean
    nextContextId: number

    // Used to signify the start of a potential arrow function
    potentialArrowAt: number = -1;

    // Used by Flow to handle an edge case involving function type parsing.
    noAnonFunctionType: boolean = false;

    // Used by TypeScript to handle ambiguities when parsing conditional types.
    inDisallowConditionalTypesContext: boolean = false;

    // Token store.
    tokens: Array<Token> = [];

    // Array of all observed scopes, ordered by their ending position.
    scopes: Array<Scope> = [];

    // The current position of the tokenizer in the input.
    pos: number = 0;

    // Information about the current token.
    type: TokenType = TokenType.eof;
    contextualKeyword: ContextualKeyword = ContextualKeyword.NONE;
    start: number = 0;
    end: number = 0;

    isType: boolean = false;
    scopeDepth: number = 0;

    /**
     * If the parser is in an error state, then the token is always tt.eof and all functions can
     * keep executing but should be written so they don't get into an infinite loop in this situation.
     *
     * This approach, combined with the ability to snapshot and restore state, allows us to implement
     * backtracking without exceptions and without needing to explicitly propagate error states
     * everywhere.
     */
    error: Error | null = null;

    scanner: Scanner
    constructor(input: string, isTypeScriptEnabled: boolean, isFlowEnabled: boolean, isJSXEnabled: boolean) {
        this.input = input
        this.isTypeScriptEnabled = isTypeScriptEnabled
        this.isFlowEnabled = isFlowEnabled
        this.isJSXEnabled = isJSXEnabled
        this.nextContextId = 1

        this.scanner = createScanner(this)
    }

    snapshot(): StateSnapshot {
        return new StateSnapshot(
            this.potentialArrowAt,
            this.noAnonFunctionType,
            this.inDisallowConditionalTypesContext,
            this.tokens.length,
            this.scopes.length,
            this.pos,
            this.type,
            this.contextualKeyword,
            this.start,
            this.end,
            this.isType,
            this.scopeDepth,
            this.error,
        )
    }

    restoreFromSnapshot(snapshot: StateSnapshot): void {
        this.potentialArrowAt = snapshot.potentialArrowAt
        this.noAnonFunctionType = snapshot.noAnonFunctionType
        this.inDisallowConditionalTypesContext = snapshot.inDisallowConditionalTypesContext
        this.tokens.length = snapshot.tokensLength
        this.scopes.length = snapshot.scopesLength
        this.pos = snapshot.pos
        this.type = snapshot.type
        this.contextualKeyword = snapshot.contextualKeyword
        this.start = snapshot.start
        this.end = snapshot.end
        this.isType = snapshot.isType
        this.scopeDepth = snapshot.scopeDepth
        this.error = snapshot.error
    }

    getNextContextId(): number {
        return this.nextContextId++
    }

    pushTypeContext(existingTokensInType: number): boolean {
        for (let i = this.tokens.length - existingTokensInType; i < this.tokens.length; i++) {
            this.tokens[i].isType = true
        }
        const oldIsType = this.isType
        this.isType = true
        return oldIsType
    }

    popTypeContext(oldIsType: boolean): void {
        this.isType = oldIsType
    }

    // Move to the next token
    next(): void {
        this.tokens.push(new Token(this))
        this.scanner.nextToken()
    }

    // Call instead of next when inside a template, since that needs to be handled differently.
    nextTemplateToken(): void {
        this.tokens.push(new Token(this))
        this.start = this.pos
        this.scanner.readTmplToken()
    }

    // The tokenizer never parses regexes by default. Instead, the parser is responsible for
    // instructing it to parse a regex when we see a slash at the start of an expression.
    retokenizeSlashAsRegex(): void {
        if (this.type === TokenType.assign) {
            --this.pos
        }
        this.scanner.readRegexp()
    }

    nextJSXTagToken(): void {
        this.tokens.push(new Token(this))
        this.scanner.skipSpace()
        this.start = this.pos
        this.scanner.jsxReadTag()
    }

    nextJSXExprToken(): void {
        this.tokens.push(new Token(this))
        this.start = this.pos
        this.scanner.jsxReadToken()
    }

    match(type: TokenType): boolean {
        return this.type === type
    }

    eat(type: TokenType): boolean {
        if (this.match(type)) {
            this.next()
            return true
        } else {
            return false
        }
    }

    eatTypeToken(tokenType: TokenType): void {
        const oldIsType = this.isType
        this.isType = true
        this.eat(tokenType)
        this.isType = oldIsType
    }

    lookaheadType(): TokenType {
        const snapshot = this.snapshot()
        this.next()
        const type = this.type
        this.restoreFromSnapshot(snapshot)
        return type
    }

    lookaheadTypeAndKeyword(): TypeAndKeyword {
        const snapshot = this.snapshot()
        this.next()
        const type = this.type
        const contextualKeyword = this.contextualKeyword
        this.restoreFromSnapshot(snapshot)
        return new TypeAndKeyword(type, contextualKeyword)
    }

    nextTokenStart(): number {
        return this.nextTokenStartSince(this.pos)
    }

    nextTokenStartSince(pos: number): number {
        skipWhiteSpace.lastIndex = pos
        const skip = skipWhiteSpace.exec(this.input)
        return pos + skip![0].length
    }

    lookaheadCharCode(): number {
        return this.input.charCodeAt(this.nextTokenStart())
    }

    // Tests whether parsed token is a contextual keyword.
    isContextual(contextualKeyword: ContextualKeyword): boolean {
        return this.contextualKeyword === contextualKeyword
    }

    isLookaheadContextual(contextualKeyword: ContextualKeyword): boolean {
        const l = this.lookaheadTypeAndKeyword()
        return l.type === TokenType.name && l.contextualKeyword === contextualKeyword
    }

    // Consumes contextual keyword if possible.
    eatContextual(contextualKeyword: ContextualKeyword): boolean {
        return this.contextualKeyword === contextualKeyword && this.eat(TokenType.name)
    }

    // Asserts that following token is given contextual keyword.
    expectContextual(contextualKeyword: ContextualKeyword): void {
        if (!this.eatContextual(contextualKeyword)) {
            this.unexpected()
        }
    }

    // Test whether a semicolon can be inserted at the current position.
    canInsertSemicolon(): boolean {
        return this.match(TokenType.eof) || this.match(TokenType.braceR) || this.hasPrecedingLineBreak()
    }

    hasPrecedingLineBreak(): boolean {
        const prevToken = this.tokens[this.tokens.length - 1]
        const lastTokEnd = prevToken ? prevToken.end : 0
        for (let i = lastTokEnd; i < this.start; i++) {
            const code = this.input.charCodeAt(i)
            if (
                code === Charcode.lineFeed ||
                code === Charcode.carriageReturn ||
                code === 0x2028 ||
                code === 0x2029
            ) {
                return true
            }
        }
        return false
    }

    hasFollowingLineBreak(): boolean {
        const nextStart = this.nextTokenStart()
        for (let i = this.end; i < nextStart; i++) {
            const code = this.input.charCodeAt(i)
            if (
                code === Charcode.lineFeed ||
                code === Charcode.carriageReturn ||
                code === 0x2028 ||
                code === 0x2029
            ) {
                return true
            }
        }
        return false
    }

    isLineTerminator(): boolean {
        return this.eat(TokenType.semi) || this.canInsertSemicolon()
    }

    // Consume a semicolon, or, failing that, see if we are allowed to
    // pretend that there is a semicolon at this position.
    semicolon(): void {
        if (!this.isLineTerminator()) {
            this.unexpected('Unexpected token, expected ";"')
        }
    }

    // Expect a token of a given type. If found, consume it, otherwise,
    // raise an unexpected token error at given pos.
    expect(type: TokenType): void {
        const matched = this.eat(type)
        if (!matched) {
            this.unexpected(`Unexpected token, expected "${formatTokenType(type)}"`)
        }
    }

    /**
     * Transition the parser to an error state. All code needs to be written to naturally unwind in this
     * state, which allows us to backtrack without exceptions and without error plumbing everywhere.
     */
    unexpected(message: string = "Unexpected token", pos: number = this.start): void {
        if (this.error) {
            return
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const err: any = new SyntaxError(message)
        err.pos = pos
        this.error = err
        this.pos = this.input.length
        this.scanner.finishToken(TokenType.eof)
    }

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    augmentError(error: any): any {
        if ("pos" in error) {
            const loc = this.locationForIndex(error.pos)
            error.message += ` (${loc.line}:${loc.column})`
            error.loc = loc
        }
        return error
    }

    locationForIndex(pos: number): Loc {
        let line = 1
        let column = 1
        for (let i = 0; i < pos; i++) {
            if (this.input.charCodeAt(i) === Charcode.lineFeed) {
                line++
                column = 1
            } else {
                column++
            }
        }
        return new Loc(line, column)
    }

}

export function parseFile(state: State) {
    // If enabled, skip leading hashbang line.
    if (
        state.pos === 0 &&
        state.input.charCodeAt(0) === Charcode.numberSign &&
        state.input.charCodeAt(1) === Charcode.exclamationMark
    ) {
        state.scanner.skipLineComment(2)
    }
    state.scanner.nextToken()
    return parser.parseTopLevel()
}

export function parse(
    input: string,
    isJSXEnabled: boolean,
    isTypeScriptEnabled: boolean,
    isFlowEnabled: boolean,
  ) {
    if (isFlowEnabled && isTypeScriptEnabled) {
      throw new Error("Cannot combine flow and typescript plugins.");
    }
    const { state } = initParser(input, isJSXEnabled, isTypeScriptEnabled, isFlowEnabled);
    const result = parseFile(state);
    if (state.error) {
      throw state.augmentError(state.error);
    }
    return result;
  }
  