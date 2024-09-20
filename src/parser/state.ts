import { type TokenType, TokenType as tt  } from './generated/types'
import { ContextualKeyword } from './keywords'
import { Token, TypeAndKeyword } from './token'
import { createScanner, type Scanner } from './scanner'
import { skipWhiteSpace } from './util'

export let state: State

export function initParser(
    inputCode: string,
    isJSXEnabledArg: boolean,
    isTypeScriptEnabledArg: boolean,
    isFlowEnabledArg: boolean,
): void {
    state = new State(inputCode, isTypeScriptEnabledArg, isFlowEnabledArg, isJSXEnabledArg)
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
    type: TokenType = tt.eof;
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
        this.start = state.pos
        this.scanner.readTmplToken()
    }

    // The tokenizer never parses regexes by default. Instead, the parser is responsible for
    // instructing it to parse a regex when we see a slash at the start of an expression.
    retokenizeSlashAsRegex(): void {
        if (this.type === tt.assign) {
            --this.pos
        }
        this.scanner.readRegexp()
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
}
