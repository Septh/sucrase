import type { State } from './state'
import { IS_IDENTIFIER_CHAR, IS_IDENTIFIER_START, IS_WHITESPACE, Charcode } from './charcode'
import { ContextualKeyword } from './keywords'
import { TokenType as tt, type TokenType } from './types.generated'
import { READ_WORD_TREE } from './readWordTree.generated'

export type Scanner = ReturnType<typeof createScanner>

export function createScanner(state: State) {

    const { input } = state
    const end = input.length

    // Called at the end of every token. Sets various fields, and skips the space after the token, 
    // so that the next one's `start` will point at the right position.
    function finishToken(type: TokenType, contextualKeyword: ContextualKeyword = ContextualKeyword.NONE) {
        state.end = state.pos
        state.type = type
        state.contextualKeyword = contextualKeyword
    }
    
    function finishOp(type: TokenType, size: number): void {
        state.pos += size
        finishToken(type)
    }    

    // Called at the start of the parse and after every token. Skips whitespace and comments.
    function skipSpace(): void {
        while (state.pos < end) {
            const ch = input.charCodeAt(state.pos)
            switch (ch) {
                case Charcode.carriageReturn:
                    if (input.charCodeAt(state.pos + 1) === Charcode.lineFeed) {
                        ++state.pos
                    }

                case Charcode.lineFeed:
                case Charcode.lineSeparator:
                case Charcode.paragraphSeparator:
                    ++state.pos
                    break

                case Charcode.slash:
                    switch (input.charCodeAt(state.pos + 1)) {
                        case Charcode.asterisk:
                            state.pos += 2
                            skipBlockComment()
                            break

                        case Charcode.slash:
                            skipLineComment(2)
                            break

                        default:
                            return
                    }
                    break

                default:
                    if (IS_WHITESPACE[ch]) {
                        ++state.pos
                    } else {
                        return
                    }
            }
        }
    }

    function skipLineComment(startSkip: number): void {
        let ch = input.charCodeAt((state.pos += startSkip))
        if (state.pos < end) {
            while (
                ch !== Charcode.lineFeed &&
                ch !== Charcode.carriageReturn &&
                ch !== Charcode.lineSeparator &&
                ch !== Charcode.paragraphSeparator &&
                ++state.pos < end
            ) {
                ch = input.charCodeAt(state.pos)
            }
        }
    }    

    function skipBlockComment(): void {
        while (
            input.charCodeAt(state.pos) !== Charcode.asterisk ||
            input.charCodeAt(state.pos + 1) !== Charcode.slash
        ) {
            state.pos++
            if (state.pos > end) {
                state.unexpected("Unterminated comment", state.pos - 2)
                return
            }
        }
        state.pos += 2
    }

    // Skip to the end of the current word. Note that this is the same as the snippet at the end of
    // readWord, but calling skipWord from readWord seems to slightly hurt performance from some rough
    // measurements.
    function skipWord(): void {
        while (state.pos < end) {
            const ch = input.charCodeAt(state.pos)
            if (IS_IDENTIFIER_CHAR[ch]) {
                state.pos++
            } else if (ch === Charcode.backslash) {
                // \u
                state.pos += 2
                if (input.charCodeAt(state.pos) === Charcode.leftCurlyBrace) {
                    while (
                        state.pos < end &&
                        input.charCodeAt(state.pos) !== Charcode.rightCurlyBrace
                    ) {
                        state.pos++
                    }
                    state.pos++
                }
            } else {
                break
            }
        }
    }

    // #region JSX -------------------------------------------------------------

    function jsxReadToken(): void {
        let sawNewline = false
        let sawNonWhitespace = false
        while (true) {
            if (state.pos >= state.input.length) {
                state.unexpected("Unterminated JSX contents")
                return
            }
    
            const ch = state.input.charCodeAt(state.pos)
            if (ch === Charcode.lessThan || ch === Charcode.leftCurlyBrace) {
                if (state.pos === state.start) {
                    if (ch === Charcode.lessThan) {
                        state.pos++
                        state.scanner.finishToken(tt.jsxTagStart)
                        return
                    }
                    state.scanner.getTokenFromCode(ch)
                    return
                }
                if (sawNewline && !sawNonWhitespace) {
                    state.scanner.finishToken(tt.jsxEmptyText)
                } else {
                    state.scanner.finishToken(tt.jsxText)
                }
                return
            }
    
            // This is part of JSX text.
            if (ch === Charcode.lineFeed) {
                sawNewline = true
            } else if (ch !== Charcode.space && ch !== Charcode.carriageReturn && ch !== Charcode.tab) {
                sawNonWhitespace = true
            }
            state.pos++
        }
    }
    
    function jsxReadString(quote: number): void {
        state.pos++
        for (;;) {
            if (state.pos >= state.input.length) {
                state.unexpected("Unterminated string constant")
                return
            }
    
            const ch = state.input.charCodeAt(state.pos)
            if (ch === quote) {
                state.pos++
                break
            }
            state.pos++
        }
        state.scanner.finishToken(tt.string)
    }
    
    // Read a JSX identifier (valid tag or attribute name).
    //
    // Optimized version since JSX identifiers can't contain
    // escape characters and so can be read as single slice.
    // Also assumes that first character was already checked
    // by isIdentifierStart in readToken.
    function jsxReadWord(): void {
        let ch: number
        do {
            if (state.pos > state.input.length) {
                state.unexpected("Unexpectedly reached the end of input.")
                return
            }
            ch = state.input.charCodeAt(++state.pos)
        } while (IS_IDENTIFIER_CHAR[ch] || ch === Charcode.dash)
        state.scanner.finishToken(tt.jsxName)
    }
    
    function jsxReadTag(): void {
        const code = state.input.charCodeAt(state.pos)
    
        if (IS_IDENTIFIER_START[code]) {
            jsxReadWord()
        } else if (code === Charcode.quotationMark || code === Charcode.apostrophe) {
            jsxReadString(code)
        } else {
            // The following tokens are just one character each.
            ++state.pos
            switch (code) {
                case Charcode.greaterThan:
                    state.scanner.finishToken(tt.jsxTagEnd)
                    break
                case Charcode.lessThan:
                    state.scanner.finishToken(tt.jsxTagStart)
                    break
                case Charcode.slash:
                    state.scanner.finishToken(tt.slash)
                    break
                case Charcode.equalsTo:
                    state.scanner.finishToken(tt.eq)
                    break
                case Charcode.leftCurlyBrace:
                    state.scanner.finishToken(tt.braceL)
                    break
                case Charcode.dot:
                    state.scanner.finishToken(tt.dot)
                    break
                case Charcode.colon:
                    state.scanner.finishToken(tt.colon)
                    break
                default:
                    state.unexpected()
            }
        }
    }
    
    // #endregion

    // #region RegExp ----------------------------------------------------------

    function readRegexp(): void {
        const start = state.pos
        let escaped = false
        let inClass = false
        for (; ;) {
            if (state.pos >= end) {
                state.unexpected("Unterminated regular expression", start)
                return
            }
            const code = input.charCodeAt(state.pos)
            if (escaped) {
                escaped = false
            } else {
                if (code === Charcode.leftSquareBracket) {
                    inClass = true
                } else if (code === Charcode.rightSquareBracket && inClass) {
                    inClass = false
                } else if (code === Charcode.slash && !inClass) {
                    break
                }
                escaped = code === Charcode.backslash
            }
            ++state.pos
        }
        ++state.pos
        // Need to use `skipWord` because '\uXXXX' sequences are allowed here (don't ask).
        skipWord()
    
        finishToken(tt.regexp)
    }    

    // #endregion

    /**
     * Read an identifier, producing either a name token or matching on one of the existing keywords.
     * For performance, we pre-generate big decision tree that we traverse. Each node represents a
     * prefix and has 27 values, where the first value is the token or contextual token, if any (-1 if
     * not), and the other 26 values are the transitions to other nodes, or -1 to stop.
     */
    function readWord(): void {
        let treePos = 0
        let code = 0
        let pos = state.pos
        while (pos < end) {
            code = input.charCodeAt(pos)
            if (code < Charcode.lowercaseA || code > Charcode.lowercaseZ) {
                break
            }
            const nxt = READ_WORD_TREE[treePos + (code - Charcode.lowercaseA) + 1]
            if (nxt === -1) {
                break
            } else {
                treePos = nxt
                pos++
            }
        }

        const keywordValue = READ_WORD_TREE[treePos]
        if (keywordValue > -1 && !IS_IDENTIFIER_CHAR[code]) {
            state.pos = pos
            if (keywordValue & 1) {
                finishToken(keywordValue >>> 1)
            } else {
                finishToken(tt.name, keywordValue >>> 1)
            }
            return
        }

        while (pos < end) {
            const ch = input.charCodeAt(pos)
            if (IS_IDENTIFIER_CHAR[ch]) {
                pos++
            } else if (ch === Charcode.backslash) {
                // \u
                pos += 2
                if (input.charCodeAt(pos) === Charcode.leftCurlyBrace) {
                    while (pos < end && input.charCodeAt(pos) !== Charcode.rightCurlyBrace) {
                        pos++
                    }
                    pos++
                }
            } else if (ch === Charcode.atSign && input.charCodeAt(pos + 1) === Charcode.atSign) {
                pos += 2
            } else {
                break
            }
        }
        state.pos = pos
        finishToken(tt.name)
    }

    /**
     * Read a decimal integer. Note that this can't be unified with the similar code
     * in readRadixNumber (which also handles hex digits) because "e" needs to be
     * the end of the integer so that we can properly handle scientific notation.
     */
    function readInt(): void {
        while (true) {
            const code = input.charCodeAt(state.pos)
            if ((code >= Charcode.digit0 && code <= Charcode.digit9) || code === Charcode.underscore) {
                state.pos++
            } else {
                break
            }
        }
    }

    function readRadixNumber(): void {
        state.pos += 2 // 0x

        // Walk to the end of the number, allowing hex digits.
        while (true) {
            const code = input.charCodeAt(state.pos)
            if (
                (code >= Charcode.digit0 && code <= Charcode.digit9) ||
                (code >= Charcode.lowercaseA && code <= Charcode.lowercaseF) ||
                (code >= Charcode.uppercaseA && code <= Charcode.uppercaseF) ||
                code === Charcode.underscore
            ) {
                state.pos++
            } else {
                break
            }
        }

        const nextChar = input.charCodeAt(state.pos)
        if (nextChar === Charcode.lowercaseN) {
            ++state.pos
            finishToken(tt.bigint)
        } else {
            finishToken(tt.num)
        }
    }

    // Read an integer, octal integer, or floating-point number.
    function readNumber(startsWithDot: boolean): void {
        let isBigInt = false
        let isDecimal = false

        if (!startsWithDot) {
            readInt()
        }

        let nextChar = input.charCodeAt(state.pos)
        if (nextChar === Charcode.dot) {
            ++state.pos
            readInt()
            nextChar = input.charCodeAt(state.pos)
        }

        if (nextChar === Charcode.uppercaseE || nextChar === Charcode.lowercaseE) {
            nextChar = input.charCodeAt(++state.pos)
            if (nextChar === Charcode.plusSign || nextChar === Charcode.dash) {
                ++state.pos
            }
            readInt()
            nextChar = input.charCodeAt(state.pos)
        }

        if (nextChar === Charcode.lowercaseN) {
            ++state.pos
            isBigInt = true
        } else if (nextChar === Charcode.lowercaseM) {
            ++state.pos
            isDecimal = true
        }

        if (isBigInt) {
            finishToken(tt.bigint)
            return
        }

        if (isDecimal) {
            finishToken(tt.decimal)
            return
        }

        finishToken(tt.num)
    }

    function readString(quote: number): void {
        state.pos++
        for (; ;) {
            if (state.pos >= end) {
                state.unexpected("Unterminated string constant")
                return
            }
            const ch = input.charCodeAt(state.pos)
            if (ch === Charcode.backslash) {
                state.pos++
            } else if (ch === quote) {
                break
            }
            state.pos++
        }
        state.pos++
        finishToken(tt.string)
    }

    // Reads template string tokens.
    function readTmplToken(): void {
        for (; ;) {
            if (state.pos >= end) {
                state.unexpected("Unterminated template")
                return
            }
            const ch = input.charCodeAt(state.pos)
            if (
                ch === Charcode.graveAccent ||
                (ch === Charcode.dollarSign && input.charCodeAt(state.pos + 1) === Charcode.leftCurlyBrace)
            ) {
                if (state.pos === state.start && state.match(tt.template)) {
                    if (ch === Charcode.dollarSign) {
                        state.pos += 2
                        finishToken(tt.dollarBraceL)
                        return
                    } else {
                        ++state.pos
                        finishToken(tt.backQuote)
                        return
                    }
                }
                finishToken(tt.template)
                return
            }
            if (ch === Charcode.backslash) {
                state.pos++
            }
            state.pos++
        }
    }

    function readToken_dot(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar >= Charcode.digit0 && nextChar <= Charcode.digit9) {
            readNumber(true)
            return
        }
    
        if (nextChar === Charcode.dot && input.charCodeAt(state.pos + 2) === Charcode.dot) {
            state.pos += 3
            finishToken(tt.ellipsis)
        } else {
            ++state.pos
            finishToken(tt.dot)
        }
    }
    
    function readToken_slash(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar === Charcode.equalsTo) {
            finishOp(tt.assign, 2)
        } else {
            finishOp(tt.slash, 1)
        }
    }
    
    function readToken_mult_modulo(code: number): void {
        // '%*'
        let tokenType = code === Charcode.asterisk ? tt.star : tt.modulo
        let width = 1
        let nextChar = input.charCodeAt(state.pos + 1)
    
        // Exponentiation operator **
        if (code === Charcode.asterisk && nextChar === Charcode.asterisk) {
            width++
            nextChar = input.charCodeAt(state.pos + 2)
            tokenType = tt.exponent
        }
    
        // Match *= or %=, disallowing *=> which can be valid in flow.
        if (
            nextChar === Charcode.equalsTo &&
            input.charCodeAt(state.pos + 2) !== Charcode.greaterThan
        ) {
            width++
            tokenType = tt.assign
        }
    
        finishOp(tokenType, width)
    }
    
    function readToken_pipe_amp(code: number): void {
        // '|&'
        const nextChar = input.charCodeAt(state.pos + 1)
    
        if (nextChar === code) {
            if (input.charCodeAt(state.pos + 2) === Charcode.equalsTo) {
                // ||= or &&=
                finishOp(tt.assign, 3)
            } else {
                // || or &&
                finishOp(code === Charcode.verticalBar ? tt.logicalOR : tt.logicalAND, 2)
            }
            return
        }
    
        if (code === Charcode.verticalBar) {
            // '|>'
            if (nextChar === Charcode.greaterThan) {
                finishOp(tt.pipeline, 2)
                return
            } else if (nextChar === Charcode.rightCurlyBrace && state.isFlowEnabled) {
                // '|}'
                finishOp(tt.braceBarR, 2)
                return
            }
        }
    
        if (nextChar === Charcode.equalsTo) {
            finishOp(tt.assign, 2)
            return
        }
    
        finishOp(code === Charcode.verticalBar ? tt.bitwiseOR : tt.bitwiseAND, 1)
    }
    
    function readToken_caret(): void {
        // '^'
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar === Charcode.equalsTo) {
            finishOp(tt.assign, 2)
        } else {
            finishOp(tt.bitwiseXOR, 1)
        }
    }
    
    function readToken_plus_min(code: number): void {
        // '+-'
        const nextChar = input.charCodeAt(state.pos + 1)
    
        if (nextChar === code) {
            // Tentatively call this a prefix operator, but it might be changed to postfix later.
            finishOp(tt.preIncDec, 2)
            return
        }
    
        if (nextChar === Charcode.equalsTo) {
            finishOp(tt.assign, 2)
        } else if (code === Charcode.plusSign) {
            finishOp(tt.plus, 1)
        } else {
            finishOp(tt.minus, 1)
        }
    }
    
    function readToken_lt(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
    
        if (nextChar === Charcode.lessThan) {
            if (input.charCodeAt(state.pos + 2) === Charcode.equalsTo) {
                finishOp(tt.assign, 3)
                return
            }
            // We see <<, but need to be really careful about whether to treat it as a
            // true left-shift or as two < tokens.
            if (state.isType) {
                // Within a type, << might come up in a snippet like `Array<<T>() => void>`,
                // so treat it as two < tokens. Importantly, this should only override <<
                // rather than other tokens like <= . If we treated <= as < in a type
                // context, then the snippet `a as T <= 1` would incorrectly start parsing
                // a type argument on T. We don't need to worry about `a as T << 1`
                // because TypeScript disallows that syntax.
                finishOp(tt.lessThan, 1)
            } else {
                // Outside a type, this might be a true left-shift operator, or it might
                // still be two open-type-arg tokens, such as in `f<<T>() => void>()`. We
                // look at the token while considering the `f`, so we don't yet know that
                // we're in a type context. In this case, we initially tokenize as a
                // left-shift and correct after-the-fact as necessary in
                // tsParseTypeArgumentsWithPossibleBitshift .
                finishOp(tt.bitShiftL, 2)
            }
            return
        }
    
        if (nextChar === Charcode.equalsTo) {
            // <=
            finishOp(tt.relationalOrEqual, 2)
        } else {
            finishOp(tt.lessThan, 1)
        }
    }
    
    function readToken_gt(): void {
        if (state.isType) {
            // Avoid right-shift for things like `Array<Array<string>>` and
            // greater-than-or-equal for things like `const a: Array<number>=[];`.
            finishOp(tt.greaterThan, 1)
            return
        }
    
        const nextChar = input.charCodeAt(state.pos + 1)
    
        if (nextChar === Charcode.greaterThan) {
            const size = input.charCodeAt(state.pos + 2) === Charcode.greaterThan ? 3 : 2
            if (input.charCodeAt(state.pos + size) === Charcode.equalsTo) {
                finishOp(tt.assign, size + 1)
                return
            }
            finishOp(tt.bitShiftR, size)
            return
        }
    
        if (nextChar === Charcode.equalsTo) {
            // >=
            finishOp(tt.relationalOrEqual, 2)
        } else {
            finishOp(tt.greaterThan, 1)
        }
    }
    
    /**
     * Reinterpret a possible > token when transitioning from a type to a non-type
     * context.
     *
     * This comes up in two situations where >= needs to be treated as one token:
     * - After an `as` expression, like in the code `a as T >= 1`.
     * - In a type argument in an expression context, e.g. `f(a < b, c >= d)`, we
     *   need to see the token as >= so that we get an error and backtrack to
     *   normal expression parsing.
     *
     * Other situations require >= to be seen as two tokens, e.g.
     * `const x: Array<T>=[];`, so it's important to treat > as its own token in
     * typical type parsing situations.
     */
    function rescan_gt(): void {
        if (state.type === tt.greaterThan) {
            state.pos -= 1
            readToken_gt()
        }
    }
    
    function readToken_eq_excl(code: number): void {
        // '=!'
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar === Charcode.equalsTo) {
            finishOp(tt.equality, input.charCodeAt(state.pos + 2) === Charcode.equalsTo ? 3 : 2)
            return
        }
        if (code === Charcode.equalsTo && nextChar === Charcode.greaterThan) {
            // '=>'
            state.pos += 2
            finishToken(tt.arrow)
            return
        }
        finishOp(code === Charcode.equalsTo ? tt.eq : tt.bang, 1)
    }
    
    function readToken_question(): void {
        // '?'
        const nextChar = input.charCodeAt(state.pos + 1)
        const nextChar2 = input.charCodeAt(state.pos + 2)
        if (
            nextChar === Charcode.questionMark &&
            // In Flow (but not TypeScript), ??string is a valid type that should be
            // tokenized as two individual ? tokens.
            !(state.isFlowEnabled && state.isType)
        ) {
            if (nextChar2 === Charcode.equalsTo) {
                // '??='
                finishOp(tt.assign, 3)
            } else {
                // '??'
                finishOp(tt.nullishCoalescing, 2)
            }
        } else if (
            nextChar === Charcode.dot &&
            !(nextChar2 >= Charcode.digit0 && nextChar2 <= Charcode.digit9)
        ) {
            // '.' not followed by a number
            state.pos += 2
            finishToken(tt.questionDot)
        } else {
            ++state.pos
            finishToken(tt.question)
        }
    }    

    function getTokenFromCode(code: number): void {
        switch (code) {
            case Charcode.numberSign:
                ++state.pos
                finishToken(tt.hash)
                return
    
            // The interpretation of a dot depends on whether it is followed
            // by a digit or another two dots.
    
            case Charcode.dot:
                readToken_dot()
                return
    
            // Punctuation tokens.
            case Charcode.leftParenthesis:
                ++state.pos
                finishToken(tt.parenL)
                return
            case Charcode.rightParenthesis:
                ++state.pos
                finishToken(tt.parenR)
                return
            case Charcode.semicolon:
                ++state.pos
                finishToken(tt.semi)
                return
            case Charcode.comma:
                ++state.pos
                finishToken(tt.comma)
                return
            case Charcode.leftSquareBracket:
                ++state.pos
                finishToken(tt.bracketL)
                return
            case Charcode.rightSquareBracket:
                ++state.pos
                finishToken(tt.bracketR)
                return
    
            case Charcode.leftCurlyBrace:
                if (state.isFlowEnabled && input.charCodeAt(state.pos + 1) === Charcode.verticalBar) {
                    finishOp(tt.braceBarL, 2)
                } else {
                    ++state.pos
                    finishToken(tt.braceL)
                }
                return
    
            case Charcode.rightCurlyBrace:
                ++state.pos
                finishToken(tt.braceR)
                return
    
            case Charcode.colon:
                if (input.charCodeAt(state.pos + 1) === Charcode.colon) {
                    finishOp(tt.doubleColon, 2)
                } else {
                    ++state.pos
                    finishToken(tt.colon)
                }
                return
    
            case Charcode.questionMark:
                readToken_question()
                return
            case Charcode.atSign:
                ++state.pos
                finishToken(tt.at)
                return
    
            case Charcode.graveAccent:
                ++state.pos
                finishToken(tt.backQuote)
                return
    
            case Charcode.digit0: {
                const nextChar = input.charCodeAt(state.pos + 1)
                // '0x', '0X', '0o', '0O', '0b', '0B'
                if (
                    nextChar === Charcode.lowercaseX ||
                    nextChar === Charcode.uppercaseX ||
                    nextChar === Charcode.lowercaseO ||
                    nextChar === Charcode.uppercaseO ||
                    nextChar === Charcode.lowercaseB ||
                    nextChar === Charcode.uppercaseB
                ) {
                    readRadixNumber()
                    return
                }
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            case Charcode.digit1:
            case Charcode.digit2:
            case Charcode.digit3:
            case Charcode.digit4:
            case Charcode.digit5:
            case Charcode.digit6:
            case Charcode.digit7:
            case Charcode.digit8:
            case Charcode.digit9:
                readNumber(false)
                return
    
            // Quotes produce strings.
            case Charcode.quotationMark:
            case Charcode.apostrophe:
                readString(code)
                return
    
            // Operators are parsed inline in tiny state machines. '=' (charCodes.equalsTo) is
            // often referred to. `finishOp` simply skips the amount of
            // characters it is given as second argument, and returns a token
            // of the type given by its first argument.
    
            case Charcode.slash:
                readToken_slash()
                return
    
            case Charcode.percentSign:
            case Charcode.asterisk:
                readToken_mult_modulo(code)
                return
    
            case Charcode.verticalBar:
            case Charcode.ampersand:
                readToken_pipe_amp(code)
                return
    
            case Charcode.caret:
                readToken_caret()
                return
    
            case Charcode.plusSign:
            case Charcode.dash:
                readToken_plus_min(code)
                return
    
            case Charcode.lessThan:
                readToken_lt()
                return
    
            case Charcode.greaterThan:
                readToken_gt()
                return
    
            case Charcode.equalsTo:
            case Charcode.exclamationMark:
                readToken_eq_excl(code)
                return
    
            case Charcode.tilde:
                finishOp(tt.tilde, 1)
                return
    
            default:
                break
        }
    
        state.unexpected(`Unexpected character '${String.fromCharCode(code)}'`, state.pos)
    }    

    function readToken(code: number): void {
        // Identifier or keyword. '\uXXXX' sequences are allowed in
        // identifiers, so '\' also dispatches to that.
        if (
            IS_IDENTIFIER_START[code] ||
            code === Charcode.backslash ||
            (code === Charcode.atSign && input.charCodeAt(state.pos + 1) === Charcode.atSign)
        ) {
            readWord()
        } else {
            getTokenFromCode(code)
        }
    }

    // Read a single token, updating the parser object's token-related properties.
    function nextToken(): void {
        skipSpace()
        state.start = state.pos
        if (state.pos >= end) {
            const tokens = state.tokens
            // We normally run past the end a bit, but if we're way past the end, avoid an infinite loop.
            // Also check the token positions rather than the types since sometimes we rewrite the token
            // type to something else.
            if (
                tokens.length >= 2 &&
                tokens[tokens.length - 1].start >= end &&
                tokens[tokens.length - 2].start >= end
            ) {
                state.unexpected("Unexpectedly reached the end of input.")
            }
            finishToken(tt.eof)
            return
        }
        readToken(input.charCodeAt(state.pos))
    }

    return {
        nextToken,
        readTmplToken,
        readRegexp,
        skipSpace,
        skipLineComment,
        skipBlockComment,
        finishToken,
        rescan_gt,
        getTokenFromCode,
        skipWord,
        readWord,
        jsxReadTag,
        jsxReadWord,
        jsxReadToken
    }
}
