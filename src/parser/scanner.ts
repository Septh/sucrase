import type { State } from './state'
import { unexpected } from './traverser'
import { IS_IDENTIFIER_CHAR, IS_IDENTIFIER_START, IS_WHITESPACE, charCodes } from './util'
import { ContextualKeyword } from './keywords'
import { TokenType as tt, type TokenType } from './generated/types'
import { READ_WORD_TREE } from './generated/readWordTree'

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
                case charCodes.carriageReturn:
                    if (input.charCodeAt(state.pos + 1) === charCodes.lineFeed) {
                        ++state.pos
                    }

                case charCodes.lineFeed:
                case charCodes.lineSeparator:
                case charCodes.paragraphSeparator:
                    ++state.pos
                    break

                case charCodes.slash:
                    switch (input.charCodeAt(state.pos + 1)) {
                        case charCodes.asterisk:
                            state.pos += 2
                            skipBlockComment()
                            break

                        case charCodes.slash:
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
                ch !== charCodes.lineFeed &&
                ch !== charCodes.carriageReturn &&
                ch !== charCodes.lineSeparator &&
                ch !== charCodes.paragraphSeparator &&
                ++state.pos < end
            ) {
                ch = input.charCodeAt(state.pos)
            }
        }
    }    

    function skipBlockComment(): void {
        while (
            input.charCodeAt(state.pos) !== charCodes.asterisk ||
            input.charCodeAt(state.pos + 1) !== charCodes.slash
        ) {
            state.pos++
            if (state.pos > end) {
                unexpected("Unterminated comment", state.pos - 2)
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
            } else if (ch === charCodes.backslash) {
                // \u
                state.pos += 2
                if (input.charCodeAt(state.pos) === charCodes.leftCurlyBrace) {
                    while (
                        state.pos < end &&
                        input.charCodeAt(state.pos) !== charCodes.rightCurlyBrace
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

    function readRegexp(): void {
        const start = state.pos
        let escaped = false
        let inClass = false
        for (; ;) {
            if (state.pos >= end) {
                unexpected("Unterminated regular expression", start)
                return
            }
            const code = input.charCodeAt(state.pos)
            if (escaped) {
                escaped = false
            } else {
                if (code === charCodes.leftSquareBracket) {
                    inClass = true
                } else if (code === charCodes.rightSquareBracket && inClass) {
                    inClass = false
                } else if (code === charCodes.slash && !inClass) {
                    break
                }
                escaped = code === charCodes.backslash
            }
            ++state.pos
        }
        ++state.pos
        // Need to use `skipWord` because '\uXXXX' sequences are allowed here (don't ask).
        skipWord()
    
        finishToken(tt.regexp)
    }    

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
            if (code < charCodes.lowercaseA || code > charCodes.lowercaseZ) {
                break
            }
            const nxt = READ_WORD_TREE[treePos + (code - charCodes.lowercaseA) + 1]
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
            } else if (ch === charCodes.backslash) {
                // \u
                pos += 2
                if (input.charCodeAt(pos) === charCodes.leftCurlyBrace) {
                    while (pos < end && input.charCodeAt(pos) !== charCodes.rightCurlyBrace) {
                        pos++
                    }
                    pos++
                }
            } else if (ch === charCodes.atSign && input.charCodeAt(pos + 1) === charCodes.atSign) {
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
            if ((code >= charCodes.digit0 && code <= charCodes.digit9) || code === charCodes.underscore) {
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
                (code >= charCodes.digit0 && code <= charCodes.digit9) ||
                (code >= charCodes.lowercaseA && code <= charCodes.lowercaseF) ||
                (code >= charCodes.uppercaseA && code <= charCodes.uppercaseF) ||
                code === charCodes.underscore
            ) {
                state.pos++
            } else {
                break
            }
        }

        const nextChar = input.charCodeAt(state.pos)
        if (nextChar === charCodes.lowercaseN) {
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
        if (nextChar === charCodes.dot) {
            ++state.pos
            readInt()
            nextChar = input.charCodeAt(state.pos)
        }

        if (nextChar === charCodes.uppercaseE || nextChar === charCodes.lowercaseE) {
            nextChar = input.charCodeAt(++state.pos)
            if (nextChar === charCodes.plusSign || nextChar === charCodes.dash) {
                ++state.pos
            }
            readInt()
            nextChar = input.charCodeAt(state.pos)
        }

        if (nextChar === charCodes.lowercaseN) {
            ++state.pos
            isBigInt = true
        } else if (nextChar === charCodes.lowercaseM) {
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
                unexpected("Unterminated string constant")
                return
            }
            const ch = input.charCodeAt(state.pos)
            if (ch === charCodes.backslash) {
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
                unexpected("Unterminated template")
                return
            }
            const ch = input.charCodeAt(state.pos)
            if (
                ch === charCodes.graveAccent ||
                (ch === charCodes.dollarSign && input.charCodeAt(state.pos + 1) === charCodes.leftCurlyBrace)
            ) {
                if (state.pos === state.start && state.match(tt.template)) {
                    if (ch === charCodes.dollarSign) {
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
            if (ch === charCodes.backslash) {
                state.pos++
            }
            state.pos++
        }
    }

    function readToken_dot(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar >= charCodes.digit0 && nextChar <= charCodes.digit9) {
            readNumber(true)
            return
        }
    
        if (nextChar === charCodes.dot && input.charCodeAt(state.pos + 2) === charCodes.dot) {
            state.pos += 3
            finishToken(tt.ellipsis)
        } else {
            ++state.pos
            finishToken(tt.dot)
        }
    }
    
    function readToken_slash(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar === charCodes.equalsTo) {
            finishOp(tt.assign, 2)
        } else {
            finishOp(tt.slash, 1)
        }
    }
    
    function readToken_mult_modulo(code: number): void {
        // '%*'
        let tokenType = code === charCodes.asterisk ? tt.star : tt.modulo
        let width = 1
        let nextChar = input.charCodeAt(state.pos + 1)
    
        // Exponentiation operator **
        if (code === charCodes.asterisk && nextChar === charCodes.asterisk) {
            width++
            nextChar = input.charCodeAt(state.pos + 2)
            tokenType = tt.exponent
        }
    
        // Match *= or %=, disallowing *=> which can be valid in flow.
        if (
            nextChar === charCodes.equalsTo &&
            input.charCodeAt(state.pos + 2) !== charCodes.greaterThan
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
            if (input.charCodeAt(state.pos + 2) === charCodes.equalsTo) {
                // ||= or &&=
                finishOp(tt.assign, 3)
            } else {
                // || or &&
                finishOp(code === charCodes.verticalBar ? tt.logicalOR : tt.logicalAND, 2)
            }
            return
        }
    
        if (code === charCodes.verticalBar) {
            // '|>'
            if (nextChar === charCodes.greaterThan) {
                finishOp(tt.pipeline, 2)
                return
            } else if (nextChar === charCodes.rightCurlyBrace && state.isFlowEnabled) {
                // '|}'
                finishOp(tt.braceBarR, 2)
                return
            }
        }
    
        if (nextChar === charCodes.equalsTo) {
            finishOp(tt.assign, 2)
            return
        }
    
        finishOp(code === charCodes.verticalBar ? tt.bitwiseOR : tt.bitwiseAND, 1)
    }
    
    function readToken_caret(): void {
        // '^'
        const nextChar = input.charCodeAt(state.pos + 1)
        if (nextChar === charCodes.equalsTo) {
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
    
        if (nextChar === charCodes.equalsTo) {
            finishOp(tt.assign, 2)
        } else if (code === charCodes.plusSign) {
            finishOp(tt.plus, 1)
        } else {
            finishOp(tt.minus, 1)
        }
    }
    
    function readToken_lt(): void {
        const nextChar = input.charCodeAt(state.pos + 1)
    
        if (nextChar === charCodes.lessThan) {
            if (input.charCodeAt(state.pos + 2) === charCodes.equalsTo) {
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
    
        if (nextChar === charCodes.equalsTo) {
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
    
        if (nextChar === charCodes.greaterThan) {
            const size = input.charCodeAt(state.pos + 2) === charCodes.greaterThan ? 3 : 2
            if (input.charCodeAt(state.pos + size) === charCodes.equalsTo) {
                finishOp(tt.assign, size + 1)
                return
            }
            finishOp(tt.bitShiftR, size)
            return
        }
    
        if (nextChar === charCodes.equalsTo) {
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
        if (nextChar === charCodes.equalsTo) {
            finishOp(tt.equality, input.charCodeAt(state.pos + 2) === charCodes.equalsTo ? 3 : 2)
            return
        }
        if (code === charCodes.equalsTo && nextChar === charCodes.greaterThan) {
            // '=>'
            state.pos += 2
            finishToken(tt.arrow)
            return
        }
        finishOp(code === charCodes.equalsTo ? tt.eq : tt.bang, 1)
    }
    
    function readToken_question(): void {
        // '?'
        const nextChar = input.charCodeAt(state.pos + 1)
        const nextChar2 = input.charCodeAt(state.pos + 2)
        if (
            nextChar === charCodes.questionMark &&
            // In Flow (but not TypeScript), ??string is a valid type that should be
            // tokenized as two individual ? tokens.
            !(state.isFlowEnabled && state.isType)
        ) {
            if (nextChar2 === charCodes.equalsTo) {
                // '??='
                finishOp(tt.assign, 3)
            } else {
                // '??'
                finishOp(tt.nullishCoalescing, 2)
            }
        } else if (
            nextChar === charCodes.dot &&
            !(nextChar2 >= charCodes.digit0 && nextChar2 <= charCodes.digit9)
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
            case charCodes.numberSign:
                ++state.pos
                finishToken(tt.hash)
                return
    
            // The interpretation of a dot depends on whether it is followed
            // by a digit or another two dots.
    
            case charCodes.dot:
                readToken_dot()
                return
    
            // Punctuation tokens.
            case charCodes.leftParenthesis:
                ++state.pos
                finishToken(tt.parenL)
                return
            case charCodes.rightParenthesis:
                ++state.pos
                finishToken(tt.parenR)
                return
            case charCodes.semicolon:
                ++state.pos
                finishToken(tt.semi)
                return
            case charCodes.comma:
                ++state.pos
                finishToken(tt.comma)
                return
            case charCodes.leftSquareBracket:
                ++state.pos
                finishToken(tt.bracketL)
                return
            case charCodes.rightSquareBracket:
                ++state.pos
                finishToken(tt.bracketR)
                return
    
            case charCodes.leftCurlyBrace:
                if (state.isFlowEnabled && input.charCodeAt(state.pos + 1) === charCodes.verticalBar) {
                    finishOp(tt.braceBarL, 2)
                } else {
                    ++state.pos
                    finishToken(tt.braceL)
                }
                return
    
            case charCodes.rightCurlyBrace:
                ++state.pos
                finishToken(tt.braceR)
                return
    
            case charCodes.colon:
                if (input.charCodeAt(state.pos + 1) === charCodes.colon) {
                    finishOp(tt.doubleColon, 2)
                } else {
                    ++state.pos
                    finishToken(tt.colon)
                }
                return
    
            case charCodes.questionMark:
                readToken_question()
                return
            case charCodes.atSign:
                ++state.pos
                finishToken(tt.at)
                return
    
            case charCodes.graveAccent:
                ++state.pos
                finishToken(tt.backQuote)
                return
    
            case charCodes.digit0: {
                const nextChar = input.charCodeAt(state.pos + 1)
                // '0x', '0X', '0o', '0O', '0b', '0B'
                if (
                    nextChar === charCodes.lowercaseX ||
                    nextChar === charCodes.uppercaseX ||
                    nextChar === charCodes.lowercaseO ||
                    nextChar === charCodes.uppercaseO ||
                    nextChar === charCodes.lowercaseB ||
                    nextChar === charCodes.uppercaseB
                ) {
                    readRadixNumber()
                    return
                }
            }
            // Anything else beginning with a digit is an integer, octal
            // number, or float.
            case charCodes.digit1:
            case charCodes.digit2:
            case charCodes.digit3:
            case charCodes.digit4:
            case charCodes.digit5:
            case charCodes.digit6:
            case charCodes.digit7:
            case charCodes.digit8:
            case charCodes.digit9:
                readNumber(false)
                return
    
            // Quotes produce strings.
            case charCodes.quotationMark:
            case charCodes.apostrophe:
                readString(code)
                return
    
            // Operators are parsed inline in tiny state machines. '=' (charCodes.equalsTo) is
            // often referred to. `finishOp` simply skips the amount of
            // characters it is given as second argument, and returns a token
            // of the type given by its first argument.
    
            case charCodes.slash:
                readToken_slash()
                return
    
            case charCodes.percentSign:
            case charCodes.asterisk:
                readToken_mult_modulo(code)
                return
    
            case charCodes.verticalBar:
            case charCodes.ampersand:
                readToken_pipe_amp(code)
                return
    
            case charCodes.caret:
                readToken_caret()
                return
    
            case charCodes.plusSign:
            case charCodes.dash:
                readToken_plus_min(code)
                return
    
            case charCodes.lessThan:
                readToken_lt()
                return
    
            case charCodes.greaterThan:
                readToken_gt()
                return
    
            case charCodes.equalsTo:
            case charCodes.exclamationMark:
                readToken_eq_excl(code)
                return
    
            case charCodes.tilde:
                finishOp(tt.tilde, 1)
                return
    
            default:
                break
        }
    
        unexpected(`Unexpected character '${String.fromCharCode(code)}'`, state.pos)
    }    

    function readToken(code: number): void {
        // Identifier or keyword. '\uXXXX' sequences are allowed in
        // identifiers, so '\' also dispatches to that.
        if (
            IS_IDENTIFIER_START[code] ||
            code === charCodes.backslash ||
            (code === charCodes.atSign && input.charCodeAt(state.pos + 1) === charCodes.atSign)
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
                unexpected("Unexpectedly reached the end of input.")
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
        readWord
    }
}
