
export const enum Charcode {
    backSpace          = 8,
    lineFeed           = 10,      //  '\n'
    tab                = 9,       //  '\t'
    carriageReturn     = 13,      //  '\r'
    shiftOut           = 14,
    space              = 32,
    exclamationMark    = 33,      //  '!'
    quotationMark      = 34,      //  '"'
    numberSign         = 35,      //  '#'
    dollarSign         = 36,      //  '$'
    percentSign        = 37,      //  '%'
    ampersand          = 38,      //  '&'
    apostrophe         = 39,      //  '''
    leftParenthesis    = 40,      //  '('
    rightParenthesis   = 41,      //  ')'
    asterisk           = 42,      //  '*'
    plusSign           = 43,      //  '+'
    comma              = 44,      //  ','
    dash               = 45,      //  '-'
    dot                = 46,      //  '.'
    slash              = 47,      //  '/'
    digit0             = 48,      //  '0'
    digit1             = 49,      //  '1'
    digit2             = 50,      //  '2'
    digit3             = 51,      //  '3'
    digit4             = 52,      //  '4'
    digit5             = 53,      //  '5'
    digit6             = 54,      //  '6'
    digit7             = 55,      //  '7'
    digit8             = 56,      //  '8'
    digit9             = 57,      //  '9'
    colon              = 58,      //  ':'
    semicolon          = 59,      //  ';'
    lessThan           = 60,      //  '<'
    equalsTo           = 61,      //  '='
    greaterThan        = 62,      //  '>'
    questionMark       = 63,      //  '?'
    atSign             = 64,      //  '@'
    uppercaseA         = 65,      //  'A'
    uppercaseB         = 66,      //  'B'
    uppercaseC         = 67,      //  'C'
    uppercaseD         = 68,      //  'D'
    uppercaseE         = 69,      //  'E'
    uppercaseF         = 70,      //  'F'
    uppercaseG         = 71,      //  'G'
    uppercaseH         = 72,      //  'H'
    uppercaseI         = 73,      //  'I'
    uppercaseJ         = 74,      //  'J'
    uppercaseK         = 75,      //  'K'
    uppercaseL         = 76,      //  'L'
    uppercaseM         = 77,      //  'M'
    uppercaseN         = 78,      //  'N'
    uppercaseO         = 79,      //  'O'
    uppercaseP         = 80,      //  'P'
    uppercaseQ         = 81,      //  'Q'
    uppercaseR         = 82,      //  'R'
    uppercaseS         = 83,      //  'S'
    uppercaseT         = 84,      //  'T'
    uppercaseU         = 85,      //  'U'
    uppercaseV         = 86,      //  'V'
    uppercaseW         = 87,      //  'W'
    uppercaseX         = 88,      //  'X'
    uppercaseY         = 89,      //  'Y'
    uppercaseZ         = 90,      //  'Z'
    leftSquareBracket  = 91,      //  '['
    backslash          = 92,      //  '\    '
    rightSquareBracket = 93,      //  ']'
    caret              = 94,      //  '^'
    underscore         = 95,      //  '_'
    graveAccent        = 96,      //  '`'
    lowercaseA         = 97,      //  'a'
    lowercaseB         = 98,      //  'b'
    lowercaseC         = 99,      //  'c'
    lowercaseD         = 100,     //  'd'
    lowercaseE         = 101,     //  'e'
    lowercaseF         = 102,     //  'f'
    lowercaseG         = 103,     //  'g'
    lowercaseH         = 104,     //  'h'
    lowercaseI         = 105,     //  'i'
    lowercaseJ         = 106,     //  'j'
    lowercaseK         = 107,     //  'k'
    lowercaseL         = 108,     //  'l'
    lowercaseM         = 109,     //  'm'
    lowercaseN         = 110,     //  'n'
    lowercaseO         = 111,     //  'o'
    lowercaseP         = 112,     //  'p'
    lowercaseQ         = 113,     //  'q'
    lowercaseR         = 114,     //  'r'
    lowercaseS         = 115,     //  's'
    lowercaseT         = 116,     //  't'
    lowercaseU         = 117,     //  'u'
    lowercaseV         = 118,     //  'v'
    lowercaseW         = 119,     //  'w'
    lowercaseX         = 120,     //  'x'
    lowercaseY         = 121,     //  'y'
    lowercaseZ         = 122,     //  'z'
    leftCurlyBrace     = 123,     //  '{'
    verticalBar        = 124,     //  '|'
    rightCurlyBrace    = 125,     //  '}'
    tilde              = 126,     //  '~'
    nonBreakingSpace   = 160,
    // eslint-disable-next-line no-irregular-whitespace
    oghamSpaceMark     = 5760,    // ' '
    lineSeparator      = 8232,
    paragraphSeparator = 8233,
}

export function isDigit(code: number): boolean {
    return (
        (code >= Charcode.digit0 && code <= Charcode.digit9) ||
        (code >= Charcode.lowercaseA && code <= Charcode.lowercaseF) ||
        (code >= Charcode.uppercaseA && code <= Charcode.uppercaseF)
    )
}

// https://tc39.github.io/ecma262/#sec-white-space
export const WHITESPACE_CHARS: Array<number> = [
    0x0009,
    0x000b,
    0x000c,
    Charcode.space,
    Charcode.nonBreakingSpace,
    Charcode.oghamSpaceMark,
    0x2000, // EN QUAD
    0x2001, // EM QUAD
    0x2002, // EN SPACE
    0x2003, // EM SPACE
    0x2004, // THREE-PER-EM SPACE
    0x2005, // FOUR-PER-EM SPACE
    0x2006, // SIX-PER-EM SPACE
    0x2007, // FIGURE SPACE
    0x2008, // PUNCTUATION SPACE
    0x2009, // THIN SPACE
    0x200a, // HAIR SPACE
    0x202f, // NARROW NO-BREAK SPACE
    0x205f, // MEDIUM MATHEMATICAL SPACE
    0x3000, // IDEOGRAPHIC SPACE
    0xfeff, // ZERO WIDTH NO-BREAK SPACE
]

export const skipWhiteSpace = /(?:\s|\/\/.*|\/\*[^]*?\*\/)*/g

export const IS_WHITESPACE = new Uint8Array(65536)
for (const char of WHITESPACE_CHARS) {
    IS_WHITESPACE[char] = 1
}

function computeIsIdentifierChar(code: number): boolean {
    if (code < 48) return code === 36
    if (code < 58) return true
    if (code < 65) return false
    if (code < 91) return true
    if (code < 97) return code === 95
    if (code < 123) return true
    if (code < 128) return false
    throw new Error("Should not be called with non-ASCII char code.")
}

export const IS_IDENTIFIER_CHAR = new Uint8Array(65536)
for (let i = 0; i < 128; i++) {
    IS_IDENTIFIER_CHAR[i] = computeIsIdentifierChar(i) ? 1 : 0
}
for (let i = 128; i < 65536; i++) {
    IS_IDENTIFIER_CHAR[i] = 1
}
// Aside from whitespace and newlines, all characters outside the ASCII space are either
// identifier characters or invalid. Since we're not performing code validation, we can just
// treat all invalid characters as identifier characters.
for (const whitespaceChar of WHITESPACE_CHARS) {
    IS_IDENTIFIER_CHAR[whitespaceChar] = 0
}
IS_IDENTIFIER_CHAR[0x2028] = 0
IS_IDENTIFIER_CHAR[0x2029] = 0

export const IS_IDENTIFIER_START = IS_IDENTIFIER_CHAR.slice()
for (let numChar = Charcode.digit0; numChar <= Charcode.digit9; numChar++) {
    IS_IDENTIFIER_START[numChar] = 0
}
