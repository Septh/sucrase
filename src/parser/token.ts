import type { ContextualKeyword } from "./keywords"
import type { TokenType } from "./generated/types"
import type { State } from './state'

export const enum IdentifierRole {
    Access,
    ExportAccess,
    TopLevelDeclaration,
    FunctionScopedDeclaration,
    BlockScopedDeclaration,
    ObjectShorthandTopLevelDeclaration,
    ObjectShorthandFunctionScopedDeclaration,
    ObjectShorthandBlockScopedDeclaration,
    ObjectShorthand,
    // Any identifier bound in an import statement, e.g. both A and b from
    // `import A, * as b from 'A';`
    ImportDeclaration,
    ObjectKey,
    // The `foo` in `import {foo as bar} from "./abc";`.
    ImportAccess,
}

/**
 * Extra information on jsxTagStart tokens, used to determine which of the three
 * jsx functions are called in the automatic transform.
 */
export const enum JSXRole {
    // The element is self-closing or has a body that resolves to empty. We
    // shouldn't emit children at all in this case.
    NoChildren,
    // The element has a single explicit child, which might still be an arbitrary
    // expression like an array. We should emit that expression as the children.
    OneChild,
    // The element has at least two explicitly-specified children or has spread
    // children, so child positions are assumed to be "static". We should wrap
    // these children in an array.
    StaticChildren,
    // The element has a prop named "key" after a prop spread, so we should fall
    // back to the createElement function.
    KeyAfterPropSpread,
}

export class TypeAndKeyword {
    type: TokenType
    contextualKeyword: ContextualKeyword
    constructor(type: TokenType, contextualKeyword: ContextualKeyword) {
        this.type = type
        this.contextualKeyword = contextualKeyword
    }
}

// Object type used to represent tokens. Note that normally, tokens
// simply exist as properties on the parser object. This is only
// used for the onToken callback and the external tokenizer.
export class Token {

    type: TokenType
    contextualKeyword: ContextualKeyword
    start: number
    end: number
    scopeDepth: number
    isType: boolean
    identifierRole: IdentifierRole | null
    jsxRole: JSXRole | null
    // Initially false for all tokens, then may be computed in a follow-up step that does scope analysis.
    shadowsGlobal: boolean
    // Initially false for all tokens, but may be set during transform to mark it as containing an await operation.
    isAsyncOperation: boolean
    contextId: number | null
    // For assignments, the index of the RHS. For export tokens, the end of the export.
    rhsEndIndex: number | null
    // For class tokens, records if the class is a class expression or a class statement.
    isExpression: boolean
    // Number of times to insert a `nullishCoalesce(` snippet before this token.
    numNullishCoalesceStarts: number
    // Number of times to insert a `)` snippet after this token.
    numNullishCoalesceEnds: number
    // If true, insert an `optionalChain([` snippet before this token.
    isOptionalChainStart: boolean
    // If true, insert a `])` snippet after this token.
    isOptionalChainEnd: boolean
    // Tag for `.`, `?.`, `[`, `?.[`, `(`, and `?.(` to denote the "root" token for this
    // subscript chain. This can be used to determine if this chain is an optional chain.
    subscriptStartIndex: number | null
    // Tag for `??` operators to denote the root token for this nullish coalescing call.
    nullishStartIndex: number | null

    constructor(state: State) {
        this.type = state.type
        this.contextualKeyword = state.contextualKeyword
        this.start = state.start
        this.end = state.end
        this.scopeDepth = state.scopeDepth
        this.isType = state.isType
        this.identifierRole = null
        this.jsxRole = null
        this.shadowsGlobal = false
        this.isAsyncOperation = false
        this.contextId = null
        this.rhsEndIndex = null
        this.isExpression = false
        this.numNullishCoalesceStarts = 0
        this.numNullishCoalesceEnds = 0
        this.isOptionalChainStart = false
        this.isOptionalChainEnd = false
        this.subscriptStartIndex = null
        this.nullishStartIndex = null
    }

    isDeclaration(): boolean {
        const role = this.identifierRole
        return (
            role === IdentifierRole.TopLevelDeclaration ||
            role === IdentifierRole.FunctionScopedDeclaration ||
            role === IdentifierRole.BlockScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandTopLevelDeclaration ||
            role === IdentifierRole.ObjectShorthandFunctionScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandBlockScopedDeclaration
        )
    }
    
    isNonTopLevelDeclaration(): boolean {
        const role = this.identifierRole
        return (
            role === IdentifierRole.FunctionScopedDeclaration ||
            role === IdentifierRole.BlockScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandFunctionScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandBlockScopedDeclaration
        )
    }
    
    isTopLevelDeclaration(): boolean {
        const role = this.identifierRole
        return (
            role === IdentifierRole.TopLevelDeclaration ||
            role === IdentifierRole.ObjectShorthandTopLevelDeclaration ||
            role === IdentifierRole.ImportDeclaration
        )
    }
    
    isBlockScopedDeclaration(): boolean {
        const role = this.identifierRole
        // Treat top-level declarations as block scope since the distinction doesn't matter here.
        return (
            role === IdentifierRole.TopLevelDeclaration ||
            role === IdentifierRole.BlockScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandTopLevelDeclaration ||
            role === IdentifierRole.ObjectShorthandBlockScopedDeclaration
        )
    }
    
    isFunctionScopedDeclaration(): boolean {
        const role = this.identifierRole
        return (
            role === IdentifierRole.FunctionScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandFunctionScopedDeclaration
        )
    }
    
    isObjectShorthandDeclaration(): boolean {
        const role = this.identifierRole
        return (
            role === IdentifierRole.ObjectShorthandTopLevelDeclaration ||
            role === IdentifierRole.ObjectShorthandBlockScopedDeclaration ||
            role === IdentifierRole.ObjectShorthandFunctionScopedDeclaration
        )
    }
}    
