import { CJSImportProcessor } from "./transformers/processors/CJSImportProcessor"
import { HelperManager } from "./transformers/managers/HelperManager"
import { identifyShadowedGlobals } from "./transformers/util/identifyShadowedGlobals"
import { NameManager } from "./transformers/managers/NameManager"
import type { Options } from "./options"
import type { Scope } from "./parser/state"
import { TokenProcessor } from "./transformers/processors/TokenProcessor"
import { getTSImportedNames } from "./transformers/util/getTSImportedNames"
import { parse } from './parser/state'
import { formatTokens } from './transformers/util/formatTokens'

export interface SucraseContext {
    tokenProcessor: TokenProcessor
    scopes: Array<Scope>
    nameManager: NameManager
    importProcessor: CJSImportProcessor | null
    helperManager: HelperManager
}

/**
 * Call into the parser/tokenizer and do some further preprocessing:
 * - Come up with a set of used names so that we can assign new names.
 * - Preprocess all import/export statements so we know which globals we are interested in.
 * - Compute situations where any of those globals are shadowed.
 *
 * In the future, some of these preprocessing steps can be skipped based on what actual work is
 * being done.
 */
export function getSucraseContext(code: string, options: Options): SucraseContext {
    const isJSXEnabled = options.transforms.includes("jsx")
    const isTypeScriptEnabled = options.transforms.includes("typescript")
    const isFlowEnabled = options.transforms.includes("flow")
    const disableESTransforms = options.disableESTransforms === true
    const { tokens, scopes } = parse(code, isJSXEnabled, isTypeScriptEnabled, isFlowEnabled)

    const nameManager = new NameManager(code, tokens)
    const helperManager = new HelperManager(nameManager)
    const tokenProcessor = new TokenProcessor(
        code,
        tokens,
        isFlowEnabled,
        disableESTransforms,
        helperManager,
    )
    const enableLegacyTypeScriptModuleInterop = Boolean(options.enableLegacyTypeScriptModuleInterop)

    let importProcessor = null
    if (options.transforms.includes("imports")) {
        importProcessor = new CJSImportProcessor(
            nameManager,
            tokenProcessor,
            enableLegacyTypeScriptModuleInterop,
            options,
            options.transforms.includes("typescript"),
            Boolean(options.keepUnusedImports),
            helperManager,
        )
        importProcessor.preprocessTokens()
        // We need to mark shadowed globals after processing imports so we know that the globals are,
        // but before type-only import pruning, since that relies on shadowing information.
        identifyShadowedGlobals(tokenProcessor, scopes, importProcessor.getGlobalNames())
        if (options.transforms.includes("typescript") && !options.keepUnusedImports) {
            importProcessor.pruneTypeOnlyImports()
        }
    } else if (options.transforms.includes("typescript") && !options.keepUnusedImports) {
        // Shadowed global detection is needed for TS implicit elision of imported names.
        identifyShadowedGlobals(tokenProcessor, scopes, getTSImportedNames(tokenProcessor))
    }
    return { tokenProcessor, scopes, nameManager, importProcessor, helperManager }
}

/**
 * Return a string representation of the sucrase tokens, mostly useful for
 * diagnostic purposes.
 */
export function getFormattedTokens(code: string, options: Options): string {
    const tokens = getSucraseContext(code, options).tokenProcessor.tokens
    return formatTokens(code, tokens)
}
