import { getSucraseContext } from './context'
import { computeSourceMap, type RawSourceMap } from "./sourcemap"
import { RootTransformer } from "./transformers/RootTransformer"
import type { Options, SourceMapOptions, Transform } from "./options"

export type { Options, SourceMapOptions, Transform }

export function getVersion(): string {
    /* istanbul ignore next */
    return "3.35.0"
}

export interface TransformResult {
    code: string
    sourceMap?: RawSourceMap
}

export function transform(code: string, options: Options): TransformResult {
    try {
        const sucraseContext = getSucraseContext(code, options)
        const transformer = new RootTransformer(
            sucraseContext,
            options.transforms,
            Boolean(options.enableLegacyBabel5ModuleInterop),
            options,
        )
        const transformerResult = transformer.transform()
        let result: TransformResult = { code: transformerResult.code }
        if (options.sourceMapOptions) {
            if (!options.filePath) {
                throw new Error("filePath must be specified when generating a source map.")
            }
            result = {
                ...result,
                sourceMap: computeSourceMap(
                    transformerResult,
                    options.filePath,
                    options.sourceMapOptions,
                    code,
                    sucraseContext.tokenProcessor.tokens,
                ),
            }
        }
        return result
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } catch (e: any) {
        if (options.filePath) {
            e.message = `Error transforming ${options.filePath}: ${e.message}`
        }
        throw e
    }
}
