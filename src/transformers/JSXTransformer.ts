import type { CJSImportProcessor } from "../processors/CJSImportProcessor"
import type { Options } from "../index"
import type { NameManager } from "../managers/NameManager"
import { JSXRole } from "../parser/token"
import { TokenType as tt } from "../parser/generated/types"
import { Charcode } from "../parser/charcode"
import type { TokenProcessor } from "../processors/TokenProcessor"
import { getJSXPragmaInfo, type JSXPragmaInfo } from "../util/getJSXPragmaInfo"
import type { RootTransformer } from "./RootTransformer"
import Transformer from "./Transformer"

export class JSXTransformer extends Transformer {
	jsxPragmaInfo: JSXPragmaInfo
	jsxImportSource: string
	isAutomaticRuntime: boolean

	// State for calculating the line number of each JSX tag in development.
	lastLineNumber: number = 1;
	lastIndex: number = 0;

	// In development, variable name holding the name of the current file.
	filenameVarName: string | null = null;
	// Mapping of claimed names for imports in the automatic transform, e,g.
	// {jsx: "_jsx"}. This determines which imports to generate in the prefix.
	esmAutomaticImportNameResolutions: { [name: string]: string } = {};
	// When automatically adding imports in CJS mode, we store the variable name
	// holding the imported CJS module so we can require it in the prefix.
	cjsAutomaticModuleNameResolutions: { [path: string]: string } = {};

	constructor(
		readonly rootTransformer: RootTransformer,
		readonly tokens: TokenProcessor,
		readonly importProcessor: CJSImportProcessor | null,
		readonly nameManager: NameManager,
		readonly options: Options,
	) {
		super()
		this.jsxPragmaInfo = getJSXPragmaInfo(options)
		this.isAutomaticRuntime = options.jsxRuntime === "automatic"
		this.jsxImportSource = options.jsxImportSource || "react"
	}

	process(): boolean {
		if (this.tokens.matches1(tt.jsxTagStart)) {
			this.processJSXTag()
			return true
		}
		return false
	}

	override getPrefixCode(): string {
		let prefix = ""
		if (this.filenameVarName) {
			prefix += `const ${this.filenameVarName} = ${JSON.stringify(this.options.filePath || "")};`
		}
		if (this.isAutomaticRuntime) {
			if (this.importProcessor) {
				// CJS mode: emit require statements for all modules that were referenced.
				for (const [path, resolvedName] of Object.entries(this.cjsAutomaticModuleNameResolutions)) {
					prefix += `var ${resolvedName} = require("${path}");`
				}
			} else {
				// ESM mode: consolidate and emit import statements for referenced names.
				const { createElement: createElementResolution, ...otherResolutions } =
					this.esmAutomaticImportNameResolutions
				if (createElementResolution) {
					prefix += `import {createElement as ${createElementResolution}} from "${this.jsxImportSource}";`
				}
				const importSpecifiers = Object.entries(otherResolutions)
					.map(([name, resolvedName]) => `${name} as ${resolvedName}`)
					.join(", ")
				if (importSpecifiers) {
					const importPath =
						this.jsxImportSource + (this.options.production ? "/jsx-runtime" : "/jsx-dev-runtime")
					prefix += `import {${importSpecifiers}} from "${importPath}";`
				}
			}
		}
		return prefix
	}

	processJSXTag(): void {
		const { jsxRole, start } = this.tokens.currentToken()
		// Calculate line number information at the very start (if in development
		// mode) so that the information is guaranteed to be queried in token order.
		const elementLocationCode = this.options.production ? null : this.getElementLocationCode(start)
		if (this.isAutomaticRuntime && jsxRole !== JSXRole.KeyAfterPropSpread) {
			this.transformTagToJSXFunc(elementLocationCode, jsxRole!)
		} else {
			this.transformTagToCreateElement(elementLocationCode)
		}
	}

	getElementLocationCode(firstTokenStart: number): string {
		const lineNumber = this.getLineNumberForIndex(firstTokenStart)
		return `lineNumber: ${lineNumber}`
	}

	/**
	 * Get the line number for this source position. This is calculated lazily and
	 * must be called in increasing order by index.
	 */
	getLineNumberForIndex(index: number): number {
		const code = this.tokens.code
		while (this.lastIndex < index && this.lastIndex < code.length) {
			if (code[this.lastIndex] === "\n") {
				this.lastLineNumber++
			}
			this.lastIndex++
		}
		return this.lastLineNumber
	}

	/**
	 * Convert the current JSX element to a call to jsx, jsxs, or jsxDEV. This is
	 * the primary transformation for the automatic transform.
	 *
	 * Example:
	 * <div a={1} key={2}>Hello{x}</div>
	 * becomes
	 * jsxs('div', {a: 1, children: ["Hello", x]}, 2)
	 */
	transformTagToJSXFunc(elementLocationCode: string | null, jsxRole: JSXRole): void {
		const isStatic = jsxRole === JSXRole.StaticChildren
		// First tag is always jsxTagStart.
		this.tokens.replaceToken(this.getJSXFuncInvocationCode(isStatic))

		let keyCode = null
		if (this.tokens.matches1(tt.jsxTagEnd)) {
			// Fragment syntax.
			this.tokens.replaceToken(`${this.getFragmentCode()}, {`)
			this.processAutomaticChildrenAndEndProps(jsxRole)
		} else {
			// Normal open tag or self-closing tag.
			this.processTagIntro()
			this.tokens.appendCode(", {")
			keyCode = this.processProps(true)

			if (this.tokens.matches2(tt.slash, tt.jsxTagEnd)) {
				// Self-closing tag, no children to add, so close the props.
				this.tokens.appendCode("}")
			} else if (this.tokens.matches1(tt.jsxTagEnd)) {
				// Tag with children.
				this.tokens.removeToken()
				this.processAutomaticChildrenAndEndProps(jsxRole)
			} else {
				throw new Error("Expected either /> or > at the end of the tag.")
			}
			// If a key was present, move it to its own arg. Note that moving code
			// like this will cause line numbers to get out of sync within the JSX
			// element if the key expression has a newline in it. This is unfortunate,
			// but hopefully should be rare.
			if (keyCode) {
				this.tokens.appendCode(`, ${keyCode}`)
			}
		}
		if (!this.options.production) {
			// If the key wasn't already added, add it now so we can correctly set
			// positional args for jsxDEV.
			if (keyCode === null) {
				this.tokens.appendCode(", void 0")
			}
			this.tokens.appendCode(`, ${isStatic}, ${this.getDevSource(elementLocationCode!)}, this`)
		}
		// We're at the close-tag or the end of a self-closing tag, so remove
		// everything else and close the function call.
		this.tokens.removeInitialToken()
		while (!this.tokens.matches1(tt.jsxTagEnd)) {
			this.tokens.removeToken()
		}
		this.tokens.replaceToken(")")
	}

	/**
	 * Convert the current JSX element to a createElement call. In the classic
	 * runtime, this is the only case. In the automatic runtime, this is called
	 * as a fallback in some situations.
	 *
	 * Example:
	 * <div a={1} key={2}>Hello{x}</div>
	 * becomes
	 * React.createElement('div', {a: 1, key: 2}, "Hello", x)
	 */
	transformTagToCreateElement(elementLocationCode: string | null): void {
		// First tag is always jsxTagStart.
		this.tokens.replaceToken(this.getCreateElementInvocationCode())

		if (this.tokens.matches1(tt.jsxTagEnd)) {
			// Fragment syntax.
			this.tokens.replaceToken(`${this.getFragmentCode()}, null`)
			this.processChildren(true)
		} else {
			// Normal open tag or self-closing tag.
			this.processTagIntro()
			this.processPropsObjectWithDevInfo(elementLocationCode)

			if (this.tokens.matches2(tt.slash, tt.jsxTagEnd)) {
				// Self-closing tag; no children to process.
			} else if (this.tokens.matches1(tt.jsxTagEnd)) {
				// Tag with children and a close-tag; process the children as args.
				this.tokens.removeToken()
				this.processChildren(true)
			} else {
				throw new Error("Expected either /> or > at the end of the tag.")
			}
		}
		// We're at the close-tag or the end of a self-closing tag, so remove
		// everything else and close the function call.
		this.tokens.removeInitialToken()
		while (!this.tokens.matches1(tt.jsxTagEnd)) {
			this.tokens.removeToken()
		}
		this.tokens.replaceToken(")")
	}

	/**
	 * Get the code for the relevant function for this context: jsx, jsxs,
	 * or jsxDEV. The following open-paren is included as well.
	 *
	 * These functions are only used for the automatic runtime, so they are always
	 * auto-imported, but the auto-import will be either CJS or ESM based on the
	 * target module format.
	 */
	getJSXFuncInvocationCode(isStatic: boolean): string {
		if (this.options.production) {
			if (isStatic) {
				return this.claimAutoImportedFuncInvocation("jsxs", "/jsx-runtime")
			} else {
				return this.claimAutoImportedFuncInvocation("jsx", "/jsx-runtime")
			}
		} else {
			return this.claimAutoImportedFuncInvocation("jsxDEV", "/jsx-dev-runtime")
		}
	}

	/**
	 * Return the code to use for the createElement function, e.g.
	 * `React.createElement`, including the following open-paren.
	 *
	 * This is the main function to use for the classic runtime. For the
	 * automatic runtime, this function is used as a fallback function to
	 * preserve behavior when there is a prop spread followed by an explicit
	 * key. In that automatic runtime case, the function should be automatically
	 * imported.
	 */
	getCreateElementInvocationCode(): string {
		if (this.isAutomaticRuntime) {
			return this.claimAutoImportedFuncInvocation("createElement", "")
		} else {
			const { jsxPragmaInfo } = this
			const resolvedPragmaBaseName = this.importProcessor
				? this.importProcessor.getIdentifierReplacement(jsxPragmaInfo.base) || jsxPragmaInfo.base
				: jsxPragmaInfo.base
			return `${resolvedPragmaBaseName}${jsxPragmaInfo.suffix}(`
		}
	}

	/**
	 * Return the code to use as the component when compiling a shorthand
	 * fragment, e.g. `React.Fragment`.
	 *
	 * This may be called from either the classic or automatic runtime, and
	 * the value should be auto-imported for the automatic runtime.
	 */
	getFragmentCode(): string {
		if (this.isAutomaticRuntime) {
			return this.claimAutoImportedName(
				"Fragment",
				this.options.production ? "/jsx-runtime" : "/jsx-dev-runtime",
			)
		} else {
			const { jsxPragmaInfo } = this
			const resolvedFragmentPragmaBaseName = this.importProcessor
				? this.importProcessor.getIdentifierReplacement(jsxPragmaInfo.fragmentBase) ||
				jsxPragmaInfo.fragmentBase
				: jsxPragmaInfo.fragmentBase
			return resolvedFragmentPragmaBaseName + jsxPragmaInfo.fragmentSuffix
		}
	}

	/**
	 * Return code that invokes the given function.
	 *
	 * When the imports transform is enabled, use the CJSImportTransformer
	 * strategy of using `.call(void 0, ...` to avoid passing a `this` value in a
	 * situation that would otherwise look like a method call.
	 */
	claimAutoImportedFuncInvocation(funcName: string, importPathSuffix: string): string {
		const funcCode = this.claimAutoImportedName(funcName, importPathSuffix)
		if (this.importProcessor) {
			return `${funcCode}.call(void 0, `
		} else {
			return `${funcCode}(`
		}
	}

	claimAutoImportedName(funcName: string, importPathSuffix: string): string {
		if (this.importProcessor) {
			// CJS mode: claim a name for the module and mark it for import.
			const path = this.jsxImportSource + importPathSuffix
			if (!this.cjsAutomaticModuleNameResolutions[path]) {
				this.cjsAutomaticModuleNameResolutions[path] =
					this.importProcessor.getFreeIdentifierForPath(path)
			}
			return `${this.cjsAutomaticModuleNameResolutions[path]}.${funcName}`
		} else {
			// ESM mode: claim a name for this function and add it to the names that
			// should be auto-imported when the prefix is generated.
			if (!this.esmAutomaticImportNameResolutions[funcName]) {
				this.esmAutomaticImportNameResolutions[funcName] = this.nameManager.claimFreeName(
					`_${funcName}`,
				)
			}
			return this.esmAutomaticImportNameResolutions[funcName]
		}
	}

	/**
	 * Process the first part of a tag, before any props.
	 */
	processTagIntro(): void {
		// Walk forward until we see one of these patterns:
		// jsxName to start the first prop, preceded by another jsxName to end the tag name.
		// jsxName to start the first prop, preceded by greaterThan to end the type argument.
		// [open brace] to start the first prop.
		// [jsxTagEnd] to end the open-tag.
		// [slash, jsxTagEnd] to end the self-closing tag.
		let introEnd = this.tokens.currentIndex() + 1
		while (
			this.tokens.tokens[introEnd].isType ||
			(!this.tokens.matches2AtIndex(introEnd - 1, tt.jsxName, tt.jsxName) &&
				!this.tokens.matches2AtIndex(introEnd - 1, tt.greaterThan, tt.jsxName) &&
				!this.tokens.matches1AtIndex(introEnd, tt.braceL) &&
				!this.tokens.matches1AtIndex(introEnd, tt.jsxTagEnd) &&
				!this.tokens.matches2AtIndex(introEnd, tt.slash, tt.jsxTagEnd))
		) {
			introEnd++
		}
		if (introEnd === this.tokens.currentIndex() + 1) {
			const tagName = this.tokens.identifierName()
			if (startsWithLowerCase(tagName)) {
				this.tokens.replaceToken(`'${tagName}'`)
			}
		}
		while (this.tokens.currentIndex() < introEnd) {
			this.rootTransformer.processToken()
		}
	}

	/**
	 * Starting at the beginning of the props, add the props argument to
	 * React.createElement, including the comma before it.
	 */
	processPropsObjectWithDevInfo(elementLocationCode: string | null): void {
		const devProps = this.options.production
			? ""
			: `__self: this, __source: ${this.getDevSource(elementLocationCode!)}`
		if (!this.tokens.matches1(tt.jsxName) && !this.tokens.matches1(tt.braceL)) {
			if (devProps) {
				this.tokens.appendCode(`, {${devProps}}`)
			} else {
				this.tokens.appendCode(`, null`)
			}
			return
		}
		this.tokens.appendCode(`, {`)
		this.processProps(false)
		if (devProps) {
			this.tokens.appendCode(` ${devProps}}`)
		} else {
			this.tokens.appendCode("}")
		}
	}

	/**
	 * Transform the core part of the props, assuming that a { has already been
	 * inserted before us and that a } will be inserted after us.
	 *
	 * If extractKeyCode is true (i.e. when using any jsx... function), any prop
	 * named "key" has its code captured and returned rather than being emitted to
	 * the output code. This shifts line numbers, and emitting the code later will
	 * correct line numbers again. If no key is found or if extractKeyCode is
	 * false, this function returns null.
	 */
	processProps(extractKeyCode: boolean): string | null {
		let keyCode = null
		while (true) {
			if (this.tokens.matches2(tt.jsxName, tt.eq)) {
				// This is a regular key={value} or key="value" prop.
				const propName = this.tokens.identifierName()
				if (extractKeyCode && propName === "key") {
					if (keyCode !== null) {
						// The props list has multiple keys. Different implementations are
						// inconsistent about what to do here: as of this writing, Babel and
						// swc keep the *last* key and completely remove the rest, while
						// TypeScript uses the *first* key and leaves the others as regular
						// props. The React team collaborated with Babel on the
						// implementation of this behavior, so presumably the Babel behavior
						// is the one to use.
						// Since we won't ever be emitting the previous key code, we need to
						// at least emit its newlines here so that the line numbers match up
						// in the long run.
						this.tokens.appendCode(keyCode.replace(/[^\n]/g, ""))
					}
					// key
					this.tokens.removeToken()
					// =
					this.tokens.removeToken()
					const snapshot = this.tokens.snapshot()
					this.processPropValue()
					keyCode = this.tokens.dangerouslyGetAndRemoveCodeSinceSnapshot(snapshot)
					// Don't add a comma
					continue
				} else {
					this.processPropName(propName)
					this.tokens.replaceToken(": ")
					this.processPropValue()
				}
			} else if (this.tokens.matches1(tt.jsxName)) {
				// This is a shorthand prop like <input disabled />.
				const propName = this.tokens.identifierName()
				this.processPropName(propName)
				this.tokens.appendCode(": true")
			} else if (this.tokens.matches1(tt.braceL)) {
				// This is prop spread, like <div {...getProps()}>, which we can pass
				// through fairly directly as an object spread.
				this.tokens.replaceToken("")
				this.rootTransformer.processBalancedCode()
				this.tokens.replaceToken("")
			} else {
				break
			}
			this.tokens.appendCode(",")
		}
		return keyCode
	}

	processPropName(propName: string): void {
		if (propName.includes("-")) {
			this.tokens.replaceToken(`'${propName}'`)
		} else {
			this.tokens.copyToken()
		}
	}

	processPropValue(): void {
		if (this.tokens.matches1(tt.braceL)) {
			this.tokens.replaceToken("")
			this.rootTransformer.processBalancedCode()
			this.tokens.replaceToken("")
		} else if (this.tokens.matches1(tt.jsxTagStart)) {
			this.processJSXTag()
		} else {
			this.processStringPropValue()
		}
	}

	processStringPropValue(): void {
		const token = this.tokens.currentToken()
		const valueCode = this.tokens.code.slice(token.start + 1, token.end - 1)
		const replacementCode = formatJSXTextReplacement(valueCode)
		const literalCode = formatJSXStringValueLiteral(valueCode)
		this.tokens.replaceToken(literalCode + replacementCode)
	}

	/**
	 * Starting in the middle of the props object literal, produce an additional
	 * prop for the children and close the object literal.
	 */
	processAutomaticChildrenAndEndProps(jsxRole: JSXRole): void {
		if (jsxRole === JSXRole.StaticChildren) {
			this.tokens.appendCode(" children: [")
			this.processChildren(false)
			this.tokens.appendCode("]}")
		} else {
			// The parser information tells us whether we will see a real child or if
			// all remaining children (if any) will resolve to empty. If there are no
			// non-empty children, don't emit a children prop at all, but still
			// process children so that we properly transform the code into nothing.
			if (jsxRole === JSXRole.OneChild) {
				this.tokens.appendCode(" children: ")
			}
			this.processChildren(false)
			this.tokens.appendCode("}")
		}
	}

	/**
	 * Transform children into a comma-separated list, which will be either
	 * arguments to createElement or array elements of a children prop.
	 */
	processChildren(needsInitialComma: boolean): void {
		let needsComma = needsInitialComma
		while (true) {
			if (this.tokens.matches2(tt.jsxTagStart, tt.slash)) {
				// Closing tag, so no more children.
				return
			}
			let didEmitElement = false
			if (this.tokens.matches1(tt.braceL)) {
				if (this.tokens.matches2(tt.braceL, tt.braceR)) {
					// Empty interpolations and comment-only interpolations are allowed
					// and don't create an extra child arg.
					this.tokens.replaceToken("")
					this.tokens.replaceToken("")
				} else {
					// Interpolated expression.
					this.tokens.replaceToken(needsComma ? ", " : "")
					this.rootTransformer.processBalancedCode()
					this.tokens.replaceToken("")
					didEmitElement = true
				}
			} else if (this.tokens.matches1(tt.jsxTagStart)) {
				// Child JSX element
				this.tokens.appendCode(needsComma ? ", " : "")
				this.processJSXTag()
				didEmitElement = true
			} else if (this.tokens.matches1(tt.jsxText) || this.tokens.matches1(tt.jsxEmptyText)) {
				didEmitElement = this.processChildTextElement(needsComma)
			} else {
				throw new Error("Unexpected token when processing JSX children.")
			}
			if (didEmitElement) {
				needsComma = true
			}
		}
	}

	/**
	 * Turn a JSX text element into a string literal, or nothing at all if the JSX
	 * text resolves to the empty string.
	 *
	 * Returns true if a string literal is emitted, false otherwise.
	 */
	processChildTextElement(needsComma: boolean): boolean {
		const token = this.tokens.currentToken()
		const valueCode = this.tokens.code.slice(token.start, token.end)
		const replacementCode = formatJSXTextReplacement(valueCode)
		const literalCode = formatJSXTextLiteral(valueCode)
		if (literalCode === '""') {
			this.tokens.replaceToken(replacementCode)
			return false
		} else {
			this.tokens.replaceToken(`${needsComma ? ", " : ""}${literalCode}${replacementCode}`)
			return true
		}
	}

	getDevSource(elementLocationCode: string): string {
		return `{fileName: ${this.getFilenameVarName()}, ${elementLocationCode}}`
	}

	getFilenameVarName(): string {
		if (!this.filenameVarName) {
			this.filenameVarName = this.nameManager.claimFreeName("_jsxFileName")
		}
		return this.filenameVarName
	}
}

/**
 * Spec for identifiers: https://tc39.github.io/ecma262/#prod-IdentifierStart.
 *
 * Really only treat anything starting with a-z as tag names.  `_`, `$`, `é`
 * should be treated as component names
 */
export function startsWithLowerCase(s: string): boolean {
	const firstChar = s.charCodeAt(0)
	return firstChar >= Charcode.lowercaseA && firstChar <= Charcode.lowercaseZ
}

/**
 * Turn the given jsxText string into a JS string literal. Leading and trailing
 * whitespace on lines is removed, except immediately after the open-tag and
 * before the close-tag. Empty lines are completely removed, and spaces are
 * added between lines after that.
 *
 * We use JSON.stringify to introduce escape characters as necessary, and trim
 * the start and end of each line and remove blank lines.
 */
function formatJSXTextLiteral(text: string): string {
	let result = ""
	let whitespace = ""

	let isInInitialLineWhitespace = false
	let seenNonWhitespace = false
	for (let i = 0; i < text.length; i++) {
		const c = text[i]
		if (c === " " || c === "\t" || c === "\r") {
			if (!isInInitialLineWhitespace) {
				whitespace += c
			}
		} else if (c === "\n") {
			whitespace = ""
			isInInitialLineWhitespace = true
		} else {
			if (seenNonWhitespace && isInInitialLineWhitespace) {
				result += " "
			}
			result += whitespace
			whitespace = ""
			if (c === "&") {
				const { entity, newI } = processEntity(text, i + 1)
				i = newI - 1
				result += entity
			} else {
				result += c
			}
			seenNonWhitespace = true
			isInInitialLineWhitespace = false
		}
	}
	if (!isInInitialLineWhitespace) {
		result += whitespace
	}
	return JSON.stringify(result)
}

/**
 * Produce the code that should be printed after the JSX text string literal,
 * with most content removed, but all newlines preserved and all spacing at the
 * end preserved.
 */
function formatJSXTextReplacement(text: string): string {
	let numNewlines = 0
	let numSpaces = 0
	for (const c of text) {
		if (c === "\n") {
			numNewlines++
			numSpaces = 0
		} else if (c === " ") {
			numSpaces++
		}
	}
	return "\n".repeat(numNewlines) + " ".repeat(numSpaces)
}

/**
 * Format a string in the value position of a JSX prop.
 *
 * Use the same implementation as convertAttribute from
 * babel-helper-builder-react-jsx.
 */
function formatJSXStringValueLiteral(text: string): string {
	let result = ""
	for (let i = 0; i < text.length; i++) {
		const c = text[i]
		if (c === "\n") {
			if (/\s/.test(text[i + 1])) {
				result += " "
				while (i < text.length && /\s/.test(text[i + 1])) {
					i++
				}
			} else {
				result += "\n"
			}
		} else if (c === "&") {
			const { entity, newI } = processEntity(text, i + 1)
			result += entity
			i = newI - 1
		} else {
			result += c
		}
	}
	return JSON.stringify(result)
}

/**
 * Starting at a &, see if there's an HTML entity (specified by name, decimal
 * char code, or hex char code) and return it if so.
 *
 * Modified from jsxReadString in babel-parser.
 */
function processEntity(text: string, indexAfterAmpersand: number): { entity: string; newI: number } {
	let str = ""
	let count = 0
	let entity
	let i = indexAfterAmpersand

	if (text[i] === "#") {
		let radix = 10
		i++
		let numStart
		if (text[i] === "x") {
			radix = 16
			i++
			numStart = i
			while (i < text.length && isHexDigit(text.charCodeAt(i))) {
				i++
			}
		} else {
			numStart = i
			while (i < text.length && isDecimalDigit(text.charCodeAt(i))) {
				i++
			}
		}
		if (text[i] === ";") {
			const numStr = text.slice(numStart, i)
			if (numStr) {
				i++
				entity = String.fromCodePoint(parseInt(numStr, radix))
			}
		}
	} else {
		while (i < text.length && count++ < 10) {
			const ch = text[i]
			i++
			if (ch === ";") {
				entity = XHTMLEntities.get(str)
				break
			}
			str += ch
		}
	}

	if (!entity) {
		return { entity: "&", newI: indexAfterAmpersand }
	}
	return { entity, newI: i }
}

function isDecimalDigit(code: number): boolean {
	return code >= Charcode.digit0 && code <= Charcode.digit9
}

function isHexDigit(code: number): boolean {
	return (
		(code >= Charcode.digit0 && code <= Charcode.digit9) ||
		(code >= Charcode.lowercaseA && code <= Charcode.lowercaseF) ||
		(code >= Charcode.uppercaseA && code <= Charcode.uppercaseF)
	)
}

// Use a Map rather than object to avoid unexpected __proto__ access.
export const XHTMLEntities = new Map<string, string>([
	["quot", "\u0022"],
	["amp", "&"],
	["apos", "\u0027"],
	["lt", "<"],
	["gt", ">"],
	["nbsp", "\u00A0"],
	["iexcl", "\u00A1"],
	["cent", "\u00A2"],
	["pound", "\u00A3"],
	["curren", "\u00A4"],
	["yen", "\u00A5"],
	["brvbar", "\u00A6"],
	["sect", "\u00A7"],
	["uml", "\u00A8"],
	["copy", "\u00A9"],
	["ordf", "\u00AA"],
	["laquo", "\u00AB"],
	["not", "\u00AC"],
	["shy", "\u00AD"],
	["reg", "\u00AE"],
	["macr", "\u00AF"],
	["deg", "\u00B0"],
	["plusmn", "\u00B1"],
	["sup2", "\u00B2"],
	["sup3", "\u00B3"],
	["acute", "\u00B4"],
	["micro", "\u00B5"],
	["para", "\u00B6"],
	["middot", "\u00B7"],
	["cedil", "\u00B8"],
	["sup1", "\u00B9"],
	["ordm", "\u00BA"],
	["raquo", "\u00BB"],
	["frac14", "\u00BC"],
	["frac12", "\u00BD"],
	["frac34", "\u00BE"],
	["iquest", "\u00BF"],
	["Agrave", "\u00C0"],
	["Aacute", "\u00C1"],
	["Acirc", "\u00C2"],
	["Atilde", "\u00C3"],
	["Auml", "\u00C4"],
	["Aring", "\u00C5"],
	["AElig", "\u00C6"],
	["Ccedil", "\u00C7"],
	["Egrave", "\u00C8"],
	["Eacute", "\u00C9"],
	["Ecirc", "\u00CA"],
	["Euml", "\u00CB"],
	["Igrave", "\u00CC"],
	["Iacute", "\u00CD"],
	["Icirc", "\u00CE"],
	["Iuml", "\u00CF"],
	["ETH", "\u00D0"],
	["Ntilde", "\u00D1"],
	["Ograve", "\u00D2"],
	["Oacute", "\u00D3"],
	["Ocirc", "\u00D4"],
	["Otilde", "\u00D5"],
	["Ouml", "\u00D6"],
	["times", "\u00D7"],
	["Oslash", "\u00D8"],
	["Ugrave", "\u00D9"],
	["Uacute", "\u00DA"],
	["Ucirc", "\u00DB"],
	["Uuml", "\u00DC"],
	["Yacute", "\u00DD"],
	["THORN", "\u00DE"],
	["szlig", "\u00DF"],
	["agrave", "\u00E0"],
	["aacute", "\u00E1"],
	["acirc", "\u00E2"],
	["atilde", "\u00E3"],
	["auml", "\u00E4"],
	["aring", "\u00E5"],
	["aelig", "\u00E6"],
	["ccedil", "\u00E7"],
	["egrave", "\u00E8"],
	["eacute", "\u00E9"],
	["ecirc", "\u00EA"],
	["euml", "\u00EB"],
	["igrave", "\u00EC"],
	["iacute", "\u00ED"],
	["icirc", "\u00EE"],
	["iuml", "\u00EF"],
	["eth", "\u00F0"],
	["ntilde", "\u00F1"],
	["ograve", "\u00F2"],
	["oacute", "\u00F3"],
	["ocirc", "\u00F4"],
	["otilde", "\u00F5"],
	["ouml", "\u00F6"],
	["divide", "\u00F7"],
	["oslash", "\u00F8"],
	["ugrave", "\u00F9"],
	["uacute", "\u00FA"],
	["ucirc", "\u00FB"],
	["uuml", "\u00FC"],
	["yacute", "\u00FD"],
	["thorn", "\u00FE"],
	["yuml", "\u00FF"],
	["OElig", "\u0152"],
	["oelig", "\u0153"],
	["Scaron", "\u0160"],
	["scaron", "\u0161"],
	["Yuml", "\u0178"],
	["fnof", "\u0192"],
	["circ", "\u02C6"],
	["tilde", "\u02DC"],
	["Alpha", "\u0391"],
	["Beta", "\u0392"],
	["Gamma", "\u0393"],
	["Delta", "\u0394"],
	["Epsilon", "\u0395"],
	["Zeta", "\u0396"],
	["Eta", "\u0397"],
	["Theta", "\u0398"],
	["Iota", "\u0399"],
	["Kappa", "\u039A"],
	["Lambda", "\u039B"],
	["Mu", "\u039C"],
	["Nu", "\u039D"],
	["Xi", "\u039E"],
	["Omicron", "\u039F"],
	["Pi", "\u03A0"],
	["Rho", "\u03A1"],
	["Sigma", "\u03A3"],
	["Tau", "\u03A4"],
	["Upsilon", "\u03A5"],
	["Phi", "\u03A6"],
	["Chi", "\u03A7"],
	["Psi", "\u03A8"],
	["Omega", "\u03A9"],
	["alpha", "\u03B1"],
	["beta", "\u03B2"],
	["gamma", "\u03B3"],
	["delta", "\u03B4"],
	["epsilon", "\u03B5"],
	["zeta", "\u03B6"],
	["eta", "\u03B7"],
	["theta", "\u03B8"],
	["iota", "\u03B9"],
	["kappa", "\u03BA"],
	["lambda", "\u03BB"],
	["mu", "\u03BC"],
	["nu", "\u03BD"],
	["xi", "\u03BE"],
	["omicron", "\u03BF"],
	["pi", "\u03C0"],
	["rho", "\u03C1"],
	["sigmaf", "\u03C2"],
	["sigma", "\u03C3"],
	["tau", "\u03C4"],
	["upsilon", "\u03C5"],
	["phi", "\u03C6"],
	["chi", "\u03C7"],
	["psi", "\u03C8"],
	["omega", "\u03C9"],
	["thetasym", "\u03D1"],
	["upsih", "\u03D2"],
	["piv", "\u03D6"],
	["ensp", "\u2002"],
	["emsp", "\u2003"],
	["thinsp", "\u2009"],
	["zwnj", "\u200C"],
	["zwj", "\u200D"],
	["lrm", "\u200E"],
	["rlm", "\u200F"],
	["ndash", "\u2013"],
	["mdash", "\u2014"],
	["lsquo", "\u2018"],
	["rsquo", "\u2019"],
	["sbquo", "\u201A"],
	["ldquo", "\u201C"],
	["rdquo", "\u201D"],
	["bdquo", "\u201E"],
	["dagger", "\u2020"],
	["Dagger", "\u2021"],
	["bull", "\u2022"],
	["hellip", "\u2026"],
	["permil", "\u2030"],
	["prime", "\u2032"],
	["Prime", "\u2033"],
	["lsaquo", "\u2039"],
	["rsaquo", "\u203A"],
	["oline", "\u203E"],
	["frasl", "\u2044"],
	["euro", "\u20AC"],
	["image", "\u2111"],
	["weierp", "\u2118"],
	["real", "\u211C"],
	["trade", "\u2122"],
	["alefsym", "\u2135"],
	["larr", "\u2190"],
	["uarr", "\u2191"],
	["rarr", "\u2192"],
	["darr", "\u2193"],
	["harr", "\u2194"],
	["crarr", "\u21B5"],
	["lArr", "\u21D0"],
	["uArr", "\u21D1"],
	["rArr", "\u21D2"],
	["dArr", "\u21D3"],
	["hArr", "\u21D4"],
	["forall", "\u2200"],
	["part", "\u2202"],
	["exist", "\u2203"],
	["empty", "\u2205"],
	["nabla", "\u2207"],
	["isin", "\u2208"],
	["notin", "\u2209"],
	["ni", "\u220B"],
	["prod", "\u220F"],
	["sum", "\u2211"],
	["minus", "\u2212"],
	["lowast", "\u2217"],
	["radic", "\u221A"],
	["prop", "\u221D"],
	["infin", "\u221E"],
	["ang", "\u2220"],
	["and", "\u2227"],
	["or", "\u2228"],
	["cap", "\u2229"],
	["cup", "\u222A"],
	["int", "\u222B"],
	["there4", "\u2234"],
	["sim", "\u223C"],
	["cong", "\u2245"],
	["asymp", "\u2248"],
	["ne", "\u2260"],
	["equiv", "\u2261"],
	["le", "\u2264"],
	["ge", "\u2265"],
	["sub", "\u2282"],
	["sup", "\u2283"],
	["nsub", "\u2284"],
	["sube", "\u2286"],
	["supe", "\u2287"],
	["oplus", "\u2295"],
	["otimes", "\u2297"],
	["perp", "\u22A5"],
	["sdot", "\u22C5"],
	["lceil", "\u2308"],
	["rceil", "\u2309"],
	["lfloor", "\u230A"],
	["rfloor", "\u230B"],
	["lang", "\u2329"],
	["rang", "\u232A"],
	["loz", "\u25CA"],
	["spades", "\u2660"],
	["clubs", "\u2663"],
	["hearts", "\u2665"],
	["diams", "\u2666"],
])
