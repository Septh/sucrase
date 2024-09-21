import { Parser, StopState } from './parser_base'
import { TokenType } from './types.generated'
import { ContextualKeyword } from './keywords'
import { TypeAndKeyword } from './token'

export class ParserFlow extends Parser {

    private isMaybeDefaultImport(lookahead: TypeAndKeyword): boolean {
        return (
            (lookahead.type === TokenType.name || !!(lookahead.type & TokenType.IS_KEYWORD)) &&
            lookahead.contextualKeyword !== ContextualKeyword._from
        )
    }

    private flowParseTypeInitialiser(tok?: TokenType): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(tok || TokenType.colon)
        this.flowParseType()
        this.state.popTypeContext(oldIsType)
    }

    private flowParsePredicate(): void {
        this.state.expect(TokenType.modulo)
        this.state.expectContextual(ContextualKeyword._checks)
        if (this.state.eat(TokenType.parenL)) {
            this.parseExpression()
            this.state.expect(TokenType.parenR)
        }
    }

    private flowParseTypeAndPredicateInitialiser(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(TokenType.colon)
        if (this.state.match(TokenType.modulo)) {
            this.flowParsePredicate()
        } else {
            this.flowParseType()
            if (this.state.match(TokenType.modulo)) {
                this.flowParsePredicate()
            }
        }
        this.state.popTypeContext(oldIsType)
    }

    private flowParseDeclareClass(): void {
        this.state.next()
        this.flowParseInterfaceish(/* isClass */ true)
    }

    private flowParseDeclareFunction(): void {
        this.state.next()
        this.parseIdentifier()

        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }

        this.state.expect(TokenType.parenL)
        this.flowParseFunctionTypeParams()
        this.state.expect(TokenType.parenR)

        this.flowParseTypeAndPredicateInitialiser()

        this.state.semicolon()
    }

    private flowParseDeclare(): void {
        if (this.state.match(TokenType._class)) {
            this.flowParseDeclareClass()
        } else if (this.state.match(TokenType._function)) {
            this.flowParseDeclareFunction()
        } else if (this.state.match(TokenType._var)) {
            this.flowParseDeclareVariable()
        } else if (this.state.eatContextual(ContextualKeyword._module)) {
            if (this.state.eat(TokenType.dot)) {
                this.flowParseDeclareModuleExports()
            } else {
                this.flowParseDeclareModule()
            }
        } else if (this.state.isContextual(ContextualKeyword._type)) {
            this.flowParseDeclareTypeAlias()
        } else if (this.state.isContextual(ContextualKeyword._opaque)) {
            this.flowParseDeclareOpaqueType()
        } else if (this.state.isContextual(ContextualKeyword._interface)) {
            this.flowParseDeclareInterface()
        } else if (this.state.match(TokenType._export)) {
            this.flowParseDeclareExportDeclaration()
        } else {
            this.state.unexpected()
        }
    }

    private flowParseDeclareVariable(): void {
        this.state.next()
        this.flowParseTypeAnnotatableIdentifier()
        this.state.semicolon()
    }

    private flowParseDeclareModule(): void {
        if (this.state.match(TokenType.string)) {
            this.parseExprAtom()
        } else {
            this.parseIdentifier()
        }

        this.state.expect(TokenType.braceL)
        while (!this.state.match(TokenType.braceR) && !this.state.error) {
            if (this.state.match(TokenType._import)) {
                this.state.next()
                this.parseImport()
            } else {
                this.state.unexpected()
            }
        }
        this.state.expect(TokenType.braceR)
    }

    private flowParseDeclareExportDeclaration(): void {
        this.state.expect(TokenType._export)

        if (this.state.eat(TokenType._default)) {
            if (this.state.match(TokenType._function) || this.state.match(TokenType._class)) {
                // declare export default class ...
                // declare export default private ...
                this.flowParseDeclare()
            } else {
                // declare export default [type];
                this.flowParseType()
                this.state.semicolon()
            }
        } else if (
            this.state.match(TokenType._var) || // declare export var ...
            this.state.match(TokenType._function) || // declare protected ...
            this.state.match(TokenType._class) || // declare export class ...
            this.state.isContextual(ContextualKeyword._opaque) // declare export opaque ..
        ) {
            this.flowParseDeclare()
        } else if (
            this.state.match(TokenType.star) || // declare export * from ''
            this.state.match(TokenType.braceL) || // declare export {} ...
            this.state.isContextual(ContextualKeyword._interface) || // declare export interface ...
            this.state.isContextual(ContextualKeyword._type) || // declare export type ...
            this.state.isContextual(ContextualKeyword._opaque) // declare export opaque type ...
        ) {
            this.parseExport()
        } else {
            this.state.unexpected()
        }
    }

    private flowParseDeclareModuleExports(): void {
        this.state.expectContextual(ContextualKeyword._exports)
        this.flowParseTypeAnnotation()
        this.state.semicolon()
    }

    private flowParseDeclareTypeAlias(): void {
        this.state.next()
        this.flowParseTypeAlias()
    }

    private flowParseDeclareOpaqueType(): void {
        this.state.next()
        this.flowParseOpaqueType(true)
    }

    private flowParseDeclareInterface(): void {
        this.state.next()
        this.flowParseInterfaceish()
    }

    // Interfaces

    private flowParseInterfaceish(isClass: boolean = false): void {
        this.flowParseRestrictedIdentifier()

        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }

        if (this.state.eat(TokenType._extends)) {
            do {
                this.flowParseInterfaceExtends()
            } while (!isClass && this.state.eat(TokenType.comma))
        }

        if (this.state.isContextual(ContextualKeyword._mixins)) {
            this.state.next()
            do {
                this.flowParseInterfaceExtends()
            } while (this.state.eat(TokenType.comma))
        }

        if (this.state.isContextual(ContextualKeyword._implements)) {
            this.state.next()
            do {
                this.flowParseInterfaceExtends()
            } while (this.state.eat(TokenType.comma))
        }

        this.flowParseObjectType(isClass, false, isClass)
    }

    private flowParseInterfaceExtends(): void {
        this.flowParseQualifiedTypeIdentifier(false)
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterInstantiation()
        }
    }

    private flowParseInterface(): void {
        this.flowParseInterfaceish()
    }

    private flowParseRestrictedIdentifier(): void {
        this.parseIdentifier()
    }

    private flowParseTypeAlias(): void {
        this.flowParseRestrictedIdentifier()

        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }

        this.flowParseTypeInitialiser(TokenType.eq)
        this.state.semicolon()
    }

    private flowParseOpaqueType(declare: boolean): void {
        this.state.expectContextual(ContextualKeyword._type)
        this.flowParseRestrictedIdentifier()

        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }

        // Parse the supertype
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeInitialiser(TokenType.colon)
        }

        if (!declare) {
            this.flowParseTypeInitialiser(TokenType.eq)
        }
        this.state.semicolon()
    }

    private flowParseTypeParameter(): void {
        this.flowParseVariance()
        this.flowParseTypeAnnotatableIdentifier()

        if (this.state.eat(TokenType.eq)) {
            this.flowParseType()
        }
    }

    protected flowParseTypeParameterDeclaration(): void {
        const oldIsType = this.state.pushTypeContext(0)
        // istanbul ignore else: this condition is already checked at all call sites
        if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.typeParameterStart)) {
            this.state.next()
        } else {
            this.state.unexpected()
        }

        do {
            this.flowParseTypeParameter()
            if (!this.state.match(TokenType.greaterThan)) {
                this.state.expect(TokenType.comma)
            }
        } while (!this.state.match(TokenType.greaterThan) && !this.state.error)
        this.state.expect(TokenType.greaterThan)
        this.state.popTypeContext(oldIsType)
    }

    protected override parseClassMethod(functionStart: number, isConstructor: boolean): void {
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }
        super.parseClassMethod(functionStart, isConstructor)
    }

    private flowParseTypeParameterInstantiation(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.expect(TokenType.lessThan)
        while (!this.state.match(TokenType.greaterThan) && !this.state.error) {
            this.flowParseType()
            if (!this.state.match(TokenType.greaterThan)) {
                this.state.expect(TokenType.comma)
            }
        }
        this.state.expect(TokenType.greaterThan)
        this.state.popTypeContext(oldIsType)
    }

    private flowParseInterfaceType(): void {
        this.state.expectContextual(ContextualKeyword._interface)
        if (this.state.eat(TokenType._extends)) {
            do {
                this.flowParseInterfaceExtends()
            } while (this.state.eat(TokenType.comma))
        }
        this.flowParseObjectType(false, false, false)
    }

    private flowParseObjectPropertyKey(): void {
        if (this.state.match(TokenType.num) || this.state.match(TokenType.string)) {
            this.parseExprAtom()
        } else {
            this.parseIdentifier()
        }
    }

    private flowParseObjectTypeIndexer(): void {
        // Note: bracketL has already been consumed
        if (this.state.lookaheadType() === TokenType.colon) {
            this.flowParseObjectPropertyKey()
            this.flowParseTypeInitialiser()
        } else {
            this.flowParseType()
        }
        this.state.expect(TokenType.bracketR)
        this.flowParseTypeInitialiser()
    }

    private flowParseObjectTypeInternalSlot(): void {
        // Note: both bracketL have already been consumed
        this.flowParseObjectPropertyKey()
        this.state.expect(TokenType.bracketR)
        this.state.expect(TokenType.bracketR)
        if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.parenL)) {
            this.flowParseObjectTypeMethodish()
        } else {
            this.state.eat(TokenType.question)
            this.flowParseTypeInitialiser()
        }
    }

    private flowParseObjectTypeMethodish(): void {
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }

        this.state.expect(TokenType.parenL)
        while (!this.state.match(TokenType.parenR) && !this.state.match(TokenType.ellipsis) && !this.state.error) {
            this.flowParseFunctionTypeParam()
            if (!this.state.match(TokenType.parenR)) {
                this.state.expect(TokenType.comma)
            }
        }

        if (this.state.eat(TokenType.ellipsis)) {
            this.flowParseFunctionTypeParam()
        }
        this.state.expect(TokenType.parenR)
        this.flowParseTypeInitialiser()
    }

    private flowParseObjectTypeCallProperty(): void {
        this.flowParseObjectTypeMethodish()
    }

    private flowParseObjectType(allowStatic: boolean, allowExact: boolean, allowProto: boolean): void {
        let endDelim: TokenType
        if (allowExact && this.state.match(TokenType.braceBarL)) {
            this.state.expect(TokenType.braceBarL)
            endDelim = TokenType.braceBarR
        } else {
            this.state.expect(TokenType.braceL)
            endDelim = TokenType.braceR
        }

        while (!this.state.match(endDelim) && !this.state.error) {
            if (allowProto && this.state.isContextual(ContextualKeyword._proto)) {
                const lookahead = this.state.lookaheadType()
                if (lookahead !== TokenType.colon && lookahead !== TokenType.question) {
                    this.state.next()
                    allowStatic = false
                }
            }
            if (allowStatic && this.state.isContextual(ContextualKeyword._static)) {
                const lookahead = this.state.lookaheadType()
                if (lookahead !== TokenType.colon && lookahead !== TokenType.question) {
                    this.state.next()
                }
            }

            this.flowParseVariance()

            if (this.state.eat(TokenType.bracketL)) {
                if (this.state.eat(TokenType.bracketL)) {
                    this.flowParseObjectTypeInternalSlot()
                } else {
                    this.flowParseObjectTypeIndexer()
                }
            } else if (this.state.match(TokenType.parenL) || this.state.match(TokenType.lessThan)) {
                this.flowParseObjectTypeCallProperty()
            } else {
                if (this.state.isContextual(ContextualKeyword._get) || this.state.isContextual(ContextualKeyword._set)) {
                    const lookahead = this.state.lookaheadType()
                    if (lookahead === TokenType.name || lookahead === TokenType.string || lookahead === TokenType.num) {
                        this.state.next()
                    }
                }

                this.flowParseObjectTypeProperty()
            }

            this.flowObjectTypeSemicolon()
        }

        this.state.expect(endDelim)
    }

    private flowParseObjectTypeProperty(): void {
        if (this.state.match(TokenType.ellipsis)) {
            this.state.expect(TokenType.ellipsis)
            if (!this.state.eat(TokenType.comma)) {
                this.state.eat(TokenType.semi)
            }
            // Explicit inexact object syntax.
            if (this.state.match(TokenType.braceR)) {
                return
            }
            this.flowParseType()
        } else {
            this.flowParseObjectPropertyKey()
            if (this.state.match(TokenType.lessThan) || this.state.match(TokenType.parenL)) {
                // This is a method property
                this.flowParseObjectTypeMethodish()
            } else {
                this.state.eat(TokenType.question)
                this.flowParseTypeInitialiser()
            }
        }
    }

    private flowObjectTypeSemicolon(): void {
        if (!this.state.eat(TokenType.semi) && !this.state.eat(TokenType.comma) && !this.state.match(TokenType.braceR) && !this.state.match(TokenType.braceBarR)) {
            this.state.unexpected()
        }
    }

    private flowParseQualifiedTypeIdentifier(initialIdAlreadyParsed: boolean): void {
        if (!initialIdAlreadyParsed) {
            this.parseIdentifier()
        }
        while (this.state.eat(TokenType.dot)) {
            this.parseIdentifier()
        }
    }

    private flowParseGenericType(): void {
        this.flowParseQualifiedTypeIdentifier(true)
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterInstantiation()
        }
    }

    private flowParseTypeofType(): void {
        this.state.expect(TokenType._typeof)
        this.flowParsePrimaryType()
    }

    private flowParseTupleType(): void {
        this.state.expect(TokenType.bracketL)
        // We allow trailing commas
        while (this.state.pos < this.state.input.length && !this.state.match(TokenType.bracketR)) {
            this.flowParseType()
            if (this.state.match(TokenType.bracketR)) {
                break
            }
            this.state.expect(TokenType.comma)
        }
        this.state.expect(TokenType.bracketR)
    }

    private flowParseFunctionTypeParam(): void {
        const lookahead = this.state.lookaheadType()
        if (lookahead === TokenType.colon || lookahead === TokenType.question) {
            this.parseIdentifier()
            this.state.eat(TokenType.question)
            this.flowParseTypeInitialiser()
        } else {
            this.flowParseType()
        }
    }

    private flowParseFunctionTypeParams(): void {
        while (!this.state.match(TokenType.parenR) && !this.state.match(TokenType.ellipsis) && !this.state.error) {
            this.flowParseFunctionTypeParam()
            if (!this.state.match(TokenType.parenR)) {
                this.state.expect(TokenType.comma)
            }
        }
        if (this.state.eat(TokenType.ellipsis)) {
            this.flowParseFunctionTypeParam()
        }
    }

    // The parsing of types roughly parallels the parsing of expressions, and
    // primary types are kind of like primary expressions...they're the
    // primitives with which other types are constructed.
    private flowParsePrimaryType(): void {
        let isGroupedType = false
        const oldNoAnonFunctionType = this.state.noAnonFunctionType

        switch (this.state.type) {
            case TokenType.name: {
                if (this.state.isContextual(ContextualKeyword._interface)) {
                    this.flowParseInterfaceType()
                    return
                }
                this.parseIdentifier()
                this.flowParseGenericType()
                return
            }

            case TokenType.braceL:
                this.flowParseObjectType(false, false, false)
                return

            case TokenType.braceBarL:
                this.flowParseObjectType(false, true, false)
                return

            case TokenType.bracketL:
                this.flowParseTupleType()
                return

            case TokenType.lessThan:
                this.flowParseTypeParameterDeclaration()
                this.state.expect(TokenType.parenL)
                this.flowParseFunctionTypeParams()
                this.state.expect(TokenType.parenR)
                this.state.expect(TokenType.arrow)
                this.flowParseType()
                return

            case TokenType.parenL:
                this.state.next()

                // Check to see if this is actually a grouped type
                if (!this.state.match(TokenType.parenR) && !this.state.match(TokenType.ellipsis)) {
                    if (this.state.match(TokenType.name)) {
                        const token = this.state.lookaheadType()
                        isGroupedType = token !== TokenType.question && token !== TokenType.colon
                    } else {
                        isGroupedType = true
                    }
                }

                if (isGroupedType) {
                    this.state.noAnonFunctionType = false
                    this.flowParseType()
                    this.state.noAnonFunctionType = oldNoAnonFunctionType

                    // A `,` or a `) =>` means this is an anonymous private type
                    if (
                        this.state.noAnonFunctionType ||
                        !(this.state.match(TokenType.comma) || (this.state.match(TokenType.parenR) && this.state.lookaheadType() === TokenType.arrow))
                    ) {
                        this.state.expect(TokenType.parenR)
                        return
                    } else {
                        // Eat a comma if there is one
                        this.state.eat(TokenType.comma)
                    }
                }

                this.flowParseFunctionTypeParams()

                this.state.expect(TokenType.parenR)
                this.state.expect(TokenType.arrow)
                this.flowParseType()
                return

            case TokenType.minus:
                this.state.next()
                this.parseLiteral()
                return

            case TokenType.string:
            case TokenType.num:
            case TokenType._true:
            case TokenType._false:
            case TokenType._null:
            case TokenType._this:
            case TokenType._void:
            case TokenType.star:
                this.state.next()
                return

            default:
                if (this.state.type === TokenType._typeof) {
                    this.flowParseTypeofType()
                    return
                } else if (this.state.type & TokenType.IS_KEYWORD) {
                    this.state.next()
                    this.state.tokens[this.state.tokens.length - 1].type = TokenType.name
                    return
                }
        }

        this.state.unexpected()
    }

    private flowParsePostfixType(): void {
        this.flowParsePrimaryType()
        while (!this.state.canInsertSemicolon() && (this.state.match(TokenType.bracketL) || this.state.match(TokenType.questionDot))) {
            this.state.eat(TokenType.questionDot)
            this.state.expect(TokenType.bracketL)
            if (this.state.eat(TokenType.bracketR)) {
                // Array type
            } else {
                // Indexed access type
                this.flowParseType()
                this.state.expect(TokenType.bracketR)
            }
        }
    }

    private flowParsePrefixType(): void {
        if (this.state.eat(TokenType.question)) {
            this.flowParsePrefixType()
        } else {
            this.flowParsePostfixType()
        }
    }

    private flowParseAnonFunctionWithoutParens(): void {
        this.flowParsePrefixType()
        if (!this.state.noAnonFunctionType && this.state.eat(TokenType.arrow)) {
            this.flowParseType()
        }
    }

    private flowParseIntersectionType(): void {
        this.state.eat(TokenType.bitwiseAND)
        this.flowParseAnonFunctionWithoutParens()
        while (this.state.eat(TokenType.bitwiseAND)) {
            this.flowParseAnonFunctionWithoutParens()
        }
    }

    private flowParseUnionType(): void {
        this.state.eat(TokenType.bitwiseOR)
        this.flowParseIntersectionType()
        while (this.state.eat(TokenType.bitwiseOR)) {
            this.flowParseIntersectionType()
        }
    }

    private flowParseType(): void {
        this.flowParseUnionType()
    }

    protected flowParseTypeAnnotation(): void {
        this.flowParseTypeInitialiser()
    }

    private flowParseTypeAnnotatableIdentifier(): void {
        this.parseIdentifier()
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAnnotation()
        }
    }

    protected flowParseVariance(): void {
        if (this.state.match(TokenType.plus) || this.state.match(TokenType.minus)) {
            this.state.next()
            this.state.tokens[this.state.tokens.length - 1].isType = true
        }
    }

    // ==================================
    // #region Overrides
    // ==================================

    protected override parsePropertyName(objectContextId: number): void {
        this.flowParseVariance()
        super.parsePropertyName(objectContextId)
    }

    protected override parseFunctionBodyAndFinish(functionStart: number, funcContextId: number = 0): void {
        // For arrow functions, `parseArrow` handles the return type itself.
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAndPredicateInitialiser()
        }

        this.parseFunctionBody(false, funcContextId)
    }

    protected override parseSubscript(startTokenIndex: number, noCalls: boolean, stopState: StopState): void {
        if (this.state.match(TokenType.questionDot) && this.state.lookaheadType() === TokenType.lessThan) {
            if (noCalls) {
                stopState.stop = true
                return
            }
            this.state.next()
            this.flowParseTypeParameterInstantiation()
            this.state.expect(TokenType.parenL)
            this.parseCallExpressionArguments()
            return
        } else if (!noCalls && this.state.match(TokenType.lessThan)) {
            const snapshot = this.state.snapshot()
            this.flowParseTypeParameterInstantiation()
            this.state.expect(TokenType.parenL)
            this.parseCallExpressionArguments()
            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
            } else {
                return
            }
        }
        this.baseParseSubscript(startTokenIndex, noCalls, stopState)
    }

    protected override parseNewArguments(): void {
        if (this.state.match(TokenType.lessThan)) {
            const snapshot = this.state.snapshot()
            this.flowParseTypeParameterInstantiation()
            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
            }
        }
    }

    // interfaces
    protected override parseStatement(declaration: boolean): void {
        if (this.state.match(TokenType.name) && this.state.contextualKeyword === ContextualKeyword._interface) {
            const oldIsType = this.state.pushTypeContext(0)
            this.state.next()
            this.flowParseInterface()
            this.state.popTypeContext(oldIsType)
            return 
        } else if (this.state.isContextual(ContextualKeyword._enum)) {
            this.flowParseEnumDeclaration()
            return 
        }
        super.parseStatement(declaration)
    }

    protected override tryParseExportDefaultExpression(): boolean {
        if (this.state.isContextual(ContextualKeyword._enum)) {
            this.flowParseEnumDeclaration()
            return true
        }
        return false
    }

    // declares, interfaces and type aliases
    protected override parseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
        if (contextualKeyword === ContextualKeyword._declare) {
            if (
                this.state.match(TokenType._class) ||
                this.state.match(TokenType.name) ||
                this.state.match(TokenType._function) ||
                this.state.match(TokenType._var) ||
                this.state.match(TokenType._export)
            ) {
                const oldIsType = this.state.pushTypeContext(1)
                this.flowParseDeclare()
                this.state.popTypeContext(oldIsType)
            }
        } else if (this.state.match(TokenType.name)) {
            if (contextualKeyword === ContextualKeyword._interface) {
                const oldIsType = this.state.pushTypeContext(1)
                this.flowParseInterface()
                this.state.popTypeContext(oldIsType)
            } else if (contextualKeyword === ContextualKeyword._type) {
                const oldIsType = this.state.pushTypeContext(1)
                this.flowParseTypeAlias()
                this.state.popTypeContext(oldIsType)
            } else if (contextualKeyword === ContextualKeyword._opaque) {
                const oldIsType = this.state.pushTypeContext(1)
                this.flowParseOpaqueType(false)
                this.state.popTypeContext(oldIsType)
            }
        }
        this.state.semicolon()
    }

    // export type
    protected flowShouldParseExportDeclaration(): boolean {
        return (
            this.state.isContextual(ContextualKeyword._type) ||
            this.state.isContextual(ContextualKeyword._interface) ||
            this.state.isContextual(ContextualKeyword._opaque) ||
            this.state.isContextual(ContextualKeyword._enum)
        )
    }

    protected flowShouldDisallowExportDefaultSpecifier(): boolean {
        return (
            this.state.match(TokenType.name) &&
            (this.state.contextualKeyword === ContextualKeyword._type ||
                this.state.contextualKeyword === ContextualKeyword._interface ||
                this.state.contextualKeyword === ContextualKeyword._opaque ||
                this.state.contextualKeyword === ContextualKeyword._enum)
        )
    }

    protected override parseExportDeclaration(): void {
        if (this.state.isContextual(ContextualKeyword._type)) {
            const oldIsType = this.state.pushTypeContext(1)
            this.state.next()

            if (this.state.match(TokenType.braceL)) {
                // export type { foo, bar };
                this.parseExportSpecifiers()
                this.parseExportFrom()
            } else {
                // export type Foo = Bar;
                this.flowParseTypeAlias()
            }
            this.state.popTypeContext(oldIsType)
        } else if (this.state.isContextual(ContextualKeyword._opaque)) {
            const oldIsType = this.state.pushTypeContext(1)
            this.state.next()
            // export opaque type Foo = Bar;
            this.flowParseOpaqueType(false)
            this.state.popTypeContext(oldIsType)
        } else if (this.state.isContextual(ContextualKeyword._interface)) {
            const oldIsType = this.state.pushTypeContext(1)
            this.state.next()
            this.flowParseInterface()
            this.state.popTypeContext(oldIsType)
        } else {
            this.parseStatement(true)
        }
    }

    protected override shouldParseExportStar(): boolean {
        return this.state.match(TokenType.star) || (this.state.isContextual(ContextualKeyword._type) && this.state.lookaheadType() === TokenType.star)
    }

    protected override parseExportStar(): void {
        if (this.state.eatContextual(ContextualKeyword._type)) {
            const oldIsType = this.state.pushTypeContext(2)
            this.baseParseExportStar()
            this.state.popTypeContext(oldIsType)
        } else {
            this.baseParseExportStar()
        }
    }

    // parse a the super class type parameters and implements
    protected flowAfterParseClassSuper(hasSuper: boolean): void {
        if (hasSuper && this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterInstantiation()
        }
        if (this.state.isContextual(ContextualKeyword._implements)) {
            const oldIsType = this.state.pushTypeContext(0)
            this.state.next()
            this.state.tokens[this.state.tokens.length - 1].type = TokenType._implements
            do {
                this.flowParseRestrictedIdentifier()
                if (this.state.match(TokenType.lessThan)) {
                    this.flowParseTypeParameterInstantiation()
                }
            } while (this.state.eat(TokenType.comma))
            this.state.popTypeContext(oldIsType)
        }
    }

    // parse type parameters for object method shorthand
    protected override parseObjPropValue(isPattern: boolean, isBlockScope: boolean, objectContextId: number): void {
        // method shorthand
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
            if (!this.state.match(TokenType.parenL)) this.state.unexpected()
        }
        super.baseParseObjPropValue(isPattern, isBlockScope, objectContextId)
    }

    protected override parseAssignableListItemTypes(): void {
        const oldIsType = this.state.pushTypeContext(0)
        this.state.eat(TokenType.question)
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAnnotation()
        }
        this.state.popTypeContext(oldIsType)
    }

    // parse typeof and type imports
    protected override parseImportSpecifiers(): void {
        if (this.state.match(TokenType._typeof) || this.state.isContextual(ContextualKeyword._type)) {
            const lh = this.state.lookaheadTypeAndKeyword()
            if (this.isMaybeDefaultImport(lh) || lh.type === TokenType.braceL || lh.type === TokenType.star) {
                this.state.next()
            }
        }
        super.parseImportSpecifiers()
    }

    // parse import-type/typeof shorthand
    protected override parseImportSpecifier(): void {
        const isTypeKeyword =
            this.state.contextualKeyword === ContextualKeyword._type || this.state.type === TokenType._typeof
        if (isTypeKeyword) {
            this.state.next()
        } else {
            this.parseIdentifier()
        }

        if (this.state.isContextual(ContextualKeyword._as) && !this.state.isLookaheadContextual(ContextualKeyword._as)) {
            this.parseIdentifier()
            if (isTypeKeyword && !this.state.match(TokenType.name) && !(this.state.type & TokenType.IS_KEYWORD)) {
                // `import {type as ,` or `import {type as }`
            } else {
                // `import {type as foo`
                this.parseIdentifier()
            }
        } else {
            if (isTypeKeyword && (this.state.match(TokenType.name) || !!(this.state.type & TokenType.IS_KEYWORD))) {
                // `import {type foo`
                this.parseIdentifier()
            }
            if (this.state.eatContextual(ContextualKeyword._as)) {
                this.parseIdentifier()
            }
        }
    }

    // parse private type parameters - private foo<T>() {}
    protected flowStartParseFunctionParams(): void {
        // Originally this checked if the method is a getter/setter, but if it was, we'd crash soon
        // anyway, so don't try to propagate that information.
        if (this.state.match(TokenType.lessThan)) {
            const oldIsType = this.state.pushTypeContext(0)
            this.flowParseTypeParameterDeclaration()
            this.state.popTypeContext(oldIsType)
        }
    }

    protected override parseFunctionParams(allowModifiers: boolean = false, funcContextId: number = 0): void {
        this.flowStartParseFunctionParams()
        super.parseFunctionParams(allowModifiers, funcContextId)
    }

    // parse flow type annotations on variable declarator heads - let foo: string = bar
    protected flowAfterParseVarHead(): void {
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAnnotation()
        }
    }

    protected override parseVarHead(isBlockScope: boolean): void {
        this.parseBindingAtom(isBlockScope)
        this.flowAfterParseVarHead()
    }

    // parse the return type of an async arrow private - let foo = (async (): number => {});
    protected override parseAsyncArrowFromCallExpression(startTokenIndex: number): void {
        if (this.state.match(TokenType.colon)) {
            const oldNoAnonFunctionType = this.state.noAnonFunctionType
            this.state.noAnonFunctionType = true
            this.flowParseTypeAnnotation()
            this.state.noAnonFunctionType = oldNoAnonFunctionType
        }
        this.baseParseAsyncArrowFromCallExpression(startTokenIndex)
    }

    // We need to support type parameter declarations for arrow functions. This
    // is tricky. There are three situations we need to handle
    //
    // 1. This is either JSX or an arrow function. We'll try JSX first. If that
    //    fails, we'll try an arrow function. If that fails, we'll throw the JSX
    //    error.
    // 2. This is an arrow function. We'll parse the type parameter declaration,
    //    parse the rest, make sure the rest is an arrow function, and go from
    //    there
    // 3. This is neither. Just call the super method
    protected override parseMaybeAssign(noIn: boolean, isWithinParens: boolean): boolean {
        if (this.state.match(TokenType.lessThan)) {
            const snapshot = this.state.snapshot()
            let wasArrow = this.baseParseMaybeAssign(noIn, isWithinParens)
            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
                this.state.type = TokenType.typeParameterStart
            } else {
                return wasArrow
            }

            const oldIsType = this.state.pushTypeContext(0)
            this.flowParseTypeParameterDeclaration()
            this.state.popTypeContext(oldIsType)
            wasArrow = this.baseParseMaybeAssign(noIn, isWithinParens)
            if (wasArrow) {
                return true
            }
            this.state.unexpected()
        }

        return this.baseParseMaybeAssign(noIn, isWithinParens)
    }

    // handle return types for arrow functions
    protected override parseArrow(): boolean {
        if (this.state.match(TokenType.colon)) {
            const oldIsType = this.state.pushTypeContext(0)
            const snapshot = this.state.snapshot()

            const oldNoAnonFunctionType = this.state.noAnonFunctionType
            this.state.noAnonFunctionType = true
            this.flowParseTypeAndPredicateInitialiser()
            this.state.noAnonFunctionType = oldNoAnonFunctionType

            if (this.state.canInsertSemicolon()) this.state.unexpected()
            if (!this.state.match(TokenType.arrow)) this.state.unexpected()

            if (this.state.error) {
                this.state.restoreFromSnapshot(snapshot)
            }
            this.state.popTypeContext(oldIsType)
        }
        return this.state.eat(TokenType.arrow)
    }

    protected override parseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
        if (
            this.state.tokens[this.state.tokens.length - 1].contextualKeyword === ContextualKeyword._async &&
            this.state.match(TokenType.lessThan)
        ) {
            const snapshot = this.state.snapshot()
            const wasArrow = this.parseAsyncArrowWithTypeParameters()
            if (wasArrow && !this.state.error) {
                return
            }
            this.state.restoreFromSnapshot(snapshot)
        }

        this.baseParseSubscripts(startTokenIndex, noCalls)
    }

    // Returns true if there was an arrow private here.
    private parseAsyncArrowWithTypeParameters(): boolean {
        this.state.scopeDepth++
        const startTokenIndex = this.state.tokens.length
        this.parseFunctionParams()
        if (!this.parseArrow()) {
            return false
        }
        this.parseArrowExpression(startTokenIndex)
        return true
    }

    private flowParseEnumDeclaration(): void {
        this.state.expectContextual(ContextualKeyword._enum)
        this.state.tokens[this.state.tokens.length - 1].type = TokenType._enum
        this.parseIdentifier()
        this.flowParseEnumBody()
    }

    private flowParseEnumBody(): void {
        if (this.state.eatContextual(ContextualKeyword._of)) {
            this.state.next()
        }
        this.state.expect(TokenType.braceL)
        this.flowParseEnumMembers()
        this.state.expect(TokenType.braceR)
    }

    private flowParseEnumMembers(): void {
        while (!this.state.match(TokenType.braceR) && !this.state.error) {
            if (this.state.eat(TokenType.ellipsis)) {
                break
            }
            this.flowParseEnumMember()
            if (!this.state.match(TokenType.braceR)) {
                this.state.expect(TokenType.comma)
            }
        }
    }

    private flowParseEnumMember(): void {
        this.parseIdentifier()
        if (this.state.eat(TokenType.eq)) {
            // Flow enum values are always just one token (a string, number, or boolean literal).
            this.state.next()
        }
    }

    // An apparent conditional expression could actually be an optional parameter in an arrow function.
    protected override parseConditional(noIn: boolean): void {
        // If we see ?:, this can't possibly be a valid conditional. typedParseParenItem will be called
        // later to finish off the arrow parameter. We also need to handle bare ? tokens for optional
        // parameters without type annotations, i.e. ?, and ?) .
        if (this.state.match(TokenType.question)) {
            const nextType = this.state.lookaheadType()
            if (nextType === TokenType.colon || nextType === TokenType.comma || nextType === TokenType.parenR) {
                return
            }
        }
        this.baseParseConditional(noIn)
    }

    protected override parseClassProperty(): void {
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAnnotation()
        }
        super.parseClassProperty()
    }

    protected override parseClassId(isStatement: boolean, optionalId: boolean = false): void {
        super.parseClassId(isStatement, optionalId)
        if (this.state.match(TokenType.lessThan)) {
            this.flowParseTypeParameterDeclaration()
        }
    }

    protected override afterParseClassSuper(hasSuper: boolean): void {
        this.flowAfterParseClassSuper(hasSuper)
    }

    protected override isExportDefaultSpecifier(): boolean {
        if (this.flowShouldDisallowExportDefaultSpecifier()) {
            return false
        }
        return super.isExportDefaultSpecifier()
    }

    protected override shouldParseExportDeclaration(): boolean {
        return this.flowShouldParseExportDeclaration() || super.shouldParseExportDeclaration()
    }

    protected override parseParenItem(): void {
        this.state.eatTypeToken(TokenType.question)
        if (this.state.match(TokenType.colon)) {
            this.flowParseTypeAnnotation()
        }
    }
}
