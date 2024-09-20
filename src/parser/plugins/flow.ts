import {
  type TypeAndKeyword,
} from "../token";
import {ContextualKeyword} from "../keywords";
import {TokenType, TokenType as tt} from "../generated/types";
import {state} from "../state";
import {
  baseParseMaybeAssign,
  baseParseSubscript,
  baseParseSubscripts,
  parseArrow,
  parseArrowExpression,
  parseCallExpressionArguments,
  parseExprAtom,
  parseExpression,
  parseFunctionBody,
  parseIdentifier,
  parseLiteral,
  type StopState,
} from "../traverser";
import {
  baseParseExportStar,
  parseExport,
  parseExportFrom,
  parseExportSpecifiers,
  parseFunctionParams,
  parseImport,
  parseStatement,
} from "../traverser";
import {
  canInsertSemicolon,
  eatContextual,
  expect,
  expectContextual,
  isContextual,
  isLookaheadContextual,
  semicolon,
  unexpected,
} from "../traverser";

function isMaybeDefaultImport(lookahead: TypeAndKeyword): boolean {
  return (
    (lookahead.type === tt.name || !!(lookahead.type & TokenType.IS_KEYWORD)) &&
    lookahead.contextualKeyword !== ContextualKeyword._from
  );
}

function flowParseTypeInitialiser(tok?: TokenType): void {
  const oldIsType = state.pushTypeContext(0);
  expect(tok || tt.colon);
  flowParseType();
  state.popTypeContext(oldIsType);
}

function flowParsePredicate(): void {
  expect(tt.modulo);
  expectContextual(ContextualKeyword._checks);
  if (state.eat(tt.parenL)) {
    parseExpression();
    expect(tt.parenR);
  }
}

function flowParseTypeAndPredicateInitialiser(): void {
  const oldIsType = state.pushTypeContext(0);
  expect(tt.colon);
  if (state.match(tt.modulo)) {
    flowParsePredicate();
  } else {
    flowParseType();
    if (state.match(tt.modulo)) {
      flowParsePredicate();
    }
  }
  state.popTypeContext(oldIsType);
}

function flowParseDeclareClass(): void {
  state.next();
  flowParseInterfaceish(/* isClass */ true);
}

function flowParseDeclareFunction(): void {
  state.next();
  parseIdentifier();

  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
  }

  expect(tt.parenL);
  flowParseFunctionTypeParams();
  expect(tt.parenR);

  flowParseTypeAndPredicateInitialiser();

  semicolon();
}

function flowParseDeclare(): void {
  if (state.match(tt._class)) {
    flowParseDeclareClass();
  } else if (state.match(tt._function)) {
    flowParseDeclareFunction();
  } else if (state.match(tt._var)) {
    flowParseDeclareVariable();
  } else if (eatContextual(ContextualKeyword._module)) {
    if (state.eat(tt.dot)) {
      flowParseDeclareModuleExports();
    } else {
      flowParseDeclareModule();
    }
  } else if (isContextual(ContextualKeyword._type)) {
    flowParseDeclareTypeAlias();
  } else if (isContextual(ContextualKeyword._opaque)) {
    flowParseDeclareOpaqueType();
  } else if (isContextual(ContextualKeyword._interface)) {
    flowParseDeclareInterface();
  } else if (state.match(tt._export)) {
    flowParseDeclareExportDeclaration();
  } else {
    unexpected();
  }
}

function flowParseDeclareVariable(): void {
  state.next();
  flowParseTypeAnnotatableIdentifier();
  semicolon();
}

function flowParseDeclareModule(): void {
  if (state.match(tt.string)) {
    parseExprAtom();
  } else {
    parseIdentifier();
  }

  expect(tt.braceL);
  while (!state.match(tt.braceR) && !state.error) {
    if (state.match(tt._import)) {
      state.next();
      parseImport();
    } else {
      unexpected();
    }
  }
  expect(tt.braceR);
}

function flowParseDeclareExportDeclaration(): void {
  expect(tt._export);

  if (state.eat(tt._default)) {
    if (state.match(tt._function) || state.match(tt._class)) {
      // declare export default class ...
      // declare export default function ...
      flowParseDeclare();
    } else {
      // declare export default [type];
      flowParseType();
      semicolon();
    }
  } else if (
    state.match(tt._var) || // declare export var ...
    state.match(tt._function) || // declare export function ...
    state.match(tt._class) || // declare export class ...
    isContextual(ContextualKeyword._opaque) // declare export opaque ..
  ) {
    flowParseDeclare();
  } else if (
    state.match(tt.star) || // declare export * from ''
    state.match(tt.braceL) || // declare export {} ...
    isContextual(ContextualKeyword._interface) || // declare export interface ...
    isContextual(ContextualKeyword._type) || // declare export type ...
    isContextual(ContextualKeyword._opaque) // declare export opaque type ...
  ) {
    parseExport();
  } else {
    unexpected();
  }
}

function flowParseDeclareModuleExports(): void {
  expectContextual(ContextualKeyword._exports);
  flowParseTypeAnnotation();
  semicolon();
}

function flowParseDeclareTypeAlias(): void {
  state.next();
  flowParseTypeAlias();
}

function flowParseDeclareOpaqueType(): void {
  state.next();
  flowParseOpaqueType(true);
}

function flowParseDeclareInterface(): void {
  state.next();
  flowParseInterfaceish();
}

// Interfaces

function flowParseInterfaceish(isClass: boolean = false): void {
  flowParseRestrictedIdentifier();

  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
  }

  if (state.eat(tt._extends)) {
    do {
      flowParseInterfaceExtends();
    } while (!isClass && state.eat(tt.comma));
  }

  if (isContextual(ContextualKeyword._mixins)) {
    state.next();
    do {
      flowParseInterfaceExtends();
    } while (state.eat(tt.comma));
  }

  if (isContextual(ContextualKeyword._implements)) {
    state.next();
    do {
      flowParseInterfaceExtends();
    } while (state.eat(tt.comma));
  }

  flowParseObjectType(isClass, false, isClass);
}

function flowParseInterfaceExtends(): void {
  flowParseQualifiedTypeIdentifier(false);
  if (state.match(tt.lessThan)) {
    flowParseTypeParameterInstantiation();
  }
}

function flowParseInterface(): void {
  flowParseInterfaceish();
}

function flowParseRestrictedIdentifier(): void {
  parseIdentifier();
}

function flowParseTypeAlias(): void {
  flowParseRestrictedIdentifier();

  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
  }

  flowParseTypeInitialiser(tt.eq);
  semicolon();
}

function flowParseOpaqueType(declare: boolean): void {
  expectContextual(ContextualKeyword._type);
  flowParseRestrictedIdentifier();

  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
  }

  // Parse the supertype
  if (state.match(tt.colon)) {
    flowParseTypeInitialiser(tt.colon);
  }

  if (!declare) {
    flowParseTypeInitialiser(tt.eq);
  }
  semicolon();
}

function flowParseTypeParameter(): void {
  flowParseVariance();
  flowParseTypeAnnotatableIdentifier();

  if (state.eat(tt.eq)) {
    flowParseType();
  }
}

export function flowParseTypeParameterDeclaration(): void {
  const oldIsType = state.pushTypeContext(0);
  // istanbul ignore else: this condition is already checked at all call sites
  if (state.match(tt.lessThan) || state.match(tt.typeParameterStart)) {
    state.next();
  } else {
    unexpected();
  }

  do {
    flowParseTypeParameter();
    if (!state.match(tt.greaterThan)) {
      expect(tt.comma);
    }
  } while (!state.match(tt.greaterThan) && !state.error);
  expect(tt.greaterThan);
  state.popTypeContext(oldIsType);
}

function flowParseTypeParameterInstantiation(): void {
  const oldIsType = state.pushTypeContext(0);
  expect(tt.lessThan);
  while (!state.match(tt.greaterThan) && !state.error) {
    flowParseType();
    if (!state.match(tt.greaterThan)) {
      expect(tt.comma);
    }
  }
  expect(tt.greaterThan);
  state.popTypeContext(oldIsType);
}

function flowParseInterfaceType(): void {
  expectContextual(ContextualKeyword._interface);
  if (state.eat(tt._extends)) {
    do {
      flowParseInterfaceExtends();
    } while (state.eat(tt.comma));
  }
  flowParseObjectType(false, false, false);
}

function flowParseObjectPropertyKey(): void {
  if (state.match(tt.num) || state.match(tt.string)) {
    parseExprAtom();
  } else {
    parseIdentifier();
  }
}

function flowParseObjectTypeIndexer(): void {
  // Note: bracketL has already been consumed
  if (state.lookaheadType() === tt.colon) {
    flowParseObjectPropertyKey();
    flowParseTypeInitialiser();
  } else {
    flowParseType();
  }
  expect(tt.bracketR);
  flowParseTypeInitialiser();
}

function flowParseObjectTypeInternalSlot(): void {
  // Note: both bracketL have already been consumed
  flowParseObjectPropertyKey();
  expect(tt.bracketR);
  expect(tt.bracketR);
  if (state.match(tt.lessThan) || state.match(tt.parenL)) {
    flowParseObjectTypeMethodish();
  } else {
    state.eat(tt.question);
    flowParseTypeInitialiser();
  }
}

function flowParseObjectTypeMethodish(): void {
  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
  }

  expect(tt.parenL);
  while (!state.match(tt.parenR) && !state.match(tt.ellipsis) && !state.error) {
    flowParseFunctionTypeParam();
    if (!state.match(tt.parenR)) {
      expect(tt.comma);
    }
  }

  if (state.eat(tt.ellipsis)) {
    flowParseFunctionTypeParam();
  }
  expect(tt.parenR);
  flowParseTypeInitialiser();
}

function flowParseObjectTypeCallProperty(): void {
  flowParseObjectTypeMethodish();
}

function flowParseObjectType(allowStatic: boolean, allowExact: boolean, allowProto: boolean): void {
  let endDelim: TokenType;
  if (allowExact && state.match(tt.braceBarL)) {
    expect(tt.braceBarL);
    endDelim = tt.braceBarR;
  } else {
    expect(tt.braceL);
    endDelim = tt.braceR;
  }

  while (!state.match(endDelim) && !state.error) {
    if (allowProto && isContextual(ContextualKeyword._proto)) {
      const lookahead = state.lookaheadType();
      if (lookahead !== tt.colon && lookahead !== tt.question) {
        state.next();
        allowStatic = false;
      }
    }
    if (allowStatic && isContextual(ContextualKeyword._static)) {
      const lookahead = state.lookaheadType();
      if (lookahead !== tt.colon && lookahead !== tt.question) {
        state.next();
      }
    }

    flowParseVariance();

    if (state.eat(tt.bracketL)) {
      if (state.eat(tt.bracketL)) {
        flowParseObjectTypeInternalSlot();
      } else {
        flowParseObjectTypeIndexer();
      }
    } else if (state.match(tt.parenL) || state.match(tt.lessThan)) {
      flowParseObjectTypeCallProperty();
    } else {
      if (isContextual(ContextualKeyword._get) || isContextual(ContextualKeyword._set)) {
        const lookahead = state.lookaheadType();
        if (lookahead === tt.name || lookahead === tt.string || lookahead === tt.num) {
          state.next();
        }
      }

      flowParseObjectTypeProperty();
    }

    flowObjectTypeSemicolon();
  }

  expect(endDelim);
}

function flowParseObjectTypeProperty(): void {
  if (state.match(tt.ellipsis)) {
    expect(tt.ellipsis);
    if (!state.eat(tt.comma)) {
      state.eat(tt.semi);
    }
    // Explicit inexact object syntax.
    if (state.match(tt.braceR)) {
      return;
    }
    flowParseType();
  } else {
    flowParseObjectPropertyKey();
    if (state.match(tt.lessThan) || state.match(tt.parenL)) {
      // This is a method property
      flowParseObjectTypeMethodish();
    } else {
      state.eat(tt.question);
      flowParseTypeInitialiser();
    }
  }
}

function flowObjectTypeSemicolon(): void {
  if (!state.eat(tt.semi) && !state.eat(tt.comma) && !state.match(tt.braceR) && !state.match(tt.braceBarR)) {
    unexpected();
  }
}

function flowParseQualifiedTypeIdentifier(initialIdAlreadyParsed: boolean): void {
  if (!initialIdAlreadyParsed) {
    parseIdentifier();
  }
  while (state.eat(tt.dot)) {
    parseIdentifier();
  }
}

function flowParseGenericType(): void {
  flowParseQualifiedTypeIdentifier(true);
  if (state.match(tt.lessThan)) {
    flowParseTypeParameterInstantiation();
  }
}

function flowParseTypeofType(): void {
  expect(tt._typeof);
  flowParsePrimaryType();
}

function flowParseTupleType(): void {
  expect(tt.bracketL);
  // We allow trailing commas
  while (state.pos < state.input.length && !state.match(tt.bracketR)) {
    flowParseType();
    if (state.match(tt.bracketR)) {
      break;
    }
    expect(tt.comma);
  }
  expect(tt.bracketR);
}

function flowParseFunctionTypeParam(): void {
  const lookahead = state.lookaheadType();
  if (lookahead === tt.colon || lookahead === tt.question) {
    parseIdentifier();
    state.eat(tt.question);
    flowParseTypeInitialiser();
  } else {
    flowParseType();
  }
}

function flowParseFunctionTypeParams(): void {
  while (!state.match(tt.parenR) && !state.match(tt.ellipsis) && !state.error) {
    flowParseFunctionTypeParam();
    if (!state.match(tt.parenR)) {
      expect(tt.comma);
    }
  }
  if (state.eat(tt.ellipsis)) {
    flowParseFunctionTypeParam();
  }
}

// The parsing of types roughly parallels the parsing of expressions, and
// primary types are kind of like primary expressions...they're the
// primitives with which other types are constructed.
function flowParsePrimaryType(): void {
  let isGroupedType = false;
  const oldNoAnonFunctionType = state.noAnonFunctionType;

  switch (state.type) {
    case tt.name: {
      if (isContextual(ContextualKeyword._interface)) {
        flowParseInterfaceType();
        return;
      }
      parseIdentifier();
      flowParseGenericType();
      return;
    }

    case tt.braceL:
      flowParseObjectType(false, false, false);
      return;

    case tt.braceBarL:
      flowParseObjectType(false, true, false);
      return;

    case tt.bracketL:
      flowParseTupleType();
      return;

    case tt.lessThan:
      flowParseTypeParameterDeclaration();
      expect(tt.parenL);
      flowParseFunctionTypeParams();
      expect(tt.parenR);
      expect(tt.arrow);
      flowParseType();
      return;

    case tt.parenL:
      state.next();

      // Check to see if this is actually a grouped type
      if (!state.match(tt.parenR) && !state.match(tt.ellipsis)) {
        if (state.match(tt.name)) {
          const token = state.lookaheadType();
          isGroupedType = token !== tt.question && token !== tt.colon;
        } else {
          isGroupedType = true;
        }
      }

      if (isGroupedType) {
        state.noAnonFunctionType = false;
        flowParseType();
        state.noAnonFunctionType = oldNoAnonFunctionType;

        // A `,` or a `) =>` means this is an anonymous function type
        if (
          state.noAnonFunctionType ||
          !(state.match(tt.comma) || (state.match(tt.parenR) && state.lookaheadType() === tt.arrow))
        ) {
          expect(tt.parenR);
          return;
        } else {
          // Eat a comma if there is one
          state.eat(tt.comma);
        }
      }

      flowParseFunctionTypeParams();

      expect(tt.parenR);
      expect(tt.arrow);
      flowParseType();
      return;

    case tt.minus:
      state.next();
      parseLiteral();
      return;

    case tt.string:
    case tt.num:
    case tt._true:
    case tt._false:
    case tt._null:
    case tt._this:
    case tt._void:
    case tt.star:
      state.next();
      return;

    default:
      if (state.type === tt._typeof) {
        flowParseTypeofType();
        return;
      } else if (state.type & TokenType.IS_KEYWORD) {
        state.next();
        state.tokens[state.tokens.length - 1].type = tt.name;
        return;
      }
  }

  unexpected();
}

function flowParsePostfixType(): void {
  flowParsePrimaryType();
  while (!canInsertSemicolon() && (state.match(tt.bracketL) || state.match(tt.questionDot))) {
    state.eat(tt.questionDot);
    expect(tt.bracketL);
    if (state.eat(tt.bracketR)) {
      // Array type
    } else {
      // Indexed access type
      flowParseType();
      expect(tt.bracketR);
    }
  }
}

function flowParsePrefixType(): void {
  if (state.eat(tt.question)) {
    flowParsePrefixType();
  } else {
    flowParsePostfixType();
  }
}

function flowParseAnonFunctionWithoutParens(): void {
  flowParsePrefixType();
  if (!state.noAnonFunctionType && state.eat(tt.arrow)) {
    flowParseType();
  }
}

function flowParseIntersectionType(): void {
  state.eat(tt.bitwiseAND);
  flowParseAnonFunctionWithoutParens();
  while (state.eat(tt.bitwiseAND)) {
    flowParseAnonFunctionWithoutParens();
  }
}

function flowParseUnionType(): void {
  state.eat(tt.bitwiseOR);
  flowParseIntersectionType();
  while (state.eat(tt.bitwiseOR)) {
    flowParseIntersectionType();
  }
}

function flowParseType(): void {
  flowParseUnionType();
}

export function flowParseTypeAnnotation(): void {
  flowParseTypeInitialiser();
}

function flowParseTypeAnnotatableIdentifier(): void {
  parseIdentifier();
  if (state.match(tt.colon)) {
    flowParseTypeAnnotation();
  }
}

export function flowParseVariance(): void {
  if (state.match(tt.plus) || state.match(tt.minus)) {
    state.next();
    state.tokens[state.tokens.length - 1].isType = true;
  }
}

// ==================================
// Overrides
// ==================================

export function flowParseFunctionBodyAndFinish(funcContextId: number): void {
  // For arrow functions, `parseArrow` handles the return type itself.
  if (state.match(tt.colon)) {
    flowParseTypeAndPredicateInitialiser();
  }

  parseFunctionBody(false, funcContextId);
}

export function flowParseSubscript(
  startTokenIndex: number,
  noCalls: boolean,
  stopState: StopState,
): void {
  if (state.match(tt.questionDot) && state.lookaheadType() === tt.lessThan) {
    if (noCalls) {
      stopState.stop = true;
      return;
    }
    state.next();
    flowParseTypeParameterInstantiation();
    expect(tt.parenL);
    parseCallExpressionArguments();
    return;
  } else if (!noCalls && state.match(tt.lessThan)) {
    const snapshot = state.snapshot();
    flowParseTypeParameterInstantiation();
    expect(tt.parenL);
    parseCallExpressionArguments();
    if (state.error) {
      state.restoreFromSnapshot(snapshot);
    } else {
      return;
    }
  }
  baseParseSubscript(startTokenIndex, noCalls, stopState);
}

export function flowStartParseNewArguments(): void {
  if (state.match(tt.lessThan)) {
    const snapshot = state.snapshot();
    flowParseTypeParameterInstantiation();
    if (state.error) {
      state.restoreFromSnapshot(snapshot);
    }
  }
}

// interfaces
export function flowTryParseStatement(): boolean {
  if (state.match(tt.name) && state.contextualKeyword === ContextualKeyword._interface) {
    const oldIsType = state.pushTypeContext(0);
    state.next();
    flowParseInterface();
    state.popTypeContext(oldIsType);
    return true;
  } else if (isContextual(ContextualKeyword._enum)) {
    flowParseEnumDeclaration();
    return true;
  }
  return false;
}

export function flowTryParseExportDefaultExpression(): boolean {
  if (isContextual(ContextualKeyword._enum)) {
    flowParseEnumDeclaration();
    return true;
  }
  return false;
}

// declares, interfaces and type aliases
export function flowParseIdentifierStatement(contextualKeyword: ContextualKeyword): void {
  if (contextualKeyword === ContextualKeyword._declare) {
    if (
      state.match(tt._class) ||
      state.match(tt.name) ||
      state.match(tt._function) ||
      state.match(tt._var) ||
      state.match(tt._export)
    ) {
      const oldIsType = state.pushTypeContext(1);
      flowParseDeclare();
      state.popTypeContext(oldIsType);
    }
  } else if (state.match(tt.name)) {
    if (contextualKeyword === ContextualKeyword._interface) {
      const oldIsType = state.pushTypeContext(1);
      flowParseInterface();
      state.popTypeContext(oldIsType);
    } else if (contextualKeyword === ContextualKeyword._type) {
      const oldIsType = state.pushTypeContext(1);
      flowParseTypeAlias();
      state.popTypeContext(oldIsType);
    } else if (contextualKeyword === ContextualKeyword._opaque) {
      const oldIsType = state.pushTypeContext(1);
      flowParseOpaqueType(false);
      state.popTypeContext(oldIsType);
    }
  }
  semicolon();
}

// export type
export function flowShouldParseExportDeclaration(): boolean {
  return (
    isContextual(ContextualKeyword._type) ||
    isContextual(ContextualKeyword._interface) ||
    isContextual(ContextualKeyword._opaque) ||
    isContextual(ContextualKeyword._enum)
  );
}

export function flowShouldDisallowExportDefaultSpecifier(): boolean {
  return (
    state.match(tt.name) &&
    (state.contextualKeyword === ContextualKeyword._type ||
      state.contextualKeyword === ContextualKeyword._interface ||
      state.contextualKeyword === ContextualKeyword._opaque ||
      state.contextualKeyword === ContextualKeyword._enum)
  );
}

export function flowParseExportDeclaration(): void {
  if (isContextual(ContextualKeyword._type)) {
    const oldIsType = state.pushTypeContext(1);
    state.next();

    if (state.match(tt.braceL)) {
      // export type { foo, bar };
      parseExportSpecifiers();
      parseExportFrom();
    } else {
      // export type Foo = Bar;
      flowParseTypeAlias();
    }
    state.popTypeContext(oldIsType);
  } else if (isContextual(ContextualKeyword._opaque)) {
    const oldIsType = state.pushTypeContext(1);
    state.next();
    // export opaque type Foo = Bar;
    flowParseOpaqueType(false);
    state.popTypeContext(oldIsType);
  } else if (isContextual(ContextualKeyword._interface)) {
    const oldIsType = state.pushTypeContext(1);
    state.next();
    flowParseInterface();
    state.popTypeContext(oldIsType);
  } else {
    parseStatement(true);
  }
}

export function flowShouldParseExportStar(): boolean {
  return state.match(tt.star) || (isContextual(ContextualKeyword._type) && state.lookaheadType() === tt.star);
}

export function flowParseExportStar(): void {
  if (eatContextual(ContextualKeyword._type)) {
    const oldIsType = state.pushTypeContext(2);
    baseParseExportStar();
    state.popTypeContext(oldIsType);
  } else {
    baseParseExportStar();
  }
}

// parse a the super class type parameters and implements
export function flowAfterParseClassSuper(hasSuper: boolean): void {
  if (hasSuper && state.match(tt.lessThan)) {
    flowParseTypeParameterInstantiation();
  }
  if (isContextual(ContextualKeyword._implements)) {
    const oldIsType = state.pushTypeContext(0);
    state.next();
    state.tokens[state.tokens.length - 1].type = tt._implements;
    do {
      flowParseRestrictedIdentifier();
      if (state.match(tt.lessThan)) {
        flowParseTypeParameterInstantiation();
      }
    } while (state.eat(tt.comma));
    state.popTypeContext(oldIsType);
  }
}

// parse type parameters for object method shorthand
export function flowStartParseObjPropValue(): void {
  // method shorthand
  if (state.match(tt.lessThan)) {
    flowParseTypeParameterDeclaration();
    if (!state.match(tt.parenL)) unexpected();
  }
}

export function flowParseAssignableListItemTypes(): void {
  const oldIsType = state.pushTypeContext(0);
  state.eat(tt.question);
  if (state.match(tt.colon)) {
    flowParseTypeAnnotation();
  }
  state.popTypeContext(oldIsType);
}

// parse typeof and type imports
export function flowStartParseImportSpecifiers(): void {
  if (state.match(tt._typeof) || isContextual(ContextualKeyword._type)) {
    const lh = state.lookaheadTypeAndKeyword();
    if (isMaybeDefaultImport(lh) || lh.type === tt.braceL || lh.type === tt.star) {
      state.next();
    }
  }
}

// parse import-type/typeof shorthand
export function flowParseImportSpecifier(): void {
  const isTypeKeyword =
    state.contextualKeyword === ContextualKeyword._type || state.type === tt._typeof;
  if (isTypeKeyword) {
    state.next();
  } else {
    parseIdentifier();
  }

  if (isContextual(ContextualKeyword._as) && !isLookaheadContextual(ContextualKeyword._as)) {
    parseIdentifier();
    if (isTypeKeyword && !state.match(tt.name) && !(state.type & TokenType.IS_KEYWORD)) {
      // `import {type as ,` or `import {type as }`
    } else {
      // `import {type as foo`
      parseIdentifier();
    }
  } else {
    if (isTypeKeyword && (state.match(tt.name) || !!(state.type & TokenType.IS_KEYWORD))) {
      // `import {type foo`
      parseIdentifier();
    }
    if (eatContextual(ContextualKeyword._as)) {
      parseIdentifier();
    }
  }
}

// parse function type parameters - function foo<T>() {}
export function flowStartParseFunctionParams(): void {
  // Originally this checked if the method is a getter/setter, but if it was, we'd crash soon
  // anyway, so don't try to propagate that information.
  if (state.match(tt.lessThan)) {
    const oldIsType = state.pushTypeContext(0);
    flowParseTypeParameterDeclaration();
    state.popTypeContext(oldIsType);
  }
}

// parse flow type annotations on variable declarator heads - let foo: string = bar
export function flowAfterParseVarHead(): void {
  if (state.match(tt.colon)) {
    flowParseTypeAnnotation();
  }
}

// parse the return type of an async arrow function - let foo = (async (): number => {});
export function flowStartParseAsyncArrowFromCallExpression(): void {
  if (state.match(tt.colon)) {
    const oldNoAnonFunctionType = state.noAnonFunctionType;
    state.noAnonFunctionType = true;
    flowParseTypeAnnotation();
    state.noAnonFunctionType = oldNoAnonFunctionType;
  }
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
export function flowParseMaybeAssign(noIn: boolean, isWithinParens: boolean): boolean {
  if (state.match(tt.lessThan)) {
    const snapshot = state.snapshot();
    let wasArrow = baseParseMaybeAssign(noIn, isWithinParens);
    if (state.error) {
      state.restoreFromSnapshot(snapshot);
      state.type = tt.typeParameterStart;
    } else {
      return wasArrow;
    }

    const oldIsType = state.pushTypeContext(0);
    flowParseTypeParameterDeclaration();
    state.popTypeContext(oldIsType);
    wasArrow = baseParseMaybeAssign(noIn, isWithinParens);
    if (wasArrow) {
      return true;
    }
    unexpected();
  }

  return baseParseMaybeAssign(noIn, isWithinParens);
}

// handle return types for arrow functions
export function flowParseArrow(): boolean {
  if (state.match(tt.colon)) {
    const oldIsType = state.pushTypeContext(0);
    const snapshot = state.snapshot();

    const oldNoAnonFunctionType = state.noAnonFunctionType;
    state.noAnonFunctionType = true;
    flowParseTypeAndPredicateInitialiser();
    state.noAnonFunctionType = oldNoAnonFunctionType;

    if (canInsertSemicolon()) unexpected();
    if (!state.match(tt.arrow)) unexpected();

    if (state.error) {
      state.restoreFromSnapshot(snapshot);
    }
    state.popTypeContext(oldIsType);
  }
  return state.eat(tt.arrow);
}

export function flowParseSubscripts(startTokenIndex: number, noCalls: boolean = false): void {
  if (
    state.tokens[state.tokens.length - 1].contextualKeyword === ContextualKeyword._async &&
    state.match(tt.lessThan)
  ) {
    const snapshot = state.snapshot();
    const wasArrow = parseAsyncArrowWithTypeParameters();
    if (wasArrow && !state.error) {
      return;
    }
    state.restoreFromSnapshot(snapshot);
  }

  baseParseSubscripts(startTokenIndex, noCalls);
}

// Returns true if there was an arrow function here.
function parseAsyncArrowWithTypeParameters(): boolean {
  state.scopeDepth++;
  const startTokenIndex = state.tokens.length;
  parseFunctionParams();
  if (!parseArrow()) {
    return false;
  }
  parseArrowExpression(startTokenIndex);
  return true;
}

function flowParseEnumDeclaration(): void {
  expectContextual(ContextualKeyword._enum);
  state.tokens[state.tokens.length - 1].type = tt._enum;
  parseIdentifier();
  flowParseEnumBody();
}

function flowParseEnumBody(): void {
  if (eatContextual(ContextualKeyword._of)) {
    state.next();
  }
  expect(tt.braceL);
  flowParseEnumMembers();
  expect(tt.braceR);
}

function flowParseEnumMembers(): void {
  while (!state.match(tt.braceR) && !state.error) {
    if (state.eat(tt.ellipsis)) {
      break;
    }
    flowParseEnumMember();
    if (!state.match(tt.braceR)) {
      expect(tt.comma);
    }
  }
}

function flowParseEnumMember(): void {
  parseIdentifier();
  if (state.eat(tt.eq)) {
    // Flow enum values are always just one token (a string, number, or boolean literal).
    state.next();
  }
}
