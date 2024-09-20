import {
  IdentifierRole,
  JSXRole,
  Token,
} from "../../token";
import {TokenType as tt} from "../../generated/types";
import {state} from "../../state";
import {parseExpression, parseMaybeAssign} from "../../traverser";
import {IS_IDENTIFIER_CHAR, IS_IDENTIFIER_START} from "../../charcode";
import {Charcode} from "../../charcode";
import {tsTryParseJSXTypeArgument} from "../typescript";

/**
 * Read token with JSX contents.
 *
 * In addition to detecting jsxTagStart and also regular tokens that might be
 * part of an expression, this code detects the start and end of text ranges
 * within JSX children. In order to properly count the number of children, we
 * distinguish jsxText from jsxEmptyText, which is a text range that simplifies
 * to the empty string after JSX whitespace trimming.
 *
 * It turns out that a JSX text range will simplify to the empty string if and
 * only if both of these conditions hold:
 * - The range consists entirely of whitespace characters (only counting space,
 *   tab, \r, and \n).
 * - The range has at least one newline.
 * This can be proven by analyzing any implementation of whitespace trimming,
 * e.g. formatJSXTextLiteral in Sucrase or cleanJSXElementLiteralChild in Babel.
 */
function jsxReadToken(): void {
  let sawNewline = false;
  let sawNonWhitespace = false;
  while (true) {
    if (state.pos >= state.input.length) {
      state.unexpected("Unterminated JSX contents");
      return;
    }

    const ch = state.input.charCodeAt(state.pos);
    if (ch === Charcode.lessThan || ch === Charcode.leftCurlyBrace) {
      if (state.pos === state.start) {
        if (ch === Charcode.lessThan) {
          state.pos++;
          state.scanner.finishToken(tt.jsxTagStart);
          return;
        }
        state.scanner.getTokenFromCode(ch);
        return;
      }
      if (sawNewline && !sawNonWhitespace) {
        state.scanner.finishToken(tt.jsxEmptyText);
      } else {
        state.scanner.finishToken(tt.jsxText);
      }
      return;
    }

    // This is part of JSX text.
    if (ch === Charcode.lineFeed) {
      sawNewline = true;
    } else if (ch !== Charcode.space && ch !== Charcode.carriageReturn && ch !== Charcode.tab) {
      sawNonWhitespace = true;
    }
    state.pos++;
  }
}

function jsxReadString(quote: number): void {
  state.pos++;
  for (;;) {
    if (state.pos >= state.input.length) {
      state.unexpected("Unterminated string constant");
      return;
    }

    const ch = state.input.charCodeAt(state.pos);
    if (ch === quote) {
      state.pos++;
      break;
    }
    state.pos++;
  }
  state.scanner.finishToken(tt.string);
}

// Read a JSX identifier (valid tag or attribute name).
//
// Optimized version since JSX identifiers can't contain
// escape characters and so can be read as single slice.
// Also assumes that first character was already checked
// by isIdentifierStart in readToken.

function jsxReadWord(): void {
  let ch: number;
  do {
    if (state.pos > state.input.length) {
      state.unexpected("Unexpectedly reached the end of input.");
      return;
    }
    ch = state.input.charCodeAt(++state.pos);
  } while (IS_IDENTIFIER_CHAR[ch] || ch === Charcode.dash);
  state.scanner.finishToken(tt.jsxName);
}

// Parse next token as JSX identifier
function jsxParseIdentifier(): void {
  nextJSXTagToken();
}

// Parse namespaced identifier.
function jsxParseNamespacedName(identifierRole: IdentifierRole): void {
  jsxParseIdentifier();
  if (!state.eat(tt.colon)) {
    // Plain identifier, so this is an access.
    state.tokens[state.tokens.length - 1].identifierRole = identifierRole;
    return;
  }
  // Process the second half of the namespaced name.
  jsxParseIdentifier();
}

// Parses element name in any form - namespaced, member
// or single identifier.
function jsxParseElementName(): void {
  const firstTokenIndex = state.tokens.length;
  jsxParseNamespacedName(IdentifierRole.Access);
  let hadDot = false;
  while (state.match(tt.dot)) {
    hadDot = true;
    nextJSXTagToken();
    jsxParseIdentifier();
  }
  // For tags like <div> with a lowercase letter and no dots, the name is
  // actually *not* an identifier access, since it's referring to a built-in
  // tag name. Remove the identifier role in this case so that it's not
  // accidentally transformed by the imports transform when preserving JSX.
  if (!hadDot) {
    const firstToken = state.tokens[firstTokenIndex];
    const firstChar = state.input.charCodeAt(firstToken.start);
    if (firstChar >= Charcode.lowercaseA && firstChar <= Charcode.lowercaseZ) {
      firstToken.identifierRole = null;
    }
  }
}

// Parses any type of JSX attribute value.
function jsxParseAttributeValue(): void {
  switch (state.type) {
    case tt.braceL:
      state.next();
      parseExpression();
      nextJSXTagToken();
      return;

    case tt.jsxTagStart:
      jsxParseElement();
      nextJSXTagToken();
      return;

    case tt.string:
      nextJSXTagToken();
      return;

    default:
      state.unexpected("JSX value should be either an expression or a quoted JSX text");
  }
}

// Parse JSX spread child, after already processing the {
// Does not parse the closing }
function jsxParseSpreadChild(): void {
  state.expect(tt.ellipsis);
  parseExpression();
}

// Parses JSX opening tag starting after "<".
// Returns true if the tag was self-closing.
// Does not parse the last token.
function jsxParseOpeningElement(initialTokenIndex: number): boolean {
  if (state.match(tt.jsxTagEnd)) {
    // This is an open-fragment.
    return false;
  }
  jsxParseElementName();
  if (state.isTypeScriptEnabled) {
    tsTryParseJSXTypeArgument();
  }
  let hasSeenPropSpread = false;
  while (!state.match(tt.slash) && !state.match(tt.jsxTagEnd) && !state.error) {
    if (state.eat(tt.braceL)) {
      hasSeenPropSpread = true;
      state.expect(tt.ellipsis);
      parseMaybeAssign();
      // }
      nextJSXTagToken();
      continue;
    }
    if (
      hasSeenPropSpread &&
      state.end - state.start === 3 &&
      state.input.charCodeAt(state.start) === Charcode.lowercaseK &&
      state.input.charCodeAt(state.start + 1) === Charcode.lowercaseE &&
      state.input.charCodeAt(state.start + 2) === Charcode.lowercaseY
    ) {
      state.tokens[initialTokenIndex].jsxRole = JSXRole.KeyAfterPropSpread;
    }
    jsxParseNamespacedName(IdentifierRole.ObjectKey);
    if (state.match(tt.eq)) {
      nextJSXTagToken();
      jsxParseAttributeValue();
    }
  }
  const isSelfClosing = state.match(tt.slash);
  if (isSelfClosing) {
    // /
    nextJSXTagToken();
  }
  return isSelfClosing;
}

// Parses JSX closing tag starting after "</".
// Does not parse the last token.
function jsxParseClosingElement(): void {
  if (state.match(tt.jsxTagEnd)) {
    // Fragment syntax, so we immediately have a tag end.
    return;
  }
  jsxParseElementName();
}

// Parses entire JSX element, including its opening tag
// (starting after "<"), attributes, contents and closing tag.
// Does not parse the last token.
function jsxParseElementAt(): void {
  const initialTokenIndex = state.tokens.length - 1;
  state.tokens[initialTokenIndex].jsxRole = JSXRole.NoChildren;
  let numExplicitChildren = 0;
  const isSelfClosing = jsxParseOpeningElement(initialTokenIndex);
  if (!isSelfClosing) {
    nextJSXExprToken();
    while (true) {
      switch (state.type) {
        case tt.jsxTagStart:
          nextJSXTagToken();
          if (state.match(tt.slash)) {
            nextJSXTagToken();
            jsxParseClosingElement();
            // Key after prop spread takes precedence over number of children,
            // since it means we switch to createElement, which doesn't care
            // about number of children.
            if (state.tokens[initialTokenIndex].jsxRole !== JSXRole.KeyAfterPropSpread) {
              if (numExplicitChildren === 1) {
                state.tokens[initialTokenIndex].jsxRole = JSXRole.OneChild;
              } else if (numExplicitChildren > 1) {
                state.tokens[initialTokenIndex].jsxRole = JSXRole.StaticChildren;
              }
            }
            return;
          }
          numExplicitChildren++;
          jsxParseElementAt();
          nextJSXExprToken();
          break;

        case tt.jsxText:
          numExplicitChildren++;
          nextJSXExprToken();
          break;

        case tt.jsxEmptyText:
          nextJSXExprToken();
          break;

        case tt.braceL:
          state.next();
          if (state.match(tt.ellipsis)) {
            jsxParseSpreadChild();
            nextJSXExprToken();
            // Spread children are a mechanism to explicitly mark children as
            // static, so count it as 2 children to satisfy the "more than one
            // child" condition.
            numExplicitChildren += 2;
          } else {
            // If we see {}, this is an empty pseudo-expression that doesn't
            // count as a child.
            if (!state.match(tt.braceR)) {
              numExplicitChildren++;
              parseExpression();
            }
            nextJSXExprToken();
          }

          break;

        // istanbul ignore next - should never happen
        default:
          state.unexpected();
          return;
      }
    }
  }
}

// Parses entire JSX element from current position.
// Does not parse the last token.
export function jsxParseElement(): void {
  nextJSXTagToken();
  jsxParseElementAt();
}

// ==================================
// Overrides
// ==================================

export function nextJSXTagToken(): void {
  state.tokens.push(new Token(state));
  state.scanner.skipSpace();
  state.start = state.pos;
  const code = state.input.charCodeAt(state.pos);

  if (IS_IDENTIFIER_START[code]) {
    jsxReadWord();
  } else if (code === Charcode.quotationMark || code === Charcode.apostrophe) {
    jsxReadString(code);
  } else {
    // The following tokens are just one character each.
    ++state.pos;
    switch (code) {
      case Charcode.greaterThan:
        state.scanner.finishToken(tt.jsxTagEnd);
        break;
      case Charcode.lessThan:
        state.scanner.finishToken(tt.jsxTagStart);
        break;
      case Charcode.slash:
        state.scanner.finishToken(tt.slash);
        break;
      case Charcode.equalsTo:
        state.scanner.finishToken(tt.eq);
        break;
      case Charcode.leftCurlyBrace:
        state.scanner.finishToken(tt.braceL);
        break;
      case Charcode.dot:
        state.scanner.finishToken(tt.dot);
        break;
      case Charcode.colon:
        state.scanner.finishToken(tt.colon);
        break;
      default:
        state.unexpected();
    }
  }
}

function nextJSXExprToken(): void {
  state.tokens.push(new Token(state));
  state.start = state.pos;
  jsxReadToken();
}
