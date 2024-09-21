import type {Token} from "../parser/token";
import {TokenType as tt} from "../parser/types.generated";

/**
 * Get all identifier names in the code, in order, including duplicates.
 */
export function getIdentifierNames(code: string, tokens: Array<Token>): Array<string> {
  const names = [];
  for (const token of tokens) {
    if (token.type === tt.name) {
      names.push(code.slice(token.start, token.end));
    }
  }
  return names;
}
