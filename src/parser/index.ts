import { state, initParser, type Scope } from './state'
import type { Token } from "./token"
import { augmentError, parseFile } from "./traverser"

export class File {
  tokens: Array<Token>;
  scopes: Array<Scope>;

  constructor(tokens: Array<Token>, scopes: Array<Scope>) {
    this.tokens = tokens;
    this.scopes = scopes;
  }
}

export function parse(
  input: string,
  isJSXEnabled: boolean,
  isTypeScriptEnabled: boolean,
  isFlowEnabled: boolean,
): File {
  if (isFlowEnabled && isTypeScriptEnabled) {
    throw new Error("Cannot combine flow and typescript plugins.");
  }
  initParser(input, isJSXEnabled, isTypeScriptEnabled, isFlowEnabled);
  const result = parseFile();
  if (state.error) {
    throw augmentError(state.error);
  }
  return result;
}
