import type {NameManager} from "./managers/NameManager";
import {TokenType as tt} from "../parser/types.generated";
import type {TokenProcessor} from "./processors/TokenProcessor";
import Transformer from "./Transformer";

export class OptionalCatchBindingTransformer extends Transformer {
  constructor(readonly tokens: TokenProcessor, readonly nameManager: NameManager) {
    super();
  }

  process(): boolean {
    if (this.tokens.matches2(tt._catch, tt.braceL)) {
      this.tokens.copyToken();
      this.tokens.appendCode(` (${this.nameManager.claimFreeName("e")})`);
      return true;
    }
    return false;
  }
}
