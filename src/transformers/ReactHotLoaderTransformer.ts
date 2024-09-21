import { IdentifierRole } from "../parser/token"
import type { TokenProcessor } from "../processors/TokenProcessor"
import Transformer from "./Transformer"

export class ReactHotLoaderTransformer extends Transformer {
  private extractedDefaultExportName: string | null = null;

  constructor(readonly tokens: TokenProcessor, readonly filePath: string) {
    super()
  }

  setExtractedDefaultExportName(extractedDefaultExportName: string): void {
    this.extractedDefaultExportName = extractedDefaultExportName
  }

  override getPrefixCode(): string {
    return `
      (function () {
        var enterModule = require('react-hot-loader').enterModule;
        enterModule && enterModule(module);
      })();`
      .replace(/\s+/g, " ")
      .trim()
  }

  override getSuffixCode(): string {
    const topLevelNames = new Set<string>()
    for (const token of this.tokens.tokens) {
      if (
        !token.isType &&
        token.isTopLevelDeclaration() &&
        token.identifierRole !== IdentifierRole.ImportDeclaration
      ) {
        topLevelNames.add(this.tokens.identifierNameForToken(token))
      }
    }
    const namesToRegister = Array.from(topLevelNames).map((name) => ({
      variableName: name,
      uniqueLocalName: name,
    }))
    if (this.extractedDefaultExportName) {
      namesToRegister.push({
        variableName: this.extractedDefaultExportName,
        uniqueLocalName: "default",
      })
    }
    return `
;(function () {
  var reactHotLoader = require('react-hot-loader').default;
  var leaveModule = require('react-hot-loader').leaveModule;
  if (!reactHotLoader) {
    return;
  }
${namesToRegister
        .map(
          ({ variableName, uniqueLocalName }) =>
            `  reactHotLoader.register(${variableName}, "${uniqueLocalName}", ${JSON.stringify(
              this.filePath || "",
            )});`,
        )
        .join("\n")}
  leaveModule(module);
})();`
  }

  process(): boolean {
    return false
  }
}
