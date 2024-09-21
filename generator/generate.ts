#!./node_modules/.bin/sucrase-node
/* eslint-disable no-console */
import {writeFile} from "mz/fs";

import run from "../script/run";
import generateReadWordTree from "./generateReadWordTree";
import generateTokenTypes from "./generateTokenTypes";

/**
 * Use code generation.
 */
async function generate(): Promise<void> {

  await writeFile("./src/parser/types.generated.ts", generateTokenTypes());
  await run("./node_modules/.bin/prettier --write ./src/parser/types.generated.ts");

  await writeFile("./src/parser/readWordTree.generated.ts", generateReadWordTree());
  await run("./node_modules/.bin/prettier --write ./src/parser/readWordTree.generated.ts");

  console.log("Done with code generation.");
}

generate().catch((e) => {
  console.error("Error during code generation!");
  console.error(e);
  process.exitCode = 1;
});
