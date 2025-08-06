// Run me like so:
// npm install
// npx ts-node iml2latex.ts

// Maybe compile a binary along these lines?
// deno compile --unstable-sloppy-imports --no-check -A -o runme iml2latex.ts

import * as fs from 'node:fs';
import { Buffer } from 'node:buffer';

import * as prettier from 'prettier';
import * as iml_prettier from './plugin';

export async function format(text: string, config_root: string | null = null, function_name: string | null = null): Promise<string> {
  try {
    let options: prettier.Options | null = null;
    if (config_root) {
      const config_file = await prettier.resolveConfigFile(config_root);
      if (config_file)
        options = await prettier.resolveConfig(config_file);
    }
    if (!options) {
      options = {
        semi: false
      };
    }
    options.parser = "iml2latex-parse";
    options.plugins = [iml_prettier];
    if (function_name)
      globalThis.function_names = [function_name]
    return await prettier.format(text, options);
  }
  catch (e: any) {
    console.log("Prettier error: " + e.toString() + "\n" + e.stack.toString());
    return "";
  }
}

export async function format_file(file_name: string, function_name: string): Promise<string> {
  const stats = fs.statSync(file_name);
  const buffer = Buffer.allocUnsafe(stats.size);
  const fd = fs.openSync(file_name, "r");
  fs.readSync(fd, buffer, 0, buffer.length, 0);
  return await format(buffer.toString(), file_name, function_name);
}

(async function main() {
  if (process.argv.length <= 2)
    console.log("Usage: npx ts-node iml2latex.ts <file name> <function name>");
  else {
    const r = await format_file(process.argv[2], process.argv[3]);
    console.log(r);
  }
})();