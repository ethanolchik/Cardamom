import assert from 'node:assert/strict';
import { existsSync } from 'node:fs';
import { readFile, readdir } from 'node:fs/promises';
import { createRequire } from 'node:module';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';
import textmate from 'vscode-textmate';
import oniguruma from 'vscode-oniguruma';

const require = createRequire(import.meta.url);
const extensionRoot = fileURLToPath(new URL('..', import.meta.url));
const projectRoot = path.resolve(extensionRoot, '../..');
const raw = JSON.parse(await readFile(path.join(extensionRoot, 'syntaxes/cardamom.tmLanguage.json'), 'utf8'));
const wasm = await readFile(require.resolve('vscode-oniguruma/release/onig.wasm'));
await oniguruma.loadWASM(wasm.buffer.slice(wasm.byteOffset, wasm.byteOffset + wasm.byteLength));

const cppPath = [
  process.env.CARDAMOM_CPP_GRAMMAR,
  '/Applications/Visual Studio Code.app/Contents/Resources/app/extensions/cpp/syntaxes/cpp.tmLanguage.json',
  '/Applications/Cursor.app/Contents/Resources/app/extensions/cpp/syntaxes/cpp.tmLanguage.json',
  '/usr/share/code/resources/app/extensions/cpp/syntaxes/cpp.tmLanguage.json',
  '/usr/share/code-insiders/resources/app/extensions/cpp/syntaxes/cpp.tmLanguage.json',
].find(candidate => candidate && existsSync(candidate));

// CI can test Cardamom and embedding boundaries without installing an editor.
// Nested C++ contexts deliberately remain open in the malformed-input regression.
const fallbackCpp = {
  scopeName: 'source.cpp',
  patterns: [
    { name: 'keyword.control.cpp', match: '\\b(?:return|if)\\b' },
    { name: 'comment.line.double-slash.cpp', begin: '//', end: '$' },
    { name: 'meta.block.cpp', begin: '\\{', end: '\\}', patterns: [{ include: 'source.cpp' }] },
    { name: 'meta.parens.cpp', begin: '\\(', end: '\\)', patterns: [{ include: 'source.cpp' }] },
  ],
};
const cpp = cppPath ? textmate.parseRawGrammar(await readFile(cppPath, 'utf8'), cppPath) : fallbackCpp;
const registry = new textmate.Registry({
  onigLib: Promise.resolve({
    createOnigScanner: patterns => new oniguruma.OnigScanner(patterns),
    createOnigString: source => new oniguruma.OnigString(source),
  }),
  loadGrammar: async scope => scope === 'source.cardamom' ? raw : scope === 'source.cpp' ? cpp : null,
});
const grammar = await registry.loadGrammar('source.cardamom');
assert.ok(grammar);

function tokenize(source) {
  let state = textmate.INITIAL;
  return source.split(/\r?\n/).map(line => {
    const result = grammar.tokenizeLine(line, state);
    state = result.ruleStack;
    return result.tokens.map(token => ({ ...token, text: line.slice(token.startIndex, token.endIndex) }));
  });
}

function scopesAt(source, needle, occurrence = 0) {
  let index = -1;
  for (let n = 0; n <= occurrence; n += 1) {
    index = source.indexOf(needle, index + 1);
    assert.notEqual(index, -1, `Missing ${needle} in the test source`);
  }
  const prefix = source.slice(0, index).split('\n');
  const column = prefix.at(-1).length;
  const token = tokenize(source)[prefix.length - 1].find(token => token.startIndex <= column && column < token.endIndex);
  assert.ok(token, `No token for ${needle}`);
  return token.scopes;
}

function hasScope(source, needle, expected, occurrence = 0) {
  const scopes = scopesAt(source, needle, occurrence);
  assert.ok(scopes.includes(expected), `${needle}: expected ${expected}, got ${scopes.join(' ')}`);
}

test('declarations, generic types, methods, module aliases, and borrows get useful scopes', () => {
  const source = `import io as console;
public class Box<T>(private value: T) {
    public get() -> T { return this.value; }
}
fn extern native(value: int) -> int {}
fn first<T>(xs: &mut T[]) -> T { return xs[0]; }
let box: Box<int> = new Box<int>(1);`;
  hasScope(source, 'console', 'entity.name.namespace.cardamom');
  hasScope(source, 'Box', 'entity.name.type.class.cardamom');
  hasScope(source, 'T>', 'entity.name.type.cardamom');
  hasScope(source, 'get', 'entity.name.function.cardamom');
  hasScope(source, 'this', 'variable.language.this.cardamom');
  hasScope(source, 'extern', 'storage.modifier.cardamom');
  hasScope(source, 'native', 'entity.name.function.cardamom');
  hasScope(source, 'first', 'entity.name.function.cardamom');
  hasScope(source, '&', 'keyword.operator.cardamom');
  hasScope(source, 'mut', 'storage.modifier.cardamom');
  hasScope(source, 'int', 'storage.type.primitive.cardamom');
});

test('selective imports highlight multiline members, aliases, and comments', () => {
  const source = `import math.{
    sin,
    sqrt as root, // aliases keep their function scope
};
import model.{Box as Parcel, Readable};
fn after() { return; }`;
  hasScope(source, 'import', 'keyword.control.import.cardamom');
  hasScope(source, 'math', 'entity.name.namespace.cardamom');
  hasScope(source, '.', 'punctuation.accessor.cardamom');
  hasScope(source, 'sin', 'entity.name.function.cardamom');
  hasScope(source, 'sqrt', 'entity.name.function.cardamom');
  hasScope(source, 'as root', 'keyword.control.import.cardamom');
  hasScope(source, 'root', 'entity.name.function.cardamom');
  hasScope(source, 'aliases', 'comment.line.double-slash.cardamom');
  hasScope(source, 'Box', 'entity.name.type.cardamom');
  hasScope(source, 'Parcel', 'entity.name.type.cardamom');
  hasScope(source, 'Readable', 'entity.name.type.cardamom');
  hasScope(source, 'after', 'entity.name.function.cardamom');
  assert.ok(!scopesAt(source, 'after').includes('meta.import.cardamom'));
});

test('an incomplete member list ends at the import semicolon', () => {
  const source = 'import math.{sin;\nfn after() {}';
  hasScope(source, 'sin', 'entity.name.function.cardamom');
  hasScope(source, 'after', 'entity.name.function.cardamom');
  assert.ok(!scopesAt(source, 'after').includes('meta.import.cardamom'));
});

test('traits, implementations, and constraints get useful scopes', () => {
  const source = `trait Printable { text() -> string; }
fn render<T>(value: &T) -> string where T: Printable { return value.text(); }
impl Printable for int { public text() -> string { return "int"; } }`;
  hasScope(source, 'trait', 'storage.type.trait.cardamom');
  hasScope(source, 'Printable', 'entity.name.type.trait.cardamom');
  hasScope(source, 'where', 'keyword.other.where.cardamom');
  hasScope(source, 'impl', 'keyword.declaration.impl.cardamom');
});

test('strings and line comments shield keywords and comment markers', () => {
  const source = `let text: string = "class // return 123";
// fn fake() -> int { return 42; }
let other: string = 'while // string';`;
  hasScope(source, 'class', 'string.quoted.double.cardamom');
  hasScope(source, 'fake', 'comment.line.double-slash.cardamom');
  hasScope(source, '42', 'comment.line.double-slash.cardamom');
  hasScope(source, 'while', 'string.quoted.single.cardamom');
  assert.ok(!scopesAt(source, '123').includes('constant.numeric.cardamom'));
});

test('multiline strings carry state and backslashes do not escape closing quotes', () => {
  const multiline = 'let text: string = "one\ntwo"; let count: int = 3;';
  hasScope(multiline, 'two', 'string.quoted.double.cardamom');
  hasScope(multiline, 'count', 'variable.other.readwrite.cardamom');
  const source = String.raw`let text: string = "ends with a slash \"; let after: int = 1;`;
  hasScope(source, 'after', 'variable.other.readwrite.cardamom');
  assert.ok(!scopesAt(source, 'after').includes('string.quoted.double.cardamom'));
});

test('C++ keywords and block-comment markers keep their Cardamom meaning outside intrinsics', () => {
  const source = 'let template: int = 0; let virtual: int = 1; /* return */';
  hasScope(source, 'template', 'variable.other.readwrite.cardamom');
  hasScope(source, 'virtual', 'variable.other.readwrite.cardamom');
  hasScope(source, 'return', 'keyword.control.cardamom');
  assert.ok(!scopesAt(source, 'return').some(scope => scope.startsWith('comment.')));
});

test('closures are distinguished from logical and bitwise OR', () => {
  const closure = 'let f: fn(int) -> int = |value| int -> { return value; };';
  hasScope(closure, 'value', 'variable.parameter.cardamom');
  hasScope(closure, '|', 'punctuation.definition.parameters.begin.cardamom');
  hasScope('let f: fn() -> int = || int -> { return 1; };', '||', 'punctuation.definition.parameters.begin.cardamom');
  hasScope('if (left || right) { return; }', '||', 'keyword.operator.cardamom');
  hasScope('let mask: int = a | b | c;', 'b', 'variable.other.readwrite.cardamom');
});

test('decimals, comparisons, and generic calls do not consume one another', () => {
  const source = 'let value: float = -12.5; if (a < b && b > c) { identity<int>(42); }';
  hasScope(source, '12.5', 'constant.numeric.cardamom');
  hasScope(source, 'if', 'keyword.control.cardamom');
  hasScope(source, '<', 'keyword.operator.cardamom');
  hasScope(source, 'b', 'variable.other.readwrite.cardamom');
  hasScope(source, 'identity', 'entity.name.function.cardamom');
});

test('native code uses C++ scopes and returns to Cardamom at the closing quote', () => {
  for (const quote of ['"', "'"]) {
    for (const newline of ['', '\n']) {
      const source = `@include("<cmath>"); @cpp(${quote}${newline}return std::sqrt(value);${quote}); let after: int = 2;`;
      hasScope(source, 'include', 'support.function.intrinsic.cardamom');
      hasScope(source, '<cmath>', 'string.quoted.double.cardamom');
      hasScope(source, 'cpp', 'support.function.intrinsic.cardamom');
      hasScope(source, 'return', 'meta.embedded.block.cpp');
      const returned = scopesAt(source, 'return');
      assert.ok(returned.some(scope => scope.startsWith('keyword.') && scope.endsWith('.cpp')), returned.join(' '));
      hasScope(source, 'after', 'variable.other.readwrite.cardamom');
      assert.ok(!scopesAt(source, 'after').includes('meta.embedded.block.cpp'));
    }
  }
});

test('unfinished C++ blocks and comments cannot swallow the surrounding Cardamom', () => {
  for (const quote of ['"', "'"]) {
    for (const body of ['if (value) {', 'call(', '// return 0;', '/* unfinished comment']) {
      const source = `@cpp(${quote}${body}${quote});\nfn after() -> int { return 1; }`;
      hasScope(source, 'after', 'entity.name.function.cardamom');
      assert.ok(!scopesAt(source, 'after').includes('meta.embedded.block.cpp'));
    }
  }
});

test('integration with the installed editor C++ grammar', { skip: !cppPath }, () => {
  const source = '@cpp("const char* value = std::getenv(name.c_str()); return value;");';
  hasScope(source, 'return', 'meta.embedded.block.cpp');
  assert.ok(scopesAt(source, 'const').some(scope => scope.startsWith('storage.') && scope.endsWith('.cpp')));
});

test('the repository source corpus tokenizes without errors', async () => {
  let files = 0;
  async function walk(directory) {
    for (const entry of await readdir(directory, { withFileTypes: true })) {
      const file = path.join(directory, entry.name);
      if (entry.isDirectory()) await walk(file);
      else if (entry.name.endsWith('.crdm')) {
        const lines = tokenize(await readFile(file, 'utf8'));
        assert.ok(lines.every(tokens => tokens.length > 0), file);
        files += 1;
      }
    }
  }
  await walk(path.join(projectRoot, 'std'));
  await walk(path.join(projectRoot, 'tests'));
  await walk(path.join(extensionRoot, 'test/fixtures'));
  assert.ok(files >= 50, `Only ${files} source files were checked`);
});
