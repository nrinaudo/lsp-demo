# LSP demo implementation

The purpose of this project is strictly to play with LSP. Do not expect anything useful out of it.

Specifically, no care was given to:
- performance
- stack safety

I also admitedly went a little overboard, since a lot of the required tools looked like much more fun to implement
than to find existing implementations for.

## Running the server

Running `sbt server/assembly` should yield an `server/target/scala-3.1.3/lsp-demo-server.jar` file.

Put that, as well as `./server/src/main/sh/lsp-demo-server.sh`, in a directory on your `$PATH`.

## Testing VSCode integration

Use VSCode to open the `./vscode` directory, and open `extension.ts`. Press `F5` to debug your extension, and start
typing in a new plain text file.

# Reading the code

I had quite a bit of fun with this, and there are a few parts that are interesting for their own sake, not just as part
of a working LSP server.

## Overview

In order to have acceptable error reports and semantic tokens, I had to implement multiple compilation phases.

First, tokenization: turning a source file (essentially a list of characters) into known tokens. This allows us to have
known tokens even if the source file isn't syntactically correct - that is, to be able to provide syntax highlighting
to the LSP client.

Second, parsing: turning a list of tokens into an untyped AST. This might fail because while we're manipulating known
tokens, their order might not make sense (`if then true =`, say). The untyped AST is useful, as it provides:
- more precise syntax highlighting information than mere tokens
- a hierarchy of symbols (used by LSP clients to display an overview of the code's organisation)

Another useful aspect of having an untyped AST is that it allows us to check the syntax without worrying about the
semantics. A source file might be syntactically correct but semantically invalid (`if true then 1 else false`).
Separating both analysis allows us to fail at the first syntax error, but, in the presence of a syntactically valid
source file, provide all known semantical (read: type) errors.

Third, type checking: turning an untyped AST into a typed one. This is where most of the clever error messages are going
to come from, as it'll expose things like "expected left hand side and right hand side to be of the same type".

## Parser combinators

The first and second phase (tokenization and parsing) are both implemented using a custom parser-combinator library.

While it's probably not very efficient, it has the advantage of abstracting over the type of atomic elements:
- characters when reading from the source file.
- tokens when trying to extract an untyped AST.

I also went through some pain implementing:
- parser labels, which might not be perfect, but do allow for some fairly understandable error messages.
- position tracking, to store where in a source file a particular token was encountered.

The code for this parsec implementation can be found in the `parser` package.

## Expressions

The [[exp]] package exposes all the tools needed to go from a source file to a typed expression.

The type checking code, in particular, is interesting: it manages to provide static type analysis at runtime through
heavy-handed usage of GADTs. This was particularly eye opening, at least for me, and made the way GADTs allow the
compiler to reason about types much more obvious.