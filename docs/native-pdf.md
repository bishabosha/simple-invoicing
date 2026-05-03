# Native PDF path

## Current renderer shape

The layout compiler emits a small drawing model:

- text runs with font, size, baseline position, and relative text steps
- stroked lines
- A4 pages

That means this project does not need a full PDF editing stack for the current invoice output.

## Options investigated

### Pure Scala PDF writer

Implemented in `ScratchPdfRenderer.scala`.

Pros:

- No JVM-only PDF dependency.
- No native dynamic library dependency.
- Small enough for the current `PageElement.Text` and `PageElement.Line` model.
- Compiles and runs under Scala Native.

Costs:

- We own PDF object writing, xref tables, content streams, text encoding, and page resources.
- Current font metrics are approximate, so wrapped or right-aligned text can drift from PDFBox/Cairo.
- Current text encoding is CP-1252-oriented. It handles common invoice text and `€`, but not arbitrary Unicode.
- No custom TTF embedding, image support, compression, metadata, PDF/A, accessibility, or font subsetting.

Use this if the native app only needs simple invoices and we value zero external PDF dependencies.

### Cairo C ABI binding

Implemented in `CairoPdf.scala` and `NativePdfRenderer.scala`.

Pros:

- Stable C ABI and already installed locally through Homebrew.
- PDF surface supports multi-page vector output.
- Cairo handles text measurement and PDF stream generation.
- Binding surface is small for our current needs.

Costs:

- Requires Cairo at build/link/runtime, or a packaging story that vendors it.
- The toy text API is adequate for this invoice, but richer typography/custom font loading would need extra bindings.
- Cairo is a drawing API rather than a document-authoring API.

Use this if we want reliable rendering, real font metrics, and less PDF-format ownership.

### libHaru

Not implemented.

Pros:

- Purpose-built ANSI C PDF generation library.
- Zlib/libpng-style license.
- Small conceptual match for lines, text, images, and generated documents.

Costs:

- Not installed locally in this environment.
- Upstream appears less actively maintained than Cairo.
- We would still need to bind enough of the API and solve packaging.

Use this if we specifically want a PDF-authoring C library instead of a drawing backend.

## Recommendation

For the native application, the least risky path is:

1. Keep the pure Scala scratch renderer as a dependency-free fallback and learning spike.
2. Use the Cairo binding as the first serious native renderer.
3. Reconsider libHaru only if Cairo's drawing model becomes awkward for future PDF features.

The main remaining native-app work is not PDF rendering; it is replacing JVM-only config/script dependencies with Scala Native-compatible code.
