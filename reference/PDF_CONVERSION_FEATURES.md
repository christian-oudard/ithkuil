# PDF Conversion Feature Requests

Feature requests for `/projects/agent-capabilities/pdf_conversion/`, motivated by converting the Ithkuil grammar reference PDFs. These are Word/PowerPoint-exported PDFs with clean extractable text, complex tables, and vector-drawn glyphs — different from the scanned academic papers the tool was built for.

## 1. Render-only mode (`pdfconvert render`)

Add a `render` subcommand that exports pages as PNG and/or SVG without running OCR or Claude review.

```bash
pdfconvert render <pdf> [--pages 1-10] [--dpi 144] [--format png] [--format svg]
```

### Behavior

- Render each page to PNG at the specified DPI (default 144), grayscale
- Optionally also export each page as SVG via `pdftocairo -svg`
- Cache pages like the existing pipeline (skip existing files unless `--redo`)

### Output structure

```
$PDFCONVERT_OUTPUT_DIR/<name>/
  png/
    page_001.png
    page_002.png
    ...
  svg/          # only if --format svg
    page_001.svg
    page_002.svg
    ...
```

### Why

Some PDFs are primarily visual (e.g. writing system specs with hand-drawn glyph charts). OCR adds no value — the content needs to be seen, not read as text. Agents can view PNGs directly via the Read tool; SVGs preserve the vector data for future rendering or extraction.

### DPI and filesize guidance

For these PDFs, 144 DPI is the sweet spot: ~120-270 KB per page, legible for both humans and vision models. 100 DPI is too small to read fine details; 200 DPI doubles filesize with marginal benefit. The default should be 144, overridable.

### Dependencies

`pdftocairo` from poppler-utils for SVG export. PNG rendering can use the existing `pymupdf` path. Consider adding poppler-utils as an optional dependency, or using pymupdf's own SVG export if available.


## 2. Improve table reconstruction in the review prompt

The current `review_ocr.txt` prompt focuses on math formatting (`$...$`, LaTeX symbols). Tables are the most important structural element in non-math documents, but the prompt doesn't mention them.

### Current problem

When marker OCR encounters complex tables (multi-row headers, merged cells, two-column layouts), it produces fragments. Claude review reassembles prose well but doesn't consistently reconstruct tables into proper markdown format.

### Suggested prompt additions

Add to `review_ocr.txt` (or make the prompt modular so domain-specific additions can be layered):

```
Tables:
- Reconstruct tables as proper markdown tables with | delimiters and --- header separators
- Preserve all column headers, even multi-level headers (use bold or repeat as needed)
- If a table has merged cells or spans, represent the content in the most readable way
  (e.g. repeat the spanning value in each column, or use a note below the table)
- Two-column page layouts are NOT tables — treat each column as sequential prose
- If a table is too wide for markdown, consider splitting into multiple tables with a
  shared header, or use a simpler layout that preserves the data accurately
- Accuracy of cell values matters more than perfect formatting
```

### Why

Many reference documents (grammar specs, data sheets, API docs) are table-heavy. The current prompt silently lets Claude decide how to handle tables, which produces inconsistent results. Explicit guidance will improve the default conversion quality for all documents, not just Ithkuil.


