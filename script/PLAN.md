# Ithkuil V4 Script Implementation Plan

## Unicode Private Use Area Allocation

Using BMP PUA (U+E000-U+F8FF, 6400 codepoints).

### Secondary Characters (consonant glyphs): U+E000-U+E01F (32)
Each consonant has a base print form. The 31 Ithkuil consonants:
p(E000) b(E001) t(E002) d(E003) k(E004) g(E005)
f(E006) v(E007) ţ(E008) ḑ(E009) s(E00A) z(E00B)
š(E00C) ž(E00D) ç(E00E) x(E00F) h(E010) ļ(E011)
c(E012) ẓ(E013) č(E014) j(E015) m(E016) n(E017)
ň(E018) r(E019) l(E01A) w(E01B) y(E01C) ř(E01D)
'(E01E) placeholder(E01F)

### Consonant Extensions (combining): U+E020-U+E05F (64)
Top extensions (preceding consonant): U+E020-U+E03F
Bottom extensions (following consonant): U+E040-U+E05F
Each consonant maps to an extension glyph that attaches to the
top or bottom of the preceding secondary character.

### Primary Character Components: U+E060-U+E0BF (96)
- Specification shapes (4): U+E060-E063 (BSC, CTE, CSV, OBJ)
- Context diacritics (4): U+E064-E067 (EXS, FNC, RPS, AMG)
- Perspective x Extension (24): U+E068-E07F
- Affiliation x Essence (8): U+E080-E087
- Configuration undermarks (10): U+E088-E091
- Stem/Function/Version/Plexity (64): U+E092-E0D1
- Relation marks (3): U+E0D2-E0D4
- Concatenation marks (2): U+E0D5-E0D6

### Tertiary Characters: U+E100-U+E17F (128)
- Valence base forms (9): U+E100-E108
- Phase forms (9): U+E109-E111
- Effect forms (9): U+E112-E11A
- Aspect forms (36): U+E11B-E13E
- Level diacritics (9): U+E13F-E147

### Quaternary Characters: U+E180-U+E1FF (128)
- Case type top extensions (8): U+E180-E187
- Case number bottom extensions (9): U+E188-E190
- Illocution top extensions (9): U+E191-E199
- Validation bottom extensions (9): U+E19A-E1A2
- Mood diacritics (6): U+E1A3-E1A8
- Case-scope diacritics (6): U+E1A9-E1AE

### Degree Diacritics (combining): U+E200-U+E20A (11)
Degrees 0-9 plus Ca-stacking mark.

### Affix Type Diacritics (combining): U+E210-U+E212 (3)
Type 1 (none), Type 2 (dot), Type 3 (bar).

### Bias Characters: U+E220-U+E25F (64)

### Register Symbols: U+E260-U+E26F (16)

### Numeral Characters: U+E270-U+E28F (32)
Base digits 0-9 (10), tens extensions (9), hundreds extensions (9),
thousands diacritics (9).

### Alphabetic Mode: U+E290-U+E2BF (48)
Vowel diacritics, stress marks, special chars.

## Combining Character System

The script uses a combining/stacking model:
1. A base Secondary Character is placed
2. Combining top extension attaches above
3. Combining bottom extension attaches below
4. Combining degree diacritic attaches below
5. Combining type diacritic attaches above

For Primary Characters, the zones overlap on a diagonal bar:
1. Specification shape (base)
2. Combining context diacritic (superposed)
3. Combining perspective+extension (upper-left zone)
4. Combining affiliation+essence (upper-right zone)
5. Combining stem/func/version/plexity (lower-right zone)
6. Combining configuration (underposed)
7. Combining relation (subscript)

## Glyph Data Format

Each glyph is defined as SVG path data in a standard em-square
(1000 units, baseline at y=200, ascender at y=800).

Strokes are defined as filled paths (not stroked lines) for
font compatibility. Stroke width is ~60 units.

## File Structure

```
script/
  PLAN.md           - This file
  glyphs.py         - Glyph path definitions (SVG path data)
  render_test.py    - Generate test SVG sheets
  build_font.py     - Generate OTF font using fonttools
  combining.py      - Combining character logic
```
