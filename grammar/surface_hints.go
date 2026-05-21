package grammar

// SurfaceHints carries orthographic choices that the surface form
// embodied but don't affect the formative's grammatical content. The
// renderer uses these to reproduce parser input verbatim — without
// them, render emits the canonical short form, which may differ from
// what the speaker actually wrote.
//
// nil means "use canonical defaults". Programmatically-built
// formatives leave it nil so render picks the shortest valid surface.
// The parser populates it from the surface state of the input.
//
// Each flag is independent and only meaningful when the formative's
// grammar permits the corresponding form:
//
//   - CcShortcut: emit the §3.2 Cc-Vv shortcut form (w-/y-/hl-/hm-/
//     hr-/hn-) when the formative is shortcut-encodable. Requires
//     CrRoot with default SlotIV, no Slot V, and a SlotVI representable
//     by the shortcut table.
//   - CnCaShortcut: emit the §3.8.1.2 Cn-in-Ca form when SlotVI is
//     default -l-, SlotVIII is VnCnValence{MNO, non-FAC Pattern-1
//     Mood/Case-Scope}, and len(SlotV) == 0.
//   - MovedGlottal: emit the §3.9.1 SPECIAL NOTE moved-glottal form for
//     cases 37-52, requiring Vr non-elided and no Cn→Ca shortcut.
//   - KeepVv: suppress the default Vv "a" elision (§3.2) so the leading
//     "a" stays visible.
//   - KeepVc: suppress the trailing-THM Vc "a" elision so the case
//     vowel stays visible.
//
// Flags that ask for a form the formative can't carry are silently
// ignored, just like a stale Options{Shortcut: true} on a non-Cr
// formative.
type SurfaceHints struct {
	CcShortcut   bool
	CnCaShortcut bool
	MovedGlottal bool
	KeepVv       bool
	KeepVc       bool
}
