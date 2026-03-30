# V3 Script TODO

## Hook fine-tuning

The hook arc angles were increased from 30° to 90° to better match reference art,
but exact values still need verification against the reference (bottom_ending_chart.png).

### Verify hook directions

For each hook type, confirm the hook curves the correct direction by comparing
the render to the reference cells (`/tmp/ref_cell_r3c0.png` etc).

Consonants with `bottom_flipped=True` have their bottom paper mirrored with
`mirror_x()` after drawing, which reverses left/right. The arc sign for these
must be chosen accounting for the flip:
- Q (FoldHookRight): bottom_flipped=True
- X (TowardRightHookLeft): bottom_flipped=True
- Xh (TowardRightHookRight): bottom_flipped=True

Suspected wrong directions (need verification):
- FoldHookLeft (T, bottom_flipped=False): reference curves LEFT, render may curve RIGHT
- Any flipped consonant where arc sign was set without accounting for the mirror

### Fine-tune arc size

Reference hooks look like ~70-80° visually. Current value of 90° may be
slightly too large for some types. Once directions are confirmed correct,
sweep arc values to find the best match per hook type.

### Update test_v3.py

Once final parameters are set, add a visual regression test or at minimum
update comments in test_v3.py to document expected hook shapes.
