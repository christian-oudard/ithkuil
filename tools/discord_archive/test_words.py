"""Tests for the corpus tokenizer.

The audit measures our parser against these tokens, so a tokenizer bug
shows up as an implementation bug. It has done: a stray re.IGNORECASE
let Unicode case folding match "i" against the dotless "ı", which put
90 pre-v4 spellings back into a corpus the date cutoff had already
excluded them from, where they read as parse failures.

Run with: python3 -m unittest discover tools/discord_archive
"""
import unittest

from words import tokens


class TestTokens(unittest.TestCase):
    def test_keeps_ithkuil_words(self):
        self.assertEqual(tokens("maţřëullait"), ["maţřëullait"])

    def test_folds_case(self):
        self.assertEqual(tokens("Maţřëullait"), ["maţřëullait"])

    def test_drops_letters_outside_the_v4_alphabet(self):
        # ı (U+0131) and İ (U+0130) belong to the pre-v4 drafts. Case
        # folding used to match them against i, splicing old spellings
        # into the corpus.
        for word in ["ažxwö'rka'súm".replace("i", "ı"), "ıţkuil", "İţkuil"]:
            for tok in tokens(word):
                self.assertNotIn("ı", tok, f"{word!r} leaked a dotless ı")
                self.assertNotIn("İ", tok, f"{word!r} leaked a dotted İ")

    def test_drops_plain_english(self):
        # No distinctively Ithkuil letter, so it is not a candidate.
        self.assertEqual(tokens("the quick brown fox"), [])

    def test_drops_urls_and_code_spans(self):
        self.assertEqual(tokens("see https://example.com/ţřšž for more"), [])
        self.assertEqual(tokens("`aţmwaroë` is code"), [])

    def test_strips_edge_punctuation(self):
        self.assertEqual(tokens("'aţmwaroë-"), ["aţmwaroë"])

    def test_keeps_internal_hyphen_for_concatenated_chains(self):
        # §3.1.7 chains are one word holding several formatives.
        self.assertEqual(
            tokens("heltyurëi-annarëi"), ["heltyurëi-annarëi"]
        )

    def test_requires_three_letters(self):
        self.assertEqual(tokens("ţa"), [])


if __name__ == "__main__":
    unittest.main()
