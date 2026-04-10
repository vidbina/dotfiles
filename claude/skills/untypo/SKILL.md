---
name: untypo
description: "Use this skill when the user wants to fix spelling, grammar, punctuation, or typos in a piece of text while preserving their personal voice and style. Trigger when the user pastes text and asks things like 'untypo', 'fix this', 'proofread', 'autocorrect', 'clean up grammar', or submits a draft (email, message, comment, commit message, social post) with visible errors and asks for a cleanup. Also trigger when the user invokes `/untypo`. Do NOT rewrite for clarity, shorten, restructure, change tone, translate, or 'improve' the writing — only fix objective mistakes. Do NOT trigger when the user explicitly asks for editing, rewriting, summarizing, translating, copyediting for style, or stylistic feedback — those are different tasks."
---

# untypo

Fix spelling, grammar, and punctuation errors while preserving the author's voice, tone, register, phrasing choices, and structure. This is an autocorrect-grade cleanup, not a rewrite.

## Scope

**In scope — fix these:**
- Typos and misspellings
- Grammar errors (subject-verb agreement, wrong tense, wrong pronoun, dangling auxiliaries)
- Clear punctuation errors (missing apostrophe, missing terminal period, stray comma, mismatched quotes)
- Capitalization errors (proper nouns, start of sentence, the pronoun "I")
- Wrong homophone where meaning is unambiguous (their/there/they're, its/it's, your/you're, to/too)
- Obvious word-swaps from autocorrect gone wrong ("duck" when "dock" was clearly meant)
- Doubled words ("the the")

**Out of scope — do NOT change:**
- Phrasing, even if awkward
- Sentence structure, length, or ordering
- Informal contractions and chatspeak ("gonna", "wanna", "idk", "ofc")
- Lowercase sentence starts if that's clearly the author's consistent style
- Technical jargon, slang, brand names, project names, usernames
- British vs. American spelling — match whatever the author uses
- Oxford comma preference — match whatever the author uses
- Tone, formality, emotional register, hedging, or exclamation use
- Profanity
- Emoji, emoticons, or punctuation used for emphasis ("!!!!", "....")

If a "correction" is a judgment call, leave it alone. When in doubt, do not change.

## Output format

Respond with **exactly two sections**, in this order, and nothing else. No preamble, no postamble.

### Section 1 — Corrected text

A single fenced code block containing ONLY the corrected text. No language tag. No commentary inside the block. No extra blank lines at the start or end. This block must be directly copy-pasteable as a drop-in replacement for the original.

### Section 2 — Reprint with corrections highlighted

Reprint the full text with corrections marked inline using this convention:

- The **original wrong bit** goes in `~~strikethrough~~`
- The **correction** immediately follows in `**bold**`
- A bracketed footnote marker `[1]`, `[2]`, … goes right after the correction, numbered in order of appearance

After the reprinted text, add a numbered list of footnotes. Each footnote is ONE short sentence explaining what was wrong and why the fix is right. No multi-sentence lectures.

If there are **zero corrections**, omit Section 2 entirely and instead print a single line after Section 1:

```
No corrections needed.
```

## Example

Input:

```
i wen't to the store yesterday and bought there new bread, its really good
```

Your response:

```
I went to the store yesterday and bought their new bread. It's really good.
```

**Reprint with corrections:**

~~i~~ **I** [1] ~~wen't~~ **went** [2] to the store yesterday and bought ~~there~~ **their** [3] new bread~~,~~**.** [4] ~~its~~ **It's** [5] really good.

1. The first-person pronoun is always capitalized.
2. Past tense of "go" is "went" — no apostrophe.
3. "Their" is the possessive; "there" refers to a place.
4. Two independent clauses need a period (or semicolon), not a comma — and the second clause's first word then capitalizes.
5. "It's" is the contraction of "it is"; "its" is the possessive.

## Reminders

- Never add or remove content. Length in ≈ length out, modulo fixes.
- Never explain beyond the footnotes. No introduction, no summary, no "let me know if…".
- Both sections, in order. No extra headings, no meta-commentary.
- If the input is already clean, say so with `No corrections needed.` and stop.
- Preserve line breaks, paragraph breaks, lists, code blocks, and any markdown structure in the original.
- Never correct inside fenced code blocks or inline code spans — treat code as literal.
