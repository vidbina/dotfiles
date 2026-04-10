---
name: qsearch
description: "Use this skill when the user asks you to quickly research, investigate, look into, find out about, compare, summarize, or get up-to-speed on a topic that benefits from fresh information from the web. Trigger for prompts like 'research X', 'qsearch Y', 'what are the best Y for Z', 'compare A vs B', 'find me sources on...', 'is it true that...', 'what's the current state of...', 'dig into...', or any question where a good answer requires fetching and synthesizing web content. Also trigger when the user invokes `/qsearch`. This skill batches any necessary clarifying questions into ONE up-front round before executing, so the user can answer them, walk away, and return to a finished artifact instead of being pinged for follow-ups throughout. Do NOT trigger for narrow factual questions answerable from training data, for code lookups better served by Grep/Glob, when the user provides specific URLs to fetch (just fetch them), or when the user explicitly says 'no web search' / 'from what you know'."
allowed-tools: WebSearch WebFetch AskUserQuestion
---

# qsearch

*(short for "quicksearch" — rhymes with "research", which is exactly what it does)*

Perform a focused piece of web research and return structured findings. The defining design principle of this skill is:

> **Batch every clarifying question into one up-front round, then run to completion uninterrupted.** Asking questions is fine — asking questions spread across a long timeline is not. Ask only what's materially important, treat everything else as a stated assumption, and let the user answer once and walk away.

The user should be able to kick this off, answer a small batch of up-front questions, walk away and live life, and return to a finished artifact — not be tethered to the chat fielding "wait, did you mean X or Y?" pings throughout the run.

## Tools

This skill declares `WebSearch`, `WebFetch`, and `AskUserQuestion` in `allowed-tools` so it can run without triggering per-use permission prompts — critical for the walk-away UX.

- `WebSearch` and `WebFetch` are the primary research engines (Phase 4).
- `AskUserQuestion` is the one-shot batch-clarification tool (Phase 2).

If `WebSearch` and `WebFetch` are unavailable in the current session (e.g. policy restriction, offline environment), stop at the end of Phase 3 and return the research plan without executing it — do not attempt to answer from memory alone, and do not silently degrade to training-data facts as if they were fresh sources.

## Phase 1 — Interpret

Restate the request in one or two sentences as you understand it. Be explicit about what deliverable you think is being asked for: a short answer, a prose summary, a comparison table, a ranked list, a recommendation, a literature overview, a fact-check, etc.

## Phase 2 — Clarify once, up front

Walk the rubric below and identify every interpretive choice the prompt left open:

- **Scope** — geography, industry, audience, language, platform
- **Time window / recency** — last week, last year, last decade, all-time
- **Depth** — quick scan (3–5 sources) vs. deep dive (15+ sources)
- **Deliverable shape** — bullets, prose, table, pros/cons, recommendation, sourced fact list
- **Perspective** — neutral overview vs. opinionated pick
- **Inclusions the prompt didn't spell out** but a reasonable reader might expect
- **Exclusions** the prompt might implicitly want
- **Definitions** — any term in the prompt with multiple meanings
- **Success criteria** — what "good enough" looks like for this answer

For each choice, decide **ask** or **assume**:

- **Ask** only when *both* conditions hold: (a) the answer would materially change the research — different sources, different deliverable, different scope — and (b) a reasonable default has a real chance of being wrong. When in doubt, assume.
- **Assume** everything else. Anything a reasonable reader would guess correctly, anything low-stakes, anything easy for the user to course-correct from the final output.

### Batch the questions

If you have one or more questions, issue **all of them in a single `AskUserQuestion` tool call** — one round, then no more. Prefer **0–3 questions**; **4 is the hard cap**. If you're tempted to ask a 5th, you're asking things that should be assumptions instead.

For each question:

- Offer concrete options — never a free-text-only prompt. Options are faster for a user who wants to answer and walk away.
- Put the most likely answer first.
- Keep the question text short and specific.
- Only ask things whose answer you'll actually use to shape the research.

If you have zero questions, skip the tool call entirely and write:

> **No clarification needed — proceeding directly.**

### Commit to the interpretation

After the user answers (or immediately if you asked nothing), write out the **locked decisions** in one combined list — answered questions and assumed defaults side by side, flagged so the user can see which is which. Format:

> - **D1 (scope):** US market — *you chose*
> - **D2 (recency):** Last 18 months — *assumed, easy to broaden*
> - **D3 (shape):** Ranked shortlist of 5 with one-paragraph rationales — *assumed*
> - **D4 (depth):** ~8 sources, cross-checked on load-bearing claims — *assumed*

Aim for **4–8 total decisions** (asked + assumed combined). If you have fewer than 3, you probably missed some interpretive choices — go back through the rubric.

### The hard rule

Once Phase 2 closes, **no more questions for the rest of the run.** The research executes to completion without further interruption — that's the entire UX contract this skill exists to uphold. If something surfaces mid-research that would have been worth asking, note it in "Caveats & open questions" at the end and keep going.

## Phase 3 — Research plan

Before running any searches, outline what you're going to do:

- **Queries** — list 3–6 specific search queries you intend to run, in priority order
- **Source tiers** — which kinds of sources you'll prioritize (primary docs, official announcements, peer-reviewed, reputable reporting, practitioner blogs, forums) and which you'll deprioritize (SEO farms, affiliate roundups, undated content mills, AI-generated listicles)
- **Stopping criteria** — what tells you the research is "done enough" given the depth assumed in Phase 2 (e.g. "stop after 6 sources if findings converge", "stop after cross-checking the top 3 claims")

Keep this section short — half a dozen bullets, not an essay.

## Phase 4 — Execute

Run the searches. Fetch full pages with `WebFetch` when search snippets are insufficient. As you work, keep a running list of sources: URL, title, publication date if known, and a one-line note of what you took from it.

Execution guidelines:

- **Prefer primary sources.** Official docs, press releases, SEC filings, GitHub repos, academic papers, original reporting.
- **Cross-check load-bearing claims.** Any number, date, attribution, or headline-level claim should appear in at least two independent sources before you state it as fact.
- **Note disagreements.** If sources conflict, don't silently pick one — surface the conflict in the Caveats section.
- **Flag suspicious content.** SEO-farm roundups, undated claims, unsourced stats, AI-generated "review" sites. Either verify elsewhere or drop.
- **Don't retry dead ends.** If a search or fetch fails, try an alternative source rather than retrying the same URL or near-identical query.
- **Respect the stopping criteria.** Over-research is a failure mode too.

## Phase 5 — Deliver

Structure the final answer in this exact order:

### Summary

A 2–4 sentence direct answer to the original question, shaped per the assumed deliverable. This is what a reader sees first and should be able to act on.

### Key findings

The substance. Use the format committed to in Phase 2 (table, ranked list, pros/cons, prose sections, etc.). Cite inline with `[1]`, `[2]`, etc. Keep it scannable — the user is returning to a finished artifact, not settling in for a novel.

### Caveats and open questions

Anything you couldn't resolve, conflicts between sources, areas where a different assumption would have materially changed the conclusion, and what a deeper pass would look like. Be honest about confidence levels.

### Sources

A numbered list, referenced inline in Key Findings. For each source:

```
[N] Title — Publisher (YYYY-MM-DD if known) — URL
    One-line note on what this source contributed.
```

### Decisions recap

Restate the locked decisions from Phase 2 in a compact form so the user can course-correct in a single reply without re-reading the whole output. Cover both what they answered and what you assumed, and give a concrete nudge for how to change each one. Format:

> - **D1 (scope):** US market *(you chose)* → say "global" to redo
> - **D2 (recency):** last 18 months *(assumed)* → say "historical" to broaden
> - **D3 (shape):** ranked shortlist of 5 *(assumed)* → say "table" / "prose" / etc. to reshape
> - **D4 (depth):** ~8 sources *(assumed)* → say "deep dive" for more

## Anti-patterns

- **Don't trickle questions across multiple turns.** All asking happens in one `AskUserQuestion` batch in Phase 2, then never again. Multi-round clarification defeats the walk-away UX.
- **Don't ask free-text questions when concrete options exist.** A user who wants to answer and walk away will click options much faster than they'll type prose.
- **Don't exceed 4 up-front questions.** If you want a 5th, demote one of the others to an assumption.
- **Don't ask questions mid-research.** If something material surfaces after Phase 2, note it in Caveats and keep going. The run must complete without further interruption.
- **Don't hedge every sentence.** Good research states findings with citations and distinguishes confidence in the Caveats.
- **Don't dump raw search results or fetched pages.** Synthesize.
- **Don't pad.** If the answer is short, the answer is short. Depth should match assumed depth.
- **Don't skip Phase 2** on the grounds that "the request seemed clear." It rarely is — walking the rubric is the whole reason this skill exists.
- **Don't cite a source you didn't actually read.** If a search snippet is all you have, either fetch the page or don't cite it as a source.
- **Don't mix phases.** Phase 1 is interpretation, Phase 2 is clarification + commitment, Phase 3 is the plan. Don't start planning in Phase 1 or researching in Phase 3.
