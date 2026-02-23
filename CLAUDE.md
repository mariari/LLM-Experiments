# ObjVLisp LLM Experiments

This repository studies how prompting strategies affect LLM-generated code
architecture, using ObjVLisp-style reflective object systems in SWI-Prolog
as the test case.

## Project structure

- `objvlisp-experiment/` — four Claude attempts + analysis documents
- `.claude/skills/` — Anoma shared LLM conventions
- `docs/` — LLM configuration docs

## Skills

- @.claude/skills/general-conventions/SKILL.md
- @.claude/skills/git-conventions/SKILL.md
- @.claude/skills/code-review/SKILL.md

## Language

SWI-Prolog. Use `swipl` to run files. Tests use `plunit`.

## Conventions

- Prefer static facts over `assertz` at bootstrap time
- All dispatch must go through a single `send` predicate — no special-casing
- Methods are entries in a method table (`has/3` + `oapply/2`), not hardcoded clauses
- Test determinism explicitly: `[true]` not `[nondet]` when a predicate should have exactly one solution
