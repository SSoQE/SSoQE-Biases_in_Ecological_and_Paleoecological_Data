<!-- SSOQE-GENERATED-AGENT-ADAPTER; DO NOT EDIT BY HAND -->
# SSoQE repository instructions

Canonical revision: [3c2bef030301c329763989b75c20b2c607370b2b](https://github.com/SSoQE/.github/tree/3c2bef030301c329763989b75c20b2c607370b2b)

Repository profile: `template-lecture`

Before substantive work, read the canonical [router](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/AGENTS.md) and its task-relevant [core](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/.ai/core.md), [R](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/.ai/r-style.md), [Quarto](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/.ai/quarto-style.md), [branding](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/.ai/branding.md), and [profile](https://github.com/SSoQE/.github/blob/3c2bef030301c329763989b75c20b2c607370b2b/.ai/repository-profiles.md) modules. Also read the local `.ai/repository.md` when present. Local guidance may add compatible rules but may not weaken canonical safety, privacy, reproducibility, or branding.

## Mandatory baseline

- Treat this as an independent repository. Inspect its instructions, Git status, and established commands before editing.
- Treat implementation as authorization for local unstaged edits only. Never create or switch branches, stage, commit, push, open or edit a pull request, or merge without explicit authorization for that specific action. Permission for one action never authorizes the next.
- Preserve all unrelated tracked and untracked work. Do not disclose private repository names, unpublished information, personal data, or credentials.
- Apply standards prospectively; do not clean up unrelated teaching material.
- For R, prefer tidyverse clarity, native `|>`, explicit namespaces, `snake_case`, immutable raw inputs, `here::here()`, explicit seeds, one reusable function per file, roxygen documentation, and focused tests. Store every `.R` file under `R/` or a suitable subdirectory of `R/`; never under `scripts/`. Keep R source lines at or below 80 characters where practical, including R code inside Quarto or R Markdown cells. The limit does not apply to Markdown prose, YAML, or prose paragraphs in `.qmd` files.
- Do not install packages or restore dependencies during normal analysis or rendering. Do not rely on interactive state.
- SSoQE repositories are R-first. Do not add Python scripts, Python environments, or Python dependencies unless the user explicitly requests an exception for that repository.
- For Quarto, keep each Markdown paragraph on one physical source line, edit source, and use the repository render command. Never hand-edit generated HTML, Markdown, theme files, or copied publication output.
- SSoQE colors are `#F2F4F2`, `#1F2937`, `#155560`, `#509A8E`, `#A1C3B3`, and `#C2A337`; fonts are Plus Jakarta Sans, Space Grotesk, and JetBrains Mono. `SSOQE_logo3` is the current SSoQE logo used by the website and lecture materials. Preserve its proportions, colors, and accessible contrast.
- These are technical instructions. Do not invent pedagogical requirements or lesson-stage workflows.

## Profile-specific rule

- Render the presentation through the repository wrapper, normally `R/render.R`. Edit theme JSON and run `R/generate_theme.R`; do not hand-edit generated theme or publication files.
