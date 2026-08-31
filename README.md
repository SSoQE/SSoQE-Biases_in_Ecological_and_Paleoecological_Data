<div align="center">

<img src="https://ssoqe.github.io/SSoQE_website/photos/SSOQE_logo3.png" width="150" alt="SSoQE logo">

# SSoQE Lecture Template

**Template for R and Quarto teaching repositories**

[SSoQE website](https://ssoqe.github.io/SSoQE_website/) · [GitHub organization](https://github.com/SSoQE)


| **📅 Course information** | **🧰 Technical** | **📌 Status** |
|:---:|:---:|:---:|
| ![SSoQE 2026](https://img.shields.io/badge/SSoQE-2026-155560) | ![Type](https://img.shields.io/badge/Type-Template-155560) | ![Status](https://img.shields.io/badge/Status-Template-155560) |
| ![Day](https://img.shields.io/badge/Day-Not_scheduled-C2A337) | ![Topic](https://img.shields.io/badge/Topic-Lecture_Infrastructure-155560) | ![Tools](https://img.shields.io/badge/Tools-R_%7C_Quarto-276DC3) |

</div>

## 🧩 Purpose

This repository provides the current SSoQE structure for Quarto RevealJS presentations, HTML exercises, reproducible R environments, shared visual styling, and GitHub Pages publication output. It is infrastructure for lecturers, not a scheduled lesson.

Use the template as a starting point only when creating or deliberately migrating a template-derived lecture. Older repositories may have valid PowerPoint, R Markdown, Quarto, or other author-owned workflows and must not be migrated merely for uniformity.

## 📚 Main sources

- `Presentation/presentation.qmd` is the slide source.
- `R/Exercises/01_example.qmd` is the example exercise source.
- `Presentation/colors.json`, `Presentation/fonts.json`, and `Presentation/custom_theme.json` are the editable presentation-theme sources.
- `R/generate_theme.R` generates the tracked presentation, exercise, and R theme files.
- `R/render.R` renders the presentation and updates the publication copy.
- `R/___Init_project___.R` is the explicit environment-setup entry point.

Generated HTML, Markdown, SCSS, font includes, and plotting themes must not be hand-edited.

## 🧱 Creating a lesson

1. Create a repository from this template or download a release.
2. Rename the R project and replace template-specific titles, links, authorship, assets, and exercises.
3. Keep reusable R functions under `R/` and analytical documents focused on orchestration.
4. Record repository-specific commands, content ownership, or justified branding deviations in `.ai/repository.md`.
5. Verify the complete render and publication-copy contract before distributing the lesson.

## 🛠️ Setup and rendering

Complete the explicit setup workflow before rendering. Dependency installation or restoration does not belong inside an ordinary render.

Render the presentation from a clean R session with:

```powershell
Rscript R/render.R
```

Render exercises through their Quarto project under `R/Exercises/`. Review warnings, links, resources, accessibility, responsive layout, code overflow, and RevealJS fragment states.

## 🎨 Branding

Use the SSoQE logo without stretching, cropping, recolouring, or substitution. Shared colors and fonts come from the JSON sources and are regenerated through `R/generate_theme.R`; repository-specific visual exceptions must be documented.

## 📄 Licence

See [LICENSE](LICENSE).
