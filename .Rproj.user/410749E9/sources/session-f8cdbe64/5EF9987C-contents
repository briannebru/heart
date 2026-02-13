# AGENTS.md

Local instructions for Codex agents working in this repository.

## Project Overview
- This is an R Shiny app for a Heart Attack Dashboard.
- Main app entry: `app.R`
- Data: `data/heart.rds`
- Helper modules: `R/helpers.R`, `R/mod_download_plot.R`
- Static assets: `www/`

## Workflow
- Prefer reading `app.R` first to understand UI/server wiring.
- Use `rg` for searching (e.g., `rg "download"`).
- Keep changes small and focused; avoid unrelated refactors.
- Avoid modifying data files unless explicitly requested.

## R/Shiny Conventions
- Use `reactive()` for shared plot/data logic when reused.
- When adding downloads, ensure the plot object exists and is reactive.
- Validate reactive inputs with `req()` before plotting.

## Output & Testing
- If you run the app, use `R -e "shiny::runApp()"`.
- If you cannot run tests or the app, state that clearly.

## Formatting
- Keep code ASCII unless the file already contains Unicode.
- Follow existing formatting style in `app.R`.
