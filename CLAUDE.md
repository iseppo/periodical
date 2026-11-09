# CLAUDE.md - AI Assistant Guide for Periodical

This document provides comprehensive guidance for AI assistants working with the Periodical codebase.

## Project Overview

**Periodical** is an Estonian pension fund analytics platform that generates daily market reports and fund comparisons. It fetches data from multiple sources, creates visualizations (static and animated), renders Quarto reports, and publishes them to seppo.ai/kihlveod/ and seppo.ai/tuleva/ via a Cloudflare Worker (`cloudflare/`).

**Primary Language**: Estonian (code comments, variable names, and reports are in Estonian)

### Key Outputs

- **turuylevaade.html** - II pillar (second pillar) market overview report
- **tuleva.html** - Pension fund betting/comparison page (Tuleva vs LHV vs SEB)
- **III_sammas.html** - III pillar (third pillar) pension fund overview
- **aastane_tulu_animeeritud.mp4** - Animated video showing historical fund performance
- **Various CSV exports** - Market data summaries

## Repository Structure

```
periodical/
├── NAV_kasv.R              # Main data processing and visualization script
├── run_turuylevaade.R      # Orchestrator script (runs analysis; legacy SCP upload, skipped in CI)
├── turuylevaade.qmd        # II pillar market overview (Quarto)
├── tuleva.qmd              # Pension fund comparison page (Quarto)
├── III_sammas.qmd          # III pillar market overview (Quarto)
├── Pensionifondid.csv      # II pillar fund list with IDs
├── Dockerfile              # Container environment (R 4.6.0 / Ubuntu noble, Quarto 1.9.37)
├── renv.lock               # R package dependencies (renv lockfile)
├── .github/workflows/
│   ├── main.yml            # Daily scheduled run (11:05 UTC)
│   └── build_image.yml     # Docker image build workflow
├── data/                   # Local data cache directory
├── analytics.html          # Google Analytics snippet
├── seo.html                # SEO meta tags for tuleva.qmd
└── footer.html             # Footer HTML for reports
```

## Key Scripts

### NAV_kasv.R

The main data processing script with the following sections:

1. **Package Loading** (lines 14-37) - Loads tidyverse, ggplot2, gganimate, furrr, etc.
2. **Cache Functions** (lines 68-79) - `is_cache_valid()` for API response caching
3. **Data Fetching** (lines 88-225):
   - `get_nav_data()` - Fetches NAV data from Pensionikeskus
   - `get_inflation_data()` - Fetches inflation from Statistikaamet
4. **Return Calculations** (lines 231-376) - `compute_returns()`, `arvuta_aastane_tootlus_hetkes()`
5. **Static Charts** (lines 382-496) - `plot_static_chart()`, `generate_static_charts()`
6. **Animations** (lines 502-813) - `create_specific_animation()`, `generate_all_animations()`
7. **Main Function** (lines 831-864) - `main()` orchestrates everything

### Quarto Reports

All `.qmd` files share common structure:
- YAML frontmatter with `lang: et_EE`, `theme: slate` (or `lumen`), `self-contained: true`
- R code chunks with `echo: false, warning: false, message: false`
- Data fetching via `furrr::future_map_dfr()` for parallel processing

## Data Sources

| Source | URL | Data |
|--------|-----|------|
| Pensionikeskus | pensionikeskus.ee | Fund NAV, investor counts, fund sizes |
| Statistikaamet | andmed.stat.ee/api/v1/et/stat/IA002 | Consumer Price Index (inflation) |
| Yahoo Finance | (via quantmod) | S&P 500, EUR/USD exchange rate |

### Fund IDs

Fund IDs are defined in `Pensionifondid.csv` (III pillar: fetched live in `III_sammas.qmd`, fallback `data/Pensionifondid_III_seed.csv`). Key funds:

- **77** - Tuleva Maailma Aktsiate Pensionifond (index fund)
- **73** - LHV Pensionifond Indeks
- **38** - LHV Pensionifond XL/Julge
- **47** - LHV Pensionifond L/Ettevõtlik
- **EPI** - II pillar general index

## Development Workflow

### Local Development

```bash
# Install R dependencies
R -e "renv::restore()"

# Run the main script
Rscript NAV_kasv.R

# Render individual Quarto reports
quarto render turuylevaade.qmd
quarto render tuleva.qmd
quarto render III_sammas.qmd

# Full pipeline (generates reports; SKIP_UPLOAD=1 skips the legacy Zone SCP step)
SKIP_UPLOAD=1 Rscript run_turuylevaade.R

# Publishing: ONLY via GitHub Actions. Local reports are stale; stage.mjs and
# `wrangler deploy` refuse to run outside CI (cloudflare/ci-only.mjs).
gh workflow run main.yml
```

### Docker Development

```bash
# Build the image
docker build -t periodical .

# Run the analysis
docker run --rm -v "$PWD:/home/rstudio/project" periodical Rscript NAV_kasv.R
```

### CI/CD

- **main.yml** - Runs daily at 11:05 UTC
  1. Disables renv (uses pre-built Docker image)
  2. Runs `NAV_kasv.R`
  3. Renders all Quarto reports
  4. Runs the analysis in Docker with `SKIP_UPLOAD=1` (no SCP)
  5. "Deploy reports to Cloudflare": `node cloudflare/stage.mjs` copies reports into
     `cloudflare/public/{kihlveod,tuleva}/`, uploads video (`cloudflare/media/`) to R2 bucket
     `seppo-ai-files`, then `wrangler deploy` publishes Worker `seppo-ai-kihlveod`
     (routes `seppo.ai/kihlveod/*` and `seppo.ai/tuleva/*`)

- **build_image.yml** - Triggered on Dockerfile/renv.lock changes
  - Builds and pushes to `ghcr.io/iseppo/periodical:main`

## Conventions for AI Assistants

### Language

- **Code comments**: Write in Estonian
- **Variable names**: Use Estonian (e.g., `fondid`, `Kuupäev`, `maht`)
- **Commit messages**: Can be in English or Estonian
- **Function documentation**: Use roxygen2-style comments in Estonian

### Common Estonian Terms

| Estonian | English |
|----------|---------|
| fond/fondid | fund/funds |
| Kuupäev | date |
| maht | volume/size |
| tootlus | return/yield |
| inflatsioon | inflation |
| sammas | pillar (pension pillar) |
| kasv | growth |
| aasta/aastane | year/annual |
| indeks | index |

### Code Style

- Use tidyverse conventions (pipes `%>%`, dplyr verbs)
- Suppress package startup messages: `suppressPackageStartupMessages()`
- Use `message()` for logging, not `print()` or `cat()`
- Handle errors with `tryCatch()` with informative messages
- Use parallel processing via `furrr::future_map_dfr()` with dynamic worker allocation

### Parallel Processing Pattern

```r
available_cores <- parallel::detectCores()
worker_count <- max(2, available_cores - 1)
plan(multisession, workers = worker_count)

# Use furrr for parallel operations
results <- future_map_dfr(items, function(item) {
  # processing
})

plan(sequential)  # Reset when done
```

### Caching Pattern

```r
is_cache_valid <- function(cache_file, max_age_hours = 1) {
  if (!file.exists(cache_file)) return(FALSE)
  file_age_hours <- as.numeric(difftime(Sys.time(),
    file.info(cache_file)$mtime, units = "hours"))
  return(file_age_hours < max_age_hours)
}
```

Cache files are stored as `.rds` and listed in `.gitignore`.

### Error Handling

The pipeline uses "fail-forward" strategy - individual failures don't stop the entire pipeline:

```bash
Rscript NAV_kasv.R || echo 'NAV_kasv.R failed, continuing...'
```

This ensures partial outputs are still published even if one component fails.

## Testing and Validation

### Manual Testing

1. Run the script and verify outputs:
   ```bash
   Rscript NAV_kasv.R
   ```
   Expected: Creates `aastane_tulu_tuleva_lhv.png`, `aastane_tulu_animeeritud.mp4`

2. Check console for worker count: `"Kasutan X tuuma paralleliseerimiseks"`

3. Verify video quality: `aastane_tulu_animeeritud.mp4` should be smooth and readable

### Output Verification

- Static charts should have proper fund colors (FUND_COLORS constant)
- Dates should be in Estonian locale format
- All percentage values should use comma as decimal separator for display

## Performance Considerations

See `PERFORMANCE_OPTIMIZATIONS.md` for detailed optimization history.

### Key Optimizations

1. **Dynamic worker allocation**: Uses `N-1` cores instead of hardcoded 2
2. **API response caching**: 1-hour cache validity for NAV/inflation data
3. **Vectorized calculations**: Pre-computed monthly aggregations
4. **Video encoding**: 15 FPS, 150 DPI, fast preset (reduced from 20 FPS, 300 DPI)

### Runtime Expectations

- First run (no cache): 4-6 minutes
- Subsequent runs (with cache): 3-5 minutes

## Common Issues and Solutions

### New, closed and merged II pillar funds (turuylevaade.qmd)

Handled automatically, no code change needed:
- **Fund list** comes from the fund selector on Pensionikeskus' volume page
  (`Pensionifondid.csv` is only a fallback). Manager, index/active type and risk group of an
  unknown fund are derived from its name (`lisa_meta`, `riskirühm`); add a row to `fondi_meta`
  / `riskirühm` only to override.
- **Closed/merged funds** disappear from the per-fund export with their whole history. The
  official market total (`f[0]=-1`) still includes them: the residual is treated as closed-fund
  volume, and mergers are detected automatically (residual drop or archived fund ending + a
  matching abnormal inflow in receiver funds). Merger transfers go to `yhinemine`, not net flow.
- **Archive:** when a fund vanishes between runs, its history (from the previous run's
  `cache_ii_fondid.rds`, kept by actions/cache) is written to `data/ii_suletud_fondid.csv`,
  which the workflow commits. Detected mergers are logged in the render output (`Ühinemine …`).

### III pillar (III_sammas.qmd)

Same design and approach as the II pillar overview (OJS/Plot, `turuylevaade-style.css`, `ojs-lib/`):
- **Full history every run** (volume, NAV, official market total, pension accounts from 2004/2018). The old
  "download our own published CSV and append" approach is gone.
- **Fund list and metadata** come from Pensionikeskus: fund IDs from the volume page selector, index/active and
  risk level (`data-is_index_fund`, `data-srri`) plus fee and unit-holder count from the statistics table.
  `data/Pensionifondid_III_seed.csv` is only a fallback. Short display names: `lyhinimi` (new funds get an
  automatic name).
- **Closed funds** = official total − fund sum ("Suletud fondid"). One-day glitches in the official total are
  dropped; mergers are detected from a residual drop matched to abnormal inflows within 0–2 market days.
- **Unit-holder history per fund** is only published as the latest value, so it accumulates:
  `data/iii_investorid_ajalugu.csv` (committed start) ∪ CI cache ∪ the published CSV on seppo.ai.
- Render stops (CI then publishes last-good) if the export is older than 10 days or the official total exceeds
  the fund sum by > max(5 mln €, 1 %) (a fund missing from the export).

### Animation Encoding Failures

Ensure ffmpeg is installed and accessible. The Docker image includes ffmpeg.

### Cache Invalidation

To force fresh data fetch:
```bash
rm cache_nav_data.rds cache_inflation_data.rds
```

### Publishing (Cloudflare)

seppo.ai moved from Zone.ee to Cloudflare Workers on 2026-11-08. Never SCP/SSH to the old Zone
server. The SCP code in `run_turuylevaade.R` is legacy and CI always skips it (`SKIP_UPLOAD=1`).

- **To add a published file:** add it to a group in `GROUPS` in `cloudflare/stage.mjs`. Video
  (mp4/webm/mov) goes to R2 automatically. If any file of a group is missing (failed render, or the
  headless-Chrome check in `main.yml` rejected `turuylevaade.html`), the whole group is published
  from the last successful deploy (`cloudflare/last-good/`, actions/cache).
- **OJS libraries** (d3, Plot, Inputs, htl) are vendored in `ojs-lib/` (same versions as the
  Quarto OJS runtime) so the charts do not depend on jsdelivr.
- **Search indexing:** `kihlveod/` is indexable (`kihlveod/tuleva.html` is in seppo.ai's sitemap).
  `tuleva/` is `noindex`, set via `_headers` written by `stage.mjs` and in `cloudflare/worker.js`.
- **`seppo.ai/tuleva/`** shows the market overview: `stage.mjs` copies `turuylevaade.html` to
  `tuleva/index.html`. There is no directory listing any more (`listing.js` was removed).
- **Browser check** in `main.yml` requires at least 13 rendered Plot charts in `turuylevaade.html` and 11 in
  `III_sammas.html`; raise the threshold when adding charts. Below it, that report group is published from last-good.
  Both widths must be checked successfully (Chrome failure = fallback). Charts with missing data render an
  empty Plot with a message (`tyhiGraafik`), so a data outage does not drop the chart count.
- **No partial deploys:** if a report group is missing and has no last-good copy, `stage.mjs` exits 1 and
  nothing is deployed (`wrangler deploy` replaces the whole site, so a partial set would delete live files).
- **Closed-fund archive:** the committed `data/ii_suletud_fondid.csv` wins over the cached copy per fund
  (step "Merge closed-fund archive"); the archive is committed from a separate worktree so the published
  files stay on the rendered commit.
- **Render stops on bad data** (CI then publishes last-good): export older than 10 days, more than 3 funds
  missing at once, or a fund missing from the export while the official total still contains it.
- **Config:** `cloudflare/wrangler.jsonc`. Assets use `html_handling: "none"` so `.html` URLs stay
  canonical. The Worker only runs for `dir/` index pages and R2 video.
- **Deploy failures:** check the `CLOUDFLARE_API_TOKEN` secret, which maps to Cloudflare account token
  `github_periodical` (Workers Scripts/Routes + R2 write). If it is missing, the step skips with a warning.
- **Main site** (everything else on seppo.ai) lives in the separate repo `iseppo/seppoai`
  (`./cloudflare/deploy.sh`).

## File Outputs Summary

| File | Generated By | Destination |
|------|--------------|-------------|
| `aastane_tulu_tuleva_lhv.png` | NAV_kasv.R | kihlveod/ |
| `aastane_tulu_animeeritud.mp4` | NAV_kasv.R | kihlveod/ |
| `kihlvedu.png` | NAV_kasv.R | kihlveod/ |
| `turuylevaade.html` | turuylevaade.qmd | tuleva/ (also copied to `tuleva/index.html` by `stage.mjs`) |
| `tuleva.html` | tuleva.qmd | kihlveod/ |
| `III_sammas.html` | III_sammas.qmd | tuleva/ |
| `koguturg_*.csv` | turuylevaade.qmd | tuleva/ |
| `iii_sammas_*.csv` | III_sammas.qmd | tuleva/ |

The three Quarto-rendered HTML reports are also mirrored to GitHub Pages by
`main.yml`: `tuleva.html` is served as the site root (`index.html`); the
other two keep their filenames. Site: <https://iseppo.github.io/periodical/>

## Dependencies

### R Packages (Key)

- **Data**: httr, jsonlite, rvest, readr
- **Manipulation**: dplyr, tidyr, forcats, stringr, lubridate
- **Visualization**: ggplot2, hrbrthemes, ggfittext, plotly
- **Animation**: gganimate
- **Parallel**: future, furrr, parallel
- **SSH**: ssh (legacy Zone upload only, unused in CI)

### System Dependencies

- R 4.6 (renv.lock is pinned to 4.6.0)
- Quarto >= 1.4
- ffmpeg (for video encoding)
- Node.js + wrangler 4 (Cloudflare deploy; CI uses `npx wrangler@4`)

## Secrets Required (GitHub Actions)

- `CLOUDFLARE_API_TOKEN` - Cloudflare API token (`github_periodical`) for `wrangler deploy` and R2 uploads
- `R2_ACCOUNT_ID` - Cloudflare account ID (used as `CLOUDFLARE_ACCOUNT_ID`)
- `DOCKERHUB_USERNAME` - Docker Hub login
- `DOCKERHUB_TOKEN2` - Docker Hub access token
