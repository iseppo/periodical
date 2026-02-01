# CLAUDE.md - AI Assistant Guide for Periodical

This document provides comprehensive guidance for AI assistants working with the Periodical codebase.

## Project Overview

**Periodical** is an Estonian pension fund analytics platform that generates daily market reports and fund comparisons. It fetches data from multiple sources, creates visualizations (static and animated), renders Quarto reports, and uploads them to seppo.ai.

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
├── run_turuylevaade.R      # Orchestrator script (runs analysis + uploads)
├── fetch_iii_sammas_fondid.R  # III pillar fund list fetcher
├── turuylevaade.qmd        # II pillar market overview (Quarto)
├── tuleva.qmd              # Pension fund comparison page (Quarto)
├── III_sammas.qmd          # III pillar market overview (Quarto)
├── Pensionifondid.csv      # II pillar fund list with IDs
├── Pensionifondid_III.csv  # III pillar fund list with IDs
├── Dockerfile              # Container environment (R 4.5.2, Quarto 1.8.26)
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
| Statistikaamet | andmed.stat.ee/api/v1/et/stat/IA02 | Consumer Price Index (inflation) |
| Yahoo Finance | (via quantmod) | S&P 500, EUR/USD exchange rate |

### Fund IDs

Fund IDs are defined in `Pensionifondid.csv` and `Pensionifondid_III.csv`. Key funds:

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

# Full pipeline (generates + uploads)
Rscript run_turuylevaade.R
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
  4. Uploads to seppo.ai via SCP

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

This ensures partial outputs are still uploaded even if one component fails.

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

### "Puuduvad fondid" Warning

New funds may be added to Pensionikeskus. Check:
1. `fund_info` scraping in `turuylevaade.qmd`
2. Update `Pensionifondid.csv` if needed

### Animation Encoding Failures

Ensure ffmpeg is installed and accessible. The Docker image includes ffmpeg.

### Cache Invalidation

To force fresh data fetch:
```bash
rm cache_nav_data.rds cache_inflation_data.rds
```

### SSH Upload Failures

Check:
1. SSH key is properly configured (`UPLOAD_TO_ZONE` secret)
2. Server fingerprint is in known_hosts
3. Target directories exist on server

## File Outputs Summary

| File | Generated By | Destination |
|------|--------------|-------------|
| `aastane_tulu_tuleva_lhv.png` | NAV_kasv.R | kihlveod/ |
| `aastane_tulu_animeeritud.mp4` | NAV_kasv.R | kihlveod/ |
| `kihlvedu.png` | NAV_kasv.R | kihlveod/ |
| `turuylevaade.html` | turuylevaade.qmd | tuleva/ |
| `tuleva.html` | tuleva.qmd | kihlveod/ |
| `III_sammas.html` | III_sammas.qmd | tuleva/ |
| `koguturg_*.csv` | turuylevaade.qmd | tuleva/ |
| `iii_sammas_*.csv` | III_sammas.qmd | tuleva/ |

## Dependencies

### R Packages (Key)

- **Data**: httr, jsonlite, rvest, readr
- **Manipulation**: dplyr, tidyr, forcats, stringr, lubridate
- **Visualization**: ggplot2, hrbrthemes, ggfittext, plotly
- **Animation**: gganimate
- **Parallel**: future, furrr, parallel
- **SSH**: ssh (for uploads)

### System Dependencies

- R >= 4.3
- Quarto >= 1.4
- ffmpeg (for video encoding)
- SSH client (for uploads)

## Secrets Required (GitHub Actions)

- `UPLOAD_TO_ZONE` - SSH private key for server access
- `SERVER_HOST` - Target server hostname
- `DOCKERHUB_USERNAME` - Docker Hub login
- `DOCKERHUB_TOKEN2` - Docker Hub access token
