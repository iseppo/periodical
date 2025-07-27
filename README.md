# Periodical

This project generates regular market overviews and reports using R and Quarto.

## Prerequisites

- **R** &ge; 4.3 (the lock file uses R 4.3.3)
- **Quarto CLI** &ge; 1.4 for rendering `.qmd` files
- Optional: **Docker** if you prefer running the analysis inside a container

## Manual execution

Run the main script from the repository root:

```sh
Rscript run_turuylevaade.R
```

The script renders `turuylevaade.qmd` and `tuleva.qmd` with Quarto and uploads the
results to the configured server via SSH.

## GitHub Actions

The workflow `.github/workflows/main.yml` runs the script automatically every
morning (07:05&nbsp;UTC) and can also be triggered manually. It installs R,
Quarto and the required packages before calling `Rscript run_turuylevaade.R`.

## Docker

A `Dockerfile` is provided to reproduce the environment. Build and run the image
with:

```sh
docker build -t periodical .
docker run --rm periodical
```

The container defaults to executing `run_turuylevaade.R`.

## Data sources

The analysis fetches data from:

- [Pensionikeskus](https://www.pensionikeskus.ee/) for Estonian pension fund
  statistics
- [Yahoo Finance](https://finance.yahoo.com/) for market data

## License

Released under the BSD&nbsp;3‑Clause License. See [LICENSE](LICENSE) for details.

