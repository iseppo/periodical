# 1. Base image with R and minimal OS
FROM rocker/r-ver:4.4.0

LABEL maintainer="your.name@company.com"

# 2. Install system dependencies (headers, Pandoc, fonts)
RUN apt-get update -qq && \
    # Pre-accept MS Core Fonts EULA non-interactively
    echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" \
      | debconf-set-selections && \
    export DEBIAN_FRONTEND=noninteractive && \
    apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssh-dev libssl-dev \
      libcairo2-dev libfontconfig1-dev libfreetype6-dev \
      libpng-dev libjpeg-dev libxml2-dev libproj-dev \
      pandoc ttf-mscorefonts-installer fonts-liberation \
      fonts-roboto fonts-inter fonts-ibm-plex fonts-open-sans && \
    rm -rf /var/lib/apt/lists/*

# 3. Set up Posit Package Manager snapshots
ENV RSPM_BIN="https://packagemanager.posit.co/cran/__linux__/jammy/latest" \
    RSPM_SRC="https://packagemanager.posit.co/cran/2025-05-12" \
    RENV_CONFIG_REPOS_OVERRIDE=$RSPM_BIN \
    RENV_CONFIG_INSTALL_PRECOMPILED=true

# 4. Create cache directory for renv
RUN mkdir -p /opt/renv-cache

# 5. Copy only what’s needed for renv restore
COPY renv.lock renv/activate.R ./

# 6. Install renv and restore

# Install renv and restore in one go
# ── Install renv and restore in one RUN ─────────────────────────────────
RUN Rscript -e '\
  install.packages("renv", repos="https://cloud.r-project.org"); \
  options( \
    repos = c( \
      RSPM = Sys.getenv("RSPM_BIN"), \
      SNAP = Sys.getenv("RSPM_SRC") \
    ), \
    install.packages.compile.from.source = "never" \
  ); \
  renv::restore(lockfile="renv.lock", prompt=FALSE) \
'

# 7. Copy the rest of your project (scripts, data, etc.)
COPY . .

WORKDIR /workspace

# 8. Default command to run your report
CMD ["Rscript", "run_turuylevaade.R"]
