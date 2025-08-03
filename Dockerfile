# Alustame ametlikust R-i baaspildist, mida haldab R-i kogukond ise.
FROM r-base:4.3.3

# Seadistame ajavööndi ja väldime interaktiivseid dialooge
ENV TZ=Europe/Tallinn
ENV DEBIAN_FRONTEND=noninteractive

# Paigaldame Quarto eeltingimused ja muud vajalikud tööriistad
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    gdebi-core \
    pandoc \
    ssh-client \
    # Süsteemi sõltuvused R-i pakettidele
    libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libproj-dev \
    libudunits2-dev libgdal-dev libgeos-dev libssh-dev \
    # Fondid (probleemne 'fonts-ibm-plex' on eemaldatud)
    fonts-liberation fonts-roboto fonts-inter fonts-open-sans \
    && rm -rf /var/lib/apt/lists/*

# Paigaldame Quarto käsitsi
RUN QUARTO_VERSION="1.4.554" && \
    curl -o quarto.deb -L "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb" && \
    gdebi --non-interactive quarto.deb && \
    rm quarto.deb

# Paigaldame MS fondid eraldi, kuna see on tülikas
RUN apt-get update && \
    echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections && \
    apt-get install -y --no-install-recommends ttf-mscorefonts-installer && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Paigaldame Rust/Cargo {gifski} paketi jaoks
RUN apt-get update && apt-get install -y --no-install-recommends cargo && apt-get clean && rm -rf /var/lib/apt/lists/*

# Määrame töökataloogi ja kopeerime vajalikud failid
# NB! Muudetud, et vältida kogu repo kopeerimist liiga vara
WORKDIR /build
COPY renv.lock .

# Paigaldame ja taastame R-i paketid renv abil
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# Registreerime hrbrthemes fondid R-is
RUN R -e "options(hrbrthemes.loadfonts=TRUE); suppressPackageStartupMessages(library(hrbrthemes))"

# Kopeerime ülejäänud koodi alles päris lõpus
WORKDIR /home/rstudio/project
COPY . .