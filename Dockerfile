# Alustame täpselt õigest R-i stabiilsest versioonist.
FROM r-base:4.5.1

# Seadistame ajavööndi ja väldime interaktiivseid dialooge.
ENV TZ=Europe/Tallinn
ENV DEBIAN_FRONTEND=noninteractive

# Paigaldame kõik vajalikud süsteemi sõltuvused.
# Muudame olemasolevat ja korrektset konfiguratsioonifaili, et lisada 'contrib' ja 'non-free'.
RUN sed -i 's/Components: main/Components: main contrib non-free/g' /etc/apt/sources.list.d/debian.sources && \
    apt-get update && \
    # MS fondide EULA eelnev aktsepteerimine
    echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections && \
    # Paigaldame kõik paketid üheskoos, lisades puuduvad sõltuvused.
    apt-get install -y --no-install-recommends \
    curl \
    pandoc \
    ssh-client \
    # Lisatud vastavalt renv hoiatusele
    cmake \
    gdal-bin \
    git \
    # Süsteemi sõltuvused R-i pakettidele
    libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libproj-dev \
    libudunits2-dev libgdal-dev libgeos-dev libssh-dev \
    # Fondid ja Rust
    fonts-liberation fonts-roboto fonts-inter fonts-open-sans \
    ttf-mscorefonts-installer \
    cargo \
    # Puhastame vahemälu
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Paigaldame Quarto, vältides süsteemi paketihaldurit.
RUN QUARTO_VERSION="1.7.32" && \
    curl -o quarto.tar.gz -L "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz" && \
    mkdir -p /opt/quarto && \
    tar -xzf quarto.tar.gz -C /opt/quarto --strip-components=1 && \
    rm quarto.tar.gz

# Lisame Quarto binaarfailide kausta süsteemi PATH-i.
ENV PATH="/opt/quarto/bin:${PATH}"

# Määrame töökataloogi ja kopeerime vajalikud failid.
WORKDIR /build
COPY renv.lock .

# Paigaldame renv paketi.
RUN R -e "install.packages('renv')"

# Taastame R-i paketid, kasutades Posit'i binaarsete pakettide repositooriumi.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); renv::restore()"

# Registreerime hrbrthemes fondid R-is.
RUN R -e "options(hrbrthemes.loadfonts=TRUE); suppressPackageStartupMessages(library(hrbrthemes))"

# Kopeerime ülejäänud koodi alles päris lõpus.
WORKDIR /home/rstudio/project
COPY . .