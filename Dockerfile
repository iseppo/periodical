# Alustame Rocker Projecti pildist, mis on spetsiaalselt loodud Positi binaaridega töötamiseks.
FROM rocker/r-ver:4.5.1

# Seadistame ajavööndi ja väldime interaktiivseid dialooge.
ENV TZ=Europe/Tallinn
ENV DEBIAN_FRONTEND=noninteractive

# Paigaldame kõik vajalikud süsteemi sõltuvused.
# Muudame Rocker pildi traditsioonilist sources.list faili, et lisada 'contrib' ja 'non-free'.
RUN sed -i 's/main$/main contrib non-free/g' /etc/apt/sources.list && \
    apt-get update && \
    echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections && \
    apt-get install -y --no-install-recommends \
    curl \
    pandoc \
    ssh-client \
    cmake \
    gdal-bin \
    git \
    # Lisatud viimane puuduv sõltuvus .xz failide lahtipakkimiseks
    xz-utils \
    # Süsteemi sõltuvused R-i pakettidele
    libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libproj-dev \
    libudunits2-dev libgdal-dev libgeos-dev libssh-dev \
    # Fondid ja Rust
    fonts-liberation fonts-roboto fonts-inter fonts-open-sans \
    ttf-mscorefonts-installer \
    cargo \
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

# Taastame R-i paketid. Rocker pildid on juba seadistatud kasutama Positi binaarset repositooriumi.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); renv::restore()"

# Registreerime hrbrthemes fondid R-is.
RUN R -e "options(hrbrthemes.loadfonts=TRUE); suppressPackageStartupMessages(library(hrbrthemes))"

# Kopeerime ülejäänud koodi alles päris lõpus.
WORKDIR /home/rstudio/project
COPY . .