# Alustame ametlikust Rocker'i ja Quarto pildist Docker Hubist
FROM rocker/quarto:latest

# Väldime interaktiivseid dialooge paigaldamise ajal
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Tallinn

# Uuendame pakettide nimekirja ja paigaldame süsteemi sõltuvused ja fondid
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    # SSH kliendi paigaldamine (vajalik scp jaoks)
    ssh-client \
    # R-i pakettide süsteemi sõltuvused
    libcurl4-openssl-dev libssh-dev libssl-dev \
    libcairo2-dev libfontconfig1-dev libfreetype6-dev \
    libpng-dev libjpeg-dev libxml2-dev libproj-dev \
    libudunits2-dev libgdal-dev libgeos-dev \
    # FFmpeg video loomiseks
    ffmpeg \
    # Microsofti fontide EULA aktsepteerimine
    && echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections \
    && apt-get install -y ttf-mscorefonts-installer \
    # Muud fondid
    && apt-get install -y fonts-liberation fonts-roboto fonts-inter fonts-ibm-plex fonts-open-sans \
    # Puhastame vahemälu, et pilt oleks väiksem
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Paigaldame Rust/Cargo {gifski} paketi jaoks
RUN apt-get update -qq && apt-get install -y --no-install-recommends cargo && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Määrame renv teegi asukoha pildi sees
ENV R_LIBS_USER=/opt/R/renv/library

# Kopeerime renv-i lukustusfaili pildile
COPY renv.lock renv.lock

# Paigaldame renv-i ja taastame kõik R-i paketid
RUN Rscript -e "install.packages('renv')" && \
    Rscript -e "renv::restore()"

# Registreerime hrbrthemes fondid R-is
RUN Rscript -e "options(hrbrthemes.loadfonts=TRUE); suppressPackageStartupMessages(library(hrbrthemes))"

# Määrame töökataloogi, kuhu hiljem kood kopeeritakse
WORKDIR /home/rstudio/project