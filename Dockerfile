# Alustame ametlikust R-i baaspildist.
FROM r-base:4.3.3

# Seadistame ajavööndi ja väldime interaktiivseid dialooge.
ENV TZ=Europe/Tallinn
ENV DEBIAN_FRONTEND=noninteractive

# Paigaldame kõik vajalikud süsteemi sõltuvused.
# See käsk töötab, sest see ei lahenda keerulisi sõltuvusi.
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    pandoc \
    ssh-client \
    # Süsteemi sõltuvused R-i pakettidele
    libcurl4-openssl-dev libssl-dev libxml2-dev libcairo2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libproj-dev \
    libudunits2-dev libgdal-dev libgeos-dev libssh-dev \
    # Fondid
    fonts-liberation fonts-roboto fonts-inter fonts-open-sans \
    && rm -rf /var/lib/apt/lists/*

# --- QUARTO PAIGALDAMINE ILMA PAKETIHALDURITA ---
# Laeme alla .tar.gz arhiivi, pakime lahti ja lisame PATH-i.
# See väldib täielikult apt, dpkg ja gdebi kasutamist ning ka nendega seotud vigu.
RUN QUARTO_VERSION="1.7.32" && \
    curl -o quarto.tar.gz -L "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz" && \
    mkdir -p /opt/quarto && \
    tar -xzf quarto.tar.gz -C /opt/quarto --strip-components=1 && \
    rm quarto.tar.gz

# Lisame Quarto binaarfailide kausta süsteemi PATH-i, et 'quarto' käsk oleks leitav.
ENV PATH="/opt/quarto/bin:${PATH}"

# Jätkame ülejäänud sõltuvustega, mis kasutavad apt-i lihtsamal viisil.
RUN apt-get update && \
    echo "ttf-mscorefonts-installer msttcorefonts/accepted-mscorefonts-eula select true" | debconf-set-selections && \
    apt-get install -y --no-install-recommends ttf-mscorefonts-installer && \
    apt-get install -y --no-install-recommends cargo && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Määrame töökataloogi ja kopeerime vajalikud failid.
WORKDIR /build
COPY renv.lock .

# Paigaldame ja taastame R-i paketid renv abil.
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# Registreerime hrbrthemes fondid R-is.
RUN R -e "options(hrbrthemes.loadfonts=TRUE); suppressPackageStartupMessages(library(hrbrthemes))"

# Kopeerime ülejäänud koodi alles päris lõpus.
WORKDIR /home/rstudio/project
COPY . .