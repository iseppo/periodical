# Periodical

Koostab regulaarselt turuülevaateid ja pensionifondide võrdlusi. Kood renderdab aruanded HTML-formaadis ning laadib need seejärel seppo.ai-sse.

## Eeldused

- **R** vähemalt versioon 4.3
- **Quarto CLI** alates versioonist 1.4

## Repositooriumi struktuur

- `NAV_kasv.R` – hangib andmed ja koostab graafikud.
- `run_turuylevaade.R` – kanoniline käivitusskript, mis juhib kogu analüüsi ja laadib tulemused serverisse.
- `turuylevaade.qmd` ja `tuleva.qmd` – Quarto aruannete allikfailid.
- `Dockerfile` – konteiner keskkonna kirjeldus.
- `.github/workflows/main.yml` – igapäevane GitHub Actions töövoog.

## Käivitamine käsitsi

Peamenüü käsu saab käivitada reposti juurkataloogist:

```sh
Rscript run_turuylevaade.R
```

Skript käivitab kogu pipeline'i: koostab graafikud, uuendab III samba sisendandmed, renderdab Quarto failid ja laadib valminud tulemused SSH abil serverisse.

## GitHub Actions

Kataloogis `.github/workflows/main.yml` on töövoog, mis käivitab kanonilise `run_turuylevaade.R` skripti iga päev kell 11:05 UTC.


## Dockeriga käivitamine

Reproduktsiooni hõlbustamiseks saab sama keskkonna luua Dockeriga:

```sh
docker build -t periodical .
docker run --rm -v "$PWD:/home/rstudio/project" periodical Rscript run_turuylevaade.R
```

## Andmeallikad

Analüüs kasutab andmeid allikatest:

- [Pensionikeskus](https://www.pensionikeskus.ee/) – Eesti pensionifondide statistika
- [Statistikaamet](https://www.stat.ee/) – Eesti inflatsiooniandmed
- [Yahoo Finance](https://finance.yahoo.com/) – S&P 500 andmed ja euro/dollari kurss

## Litsents

Projekt on avaldatud BSD 3-Clause litsentsi alusel. Täpsemad tingimused leiad failist [LICENSE](LICENSE).
