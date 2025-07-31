# Periodical

Koostab regulaarselt turuülevaateid ja pensionifondide võrdlusi. Kood renderdab aruanded HTML-formaadis ning laadib need seejärel seppo.ai-sse.

## Eeldused

- **R** vähemalt versioon 4.3
- **Quarto CLI** alates versioonist 1.4

## Käivitamine käsitsi

Peamenüü käsu saab käivitada reposti juurkataloogist:

```sh
Rscript run_turuylevaade.R
```

Skript koostab graafikud ja renderdab `turuylevaade.qmd` ning `tuleva.qmd` failid. Lõpuks laaditakse valminud tulemused SSH abil serverisse.

## GitHub Actions

Kataloogis `.github/workflows` on töövoog, mis käivitab skripti iga päev kell 07:05 UTC.


## Andmeallikad

Analüüs kasutab andmeid allikatest:

- [Pensionikeskus](https://www.pensionikeskus.ee/) – Eesti pensionifondide statistika
- [Statistikaamet](https://www.stat.ee/) – Eesti inflatsiooniandmed
- [Yahoo Finance](https://finance.yahoo.com/) – S&P 500 andmed ja euro/dollari kurss

## Litsents

Projekt on avaldatud BSD 3-Clause litsentsi alusel. Täpsemad tingimused leiad failist [LICENSE](LICENSE).

