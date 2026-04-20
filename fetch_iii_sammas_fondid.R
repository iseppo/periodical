# Skript kolmanda samba fondide nimekirja hankimiseks
suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
})

runtime_fondiloend <- "Pensionifondid_III.csv"
seed_fondiloend <- "data/Pensionifondid_III_seed.csv"

# Korrektne URL kolmanda samba fondide jaoks
url <- "https://www.pensionikeskus.ee/iii-sammas/vabatahtlikud-fondid/fonditasude-vordlus/"

tryCatch({
  fondid_html <- read_html(url)

  # Leia kõik lingid, mis viitavad fondidele
  # Kolmanda samba fondide URL muster: /iii-sammas/vabatahtlikud-fondid/fid/[ID]/
  fund_links <- fondid_html %>% html_nodes("a[href*='/fid/']")

  # Ekstrakti 'href' atribuut
  fund_urls <- html_attr(fund_links, "href")

  # Ekstrakti fondi ID-d URL-idest (ühe sammuga, lõpu kaldkriips valikuline)
  fund_ids <- sub(".*/fid/(\\d+)/?.*", "\\1", fund_urls)
  # Märgi URL-id, mis ei sisaldanud /fid/ mustrit, NA-ks
  fund_ids[!grepl("/fid/\\d+", fund_urls)] <- NA_character_

  # Ekstrakti fondi nimetused (lingi tekst)
  fund_titles <- html_text(fund_links, trim = TRUE)

  # Eemalda duplikaadid (sama fond võib esineda mitmes kohas)
  fund_info <- data.frame(
    id = fund_ids,
    fond = fund_titles,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(id), nchar(trimws(fond)) > 0) %>%
    distinct(id, .keep_all = TRUE) %>%
    mutate(
      # Märgime indeksfondid automaatselt nime järgi
      indeks = ifelse(
        grepl("indeks|index", fond, ignore.case = TRUE),
        "x",
        ""
      )
    )

  # Kontrollime, kas leidsime piisavalt fonde enne ülekirjutamist
  if (nrow(fund_info) < 5) {
    if (file.exists(runtime_fondiloend)) {
      unlink(runtime_fondiloend)
    }
    cat("Hoiatus: leiti ainult", nrow(fund_info), "fondi — kasutan seed-faili", seed_fondiloend, "\n")
  } else {
    # Salvesta jooksu käigus genereeritud fondiloend eraldi runtime-failina
    write.csv(fund_info, runtime_fondiloend, row.names = FALSE, fileEncoding = "UTF-8")
    cat("Leitud", nrow(fund_info), "kolmanda samba fondi\n")
  }
  print(fund_info)

}, error = function(e) {
  cat("Viga fondide loendi hankimisel:", conditionMessage(e), "\n")

  if (file.exists(runtime_fondiloend)) {
    unlink(runtime_fondiloend)
  }

  if (file.exists(seed_fondiloend) && file.size(seed_fondiloend) > 50) {
    cat("Kasutan repoga kaasas olevat seed-faili:", seed_fondiloend, "\n")
  } else {
    cat("Seed-fail puudub või on vigane:", seed_fondiloend, "\n")
  }
})
