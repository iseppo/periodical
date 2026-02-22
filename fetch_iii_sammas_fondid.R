# Skript kolmanda samba fondide nimekirja hankimiseks
suppressPackageStartupMessages({
  library(rvest)
  library(dplyr)
})

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
    cat("Hoiatus: leiti ainult", nrow(fund_info), "fondi — CSV-d ei kirjutata üle.\n")
  } else {
    # Salvesta CSV-sse
    write.csv(fund_info, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")
    cat("Leitud", nrow(fund_info), "kolmanda samba fondi\n")
  }
  print(fund_info)

}, error = function(e) {
  cat("Viga fondide loendi hankimisel:", conditionMessage(e), "\n")

  # Kontrolli, kas fail juba eksisteerib
  if(!file.exists("Pensionifondid_III.csv") || file.size("Pensionifondid_III.csv") < 50) {
    cat("Loon tühja Pensionifondid_III.csv faili...\n")
    # Kui faili pole või on tühi, loo tühi struktuur
    empty_df <- data.frame(
      id = character(),
      fond = character(),
      indeks = character(),
      stringsAsFactors = FALSE
    )
    write.csv(empty_df, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    cat("Kasutan olemasolevat Pensionifondid_III.csv faili.\n")
  }
})
