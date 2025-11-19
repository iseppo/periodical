# Skript kolmanda samba fondide nimekirja hankimiseks
library(rvest)
library(dplyr)

# Korrektne URL kolmanda samba fondide jaoks
url <- "https://www.pensionikeskus.ee/iii-sammas/vabatahtlikud-fondid/fonditasude-vordlus/"

tryCatch({
  fondid_html <- read_html(url)

  # Leia kõik lingid, mis viitavad fondidele
  # Kolmanda samba fondide URL muster: /iii-sammas/vabatahtlikud-fondid/fid/[ID]/
  fund_links <- fondid_html %>% html_nodes("a[href*='/fid/']")

  # Ekstrakti 'href' atribuut
  fund_urls <- html_attr(fund_links, "href")

  # Ekstrakti fondi ID-d URL-idest
  fund_ids <- sapply(fund_urls, function(url) {
      matches <- regexpr("/fid/(\\d+)/", url)
      ifelse(matches > 0, regmatches(url, matches), NA)
  })
  fund_ids <- gsub("/fid/(\\d+)/", "\\1", fund_ids)

  # Ekstrakti fondi nimetused (lingi tekst)
  fund_titles <- html_text(fund_links, trim = TRUE)

  # Eemalda duplikaadid (sama fond võib esineda mitmes kohas)
  fund_info <- data.frame(
    id = fund_ids,
    fond = fund_titles,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(id)) %>%
    distinct(id, .keep_all = TRUE) %>%
    mutate(
      # Märgime indeksfondid automaatselt nime järgi
      indeks = ifelse(
        grepl("indeks|Indeks|index|Index", fond, ignore.case = TRUE),
        "x",
        ""
      )
    )

  # Salvesta CSV-sse
  write.csv(fund_info, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")

  cat("Leitud", nrow(fund_info), "kolmanda samba fondi\n")
  print(fund_info)

}, error = function(e) {
  cat("Viga fondide loendi hankimisel:", conditionMessage(e), "\n")
  cat("Kasutame olemasolevat Pensionifondid_III.csv faili või loome uue...\n")

  # Kontrolli, kas fail juba eksisteerib
  if(!file.exists("Pensionifondid_III.csv") || file.size("Pensionifondid_III.csv") < 50) {
    # Kui faili pole või on tühi, loo tühi struktuur
    empty_df <- data.frame(
      id = character(),
      fond = character(),
      indeks = character(),
      stringsAsFactors = FALSE
    )
    write.csv(empty_df, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")
  }
})
