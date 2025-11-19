# Skript kolmanda samba fondide nimekirja hankimiseks
library(rvest)
library(dplyr)

url <- "https://www.pensionikeskus.ee/iii-sammas/vabatahtlikud-pensionifondid/"

tryCatch({
  fondid_html <- read_html(url)

  # Vali kõik <a> elemendid klassi "fund" all
  fund_links <- fondid_html %>% html_nodes("a.fund")

  # Ekstrakti 'href' atribuut, et saada fondi URL-id
  fund_urls <- html_attr(fund_links, "href")

  # Ekstrakti fondi ID-d URL-idest
  fund_ids <- sapply(fund_urls, function(url) {
      matches <- regexpr("/fid/(\\d+)/", url)
      ifelse(matches > 0, regmatches(url, matches), NA)
  })
  fund_ids <- gsub("/fid/(\\d+)/", "\\1", fund_ids)

  # Ekstrakti 'title' atribuut fondi nimetuste jaoks
  fund_titles <- html_attr(fund_links, "title")

  # Kombineeri ID-d ja nimetused andmestikuks
  fund_info <- data.frame(
    id = fund_ids,
    fond = fund_titles,
    indeks = "",  # Täidetakse hiljem käsitsi
    stringsAsFactors = FALSE
  )

  # Salvesta CSV-sse
  write.csv(fund_info, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")

  cat("Leitud", nrow(fund_info), "kolmanda samba fondi\n")
  print(fund_info)

}, error = function(e) {
  cat("Viga fondide loendi hankimisel:", conditionMessage(e), "\n")
  cat("Loome tühja faili...\n")

  # Loo tühi fail struktuuriga
  empty_df <- data.frame(
    id = character(),
    fond = character(),
    indeks = character(),
    stringsAsFactors = FALSE
  )
  write.csv(empty_df, "Pensionifondid_III.csv", row.names = FALSE, fileEncoding = "UTF-8")
})
