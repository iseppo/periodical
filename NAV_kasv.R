# Load required packages
library(httr)
library(jsonlite)
library(tidyverse)
library(hrbrthemes)
library(ggfittext)

# Fetch NAV data from the Pensionikeskus API
get_nav_data <- function() {
  # Build URL with the latest date
  date_to <- today()
  url <- paste0(
    "https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=",
    date_to,
    "&f%5B0%5D=47&f%5B1%5D=38&f%5B2%5D=77&f%5B3%5D=EPI&f%5B4%5D=73"
  )

  # Download and shape the dataset
  navid_raw <- read.csv2(url, fileEncoding = "UTF-16", header = TRUE, sep = "\t")
  navid <- navid_raw %>%
    select(Kuupäev, Fond, NAV) %>%
    pivot_wider(names_from = Fond, values_from = NAV) %>%
    filter(
      !is.na(`Tuleva Maailma Aktsiate Pensionifond`),
      !is.na(`II samba üldindeks`)
    )
  navid$Kuupäev <- dmy(navid$Kuupäev)
  navid
}

# Compute monthly returns from the NAV table
compute_returns <- function(navid) {
  # Determine last available NAVs
  maxdate <- max(navid$Kuupäev)
  last_navs <- navid %>%
    filter(Kuupäev == maxdate) %>%
    slice(1)

  # Aggregate NAVs by month and calculate returns to last NAV
  navid_kuu <- navid %>%
    group_by(Kuupäev = floor_date(Kuupäev, "month")) %>%
    summarize(
      `II samba üldindeks` = mean(`II samba üldindeks`, na.rm = TRUE),
      `Tuleva Maailma Aktsiate Pensionifond` = mean(`Tuleva Maailma Aktsiate Pensionifond`, na.rm = TRUE),
      `LHV Pensionifond XL` = mean(`LHV Pensionifond XL`, na.rm = TRUE),
      `LHV Pensionifond L` = mean(`LHV Pensionifond L`, na.rm = TRUE),
      `LHV Pensionifond Indeks` = mean(`LHV Pensionifond Indeks`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      EPI = 100 * (last_navs$`II samba üldindeks` / `II samba üldindeks` - 1),
      Tuleva = 100 * (last_navs$`Tuleva Maailma Aktsiate Pensionifond` / `Tuleva Maailma Aktsiate Pensionifond` - 1),
      LHVXL = 100 * (last_navs$`LHV Pensionifond XL` / `LHV Pensionifond XL` - 1),
      LHVL = 100 * (last_navs$`LHV Pensionifond L` / `LHV Pensionifond L` - 1),
      LHVIndeks = 100 * (last_navs$`LHV Pensionifond Indeks` / `LHV Pensionifond Indeks` - 1)
    )

  navid_kuu_pikk <- navid_kuu %>%
    pivot_longer(cols = c(Tuleva, LHVL, LHVXL, LHVIndeks)) %>%
    mutate(value_cut = cut(value, c(-10, 0, 10, 20, 30, 40))) %>%
    mutate(value_cut = fct_recode(value_cut, "<=0%" = "(-10,0]"))

  navid_kuu_epi <- navid_kuu %>%
    pivot_longer(cols = c(Tuleva, EPI))

  list(pikk = navid_kuu_pikk, epi = navid_kuu_epi, date = maxdate)
}

# Query inflation data from Statistics Estonia
get_inflation_data <- function() {
  url <- "https://andmed.stat.ee/api/v1/et/stat/IA02"
  current_year <- year(today())
  aastad <- 2017:current_year

  query_body <- list(
    query = list(
      list(
        code = "Aasta",
        selection = list(filter = "item", values = I(aastad))
      ),
      list(
        code = "Kaubagrupp",
        selection = list(filter = "item", values = I(c("1")))
      )
    ),
    response = list(format = "csv2")
  )

  json_payload <- toJSON(query_body, auto_unbox = TRUE)
  response <- POST(url, body = json_payload, add_headers("Content-Type" = "application/json"))
  if (http_status(response)$category != "Success") {
    stop("Inflation query failed")
  }

  csv_data <- content(response, "text", encoding = "UTF-8")
  month_lookup <- c(
    "Jaanuar" = 1, "Veebruar" = 2, "Märts" = 3,
    "Aprill" = 4, "Mai" = 5, "Juuni" = 6,
    "Juuli" = 7, "August" = 8, "September" = 9,
    "Oktoober" = 10, "November" = 11, "Detsember" = 12
  )

  df <- read.csv(text = csv_data, header = TRUE, check.names = FALSE) |>
    rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) |>
    mutate(indeks = as.numeric(indeks)) |>
    mutate(kuu = month_lookup[Kuu]) |>
    mutate(kuu = as.integer(kuu)) |>
    mutate(date = as.Date(paste(Aasta, kuu, "01", sep = "-"))) |>
    arrange(date) |>
    filter(date > dmy("01-03-2017")) |>
    filter(!is.na(indeks))

  last_infl <- df %>% filter(date == max(date)) %>% pull(indeks)
  df |>
    mutate(indeks = (last_infl / indeks - 1) * 100) |>
    select(date, indeks) |>
    rename(Kuupäev = date, value = indeks) |>
    mutate(name = "inflatsioon")
}

# Save out plots illustrating the returns
plot_nav_charts <- function(pikk, epi, inflatsioon, maxdate) {
  p <- pikk %>%
    filter(name %in% c("LHVL", "LHVXL", "Tuleva")) %>%
    ggplot(aes(x = Kuupäev, y = value)) +
    geom_col() +
    scale_fill_viridis_d("kasvu-\nprotsent") +
    facet_wrap(~name, nrow = 1, axes = "all") +
    theme_ipsum_rc() +
    scale_y_continuous(breaks = c(0:5) * 10) +
    labs(
      title = "Mitu protsenti on su raha kasvanud",
      subtitle = "sõltuvalt sissemaksu kuust",
      x = "sissemaksu kuu",
      y = "sissemakse kasvanud (%)",
      caption = paste("seisuga", maxdate)
    )
  ggsave(p, file = "raha_kasv_LHV.png", height = 5, width = 7, scale = 1.1, bg = "white")

  pikk2 <- pikk %>%
    filter(name %in% c("LHVL", "LHVXL", "Tuleva")) %>%
    select(Kuupäev, name, value) %>%
    bind_rows(inflatsioon) %>%
    filter(year(Kuupäev) != max(year(Kuupäev))) %>%
    mutate(name = fct_relevel(name, "LHVL", "LHVXL", "Tuleva", "inflatsioon"))

  p <- pikk2 %>%
    group_by(aasta = as.factor(year(Kuupäev)), name) %>%
    summarize(value = round(mean(value, na.rm = TRUE) / 100, 2)) %>%
    mutate(value_pc = paste0(100 * value, "%")) %>%
    ggplot(aes(x = aasta, y = value, fill = name)) +
    geom_col(position = "dodge") +
    geom_bar_text(
      position = "dodge",
      aes(label = value_pc),
      outside = TRUE,
      min.size = 3,
      padding.x = grid::unit(0.2, "mm"),
      size = 8
    ) +
    scale_fill_manual(
      "Fond:",
      values = c(
        "LHVL" = "#4A4E5A",
        "LHVXL" = "#222221",
        "Tuleva" = "#00aeea",
        "inflatsioon" = "#FF8C00"
      )
    ) +
    theme_ipsum_rc() +
    scale_y_percent() +
    labs(
      title = "Kui palju on keskmiselt kasvanud t\xE4naseks raha",
      subtitle = "sõltuvalt sissepaneku aastast ja fondist",
      x = "raha sissepaneku aasta",
      y = "kasv tänaseks",
      caption = paste(
        "seisuga",
        maxdate,
        "\n ligikaudsed arvutused: Indrek Seppo"
      )
    ) +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    )
  ggsave(p, file = "aastane_tulu_tuleva_lhv.png", height = 5, width = 7, scale = 1.1, bg = "white")

  p <- epi %>%
    filter(name %in% c("EPI", "Tuleva")) %>%
    mutate(name = fct_recode(name, "Keskmine pensionikoguja" = "EPI")) %>%
    ggplot(aes(x = Kuupäev, y = value)) +
    geom_col() +
    scale_fill_viridis_d("kasvu-\nprotsent") +
    facet_wrap(~name, nrow = 1) +
    theme_ipsum_rc() +
    labs(
      title = "Mitu protsenti on su raha kasvanud",
      subtitle = "sõltuvalt sissemaksu kuust",
      x = "sissemaksu kuu",
      y = "sissemakse kasvanud (%)",
      caption = paste("seisuga", maxdate)
    )
  ggsave(p, file = "raha_kasv_EPI.png", height = 5, width = 7, scale = 1.1, bg = "white")
}

# Main entry point for chart generation
generate_nav_charts <- function() {
  nav_data <- get_nav_data()
  returns <- compute_returns(nav_data)
  inflation <- get_inflation_data()
  plot_nav_charts(returns$pikk, returns$epi, inflation, returns$date)
}

