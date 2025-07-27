# Lae vajalikud teegid
library(httr)
library(jsonlite)
library(tidyverse)
library(hrbrthemes)
library(ggfittext)
#url <- "https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=2023-07-21&f%5B0%5D=77&f%5B1%5D=EPI"

# --- Data Fetching (Keep as is) ---
date_to <- today()
url <- paste0("https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=", date_to, "&f%5B0%5D=47&f%5B1%5D=38&f%5B2%5D=77&f%5B3%5D=EPI&f%5B4%5D=73")
navid_raw <- read.csv2(url, fileEncoding ="UTF-16", header = TRUE, sep = "\t")

# --- Initial Processing and Finding Last Day NAVs ---
# Process the raw data first
navid <- navid_raw %>%
  select(Kuupäev, Fond, NAV) %>%
  pivot_wider(names_from = Fond, values_from = NAV) %>%
  # Filter for funds needed in *subsequent* calculations, but maybe not EPI here if it's just a benchmark?
  # Let's keep the original filter for now, assuming Tuleva and EPI are essential baseline data points.
  filter(!is.na(`Tuleva Maailma Aktsiate Pensionifond`), !is.na(`II samba üldindeks`))

# Convert dates AFTER filtering potentially incomplete rows
navid$Kuupäev <- dmy(navid$Kuupäev)

# Find the maximum date *after* processing and potential filtering
maxdate <- max(navid$Kuupäev)
print(paste("Calculating returns up to:", maxdate)) # Good practice to check

# Get the row containing the NAVs for the last day
# Use slice_max to handle potential ties (though unlikely for date)
last_day_navs <- navid %>%
  filter(Kuupäev == maxdate) %>%
  slice(1) # Take the first row if there are multiple entries for the same max date

# Extract the specific NAV values for the funds of interest from that last day
# Use .subset2 for slight efficiency and error if column doesn't exist
last_epi_nav <- last_day_navs$`II samba üldindeks`
last_tuleva_nav <- last_day_navs$`Tuleva Maailma Aktsiate Pensionifond`
last_lhvxl_nav <- last_day_navs$`LHV Pensionifond XL`
last_lhvl_nav <- last_day_navs$`LHV Pensionifond L`
last_lhvindex_nav <- last_day_navs$`LHV Pensionifond Indeks`

# Check if any NAVs are NA on the last day - this might cause issues
if (any(is.na(c(last_epi_nav, last_tuleva_nav, last_lhvxl_nav, last_lhvl_nav, last_lhvindex_nav)))) {
  warning("One or more funds have NA NAV on the last available date (", maxdate, "). Related return calculations will be NA.")
}


# --- Monthly Aggregation (Using the processed 'navid' dataframe) ---
navid_kuu <- navid %>%
  # Make sure data is sorted by date before grouping if using first/last aggregation
  # arrange(Kuupäev) # Sorting is implicit with floor_date grouping but explicit is safer if changing summarise
  group_by(Kuupäev = floor_date(Kuupäev, unit = "month")) %>%
  # Using mean NAV for the starting month, as per original code.
  # Could use first(NAV) or last(NAV) if you want start-of-month or end-of-month NAV instead.
  summarize(`II samba üldindeks` = mean(`II samba üldindeks`, na.rm = TRUE),
            `Tuleva Maailma Aktsiate Pensionifond` = mean(`Tuleva Maailma Aktsiate Pensionifond`, na.rm = TRUE),
            `LHV Pensionifond XL` = mean(`LHV Pensionifond XL`, na.rm = TRUE),
            `LHV Pensionifond L` = mean(`LHV Pensionifond L`, na.rm = TRUE),
            `LHV Pensionifond Indeks` = mean(`LHV Pensionifond Indeks`, na.rm = TRUE),
            .groups = 'drop') # Good practice to drop grouping

# --- Return Calculation using LAST DAY NAVs ---
navid_kuu <- navid_kuu %>%
  mutate(
    # Calculate return from the monthly average NAV to the specific LAST DAY NAV
    EPI = 100 * (last_epi_nav / `II samba üldindeks` - 1),
    Tuleva = 100 * (last_tuleva_nav / `Tuleva Maailma Aktsiate Pensionifond` - 1),
    LHVXL = 100 * (last_lhvxl_nav / `LHV Pensionifond XL` - 1),
    LHVL = 100 * (last_lhvl_nav / `LHV Pensionifond L` - 1),
    LHVIndeks = 100 * (last_lhvindex_nav / `LHV Pensionifond Indeks` - 1)
  )

navid_kuu_pikk <- navid_kuu %>%
  pivot_longer(cols = c(Tuleva, LHVL, LHVXL, LHVIndeks))

navid_kuu_epi <- navid_kuu %>%
  pivot_longer(cols = c(Tuleva, EPI))


navid_kuu_pikk <- navid_kuu_pikk %>%
  mutate(value_cut = cut(value, c(-10, 0, 10, 20,30, 40))) %>%
  mutate(value_cut = fct_recode(value_cut, "<=0%" = "(-10,0]"))


## Lisa inflatsioon



# API URL
url <- "https://andmed.stat.ee/api/v1/et/stat/IA02"

current_year <- year(today())

aastad <- c(2017:current_year)

# Päringu sisu R-i listina (sama, mis eelmisel korral)
query_body <- list(
  query = list(
    list(
      code = "Aasta",
      selection = list(
        filter = "item",
        values = I(aastad)
      )
    ),
    list(
      code = "Kaubagrupp",
      selection = list(
        filter = "item",
        values = I(c("1"))
      )
    )
  ),
  response = list(
    format = "csv2"
  )
)

# Teisenda R-i list JSON stringiks
json_payload <- toJSON(query_body, auto_unbox = TRUE)

# Tee POST päring
response <- POST(
  url,
  body = json_payload,
  add_headers("Content-Type" = "application/json")
)

# Kontrolli tulemust
if (http_status(response)$category == "Success") {
  # Saame vastuse tekstina kätte
  csv_data <- content(response, "text", encoding = "UTF-8")

  month_lookup <- c(
    "Jaanuar" = 1, "Veebruar" = 2, "Märts" = 3,
    "Aprill" = 4, "Mai" = 5, "Juuni" = 6,
    "Juuli" = 7, "August" = 8, "September" = 9,
    "Oktoober" = 10, "November" = 11, "Detsember" = 12
  )
  
  # Loeme CSV-teksti otse data.frame'i
  # check.names = FALSE hoiab ära selle, et R muudaks veerunimesid (nt asendaks tühikud punktidega)
  df <- read.csv(text = csv_data, header = TRUE, check.names = FALSE) |>
    rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) |>
    mutate(indeks = as.numeric(indeks)) |>
    mutate(kuu = month_lookup[Kuu]) |>
    mutate(kuu = as.integer(kuu)) |>
    mutate(date = as.Date(paste(Aasta, kuu, "01", sep = "-"))) |>
    arrange(date) |>
    filter(date > dmy("01-03-2017")) |>
    filter(!is.na(indeks)) 
  
last_inflation = df %>%
    filter(date == max(date)) %>%
    pull(indeks)

inflatsioon <- df |>
  mutate(indeks = (last_inflation/indeks -1) * 100) |>
  select(date, indeks) |>
  rename(Kuupäev = date, value = indeks) |>
  mutate(name = "inflatsioon")
  
  
  
} else {
  # Väljasta veateade, kui midagi läks valesti
  cat("Päring ebaõnnestus, staatuskood:", status_code(response), "\n")
  cat("Saadetud JSON:\n", json_payload, "\n\n")
  cat("Serveri vastus:\n")
  cat(content(response, "text", encoding = "UTF-8"))
}


p <- navid_kuu_pikk %>%
  filter(name %in% c("LHVL", "LHVXL", "Tuleva")) %>%
  ggplot(aes(x = Kuupäev, y = value)) +
  geom_col() +
#  geom_col(aes(fill = value_cut)) +
  scale_fill_viridis_d("kasvu-\nprotsent") +
  facet_wrap(~name, nrow = 1, axes = "all") +
  theme_ipsum_rc() +
  scale_y_continuous(breaks = c(0:5)*10) +
#  scale_x_date(date_breaks = "1 year") +
  labs(title = "Mitu protsenti on su raha kasvanud", subtitle = "sõltuvalt sissemaksu kuust", x = "sissemaksu kuu",
       y = "sissemakse kasvanud (%)", caption = paste("seisuga", maxdate))
p
ggsave(p, file = "raha_kasv_LHV.png", height = 5, width =7, scale = 1.1, bg = "white")

navid_kuu_pikk <- navid_kuu_pikk |>
  filter(name %in% c("LHVL", "LHVXL", "Tuleva")) |>
  select(Kuupäev, name, value) |>
  bind_rows(inflatsioon) |>
  filter(year(Kuupäev) != max(year(Kuupäev))) |>
  mutate(name = fct_relevel(name, "LHVL", "LHVXL", "Tuleva", "inflatsioon")) 
  


p <- navid_kuu_pikk |>
  group_by(aasta = as.factor(year(Kuupäev)), name) |>
  summarize(value = round(mean(value, na.rm = TRUE)/100, 2)) |>
  mutate(value_pc = paste0(100*value, "%")) |>
  ggplot(aes(x = aasta, y = value, fill = name)) +
  geom_col(position = "dodge") +
  geom_bar_text(position = "dodge", aes(label = value_pc), outside = TRUE, min.size = 3, padding.x = grid::unit(0.2, "mm"), size = 8) +
  scale_fill_manual("Fond:", values = c("LHVL" = "#4A4E5A", "LHVXL" = "#222221", 
                                        "Tuleva" = "#00aeea", "inflatsioon" = "#FF8C00")) +
  theme_ipsum_rc() +
  scale_y_percent() +
  labs(title = "Kui palju on keskmiselt kasvanud tänaseks raha",
       subtitle = "sõltuvalt sissepaneku aastast ja fondist", x = "raha sissepaneku aasta",
       y = "kasv tänaseks", caption = paste("seisuga", maxdate, "\n ligikaudsed arvutused: Indrek Seppo")) +
  theme(legend.position = "top", legend.key.size = unit(0.3, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) 
p
ggsave(p, file = "aastane_tulu_tuleva_lhv.png", height = 5, width = 7, scale = 1.1, bg = "white")


p <- navid_kuu_epi %>%
  filter(name %in% c("EPI", "Tuleva")) %>%
  mutate(name = fct_recode(name, "Keskmine pensionikoguja" = "EPI")) %>%
  ggplot(aes(x = Kuupäev, y = value)) +
  geom_col() +
  #  geom_col(aes(fill = value_cut)) +
  scale_fill_viridis_d("kasvu-\nprotsent") +
  facet_wrap(~name, nrow = 1) +
  theme_ipsum_rc() +
  labs(title = "Mitu protsenti on su raha kasvanud", subtitle = "sõltuvalt sissemaksu kuust", x = "sissemaksu kuu",
       y = "sissemakse kasvanud (%)", caption = paste("seisuga", maxdate))
p
ggsave(p, file = "raha_kasv_EPI.png", height = 5, width =7, scale = 1.1, bg = "white")




