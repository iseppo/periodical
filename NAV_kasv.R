# =============================================================================
# PENSIONIFONDIDE ANALÜÜSI JA VISUALISEERIMISE SKRIPT
# =============================================================================
# Autor: Indrek Seppo
# Kirjeldus: Skript pensionifondide tootluse analüüsimiseks ja visualiseerimiseks
# =============================================================================

# PAKETID JA SEADISTUSED --------------------------------------------------
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(lubridate)
  library(hrbrthemes)
  library(ggfittext)
  library(gganimate)
  library(gifski)
  library(av)       
  library(purrr)
})

# KONSTANTID --------------------------------------------------------------
# URL-id ja API parameetrid
PENSION_BASE_URL <- "https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/"
STAT_API_URL <- "https://andmed.stat.ee/api/v1/et/stat/IA02"

# Fondide ID-d
FOND_IDS <- c("47", "38", "77", "EPI", "73")

# Kuude lookup tabel
KUUD <- c(
  "Jaanuar" = 1, "Veebruar" = 2, "Märts" = 3, "Aprill" = 4,
  "Mai" = 5, "Juuni" = 6, "Juuli" = 7, "August" = 8,
  "September" = 9, "Oktoober" = 10, "November" = 11, "Detsember" = 12
)

# Värvid
FONDIDE_VARVID <- c(
  "LHVL" = "#4A4E5A",
  "LHVXL" = "#222221",
  "Tuleva" = "#00aeea",
  "inflatsioon" = "#FF8C00"
)

# Animatsiooni seaded
ANIM_FPS <- 10
ANIM_PAUSE_SEC <- 10

# ANDMETE LAADIMISE FUNKTSIOONID ------------------------------------------

#' Laadi pensionifondide NAV andmed
#'
#' @param algus_kp Alguskuupäev (default: 2017-03-28)
#' @param lopp_kp Lõppkuupäev (default: täna)
#' @return data.frame pensionifondide andmetega
laadi_nav_andmed <- function(algus_kp = "2017-03-28", lopp_kp = today()) {
  message("Laen pensionifondide andmeid...")
  
  url <- glue::glue(
    "{PENSION_BASE_URL}?download=xls&date_from={algus_kp}&date_to={lopp_kp}&",
    "f%5B0%5D={FOND_IDS[1]}&f%5B1%5D={FOND_IDS[2]}&f%5B2%5D={FOND_IDS[3]}&",
    "f%5B3%5D={FOND_IDS[4]}&f%5B4%5D={FOND_IDS[5]}"
  )
  
  tryCatch({
    nav_raw <- read.csv2(
      url,
      fileEncoding = "UTF-16",
      header = TRUE,
      sep = "\t"
    )
    
    nav_clean <- nav_raw |>
      select(Kuupäev, Fond, NAV) |>
      pivot_wider(names_from = Fond, values_from = NAV) |>
      filter(
        !is.na(`Tuleva Maailma Aktsiate Pensionifond`),
        !is.na(`II samba üldindeks`)
      ) |>
      mutate(Kuupäev = dmy(Kuupäev))
    
    return(nav_clean)
  }, error = function(e) {
    stop("Viga fondide andmete laadimisel: ", e$message)
  })
}

#' Laadi inflatsiooni andmed
#'
#' @param algus_aasta Algusaasta (default: 2017)
#' @return data.frame inflatsiooni andmetega
laadi_inflatsiooni_andmed <- function(algus_aasta = 2017) {
  message("Laen inflatsiooni andmeid...")
  
  query_body <- list(
    query = list(
      list(
        code = "Aasta",
        selection = list(
          filter = "item",
          values = I(algus_aasta:year(today()))
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
    response = list(format = "csv2")
  )
  
  tryCatch({
    response <- POST(
      STAT_API_URL,
      body = toJSON(query_body, auto_unbox = TRUE),
      add_headers("Content-Type" = "application/json")
    )
    
    csv_data <- content(response, "text", encoding = "UTF-8")
    
    infl_clean <- read.csv(
      text = csv_data,
      header = TRUE,
      check.names = FALSE
    ) |>
      rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) |>
      mutate(
        indeks = as.numeric(indeks),
        kuu = as.integer(KUUD[Kuu]),
        date = as.Date(paste(Aasta, kuu, "01", sep = "-"))
      ) |>
      arrange(date) |>
      filter(
        date > dmy("01-03-2017"),
        !is.na(indeks)
      )
    
    return(infl_clean)
  }, error = function(e) {
    stop("Viga inflatsiooni andmete laadimisel: ", e$message)
  })
}

# ANDMETE TÖÖTLEMISE FUNKTSIOONID -----------------------------------------

#' Arvuta fondide tootlused
#'
#' @param nav_data NAV andmed
#' @return list tootluste andmetega
arvuta_fondide_tootlused <- function(nav_data) {
  maxdate <- max(nav_data$Kuupäev)
  
  # Viimased NAV väärtused
  viimased_navid <- nav_data |>
    filter(Kuupäev == maxdate) |>
    slice(1)
  
  # Kuised keskmised ja tootlused
  kuised_tootlused <- nav_data |>
    group_by(Kuupäev = floor_date(Kuupäev, "month")) |>
    summarise(
      across(
        c(`II samba üldindeks`, `Tuleva Maailma Aktsiate Pensionifond`,
          `LHV Pensionifond XL`, `LHV Pensionifond L`, `LHV Pensionifond Indeks`),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    mutate(
      EPI = 100 * (viimased_navid$`II samba üldindeks` / `II samba üldindeks` - 1),
      Tuleva = 100 * (viimased_navid$`Tuleva Maailma Aktsiate Pensionifond` /
                        `Tuleva Maailma Aktsiate Pensionifond` - 1),
      LHVXL = 100 * (viimased_navid$`LHV Pensionifond XL` / `LHV Pensionifond XL` - 1),
      LHVL = 100 * (viimased_navid$`LHV Pensionifond L` / `LHV Pensionifond L` - 1),
      LHVIndeks = 100 * (viimased_navid$`LHV Pensionifond Indeks` /
                           `LHV Pensionifond Indeks` - 1)
    )
  
  tootlused_pikk <- kuised_tootlused |>
    pivot_longer(
      cols = c(Tuleva, LHVL, LHVXL, LHVIndeks),
      names_to = "name",
      values_to = "value"
    )
  
  return(list(pikk = tootlused_pikk, date = maxdate))
}

#' Töötle inflatsiooni andmeid tootluse jaoks
#'
#' @param infl_data Inflatsiooni andmed
#' @return data.frame töödeldud inflatsiooni andmetega
tootle_inflatsiooni_andmed <- function(infl_data) {
  viimane_indeks <- infl_data |>
    filter(date == max(date)) |>
    pull(indeks)
  
  infl_data |>
    mutate(value = (viimane_indeks / indeks - 1) * 100) |>
    select(date, value) |>
    rename(Kuupäev = date) |>
    mutate(name = "inflatsioon")
}

# VISUALISEERIMISE FUNKTSIOONID -------------------------------------------

#' Loo staatilised graafikud
#'
#' @param tootlused_data Tootluste andmed
#' @param inflatsiooni_data Inflatsiooni andmed
#' @param max_date Maksimum kuupäev
joonista_staatilised_graafikud <- function(tootlused_data, inflatsiooni_data, max_date) {
  # Ühenda andmed
  kombineeritud_andmed <- tootlused_data |>
    filter(name %in% c("LHVL", "LHVXL", "Tuleva")) |>
    select(Kuupäev, name, value) |>
    bind_rows(inflatsiooni_data) |>
    filter(year(Kuupäev) != year(max_date)) |>
    mutate(name = fct_relevel(name, "LHVL", "LHVXL", "Tuleva", "inflatsioon"))
  
  # Leia inflatsiooni viimane kuupäev
  inflatsiooni_max <- inflatsiooni_data |>
    filter(Kuupäev == max(Kuupäev, na.rm = TRUE)) |>
    pull(Kuupäev) |>
    format("%Y-%m")
  
  # Loo graafik
  p_aastane_tulu <- kombineeritud_andmed |>
    group_by(aasta = as.factor(year(Kuupäev)), name) |>
    summarise(value = round(mean(value, na.rm = TRUE) / 100, 2), .groups = "drop") |>
    mutate(value_pc = paste0(100 * value, "%")) |>
    ggplot(aes(x = aasta, y = value, fill = name)) +
    geom_col(position = "dodge", aes(color = name)) +
    geom_bar_text(
      position = "dodge",
      aes(label = value_pc),
      place = "top",
      contrast = TRUE,
      size = 8,
      padding.x = grid::unit(0.1, "mm"),
      padding.y = grid::unit(0.2, "mm"),
      min.size = 2
    ) +
    scale_fill_manual("Fond:", values = FONDIDE_VARVID) +
    scale_color_manual("Fond:", values = FONDIDE_VARVID) +
    theme_ipsum_rc() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Kui palju on keskmiselt kasvanud tänaseks raha",
      subtitle = "sõltuvalt sissepaneku aastast ja fondist",
      x = "raha sissepaneku aasta",
      y = "kasv tänaseks",
      caption = glue::glue(
        "fondid seisuga {max_date}, inflatsioon kuni {inflatsiooni_max}\n",
        "allikad: Pensionikeskus, Statistikaamet\n",
        "ligikaudsed arvutused: Indrek Seppo"
      )
    ) +
    theme(legend.position = "top")
  
  # Salvesta graafik
  ggsave(
    plot = p_aastane_tulu,
    filename = "aastane_tulu_tuleva_lhv.png",
    height = 5,
    width = 7,
    scale = 1.1,
    bg = "white"
  )
  
  message("Staatilised graafikud valmis")
}

#' Arvuta tootlused konkreetse kuupäeva seisuga
#'
#' @param seisuga_kp Kuupäev millise seisuga arvutada
#' @param nav_data NAV andmed
#' @param infl_data Inflatsiooni andmed
#' @return data.frame tootlustega või NULL kui andmeid pole piisavalt
arvuta_tootlus_kuupaeva_seisuga <- function(seisuga_kp, nav_data, infl_data) {
  # Filtreeri andmed kuupäeva järgi
  nav_filt <- nav_data |> filter(Kuupäev <= seisuga_kp)
  infl_filt <- infl_data |> filter(date <= seisuga_kp)
  
  # Kontrolli kas piisavalt andmeid
  if (nrow(nav_filt) < 2 || nrow(infl_filt) < 2) {
    return(NULL)
  }
  
  # Leia viimased väärtused
  viimased_navid <- nav_filt |>
    filter(Kuupäev == max(Kuupäev)) |>
    slice(1)
  
  viimane_infl <- infl_filt |>
    filter(date == max(date)) |>
    pull(indeks)
  
  # Arvuta fondide tootlused
  nav_kuised <- nav_filt |>
    group_by(Kuupäev = floor_date(Kuupäev, "month")) |>
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(
      Tuleva = 100 * (viimased_navid$`Tuleva Maailma Aktsiate Pensionifond` /
                        `Tuleva Maailma Aktsiate Pensionifond` - 1),
      LHVXL = 100 * (viimased_navid$`LHV Pensionifond XL` / `LHV Pensionifond XL` - 1),
      LHVL = 100 * (viimased_navid$`LHV Pensionifond L` / `LHV Pensionifond L` - 1)
    ) |>
    select(Kuupäev, Tuleva, LHVXL, LHVL) |>
    pivot_longer(cols = -Kuupäev, names_to = "name", values_to = "value")
  
  # Arvuta inflatsiooni tootlus
  infl_tootlus <- infl_filt |>
    mutate(value = (viimane_infl / indeks - 1) * 100) |>
    select(date, value) |>
    rename(Kuupäev = date) |>
    mutate(name = "inflatsioon")
  
  # Ühenda ja töötle
  bind_rows(nav_kuised, infl_tootlus) |>
    filter(
      !is.na(value),
      is.finite(value),
      year(Kuupäev) < year(seisuga_kp)
    ) |>
    group_by(aasta = as.character(year(Kuupäev)), name) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(seisuga_kp = seisuga_kp)
}

#' Loo animeeritud graafik
#'
#' @param nav_data NAV andmed
#' @param infl_data Inflatsiooni andmed
loo_animeeritud_graafik <- function(nav_data, infl_data) {
  message("Loon animeeritud graafikut...")
  
  lopp_kp <- max(nav_data$Kuupäev)
  kaadrite_kuupaevad <- seq.Date(
    from = ymd("2018-01-01"),
    to = lopp_kp,
    by = "1 month"
  )
  
  # Arvuta animeeritud andmed
  animeeritud_andmed_raw <- map_dfr(
    kaadrite_kuupaevad,
    ~ arvuta_tootlus_kuupaeva_seisuga(.x, nav_data, infl_data)
  )
  
  # Loo täielik andmestik
  scaffold <- expand_grid(
    seisuga_kp = kaadrite_kuupaevad,
    aasta = as.character(2017:(year(lopp_kp) - 1)),
    name = c("LHVL", "LHVXL", "Tuleva", "inflatsioon")
  )
  
  animeeritud_andmed_final <- scaffold |>
    left_join(animeeritud_andmed_raw, by = c("seisuga_kp", "aasta", "name")) |>
    mutate(
      value = if_else(is.na(value), 0, value),
      group_id = paste(aasta, name),
      name = factor(name, levels = c("LHVL", "LHVXL", "Tuleva", "inflatsioon")),
      aasta = factor(aasta, levels = as.character(2017:(year(lopp_kp) - 1)))
    )
  
  # Loo kuupäeva sildid
  nframes_total <- length(kaadrite_kuupaevad)
  
  # Loo animeeritud graafik
  p_anim <- ggplot(
    animeeritud_andmed_final,
    aes(x = aasta, y = value / 100, fill = name, group = group_id)
  ) +
    geom_col(position = "dodge") +
    geom_text(
      aes(x = Inf, y = Inf, label = format(seisuga_kp, "%Y-%m")),
      hjust = 1.1, vjust = 1.5,
      size = 14, color = "grey75", fontface = "bold"
    ) +
    scale_fill_manual(name = "Fond:", values = FONDIDE_VARVID) +
    theme_ipsum_rc() +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(NA, max(animeeritud_andmed_final$value, na.rm = TRUE) * 1.1 / 100)
    ) +
    theme(legend.position = "top") +
    labs(
      title = "Kui palju on keskmiselt kasvanud raha?",
      subtitle = "Sõltuvalt sissepaneku aastast ja fondist. Seisuga: {frame_time}", 
      x = "Raha sissepaneku aasta",
      y = "Kasv tänaseks",
      caption = paste(
        "Allikad: Pensionikeskus, Statistikaamet",
        "arvutused: Indrek Seppo",
        sep = " | "
      )
    ) +
    transition_time(seisuga_kp) +
    ease_aes("linear")
  
  # Salvesta animatsioon GIF-ina
  message("Salvestan animatsiooni GIF formaadis...")
  anim_save(
    "aastane_tulu_animeeritud.gif",
    animation = p_anim,
    width = 800,
    height = 600,
    renderer = gifski_renderer(loop = TRUE),
    nframes = nframes_total,
    fps = ANIM_FPS,
    end_pause = ANIM_PAUSE_SEC * ANIM_FPS
  )
  message("Animeeritud GIF valmis.")
  
  message("Salvestan animatsiooni MP4 formaadis...")
  anim_save(
    "aastane_tulu_animeeritud.mp4",
    animation = p_anim,
    width = 800,
    height = 600,
    renderer = av_renderer(), # Kasutame av_rendererit MP4 jaoks
    nframes = nframes_total,
    fps = ANIM_FPS,
    end_pause = ANIM_PAUSE_SEC * ANIM_FPS
  )
  message("Animeeritud MP4 valmis.")
  ## <<< LISATUD OSA LÕPP >>>
  
}

# UTILIIDID ---------------------------------------------------------------

#' Kontrolli kas animatsioon vajab uuendamist
#'
#' @param faili_tee Animatsiooni faili tee
#' @return logical kas uuendada
kas_uuendada_animatsiooni <- function(faili_tee) {
  if (!file.exists(faili_tee)) {
    message("Animatsiooni faili ei leitud. Loon uue.")
    return(TRUE)
  }
  
  viimane_muutmise_aeg <- file.mtime(faili_tee)
  selle_nadala_algus <- floor_date(today(), "week", week_start = 1)
  
  if (viimane_muutmise_aeg < selle_nadala_algus) {
    message("Animatsioon on eelmisest nädalast. Värskendame.")
    return(TRUE)
  }
  
  FALSE
}

# PEAFUNKTSIOONID ---------------------------------------------------------

#' Loo staatilised graafikud
loo_staatilised_graafikud <- function() {
  message("=== Alustan staatiliste graafikute loomist ===")
  
  # Laadi andmed
  nav_data <- laadi_nav_andmed()
  infl_raw <- laadi_inflatsiooni_andmed()
  
  # Töötle andmed
  tootlused <- arvuta_fondide_tootlused(nav_data)
  inflatsiooni_andmed <- tootle_inflatsiooni_andmed(infl_raw)
  
  # Joonista graafikud
  joonista_staatilised_graafikud(
    tootlused$pikk,
    inflatsiooni_andmed,
    tootlused$date
  )
  
  message("=== Staatilised graafikud valmis ===")
}

#' Loo animeeritud graafikud (nii GIF kui ka MP4) ## <<< MUUDETUD
loo_animeeritud_graafikud <- function() { ## <<< MUUDETUD
  message("=== Alustan animeeritud graafikute loomist (GIF ja MP4) ===") ## <<< MUUDETUD
  
  # Laadi andmed
  nav_data <- laadi_nav_andmed()
  infl_data <- laadi_inflatsiooni_andmed()
  
  # Loo animatsioon
  loo_animeeritud_graafik(nav_data, infl_data)
  
  message("=== Animeeritud graafikud valmis ===") ## <<< MUUDETUD
}

#' Peafunktsioon
main <- function() {
  message("Käivitan pensionifondide analüüsi...")
  
  # Loo staatilised graafikud
  loo_staatilised_graafikud()
  
  # Kontrolli kas animatsioon vajab uuendamist. Kontrollime GIF-faili olemasolu.
  if (kas_uuendada_animatsiooni("aastane_tulu_animeeritud.gif")) {
    loo_animeeritud_graafikud() ## <<< MUUDETUD
  } else {
    message("Animeeritud graafik on selle nädala seisuga värske. Jätame uuendamise vahele.")
  }
  
  message("Skripti töö on lõppenud edukalt!")
}

# KÄIVITAMINE -------------------------------------------------------------
main()