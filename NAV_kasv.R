#===============================================================================
# PENSIONIFONDIDE ANALÜÜSI JA VISUALISEERIMISE SKRIPT
# Paralleliseeritud versioon
# 
# Skripti eesmärk: Analüüsida ja visualiseerida Eesti II samba pensionifondide
# tootlusi võrreldes inflatsiooniga
#===============================================================================

#-------------------------------------------------------------------------------
# 1. PAKETTIDE LAADIMINE JA KESKKONNA SEADISTAMINE
#-------------------------------------------------------------------------------

# Laeme vajalikud paketid vaikselt
suppressPackageStartupMessages({
  # Andmete hankimine
  library(httr)          # HTTP päringud
  library(jsonlite)      # JSON töötlus
  
  # Andmetöötlus
  library(dplyr)         # Andmete manipuleerimine
  library(tidyr)         # Andmete korrastamine
  library(forcats)       # Faktorite käsitlemine
  library(readr)         # CSV lugemine
  library(stringr)       # Stringide töötlus
  library(lubridate)     # Kuupäevade töötlus
  
  # Visualiseerimine
  library(ggplot2)       # Graafikud
  library(hrbrthemes)    # Teema graafikutele
  library(ggfittext)     # Teksti paigutamine graafikutel
  library(gganimate)     # Animatsioonid
  
  # Paralleliseerimine
  library(future)        # Paralleelarvutused
  library(furrr)         # Paralleelne purrr
  library(parallel)      # Tuumade arv
})

# Seadistame paralleliseerimise kasutades kõiki saadaolevaid tuumi (jättes 1 vabaks)
# Optimeeritud kiiremaks töötluseks
available_cores <- parallel::detectCores()
worker_count <- max(2, available_cores - 1)  # Vähemalt 2, maksimaalselt N-1
plan(multisession, workers = worker_count)

#-------------------------------------------------------------------------------
# 2. ABIFUNKTSIOONID
#-------------------------------------------------------------------------------

#' Kuude nimede teisendamine numbriteks
MONTH_LOOKUP <- c(
  "Jaanuar" = 1, "Veebruar" = 2, "Märts" = 3, "Aprill" = 4,
  "Mai" = 5, "Juuni" = 6, "Juuli" = 7, "August" = 8,
  "September" = 9, "Oktoober" = 10, "November" = 11, "Detsember" = 12
)

#' Fondide värvid visualiseerimiseks
FUND_COLORS <- c(
  "LHV Ettevõtlik" = "#4A4E5A",
  "LHV Julge" = "#222221",
  "Tuleva" = "#00aeea",
  "inflatsioon" = "#FF8C00"
)

#-------------------------------------------------------------------------------
# 3. ANDMETE HANKIMISE FUNKTSIOONID
#-------------------------------------------------------------------------------

#' Kontrollib cache'i kehtivust
#' @param cache_file Cache faili asukoht
#' @param max_age_hours Maksimaalne vanus tundides (vaikimisi 1)
#' @return TRUE kui cache on kehtiv, FALSE muul juhul
is_cache_valid <- function(cache_file, max_age_hours = 1) {
  if (!file.exists(cache_file)) {
    return(FALSE)
  }

  file_age_hours <- as.numeric(difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours"))
  return(file_age_hours < max_age_hours)
}

#' Laeb NAV (Net Asset Value) andmed Pensionikeskusest
#' OPTIMEERITUD: kasutab cache'i vältimaks tarbetuid API kutseid
#'
#' @param start_date Alguskuupäev (vaikimisi 2017-03-28)
#' @param end_date Lõppkuupäev (vaikimisi tänane)
#' @param use_cache Kas kasutada cache'i (vaikimisi TRUE)
#' @return Data frame fondide NAV väärtustega
get_nav_data <- function(start_date = "2017-03-28", end_date = today(), use_cache = TRUE) {
  cache_file <- "cache_nav_data.rds"

  # Kontrollime cache'i
  if (use_cache && is_cache_valid(cache_file)) {
    message("Laen NAV andmeid cache'ist (optimeerimine)...")
    return(readRDS(cache_file))
  }

  message("Laen fondide andmeid Pensionikeskusest...")

  # Koostame URL-i koos fondide ID-dega
  base_url <- "https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/"
  
  params <- list(
    download = "xls",
    date_from = start_date,
    date_to = end_date,
    `f[0]` = "47",  # LHV Pensionifond Ettevõtlik
    `f[1]` = "38",  # LHV Pensionifond Julge
    `f[2]` = "77",  # Tuleva Maailma Aktsiate Pensionifond
    `f[3]` = "EPI", # II samba üldindeks
    `f[4]` = "73"   # LHV Pensionifond Indeks
  )
  
  # Koostame täieliku URL-i
  url <- paste0(base_url, "?", paste0(names(params), "=", params, collapse = "&"))
  
  # Loeme andmed
  navid_raw <- read.delim(
    url,
    fileEncoding = "UTF-16",
    header = TRUE
  )
  
  # Töötleme andmed õigesse formaati
  navid <- navid_raw %>%
    select(Kuupäev, Fond, NAV) %>%
    pivot_wider(
      names_from = Fond,
      values_from = NAV
    ) %>%
    filter(
      !is.na(`Tuleva Maailma Aktsiate Pensionifond`),
      !is.na(`II samba üldindeks`)
    ) %>%
    mutate(
      Kuupäev = dmy(Kuupäev)
    )

  # Salvestame cache'i
  if (use_cache) {
    saveRDS(navid, cache_file)
    message("NAV andmed salvestatud cache'i")
  }

  return(navid)
}

#' Laeb inflatsiooni andmed Statistikaametist
#' OPTIMEERITUD: kasutab cache'i vältimaks tarbetuid API kutseid
#'
#' @param start_year Algusaasta (vaikimisi 2017)
#' @param use_cache Kas kasutada cache'i (vaikimisi FALSE)
#' @param raw Kui TRUE, tagastab toorandmed (date, indeks veerud) animatsiooni jaoks
#' @return Data frame inflatsiooni andmetega
get_inflation_data <- function(start_year = 2017, use_cache = FALSE, raw = FALSE) {
  cache_file <- "cache_inflation_data.rds"

  # Kontrollime cache'i (ainult töödeldud andmete jaoks)
  if (use_cache && !raw && is_cache_valid(cache_file)) {
    message("Laen inflatsiooni andmeid cache'ist (optimeerimine)...")
    return(readRDS(cache_file))
  }

  message("Laen inflatsiooni andmeid Statistikaametist...")

  url <- "https://andmed.stat.ee/api/v1/et/stat/IA02"

  # Koostame päringu
  query_body <- list(
    query = list(
      list(
        code = "Kaubagrupp",
        selection = list(filter = "item", values = I(c("1")))
      )
    ),
    response = list(format = "csv2")
  )

  # Teeme päringu
  response <- POST(
    url,
    body = toJSON(query_body, auto_unbox = TRUE),
    add_headers("Content-Type" = "application/json")
  )

  # Loeme vastuse
  csv_data <- content(response, "text", encoding = "UTF-8")

  # Töötleme andmed
  df <- read.csv(text = csv_data, header = TRUE, check.names = FALSE) %>%
    rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) %>%
    mutate(
      indeks = as.numeric(indeks),
      kuu = as.integer(MONTH_LOOKUP[Kuu]),
      date = as.Date(paste(Aasta, kuu, "01", sep = "-"))
    ) %>%
    arrange(date) %>%
    filter(
      date > dmy("01-03-2017"),
      !is.na(indeks)
    )

  # Kui soovitakse toorandmeid (animatsiooni jaoks), tagastame siit
  if (raw) {
    return(df)
  }

  # Arvutame inflatsiooni protsendi
  last_infl <- df %>%
    filter(date == max(date)) %>%
    pull(indeks)

  inflation <- df %>%
    mutate(
      indeks = (last_infl / indeks - 1) * 100
    ) %>%
    select(date, indeks) %>%
    rename(
      Kuupäev = date,
      value = indeks
    ) %>%
    mutate(name = "inflatsioon")

  # Salvestame cache'i
  if (use_cache) {
    saveRDS(inflation, cache_file)
    message("Inflatsiooni andmed salvestatud cache'i")
  }

  return(inflation)
}

#-------------------------------------------------------------------------------
# 4. TOOTLUSE ARVUTAMISE FUNKTSIOONID
#-------------------------------------------------------------------------------

#' Arvutab fondide keskmised tootlused
#' 
#' @param navid NAV andmed
#' @return List pikk formaat andmetega ja maksimaalse kuupäevaga
compute_returns <- function(navid) {
  maxdate <- max(navid$Kuupäev)
  
  # Viimased NAV väärtused
  last_navs <- navid %>% 
    filter(Kuupäev == maxdate) %>% 
    slice(1)
  
  # Arvutame kuukeskmised ja tootlused
  navid_kuu <- navid %>%
    # Grupeerime kuu kaupa
    group_by(Kuupäev = floor_date(Kuupäev, "month")) %>%
    # Arvutame keskmised NAV väärtused
    summarize(
      across(
        c(
          `II samba üldindeks`,
          `Tuleva Maailma Aktsiate Pensionifond`,
          `LHV Pensionifond Julge`,
          `LHV Pensionifond Ettevõtlik`,
          `LHV Pensionifond Indeks`
        ),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    # Arvutame tootlused protsentides
    mutate(
      EPI = 100 * (last_navs$`II samba üldindeks` / `II samba üldindeks` - 1),
      Tuleva = 100 * (last_navs$`Tuleva Maailma Aktsiate Pensionifond` /
                        `Tuleva Maailma Aktsiate Pensionifond` - 1),
      `LHV Julge` = 100 * (last_navs$`LHV Pensionifond Julge` / `LHV Pensionifond Julge` - 1),
      `LHV Ettevõtlik` = 100 * (last_navs$`LHV Pensionifond Ettevõtlik` / `LHV Pensionifond Ettevõtlik` - 1),
      LHVIndeks = 100 * (last_navs$`LHV Pensionifond Indeks` /
                           `LHV Pensionifond Indeks` - 1)
    )
  
  # Teisendame pikka formaati
  navid_kuu_pikk <- navid_kuu %>%
    pivot_longer(
      cols = c(Tuleva, `LHV Ettevõtlik`, `LHV Julge`, LHVIndeks),
      names_to = "name",
      values_to = "value"
    )
  
  return(list(
    pikk = navid_kuu_pikk,
    date = maxdate
  ))
}

#' Arvutab aastase keskmise tootluse kindla kuupäeva seisuga
#' OPTIMEERITUD VERSIOON: kasutab eelarvutatud kuukeskmisi
#'
#' @param andmete_seisuga_kp Kuupäev, mille seisuga arvutada
#' @param nav_data NAV andmed
#' @param inflation_data Inflatsiooni andmed
#' @param nav_monthly_cache Eelarvutatud kuukeskmised NAV väärtused (valikuline)
#' @return Data frame aastaste keskmiste tootlustega
arvuta_aastane_tootlus_hetkes <- function(andmete_seisuga_kp,
                                          nav_data,
                                          inflation_data,
                                          nav_monthly_cache = NULL) {

  # Filtreerime andmed vastavalt kuupäevale
  infl_filt <- inflation_data %>%
    filter(date <= andmete_seisuga_kp)

  # Kontrollime, kas on piisavalt andmeid
  if (nrow(infl_filt) < 2) {
    return(NULL)
  }

  # Viimased väärtused NAV andmetest
  last_navs <- nav_data %>%
    filter(Kuupäev <= andmete_seisuga_kp) %>%
    filter(Kuupäev == max(Kuupäev)) %>%
    slice(1)

  if (nrow(last_navs) < 1) {
    return(NULL)
  }

  last_infl <- infl_filt %>%
    filter(date == max(date)) %>%
    pull(indeks)

  # Kasutame cache'i kui on saadaval, muidu arvutame
  if (!is.null(nav_monthly_cache)) {
    navid_kuu_base <- nav_monthly_cache %>%
      filter(Kuupäev <= andmete_seisuga_kp)
  } else {
    navid_kuu_base <- nav_data %>%
      filter(Kuupäev <= andmete_seisuga_kp) %>%
      group_by(Kuupäev = floor_date(Kuupäev, "month")) %>%
      summarize(
        across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Arvutame fondide tootlused kasutades eelarvutatud kuukeskmisi
  navid_kuu <- navid_kuu_base %>%
    mutate(
      Tuleva = 100 * (last_navs$`Tuleva Maailma Aktsiate Pensionifond` /
                        `Tuleva Maailma Aktsiate Pensionifond` - 1),
      `LHV Julge` = 100 * (last_navs$`LHV Pensionifond Julge` /
                       `LHV Pensionifond Julge` - 1),
      `LHV Ettevõtlik` = 100 * (last_navs$`LHV Pensionifond Ettevõtlik` /
                      `LHV Pensionifond Ettevõtlik` - 1)
    ) %>%
    select(Kuupäev, Tuleva, `LHV Julge`, `LHV Ettevõtlik`) %>%
    pivot_longer(
      cols = -Kuupäev,
      names_to = "name",
      values_to = "value"
    )

  # Arvutame inflatsiooni tootluse
  infl_tootlus <- infl_filt %>%
    mutate(value = (last_infl / indeks - 1) * 100) %>%
    select(date, value) %>%
    rename(Kuupäev = date) %>%
    mutate(name = "inflatsioon")

  # Ühendame ja arvutame aastased keskmised
  bind_rows(navid_kuu, infl_tootlus) %>%
    filter(
      !is.na(value),
      is.finite(value),
      year(Kuupäev) <= year(andmete_seisuga_kp)
    ) %>%
    group_by(
      aasta = as.character(year(Kuupäev)),
      name
    ) %>%
    summarize(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(seisuga_kp = andmete_seisuga_kp)
}

#-------------------------------------------------------------------------------
# 5. STAATILISTE GRAAFIKUTE GENEREERIMINE
#-------------------------------------------------------------------------------

#' Genereerib staatilise tulpdiagrammi
#' 
#' @param pikk Andmed pikkas formaadis
#' @param inflatsioon Inflatsiooni andmed
#' @param maxdate Maksimumkuupäev
plot_static_chart <- function(pikk, inflatsioon, maxdate) {
  
  # Valmistame andmed ette
  pikk2 <- pikk %>%
    filter(name %in% c("LHV Ettevõtlik", "LHV Julge", "Tuleva")) %>%
    select(Kuupäev, name, value) %>%
    bind_rows(inflatsioon) %>%
    mutate(
      name = fct_relevel(name, "LHV Ettevõtlik", "LHV Julge", "Tuleva", "inflatsioon")
    )
  
  # Inflatsiooni viimane kuupäev
  inflatsioon_max <- inflatsioon %>% 
    filter(Kuupäev == max(Kuupäev, na.rm = TRUE)) %>% 
    pull(Kuupäev) %>% 
    format("%Y-%m")
  
  # Loome graafiku
  p_aastane_tulu <- pikk2 %>%
    # Grupeerime aastate kaupa
    group_by(
      aasta = as.factor(year(Kuupäev)),
      name
    ) %>%
    # Arvutame keskmised
    summarize(
      value = round(mean(value, na.rm = TRUE) / 100, 2),
      .groups = "drop"
    ) %>%
    # Lisame protsendi märgid
    mutate(
      value_pc = paste0(format(round(100 * value, 1), nsmall = 1), "%")
    ) %>%
    # Joonistame graafiku
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
    scale_fill_manual("Fond:", values = FUND_COLORS) +
    scale_color_manual("Fond:", values = FUND_COLORS) +
    theme_ipsum_rc() +
    scale_y_percent() +
    labs(
      title = "Kui palju on keskmiselt kasvanud tänaseks raha",
      subtitle = "sõltuvalt sissepaneku aastast ja fondist",
      x = "raha sissepaneku aasta",
      y = "kasv tänaseks",
      caption = paste0(
        "fondid seisuga ", maxdate, 
        ", inflatsioon kuni ", inflatsioon_max,
        "\nallikad: Pensionikeskus, Statistikaamet",
        "\nligikaudsed arvutused: Indrek Seppo"
      )
    ) +
    theme(legend.position = "top")
  
  # Salvestame faili
  ggsave(
    p_aastane_tulu,
    file = "aastane_tulu_tuleva_lhv.png",
    width = 800,
    height = 600,
    units = "px",
    dpi = 300,
    bg = "white",
    scale = 2.5
  )
}

#' Genereerib kõik staatilised graafikud
generate_static_charts <- function() {
  message("--- Alustan staatiliste graafikute genereerimist ---")

  # Kasutame kõiki saadaolevaid andmeid
  lopp_kp <- today()

  message(paste("Staatilised graafikud kuni kuupäevani:", lopp_kp))

  # Laeme andmed paralleelselt
  message("Laen NAV ja inflatsiooni andmeid paralleelselt...")
  load_start <- Sys.time()

  # Käivitame mõlemad laadimised paralleelselt, kasutades piiratud lõppkuupäeva
  nav_future <- future({ get_nav_data(end_date = lopp_kp) })
  inflation_future <- future({ get_inflation_data() })

  # Ootame mõlemad tulemused
  navid <- value(nav_future)
  inflation <- value(inflation_future)

  load_end <- Sys.time()
  message(paste(
    "Paralleelne laadimine võttis:",
    round(difftime(load_end, load_start, units = "secs"), 2),
    "sekundit"
  ))

  # Arvutame tootlused ja joonistame
  returns <- compute_returns(navid)
  plot_static_chart(returns$pikk, inflation, returns$date)

  message("--- Staatilised graafikud valmis ---")
}

#-------------------------------------------------------------------------------
# 6. ANIMEERITUD GRAAFIKUTE GENEREERIMINE
#-------------------------------------------------------------------------------

#' Loob animeeritud graafiku
#' 
#' @param animeeritud_andmed_raw Arvutatud animatsiooni andmed
#' @param kaadrite_kuupaevad Kuupäevad, millal kaadreid näidata
#' @param lopp_kp Lõppkuupäev
#' @param funds_to_include Fondid, mida kaasata
#' @param series_order Seeriate järjekord
#' @param file_suffix Faili järelliide
#' @param plot_subtitle Graafiku alapealkiri
create_specific_animation <- function(animeeritud_andmed_raw, 
                                      kaadrite_kuupaevad, 
                                      lopp_kp,
                                      funds_to_include, 
                                      series_order, 
                                      file_suffix, 
                                      plot_subtitle) {
  message(paste("--- Loon animatsiooni failidele lõpuga:", file_suffix, "---"))
  
  # Filtreerime andmed
  anim_data_subset <- animeeritud_andmed_raw %>%
    filter(name %in% c(funds_to_include, "inflatsioon"))
  
  # Määrame värvid
  all_series_names <- series_order
  active_colors <- FUND_COLORS[names(FUND_COLORS) %in% all_series_names]
  
  # Loome tühja struktuuri kõigi kombinatsioonide jaoks
  scaffold <- tidyr::expand_grid(
    seisuga_kp = kaadrite_kuupaevad,
    aasta = as.character(2017:year(lopp_kp)),
    name = all_series_names
  )
  
  # Ühendame andmed struktuuriga
  animeeritud_andmed_final <- scaffold %>%
    left_join(
      anim_data_subset,
      by = c("seisuga_kp", "aasta", "name")
    ) %>%
    mutate(
      value = if_else(is.na(value), 0, value),
      name = factor(name, levels = series_order),
      aasta = factor(aasta, levels = as.character(2017:year(lopp_kp)))
    ) %>%
    arrange(seisuga_kp, aasta, name) %>%
    mutate(
      group_id = factor(
        paste(aasta, name),
        levels = unique(paste(aasta, name))
      )
    )
  
  # Animatsiooni parameetrid (optimeeritud kiiremaks kodeerimiseks)
  anim_fps <- 15  
  anim_pause_sec <- 10
  dynamic_nframes <- (length(kaadrite_kuupaevad) - 1) * anim_fps * 1
  end_pause_frames <- anim_pause_sec * anim_fps
  
  # Kuupäeva sildid animatsiooni jaoks
  date_labels <- tibble(
    seisuga_kp = seq(
      from = min(kaadrite_kuupaevad),
      to = max(kaadrite_kuupaevad),
      length.out = (length(kaadrite_kuupaevad) - 1) * 20
    ),
    date_label = format(seisuga_kp, "%Y-%m")
  )
  
  # Loome animeeritud graafiku
  p_anim <- ggplot(
    animeeritud_andmed_final,
    aes(x = aasta, y = value/100, fill = name, group = group_id)
  ) +
    geom_col(position = "dodge") +
    geom_text(
      data = date_labels,
      aes(label = date_label),
      x = Inf,
      y = Inf,
      inherit.aes = FALSE,
      hjust = 1.1,
      vjust = 1.5,
      size = 14,
      color = "grey75",
      fontface = "bold"
    ) +
    scale_fill_manual(name = "Võrdlus:", values = active_colors) +
    theme_ipsum_rc(base_size = 24) +
    scale_y_continuous(
      labels = scales::percent_format(decimal.mark = ","),
      limits = c(NA, max(animeeritud_andmed_final$value, na.rm = TRUE) / 100 * 1.15)
    ) +
    theme(
      legend.position = "top",
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20)
    ) +
    labs(
      title = "Kui palju on keskmiselt kasvanud raha?",
      subtitle = plot_subtitle,
      x = "raha sissepaneku aasta",
      y = "kasv",
      caption = "Allikad: Pensionikeskus, Statistikaamet | ligikaudsed arvutused: Indrek Seppo"
    ) +
    transition_time(seisuga_kp) +
    ease_aes('linear')
  
  # Failinimed
  mp4_input  <- paste0("aastane_tulu_animeeritud2", file_suffix, ".mp4")
  mp4_output <- paste0("aastane_tulu_animeeritud",  file_suffix, ".mp4")
  
  message("Salvestan MP4: ", mp4_input)
  anim_save(
    mp4_input,
    animation = p_anim,
    width     = 800,
    height    = 600,
    dpi       = 150,        # Vähendatud 300-lt 150-le (kiirem kodeerimine, piisav kvaliteet)
    scale     = 1,
    renderer  = ffmpeg_renderer(options = list(pix_fmt = 'yuv420p', vcodec = 'libx264', preset = 'fast')),
    overwrite = TRUE,
    nframes   = dynamic_nframes,
    fps       = anim_fps,
    end_pause = end_pause_frames
  )
  
  message("Lisame 10-sekundilise lõpufiltri: ", mp4_input, " -> ", mp4_output)
  # Lisame 10 sekundit lõppu (optimeeritud kiirema kodeerimisega)
  exit_code <- system(paste(
    "ffmpeg",
    "-y",
    "-i", shQuote(mp4_input),
    "-vf", "tpad=stop_mode=clone:stop_duration=10",
    "-c:v libx264", # Video kodek
    "-preset fast", # Kiirem kodeerimine
    "-crf 23",      # Hea kvaliteet/kiiruse suhe
    "-c:a copy",
    shQuote(mp4_output)
  ))

  if (exit_code != 0) {
    warning("ffmpeg ebaõnnestus (kood: ", exit_code, "). Vahefail säilitatud: ", mp4_input)
  } else {
    # Kustutame vahefaili ainult eduka kodeerimise korral
    file.remove(mp4_input)
  }
} 

#' Genereerib kõik animatsioonid
#'
#' @param create_tuleva_only_version Kas luua ka ainult Tuleva versioon
generate_all_animations <- function(create_tuleva_only_version = FALSE) {
  message("--- Alustan animeeritud graafikute genereerimist ---")

  # Laeme andmed paralleelselt
  message("Laen NAV ja inflatsiooni andmeid paralleelselt...")
  load_start <- Sys.time()

  nav_future <- future({ get_nav_data() })
  inflation_future <- future({ get_inflation_data(raw = TRUE) })

  nav_data <- value(nav_future)
  inflation_data_raw <- value(inflation_future)

  load_end <- Sys.time()
  message(paste(
    "Paralleelne laadimine võttis:",
    round(difftime(load_end, load_start, units = "secs"), 2),
    "sekundit"
  ))

  # Määrame animatsiooni parameetrid - kasutame kõiki saadaolevaid andmeid
  lopp_kp <- max(nav_data$Kuupäev)

  message(paste("Animatsioon peatub kuupäeval:", lopp_kp))

  kuude_algused <- seq.Date(
    from = ymd("2018-01-01"),
    to = lopp_kp,
    by = "1 month"
  )
  kaadrite_kuupaevad <- sort(unique(c(kuude_algused, lopp_kp)))
  
  message("Eelarvutan kuukeskmisi NAV väärtusi ...")
  cache_start <- Sys.time()

  nav_monthly_cache <- nav_data %>%
    group_by(Kuupäev = floor_date(Kuupäev, "month")) %>%
    summarize(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  cache_end <- Sys.time()
  message(paste(
    "Cache'i loomine võttis:",
    round(difftime(cache_end, cache_start, units = "secs"), 2),
    "sekundit"
  ))

  # Arvutame animatsiooni kaadrid paralleelselt kasutades cache'i
  message("Arvutan animatsiooni kaadreid paralleelselt (optimeeritud)...")
  calc_start <- Sys.time()

  animeeritud_andmed_raw <- future_map_dfr(
    kaadrite_kuupaevad,
    ~ arvuta_aastane_tootlus_hetkes(.x, nav_data, inflation_data_raw, nav_monthly_cache),
    .options = furrr_options(seed = 123)
  )

  calc_end <- Sys.time()
  message(paste(
    "Paralleelne arvutamine võttis:",
    round(difftime(calc_end, calc_start, units = "secs"), 2),
    "sekundit"
  ))
  
  # Loome animatsioonid
  if (create_tuleva_only_version) {
    # Loome mõlemad animatsioonid paralleelselt
    anim_start <- Sys.time()
    
    # Vaikimisi animatsioon
    default_anim_future <- future({
      create_specific_animation(
        animeeritud_andmed_raw = animeeritud_andmed_raw,
        kaadrite_kuupaevad = kaadrite_kuupaevad,
        lopp_kp = lopp_kp,
        funds_to_include = c("LHV Ettevõtlik", "LHV Julge", "Tuleva"),
        series_order = c("LHV Ettevõtlik", "LHV Julge", "Tuleva", "inflatsioon"),
        file_suffix = "",
        plot_subtitle = "Võrdluses LHV Julge, LHV Ettevõtlik, Tuleva ja inflatsioon"
      )
    })
    
    # Tuleva ainult animatsioon
    tuleva_anim_future <- future({
      create_specific_animation(
        animeeritud_andmed_raw = animeeritud_andmed_raw,
        kaadrite_kuupaevad = kaadrite_kuupaevad,
        lopp_kp = lopp_kp,
        funds_to_include = c("Tuleva"),
        series_order = c("Tuleva", "inflatsioon"),
        file_suffix = "_tuleva_ainult",
        plot_subtitle = "Võrdluses Tuleva ja inflatsioon"
      )
    })
    
    # Ootame mõlemad valmis
    value(default_anim_future)
    value(tuleva_anim_future)
    
    anim_end <- Sys.time()
    message(paste(
      "Paralleelsed animatsioonid valmis:",
      round(difftime(anim_end, anim_start, units = "secs"), 2),
      "sekundit"
    ))
  } else {
    # Ainult vaikimisi animatsioon
    create_specific_animation(
      animeeritud_andmed_raw = animeeritud_andmed_raw,
      kaadrite_kuupaevad = kaadrite_kuupaevad,
      lopp_kp = lopp_kp,
      funds_to_include = c("LHV Ettevõtlik", "LHV Julge", "Tuleva"),
      series_order = c("LHV Ettevõtlik", "LHV Julge", "Tuleva", "inflatsioon"),
      file_suffix = "",
      plot_subtitle = "Võrdluses LHV Julge, LHV Ettevõtlik, Tuleva ja inflatsioon"
    )
  }
  
  message("--- Kõik animatsioonid valmis ---")
}

#-------------------------------------------------------------------------------
# 7. PEAPROTSESS JA KÄIVITAMISE LOOGIKA
#-------------------------------------------------------------------------------

#' Kontrollib, kas animatsioon vajab uuendamist
#'
#' @param filepath Animatsiooni faili asukoht
#' @return TRUE kui vajab uuendamist, FALSE muul juhul
should_update_animation <- function(filepath) {
  if (!file.exists(filepath)) return(TRUE)
  age_days <- as.numeric(difftime(Sys.time(), file.info(filepath)$mtime, units = "days"))
  return(age_days > 1)
}

#' Peamine käivitamise funktsioon
#' 
#' @param generate_tuleva_version_arg Kas genereerida ka Tuleva versioon
main <- function(generate_tuleva_version_arg = TRUE) {
  on.exit(plan(sequential), add = TRUE)

  total_start <- Sys.time()
  
  # Kontrollime käsurea argumente
  cmd_args <- commandArgs(trailingOnly = TRUE)
  cmd_flag_present <- "--tuleva-ainult" %in% cmd_args
  
  create_tuleva_version <- cmd_flag_present || generate_tuleva_version_arg
  
  if (create_tuleva_version) {
    message("Parameeter on aktiveeritud. Genereeritakse ka ainult Tulevaga animatsioon.")
  }
  
  # Näitame tuumade arvu
  message(paste("Kasutan", future::nbrOfWorkers(), "tuuma paralleliseerimiseks"))
  
  # Genereerime staatilised graafikud
  generate_static_charts()
  
  # Kontrollime, kas animatsioon vajab uuendamist
  if (should_update_animation("aastane_tulu_animeeritud.mp4")) {
    generate_all_animations(create_tuleva_only_version = create_tuleva_version)
  } else {
    message("Animeeritud graafikud on selle nädala seisuga värsked. Jätame uuendamise vahele.")
  }
  
  total_end <- Sys.time()
  message(paste(
    "\nSkripti kogu töö võttis:",
    round(difftime(total_end, total_start, units = "secs"), 2),
    "sekundit"
  ))
  message("Skripti töö on lõppenud.")
}

#-------------------------------------------------------------------------------
# 8. SKRIPTI KÄIVITAMINE
#-------------------------------------------------------------------------------

# Skripti käivitamiseks käsurealt:
# Rscript pensionifondide_analyys.R
# Rscript pensionifondide_analyys.R --tuleva-ainult

# Või interaktiivselt R-i konsoolis:
# source("pensionifondide_analyys.R")

# Käivitame põhiprogrammi
main()