#-------------------------------------------------------------------------------
# PENSIONIFONDIDE ANALÜÜSI JA VISUALISEERIMISE SKRIPT (OPTIMEERITUD & PARALLELNE)
#-------------------------------------------------------------------------------

# 1. OSA: VAJALIKE PAKETTIDE LAADIMINE
#-------------------------------------------------------------------------------
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
  library(future) # <<< MUUDATUS: Lisatud paralleliseerimiseks
  library(furrr)  # <<< MUUDATUS: Lisatud paralleliseerimiseks
})


#-------------------------------------------------------------------------------
# 2. OSA: ANDMETE LAADIMISE FUNKTSIOONID
#-------------------------------------------------------------------------------

get_pension_data <- function() {
  message("Laen fondide andmeid Pensionikeskusest...")
  date_to <- today()
  url <- paste0("https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=", date_to, "&f%5B0%5D=47&f%5B1%5D=38&f%5B2%5D=77&f%5B3%5D=EPI&f%5B4%5D=73")
  navid_raw <- read.csv2(url, fileEncoding = "UTF-16", header = TRUE, sep = "\t")
  navid <- navid_raw %>%
    select(Kuupäev, Fond, NAV) %>%
    pivot_wider(names_from = Fond, values_from = NAV) %>%
    filter(!is.na(`Tuleva Maailma Aktsiate Pensionifond`), !is.na(`II samba üldindeks`)) %>%
    mutate(Kuupäev = dmy(Kuupäev))
  return(navid)
}

get_inflation_data <- function() {
  message("Laen inflatsiooni andmeid Statistikaametist...")
  url <- "https://andmed.stat.ee/api/v1/et/stat/IA02"
  query_body <- list(query = list(list(code="Aasta", selection=list(filter="item", values=I(2017:year(today())))), list(code="Kaubagrupp", selection=list(filter="item", values=I(c("1"))))), response=list(format="csv2"))
  response <- POST(url, body = toJSON(query_body, auto_unbox = TRUE), add_headers("Content-Type" = "application/json"))
  csv_data <- content(response, "text", encoding = "UTF-8")
  month_lookup <- c("Jaanuar"=1,"Veebruar"=2,"Märts"=3,"Aprill"=4,"Mai"=5,"Juuni"=6,"Juuli"=7,"August"=8,"September"=9,"Oktoober"=10,"November"=11,"Detsember"=12)
  df <- read.csv(text = csv_data, header = TRUE, check.names = FALSE) |>
    rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) |>
    mutate(indeks = as.numeric(indeks), kuu = as.integer(month_lookup[Kuu]), date = as.Date(paste(Aasta, kuu, "01", sep = "-"))) |>
    arrange(date) |> filter(date > dmy("01-03-2017"), !is.na(indeks))
  return(df)
}


#-------------------------------------------------------------------------------
# 3. OSA: STAATILISTE GRAAFIKUTE GENEREERIMINE
#-------------------------------------------------------------------------------

generate_static_charts <- function(nav_data, inflation_data) {
  message("--- Alustan staatiliste graafikute genereerimist ---")
  
  maxdate <- max(nav_data$Kuupäev)
  last_navs <- nav_data %>% filter(Kuupäev == maxdate) %>% slice(1)
  
  navid_kuu_pikk <- nav_data %>%
    group_by(Kuupäev=floor_date(Kuupäev,"month")) %>%
    summarize(across(c(`II samba üldindeks`,`Tuleva Maailma Aktsiate Pensionifond`,`LHV Pensionifond XL`,`LHV Pensionifond L`,`LHV Pensionifond Indeks`),~mean(.x,na.rm=TRUE)),.groups="drop") %>%
    mutate(
      Tuleva=100*(last_navs$`Tuleva Maailma Aktsiate Pensionifond`/`Tuleva Maailma Aktsiate Pensionifond`-1),
      LHVXL=100*(last_navs$`LHV Pensionifond XL`/`LHV Pensionifond XL`-1),
      LHVL=100*(last_navs$`LHV Pensionifond L`/`LHV Pensionifond L`-1),
    ) %>%
    pivot_longer(cols=c(Tuleva,LHVL,LHVXL))
  
  last_infl <- inflation_data %>% filter(date == max(date)) %>% pull(indeks)
  inflatsioon_pikk <- inflation_data |> 
    mutate(value = (last_infl / indeks - 1) * 100, name = "inflatsioon") |>
    select(Kuupäev = date, name, value)
  
  pikk2 <- navid_kuu_pikk %>%
    filter(name %in% c("LHVL", "LHVXL", "Tuleva")) %>%
    select(Kuupäev, name, value) %>%
    bind_rows(inflatsioon_pikk) %>%
    mutate(name = fct_relevel(name, "LHVL", "LHVXL", "Tuleva", "inflatsioon"))
  
  inflatsioon_max <- inflatsioon_pikk %>% filter(Kuupäev == max(Kuupäev, na.rm=TRUE)) %>% pull(Kuupäev) |> format("%Y-%m")
  
  p_aastane_tulu <- pikk2 %>%
    group_by(aasta = as.factor(year(Kuupäev)), name) %>%
    summarize(value = round(mean(value, na.rm = TRUE)/100, 2), .groups = "drop") %>%
    filter(!is.na(value)) %>%
    mutate(value_pc = scales::percent(value, accuracy = 0.1)) %>%
    ggplot(aes(x = aasta, y = value, fill = name)) +
    geom_col(position = "dodge") +
    geom_bar_text(position = "dodge", aes(label = value_pc), place = "top", contrast = TRUE, size = 8, min.size=1) +
    scale_fill_manual("Fond:", values = c("LHVL"="#4A4E5A","LHVXL"="#222221","Tuleva"="#00aeea","inflatsioon"="#FF8C00")) +
    scale_y_percent() + theme_ipsum_rc() +
    labs(title="Kui palju on keskmiselt kasvanud tänaseks raha", subtitle="sõltuvalt sissepaneku aastast ja fondist", x="raha sissepaneku aasta", y="kasv tänaseks", caption=paste0("fondid seisuga ", maxdate, ", inflatsioon kuni ", inflatsioon_max, "\nallikad: Pensionikeskus, Statistikaamet\nligikaudsed arvutused: Indrek Seppo")) +
    theme(legend.position = "top")
  
  ggsave(p_aastane_tulu, file = "aastane_tulu_tuleva_lhv.png", height = 5, width = 7, scale = 1.1, bg = "white")
  message("--- Staatilised graafikud valmis ---")
}


#-------------------------------------------------------------------------------
# 4. OSA: ANIMEERITUD GRAAFIKUTE GENEREERIMINE
#-------------------------------------------------------------------------------

create_specific_animation <- function(animeeritud_andmed_raw, kaadrite_kuupaevad, lopp_kp, funds_to_include, series_order, file_suffix, plot_subtitle) {
  # See funktsioon jääb samaks, aga seda kutsutakse välja paralleelselt
  message(paste("--- Loon animatsiooni failidele lõpuga:", file_suffix, "---"))
  
  anim_data_subset <- animeeritud_andmed_raw %>% filter(name %in% c(funds_to_include, "inflatsioon"))
  color_palette <- c("LHVL"="#4A4E5A", "LHVXL"="#222221", "Tuleva"="#00aeea", "inflatsioon"="#FF8C00")
  active_colors <- color_palette[names(color_palette) %in% series_order]
  
  scaffold <- tidyr::expand_grid(seisuga_kp=kaadrite_kuupaevad, aasta=as.character(2017:year(lopp_kp)), name=series_order)
  
  animeeritud_andmed_final <- scaffold %>%
    left_join(anim_data_subset, by=c("seisuga_kp","aasta","name")) %>%
    mutate(
      value=if_else(is.na(value),0,value),
      name=factor(name, levels = series_order),
      aasta=factor(aasta,levels=as.character(2017:year(lopp_kp)))
    ) %>%
    arrange(seisuga_kp, aasta, name) %>%
    mutate(group_id = factor(paste(aasta, name), levels = unique(paste(aasta, name))))
  
  date_labels <- tibble(seisuga_kp = kaadrite_kuupaevad, date_label = format(kaadrite_kuupaevad, "%Y-%m"))
  
  p_anim <- ggplot(animeeritud_andmed_final, aes(x=aasta, y=value/100, fill=name, group=group_id)) +
    geom_col(position="dodge") +
    geom_text(data = date_labels, aes(label = paste("Seisuga:", date_label)), x = Inf, y = Inf, inherit.aes = FALSE, hjust = 1.1, vjust = 1.5, size = 14, color = "grey75", fontface = "bold") +
    scale_fill_manual(name="Võrdlus:", values=active_colors) + theme_ipsum_rc() +
    scale_y_continuous(labels=scales::percent, limits=c(NA, max(animeeritud_andmed_final$value, na.rm=TRUE)*1.04/100)) + 
    theme(legend.position="top") +
    labs(title="Kui palju on keskmiselt kasvanud raha?", subtitle=plot_subtitle, x="Raha sissepaneku aasta", y="Kasv", caption="Allikad: Pensionikeskus, Statistikaamet | arvutused: Indrek Seppo | graafik: AI") +
    transition_time(seisuga_kp) +
    ease_aes('linear')
  
  anim_fps <- 10; anim_pause_sec <- 10
  dynamic_nframes <- length(kaadrite_kuupaevad) + anim_pause_sec * anim_fps
  
  gif_fail <- paste0("aastane_tulu_animeeritud", file_suffix, ".gif")
  mp4_fail <- paste0("aastane_tulu_animeeritud", file_suffix, ".mp4")
  
  message("Salvestan GIF-i: ", gif_fail, " (see võib võtta aega...)")
  anim_save(gif_fail, animation=p_anim, width=800, height=600, renderer=gifski_renderer(loop=TRUE), nframes = dynamic_nframes, fps=anim_fps)
  
  message("Konverdin GIF-i MP4-ks kasutades FFmpeg...")
  system(paste0("ffmpeg -i ", shQuote(gif_fail), " -movflags +faststart -pix_fmt yuv420p -vf 'scale=trunc(iw/2)*2:trunc(ih/2)*2' -y ", shQuote(mp4_fail)))
}

generate_all_animations <- function(nav_data, inflation_data, create_tuleva_only_version = FALSE) {
  message("--- Alustan animeeritud graafikute genereerimist (optimeeritud ja paralleelne meetod) ---")
  
  lopp_kp <- max(nav_data$Kuupäev)
  kaadrite_kuupaevad <- seq.Date(from=ymd("2018-01-01"), to=lopp_kp, by="1 month") %>% union(lopp_kp)
  
  message("Arvutan animeerimiseks vajalikke andmeid...")
  navid_kuu <- nav_data %>%
    group_by(Kuupäev = floor_date(Kuupäev, "month")) %>%
    summarize(across(where(is.numeric), ~mean(.x, na.rm=TRUE)), .groups="drop")
  
  infl_kuu <- inflation_data %>%
    mutate(Kuupäev = floor_date(date, "month")) %>%
    group_by(Kuupäev) %>%
    summarize(infl_indeks = mean(indeks, na.rm = TRUE))
  
  animeeritud_andmed_raw <- tidyr::crossing(seisuga_kp = kaadrite_kuupaevad, invest_kp = navid_kuu$Kuupäev) %>%
    filter(invest_kp <= seisuga_kp) %>%
    left_join(navid_kuu, by = c("invest_kp" = "Kuupäev")) %>%
    left_join(navid_kuu, by = c("seisuga_kp" = "Kuupäev"), suffix = c("_invest", "_seisuga")) %>%
    left_join(infl_kuu, by = c("invest_kp" = "Kuupäev")) %>%
    left_join(infl_kuu, by = c("seisuga_kp" = "Kuupäev"), suffix = c("_invest", "_seisuga")) %>%
    mutate(
      Tuleva = 100 * (`Tuleva Maailma Aktsiate Pensionifond_seisuga` / `Tuleva Maailma Aktsiate Pensionifond_invest` - 1),
      LHVXL = 100 * (`LHV Pensionifond XL_seisuga` / `LHV Pensionifond XL_invest` - 1),
      LHVL = 100 * (`LHV Pensionifond L_seisuga` / `LHV Pensionifond L_invest` - 1),
      inflatsioon = 100 * (infl_indeks_seisuga / infl_indeks_invest - 1)
    ) %>%
    select(seisuga_kp, invest_kp, Tuleva, LHVXL, LHVL, inflatsioon) %>%
    pivot_longer(cols = c(Tuleva, LHVXL, LHVL, inflatsioon), names_to = "name", values_to = "value") %>%
    filter(!is.na(value), is.finite(value)) %>%
    group_by(seisuga_kp, aasta = as.character(year(invest_kp)), name) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  message("Andmed arvutatud. Alustan animatsioonide renderdamist...")
  
  # <<< MUUDATUS ALGAB SIIN: Animatsioonide loomise loogika on muudetud paralleelseks >>>
  # 1. Koostame nimekirja ülesannetest (animatsioonidest, mida luua)
  animation_tasks <- list(
    # Esimene ülesanne on alati vaikimisi animatsioon
    list(
      funds_to_include = c("LHVL", "LHVXL", "Tuleva"),
      series_order = c("LHVL", "LHVXL", "Tuleva", "inflatsioon"),
      file_suffix = "",
      plot_subtitle = "Võrdluses LHV XL, LHV L, Tuleva ja inflatsioon"
    )
  )
  
  # Kui on vaja, lisame teise ülesande (Tuleva vs inflatsioon)
  if (create_tuleva_only_version) {
    animation_tasks <- c(animation_tasks, list(
      list(
        funds_to_include = "Tuleva",
        series_order = c("Tuleva", "inflatsioon"),
        file_suffix = "_tuleva_ainult",
        plot_subtitle = "Võrdluses Tuleva ja inflatsioon"
      )
    ))
  }
  
  # 2. Käivitame ülesanded paralleelselt kasutades furrr::future_walk
  # .progress = TRUE näitab mugavat edenemisriba
  furrr::future_walk(
    animation_tasks,
    ~ create_specific_animation(
      animeeritud_andmed_raw = animeeritud_andmed_raw,
      kaadrite_kuupaevad = kaadrite_kuupaevad,
      lopp_kp = lopp_kp,
      funds_to_include = .x$funds_to_include,
      series_order = .x$series_order,
      file_suffix = .x$file_suffix,
      plot_subtitle = .x$plot_subtitle
    ),
    .progress = TRUE
  )
  # <<< MUUDATUS LÕPPEB SIIN >>>
  
  message("--- Kõik animatsioonid valmis ---")
}


#-------------------------------------------------------------------------------
# 5. OSA: PEAPROTSESS JA KÄIVITAMISE LOOGIKA
#-------------------------------------------------------------------------------
should_update_animation <- function(filepath) {
  if (!file.exists(filepath)) { message("Animatsiooni faili ", filepath, " ei leitud. Loome uue."); return(TRUE) }
  last_mod_time <- file.mtime(filepath)
  start_of_this_week <- floor_date(today(), "week", week_start = 1)
  if (last_mod_time < start_of_this_week) { message("Animatsioon on eelmisest nädalast. Värskendame."); return(TRUE) }
  return(FALSE)
}

main <- function() {
  cmd_args <- commandArgs(trailingOnly = TRUE)
  create_tuleva_version <- "--tuleva-ainult" %in% cmd_args
  
  # <<< MUUDATUS: Seadistame paralleliseerimise plaani. Kasutame 2 tuuma. >>>
  plan(multisession, workers = 2)
  message("Kasutan skripti jooksutamiseks ", future::nbrOfWorkers(), " tuuma.")
  
  pension_data <- get_pension_data()
  inflation_data <- get_inflation_data()
  
  if (create_tuleva_version) {
    message("Parameeter on aktiveeritud. Genereeritakse ka ainult Tulevaga animatsioon.")
  }
  
  generate_static_charts(pension_data, inflation_data)
  
  if (should_update_animation("aastane_tulu_animeeritud.gif")) {
    generate_all_animations(pension_data, inflation_data, create_tuleva_only_version = create_tuleva_version)
  } else {
    message("Animeeritud graafikud on selle nädala seisuga värsked. Jätame uuendamise vahele.")
  }
  
  message("Skripti töö on lõppenud.")
}

# Käivita skript
main()