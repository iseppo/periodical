#-------------------------------------------------------------------------------
# PENSIONIFONDIDE ANALÜÜSI JA VISUALISEERIMISE SKRIPT
#
# See skript teeb järgmist:
# 1. Laeb värsked andmed Pensionikeskusest ja Statistikaametist.
# 2. Genereerib staatilise graafiku tootluste kohta.
# 3. Genereerib animeeritud GIF-i ja MP4 video, mida uuendatakse kord nädalas.
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
})


#-------------------------------------------------------------------------------
# 2. OSA: STAATILISTE GRAAFIKUTE GENEREERIMINE
#-------------------------------------------------------------------------------

generate_static_charts <- function() {
  message("--- Alustan staatiliste graafikute genereerimist ---")
  
  # --- Andmete laadimise funktsioonid ---
  get_nav_data_static <- function() {
    message("Staatika: Laen fondide andmeid...")
    date_to <- today()
    url <- paste0("https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=", date_to, "&f%5B0%5D=47&f%5B1%5D=38&f%5B2%5D=77&f%5B3%5D=EPI&f%5B4%5D=73")
    navid_raw <- read.csv2(url, fileEncoding = "UTF-16", header = TRUE, sep = "\t")
    navid <- navid_raw %>%
      select(Kuupäev, Fond, NAV) %>%
      pivot_wider(names_from = Fond, values_from = NAV) %>%
      filter(!is.na(`Tuleva Maailma Aktsiate Pensionifond`), !is.na(`II samba üldindeks`))
    navid$Kuupäev <- dmy(navid$Kuupäev)
    return(navid)
  }
  
  get_inflation_data_static <- function() {
    message("Staatika: Laen inflatsiooni andmeid...")
    url <- "https://andmed.stat.ee/api/v1/et/stat/IA02"
    query_body <- list(query = list(list(code="Aasta", selection=list(filter="item", values=I(2017:year(today())))), list(code="Kaubagrupp", selection=list(filter="item", values=I(c("1"))))), response=list(format="csv2"))
    response <- POST(url, body = toJSON(query_body, auto_unbox = TRUE), add_headers("Content-Type" = "application/json"))
    csv_data <- content(response, "text", encoding = "UTF-8")
    month_lookup <- c("Jaanuar"=1,"Veebruar"=2,"Märts"=3,"Aprill"=4,"Mai"=5,"Juuni"=6,"Juuli"=7,"August"=8,"September"=9,"Oktoober"=10,"November"=11,"Detsember"=12)
    df <- read.csv(text = csv_data, header = TRUE, check.names = FALSE) |>
      rename(indeks = `IA02: TARBIJAHINNAINDEKS, 1997 = 100`) |>
      mutate(indeks = as.numeric(indeks), kuu = as.integer(month_lookup[Kuu]), date = as.Date(paste(Aasta, kuu, "01", sep = "-"))) |>
      arrange(date) |> filter(date > dmy("01-03-2017"), !is.na(indeks))
    
    last_infl <- df %>% filter(date == max(date)) %>% pull(indeks)
    df |> mutate(indeks = (last_infl / indeks - 1) * 100) |>
      select(date, indeks) |> rename(Kuupäev = date, value = indeks) |> mutate(name = "inflatsioon")
  }
  
  # --- Andmete arvutamise funktsioon ---
  compute_returns <- function(navid) {
    maxdate <- max(navid$Kuupäev)
    last_navs <- navid %>% filter(Kuupäev == maxdate) %>% slice(1)
    navid_kuu <- navid %>%
      group_by(Kuupäev=floor_date(Kuupäev,"month")) %>%
      summarize(across(c(`II samba üldindeks`,`Tuleva Maailma Aktsiate Pensionifond`,`LHV Pensionifond XL`,`LHV Pensionifond L`,`LHV Pensionifond Indeks`),~mean(.x,na.rm=TRUE)),.groups="drop") %>%
      mutate(
        EPI=100*(last_navs$`II samba üldindeks`/`II samba üldindeks`-1),
        Tuleva=100*(last_navs$`Tuleva Maailma Aktsiate Pensionifond`/`Tuleva Maailma Aktsiate Pensionifond`-1),
        LHVXL=100*(last_navs$`LHV Pensionifond XL`/`LHV Pensionifond XL`-1),
        LHVL=100*(last_navs$`LHV Pensionifond L`/`LHV Pensionifond L`-1),
        LHVIndeks=100*(last_navs$`LHV Pensionifond Indeks`/`LHV Pensionifond Indeks`-1)
      )
    navid_kuu_pikk <- navid_kuu %>% pivot_longer(cols=c(Tuleva,LHVL,LHVXL,LHVIndeks))
    list(pikk = navid_kuu_pikk, date = maxdate)
  }
  
  # --- Graafiku joonistamise funktsioon ---
  plot_charts <- function(pikk, inflatsioon, maxdate) {
    pikk2 <- pikk %>%
      filter(name %in% c("LHVL", "LHVXL", "Tuleva")) %>%
      select(Kuupäev, name, value) %>%
      bind_rows(inflatsioon) %>%
      filter(year(Kuupäev) != year(maxdate)) %>%
      mutate(name = fct_relevel(name, "LHVL", "LHVXL", "Tuleva", "inflatsioon"))
    inflatsioon_max <- inflatsioon %>% filter(Kuupäev == max(Kuupäev, na.rm=TRUE)) %>% pull(Kuupäev) |> format("%Y-%m")
    p_aastane_tulu <- pikk2 %>%
      group_by(aasta = as.factor(year(Kuupäev)), name) %>%
      summarize(value = round(mean(value, na.rm = TRUE)/100, 2), .groups = "drop") %>%
      mutate(value_pc = paste0(100*value, "%")) %>%
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
      scale_fill_manual("Fond:", values = c("LHVL"="#4A4E5A","LHVXL"="#222221","Tuleva"="#00aeea","inflatsioon"="#FF8C00")) +
      scale_color_manual("Fond:", values = c("LHVL"="#4A4E5A","LHVXL"="#222221","Tuleva"="#00aeea","inflatsioon"="#FF8C00")) +
      theme_ipsum_rc() + scale_y_percent() +
      labs(title="Kui palju on keskmiselt kasvanud tänaseks raha", subtitle="sõltuvalt sissepaneku aastast ja fondist", x="raha sissepaneku aasta", y="kasv tänaseks", caption=paste0("fondid seisuga ", maxdate, ", inflatsioon kuni ", inflatsioon_max, "\nallikad: Pensionikeskus, Statistikaamet\nligikaudsed arvutused: Indrek Seppo")) +
      theme(legend.position = "top")
    ggsave(p_aastane_tulu, file = "aastane_tulu_tuleva_lhv.png", height = 5, width = 7, scale = 1.1, bg = "white")
  }
  
  navid <- get_nav_data_static()
  returns <- compute_returns(navid)
  inflation <- get_inflation_data_static()
  plot_charts(returns$pikk, inflation, returns$date)
  message("--- Staatilised graafikud valmis ---")
}


#-------------------------------------------------------------------------------
# 3. OSA: ANIMEERITUD GRAAFIKU GENEREERIMINE
#-------------------------------------------------------------------------------

# Abifunktsioon GIF -> MP4 konverteerimiseks, kasutades ffmpeg
convert_gif_to_mp4 <- function(gif_file, video_file) {
  ffmpeg_path <- Sys.which("ffmpeg")
  if (ffmpeg_path == "") {
    warning("ffmpeg ei ole leitud. Videofaili ei saa luua. Palun paigalda ffmpeg ja lisa see PATH-i.")
    return(invisible(NULL))
  }
  
  message(paste("Konverteerin", gif_file, "->", video_file))
  args <- c("-y","-i", gif_file,"-movflags", "faststart","-pix_fmt", "yuv420p","-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2", video_file)
  result <- system2(ffmpeg_path, args, stdout = TRUE, stderr = TRUE)
  
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    warning("ffmpeg konverteerimine ebaõnnestus.")
    print(result)
  } else {
    message("Videofail edukalt loodud.")
  }
}

generate_animated_chart <- function() {
  message("--- Alustan animeeritud graafiku genereerimist ---")
  
  # --- Andmete laadimise funktsioonid ---
  get_nav_data_anim <- function() {
    message("Animatsioon: Laen fondide andmeid...")
    date_to <- today()
    url <- paste0("https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/?download=xls&date_from=2017-03-28&date_to=", date_to, "&f%5B0%5D=47&f%5B1%5D=38&f%5B2%5D=77&f%5B3%5D=EPI&f%5B4%5D=73")
    navid_raw <- read.csv2(url, fileEncoding = "UTF-16", header = TRUE, sep = "\t")
    navid <- navid_raw %>% select(Kuupäev, Fond, NAV) %>% pivot_wider(names_from = Fond, values_from = NAV) %>%
      filter(!is.na(`Tuleva Maailma Aktsiate Pensionifond`), !is.na(`II samba üldindeks`))
    navid$Kuupäev <- dmy(navid$Kuupäev)
    return(navid)
  }
  
  get_inflation_data_raw_anim <- function() {
    message("Animatsioon: Laen inflatsiooni andmeid...")
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
  
  # --- Animatsiooni arvutamise funktsioon ---
  arvuta_aastane_tootlus_hetkes <- function(andmete_seisuga_kp, nav_data, inflation_data) {
    navid_filt <- nav_data %>% filter(Kuupäev <= andmete_seisuga_kp)
    infl_filt <- inflation_data %>% filter(date <= andmete_seisuga_kp)
    if (nrow(navid_filt) < 2 || nrow(infl_filt) < 2) return(NULL)
    last_navs <- navid_filt %>% filter(Kuupäev == max(Kuupäev)) %>% slice(1)
    last_infl <- infl_filt %>% filter(date == max(date)) %>% pull(indeks)
    navid_kuu <- navid_filt %>% group_by(Kuupäev=floor_date(Kuupäev,"month")) %>% summarize(across(where(is.numeric),~mean(.x,na.rm=TRUE)),.groups="drop") %>%
      mutate(Tuleva=100*(last_navs$`Tuleva Maailma Aktsiate Pensionifond`/`Tuleva Maailma Aktsiate Pensionifond`-1), LHVXL=100*(last_navs$`LHV Pensionifond XL`/`LHV Pensionifond XL`-1), LHVL=100*(last_navs$`LHV Pensionifond L`/`LHV Pensionifond L`-1)) %>%
      select(Kuupäev, Tuleva, LHVXL, LHVL) %>% pivot_longer(cols=-Kuupäev, names_to="name", values_to="value")
    infl_tootlus <- infl_filt %>% mutate(value=(last_infl/indeks-1)*100) %>% select(date, value) %>% rename(Kuupäev=date) %>% mutate(name="inflatsioon")
    bind_rows(navid_kuu, infl_tootlus) %>% filter(!is.na(value), is.finite(value), year(Kuupäev)<year(andmete_seisuga_kp)) %>%
      group_by(aasta=as.character(year(Kuupäev)), name) %>% summarize(value=mean(value, na.rm=TRUE), .groups="drop") %>% mutate(seisuga_kp=andmete_seisuga_kp)
  }
  
  # --- Animatsiooni loomise plokk ---
  nav_data <- get_nav_data_anim()
  inflation_data_raw <- get_inflation_data_raw_anim()
  
  lopp_kp <- max(nav_data$Kuupäev)
  kaadrite_kuupaevad <- seq.Date(from=ymd("2018-01-01"), to=lopp_kp, by="1 month")
  animeeritud_andmed_raw <- purrr::map_df(kaadrite_kuupaevad, ~arvuta_aastane_tootlus_hetkes(.x, nav_data, inflation_data_raw))
  scaffold <- tidyr::expand_grid(seisuga_kp=kaadrite_kuupaevad, aasta=as.character(2017:(year(lopp_kp)-1)), name=c("LHVL","LHVXL","Tuleva","inflatsioon"))
  animeeritud_andmed_final <- scaffold %>% 
    left_join(animeeritud_andmed_raw, by=c("seisuga_kp","aasta","name")) %>%
    mutate(value=if_else(is.na(value),0,value), group_id=paste(aasta,name), name=factor(name,levels=c("LHVL","LHVXL","Tuleva","inflatsioon")), aasta=factor(aasta,levels=as.character(2017:(year(lopp_kp)-1))))
  
  p_anim <- ggplot(animeeritud_andmed_final, aes(x=aasta, y=value/100, fill=name, group=group_id)) +
    geom_col(position="dodge") +
    annotate("text", x = Inf, y = Inf, label = "{format(frame_time, '%Y-%m')}", hjust = 1.1, vjust = 1.5, size = 14, color = "grey75", fontface = "bold") +
    scale_fill_manual(name="Fond:", values=c("LHVL"="#4A4E5A","LHVXL"="#222221","Tuleva"="#00aeea","inflatsioon"="#FF8C00")) +
    theme_ipsum_rc() +
    scale_y_continuous(labels=scales::percent, limits=c(NA, max(animeeritud_andmed_final$value, na.rm=TRUE)*1.1/100)) + 
    theme(legend.position="top") +
    labs(title="Kui palju on keskmiselt kasvanud raha?", subtitle="Sõltuvalt sissepaneku aastast ja fondist", x="Raha sissepaneku aasta", y="Kasv", caption="Allikad: Pensionikeskus, Statistikaamet | ligikaudsed arvutused: Indrek Seppo") +
    transition_time(seisuga_kp) +
    ease_aes('linear')
  
  gif_file <- "aastane_tulu_animeeritud.gif"
  video_file <- "aastane_tulu_animeeritud.mp4"
  anim_fps <- 10
  anim_pause_sec <- 10
  nframes_total <- length(kaadrite_kuupaevad) * 10
  
  anim_save(gif_file, animation = p_anim, width = 800, height = 600, renderer = gifski_renderer(loop = TRUE), nframes = nframes_total, fps = anim_fps, end_pause = anim_pause_sec * anim_fps)
  message("Animeeritud GIF salvestatud.")
  
  convert_gif_to_mp4(gif_file, video_file)
}


#-------------------------------------------------------------------------------
# 4. OSA: PEAPROTSESS JA KÄIVITAMISE LOOGIKA
#-------------------------------------------------------------------------------

should_update_animation <- function(filepath) {
  if (!file.exists(filepath)) { message("Animatsiooni faili ei leitud. Loome uue."); return(TRUE) }
  last_mod_time <- file.mtime(filepath)
  start_of_this_week <- floor_date(today(), "week", week_start = 1)
  if (last_mod_time < start_of_this_week) { message("Animatsioon on eelmisest nädalast. Värskendame."); return(TRUE) }
  return(FALSE)
}

main <- function() {
  generate_static_charts()
  if (should_update_animation("aastane_tulu_animeeritud.gif")) {
    generate_animated_chart()
  } else {
    message("Animeeritud graafik on selle nädala seisuga värske. Jätame uuendamise vahele.")
  }
  message("Skripti töö on lõppenud.")
}

# Käivita peafunktsioon
main()