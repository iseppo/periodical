# Lae vajalikud teegid
library(ssh)

# Käivitab käsu ja tagastab TRUE/FALSE selle õnnestumise kohta.
# Tõrketaluv: ei peata pipeline'i, et üks ebaõnnestunud samm ei blokeeriks teisi.
try_command <- function(command, args = character(), error_label = command) {
  status <- tryCatch(
    system2(command, args = args),
    error = function(e) {
      message("VIGA: ", error_label, " viskas erindi: ", conditionMessage(e))
      1L
    }
  )
  if (status != 0) {
    message(
      "HOIATUS: ", error_label, " ebaõnnestus (kood: ", status,
      "), jätkan järgmiste sammudega."
    )
    return(FALSE)
  }
  message("OK: ", error_label)
  TRUE
}

warn_if_missing <- function(files, label) {
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    message(
      "HOIATUS: ", label, " — puuduvad failid: ",
      paste(missing_files, collapse = ", ")
    )
  }
  invisible(missing_files)
}

warn_if_stale <- function(files, label, earliest_time) {
  stale_files <- files[
    !file.exists(files) |
      vapply(
        files,
        function(path) file.exists(path) && file.info(path)$mtime < earliest_time,
        logical(1)
      )
  ]

  if (length(stale_files) > 0) {
    message(
      "HOIATUS: ", label,
      " — järgmised failid pole selle jooksu ajal uuenenud: ",
      paste(stale_files, collapse = ", "),
      " (laen üles olemasolevad versioonid)."
    )
  }
  invisible(stale_files)
}

is_fresh_file <- function(path, earliest_time) {
  file.exists(path) && file.info(path)$mtime >= earliest_time
}

#' Laeb üles kausta struktuuri SSH kaudu
#'
#' @param session SSH sessioon
#' @param local_dir Kohalik kaust
#' @param remote_dir Serveri kaust
upload_with_structure <- function(session, local_dir, remote_dir) {
  # Leia kõik failid ja nende suhtelised teed
  all_files <- list.files(local_dir, recursive = TRUE, full.names = FALSE)

  # 1. Loo vajalikud kaustad serveris
  dirs <- unique(dirname(all_files))
  dirs <- dirs[dirs != "."]

  for(dir in dirs) {
    cmd <- sprintf("mkdir -p '%s/%s/%s'", remote_dir, basename(local_dir), dir)
    ssh_exec_wait(session, command = cmd)
  }

  # 2. Lae failid üles õigetesse kohtadesse
  for(file in all_files) {
    local_file <- file.path(local_dir, file)
    remote_file <- file.path(remote_dir, basename(local_dir), file)

    scp_upload(session,
               files = local_file,
               to = dirname(remote_file),
               verbose = TRUE)
  }
}

pipeline_start <- Sys.time()

# Kogume sammude tulemused kokku. Iga samm käivitub sõltumata sellest, kas
# eelmine õnnestus — nii ei blokeeri üks ebaõnnestunud raport teisi ega
# üleslaadimist. Kui midagi läks valesti, lõpetame veakoodiga, aga alles
# PÄRAST üleslaadimist (vt skripti lõpp).
samm_tulemused <- logical(0)

# 1. Käivita kogu R-skript. See genereerib graafikud ja animatsiooni.
message("Käivitan NAV_kasv.R skripti failide genereerimiseks...")
samm_tulemused["NAV_kasv.R"] <-
  try_command("Rscript", "NAV_kasv.R", error_label = "NAV_kasv.R")

# 2. Tõmbame III samba fondide nimekirja
message("Tõmban III samba fondide nimekirja...")
samm_tulemused["fetch_iii_sammas_fondid.R"] <-
  try_command("Rscript", "fetch_iii_sammas_fondid.R", error_label = "fetch_iii_sammas_fondid.R")

# 3. Käivita Quarto renderdamised (iga raport eraldi, üksteisest sõltumatult)
message("Renderdan Quarto faile...")
samm_tulemused["turuylevaade.qmd"] <-
  try_command("quarto", c("render", "turuylevaade.qmd"), error_label = "turuylevaade.qmd renderdamine")
samm_tulemused["tuleva.qmd"] <-
  try_command("quarto", c("render", "tuleva.qmd"), error_label = "tuleva.qmd renderdamine")
samm_tulemused["III_sammas.qmd"] <-
  try_command("quarto", c("render", "III_sammas.qmd"), error_label = "III_sammas.qmd renderdamine")

warn_if_stale(
  c(
    "aastane_tulu_tuleva_lhv.png",
    "kihlvedu.png",
    "tuleva.html",
    "turuylevaade.html",
    "III_sammas.html",
    "koguturg_indeksfondid_osakaal.csv",
    "koguturg_koguinfo.csv",
    "iii_sammas_koguturg.csv",
    "iii_sammas_ajalugu.csv",
    "iii_sammas_investorid_ajalugu.csv"
  ),
  "üleslaadimine",
  pipeline_start
)

warn_if_missing(
  c(
    "aastane_tulu_animeeritud.mp4"
  ),
  "üleslaadimine"
)

# 4. Lae failid üles.
# CI märgib SKIP_UPLOAD=1, kui serveri sõrmejälg puudub (ssh-keyscan kukkus ega
# pole cache'is — vt main.yml fingerprint samm). Sel juhul jätame SSH-ploki
# PUHTALT vahele: raportid on genereeritud ja GitHub Pages avaldab need, seega
# üleslaadimise vahelejätt ei tohi märkida analüüsi ebaõnnestunuks.
if (identical(Sys.getenv("SKIP_UPLOAD"), "1")) {
  message(
    "SKIP_UPLOAD=1 — serveri sõrmejälg puudub, jätan SCP-üleslaadimise vahele. ",
    "Raportid on genereeritud ja GitHub Pages avaldab need."
  )
} else {
message("Alustan failide üleslaadimist SSH kaudu...")
tryCatch({
  # Loo SSH ühendus
  # NB: seppo.ai peab olema ~/.ssh/known_hosts failis
  session <- ssh_connect("virt135256@seppo.ai")
  message("SSH ühendus loodud.")

  # Lae üles kihlveod failid
  kihlveod_failid <- c("./aastane_tulu_tuleva_lhv.png",
                        "./tuleva.html",
                        "./aastane_tulu_animeeritud.mp4",
                        "./kihlvedu.png")
  # Lisa ekraanipildid ainult kui eksisteerivad
  if (file.exists("ekspress_screenshot.png")) kihlveod_failid <- c(kihlveod_failid, "ekspress_screenshot.png")
  if (file.exists("fb_screenshot.png")) kihlveod_failid <- c(kihlveod_failid, "fb_screenshot.png")

  # Filtreerime välja failid, mis tegelikult eksisteerivad
  kihlveod_failid <- kihlveod_failid[file.exists(kihlveod_failid)]

  if (length(kihlveod_failid) > 0) {
    scp_upload(session,
               files = kihlveod_failid,
               to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/",
               verbose = FALSE)
    message("Kihlveod failid üles laetud: ", length(kihlveod_failid), " faili")
  }

  # Lae üles tuleva_files kaust (kui eksisteerib)
  if (dir.exists("tuleva_files")) {
    upload_with_structure(
      session,
      local_dir = "tuleva_files",
      remote_dir = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod"
    )
  } else {
    message("tuleva_files kaust puudub, vahelejätmine.")
  }

  # Lae üles tuleva/ failid
  tuleva_failid <- c("./turuylevaade.html",
                      "./koguturg_indeksfondid_osakaal.csv",
                      "./koguturg_koguinfo.csv")
  tuleva_failid <- tuleva_failid[file.exists(tuleva_failid)]

  if (length(tuleva_failid) > 0) {
    scp_upload(session,
               files = tuleva_failid,
               to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/",
               verbose = FALSE)
  }

  # Lae üles III samba failid (kui eksisteerivad)
  iii_failid <- c("./III_sammas.html",
                   "./iii_sammas_koguturg.csv",
                   "./iii_sammas_ajalugu.csv",
                   "./iii_sammas_investorid_ajalugu.csv")
  if (is_fresh_file("iii_sammas_top_fondid.csv", pipeline_start)) {
    iii_failid <- c(iii_failid, "./iii_sammas_top_fondid.csv")
  }
  iii_failid <- iii_failid[file.exists(iii_failid)]

  if (length(iii_failid) > 0) {
    scp_upload(session,
               files = iii_failid,
               to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/",
               verbose = FALSE)
    message("III samba failid üles laetud: ", length(iii_failid), " faili")
  }

  message("Kõik failid on edukalt üles laetud.")

}, error = function(e) {
  # Veakäsitlus
  message("SSH toiming ebaõnnestus: ", e$message)
  stop("SSH toiming ebaõnnestus, skript peatatakse.", call. = FALSE)
}, finally = {
  # Ühenduse sulgemine
  if (exists("session") && inherits(session, "ssh_session")) {
    ssh_disconnect(session)
    message("SSH ühendus suletud.")
  }
})
}

# Kui mõni genereerimise/renderdamise samm ebaõnnestus, lõpetame veakoodiga,
# et CI märgiks jooksu punaseks. See toimub alles pärast üleslaadimist, nii et
# õnnestunud raportid jõuavad ikkagi serverisse.
if (any(!samm_tulemused)) {
  ebaõnnestunud <- names(samm_tulemused)[!samm_tulemused]
  stop(
    "Pipeline lõpetatud, kuid järgmised sammud ebaõnnestusid: ",
    paste(ebaõnnestunud, collapse = ", "),
    call. = FALSE
  )
}

message("Pipeline edukalt lõpetatud — kõik sammud õnnestusid.")
