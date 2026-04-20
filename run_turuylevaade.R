# Lae vajalikud teegid
library(ssh)

run_command <- function(command, args = character(), error_label = command) {
  status <- system2(command, args = args)
  if (status != 0) {
    stop(error_label, " ebaõnnestus (kood: ", status, ")", call. = FALSE)
  }
}

require_existing_files <- function(files, label) {
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop(
      label,
      " jaoks puuduvad vajalikud failid: ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }
}

require_fresh_files <- function(files, label, earliest_time) {
  stale_files <- files[
    !file.exists(files) |
      vapply(
        files,
        function(path) file.info(path)$mtime < earliest_time,
        logical(1)
      )
  ]

  if (length(stale_files) > 0) {
    stop(
      label,
      " jaoks puuduvad värsked failid: ",
      paste(stale_files, collapse = ", "),
      call. = FALSE
    )
  }
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

# 1. Käivita kogu R-skript. See genereerib kõik vajalikud failid.
message("Käivitan NAV_kasv.R skripti failide genereerimiseks...")
run_command("Rscript", "NAV_kasv.R", error_label = "NAV_kasv.R")

# 2. Tõmbame III samba fondide nimekirja
message("Tõmban III samba fondide nimekirja...")
run_command("Rscript", "fetch_iii_sammas_fondid.R", error_label = "fetch_iii_sammas_fondid.R")

# 3. Käivita Quarto renderdamised
message("Renderdan Quarto faile...")
run_command("quarto", c("render", "turuylevaade.qmd"), error_label = "turuylevaade.qmd renderdamine")
run_command("quarto", c("render", "tuleva.qmd"), error_label = "tuleva.qmd renderdamine")
run_command("quarto", c("render", "III_sammas.qmd"), error_label = "III_sammas.qmd renderdamine")

require_fresh_files(
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

require_existing_files(
  c(
    "aastane_tulu_animeeritud.mp4"
  ),
  "üleslaadimine"
)

# 4. Lae failid üles
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
