# Lae vajalikud teegid
library(ssh)

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

# 1. Käivita kogu R-skript. See genereerib kõik vajalikud failid.
#    Kuna 'NAV_kasv.R' lõpus on 'main()', käivitub see automaatselt.
message("Käivitan NAV_kasv.R skripti failide genereerimiseks...")
tryCatch(source("NAV_kasv.R"), error = function(e) {
  message("NAV_kasv.R ebaõnnestus: ", e$message, " — jätkan...")
})

# 2. Tõmbame III samba fondide nimekirja
message("Tõmban III samba fondide nimekirja...")
tryCatch(source("fetch_iii_sammas_fondid.R"), error = function(e) {
  message("fetch_iii_sammas_fondid.R ebaõnnestus: ", e$message, " — jätkan...")
})

# 3. Käivita Quarto renderdamised (kontrollime tagastusväärtust)
message("Renderdan Quarto faile...")

ret <- system("quarto render turuylevaade.qmd")
if (ret != 0) warning("turuylevaade.qmd renderdamine ebaõnnestus (kood: ", ret, ")")

ret <- system("quarto render tuleva.qmd")
if (ret != 0) warning("tuleva.qmd renderdamine ebaõnnestus (kood: ", ret, ")")

ret <- system("quarto render III_sammas.qmd")
if (ret != 0) warning("III_sammas.qmd renderdamine ebaõnnestus (kood: ", ret, ")")

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
                   "./iii_sammas_top_fondid.csv",
                   "./iii_sammas_ajalugu.csv",
                   "./iii_sammas_investorid_ajalugu.csv")
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
