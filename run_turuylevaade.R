# Lae vajalikud teegid
library(ssh)

# 1. Käivita kogu R-skript. See genereerib kõik vajalikud failid.
#    Kuna 'NAV_kasv.R' lõpus on 'main()', käivitub see automaatselt.
message("Käivitan NAV_kasv.R skripti failide genereerimiseks...")
source("NAV_kasv.R")

# 2. Käivita Quarto renderdamised
message("Renderdan Quarto faile...")
system("quarto render turuylevaade.qmd")
system("quarto render tuleva.qmd")

# 3. Lae failid üles
message("Alustan failide üleslaadimist SSH kaudu...")
tryCatch({
  # Loo SSH ühendus
  session <- ssh_connect("virt135256@seppo.ai")
  message("SSH ühendus loodud.")
  
  # Lae üles failid
  # Lisasin ka animeeritud GIF-i siia nimekirja
  scp_upload(session, 
             files = c("./aastane_tulu_tuleva_lhv.png", "./tuleva.html", "./aastane_tulu_animeeritud.mp4", "ekspress_screenshot.png", "fb_screenshot.png", "kihlvedu.png"), 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/",
             verbose = FALSE)
  
  upload_with_structure <- function(session, local_dir, remote_dir) {
    # Leia kõik failid ja nende suhtelised teed
    all_files <- list.files(local_dir, recursive = TRUE, full.names = FALSE)
    
    # 1. Loo vajalikud kaustad serveris
    dirs <- unique(dirname(all_files))
    dirs <- dirs[dirs != "."]
    
    for(dir in dirs) {
      cmd <- sprintf("mkdir -p %s/%s/%s", remote_dir, basename(local_dir), dir)
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
  
  # Kasutamine
  upload_with_structure(
    session, 
    local_dir = "tuleva_files",
    remote_dir = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod"
  )
  
  
  
  scp_upload(session, 
             files = c("./turuylevaade.html", "./koguturg_indeksfondid_osakaal.csv", "./koguturg_koguinfo.csv"), 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/",
             verbose = FALSE)
  
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
