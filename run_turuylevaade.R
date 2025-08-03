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
             files = c("./aastane_tulu_tuleva_lhv.png", "./tuleva.html", "./aastane_tulu_animeeritud.mp4"), 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/",
             verbose = FALSE)
  
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