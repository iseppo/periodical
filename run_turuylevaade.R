# Lae vajalikud teegid
library(ssh)

# Käivita skriptid ja genereeri failid enne SSH-toiminguid
source("NAV_kasv.R")
generate_nav_charts()
system("quarto render turuylevaade.qmd")
system("quarto render tuleva.qmd")

# Üks tryCatch plokk kõigi SSH toimingute jaoks
tryCatch({
  # Loo SSH ühendus
  session <- ssh_connect("virt135256@seppo.ai")
  
  message("SSH ühendus loodud.")
  
  # Lae üles esimene fail
  scp_upload(session, 
             "./aastane_tulu_tuleva_lhv.png", 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/",
             verbose = FALSE)
  
  # Lae üles mitu faili korraga 'tuleva' kausta
  scp_upload(session, 
             files = c("./turuylevaade.html", 
                       "./koguturg_indeksfondid_osakaal.csv", 
                       "./koguturg_koguinfo.csv"), 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/",
             verbose = FALSE)
  

  # Lae üles viimane fail
  scp_upload(session, 
             "./tuleva.html", 
             to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/",
             verbose = FALSE)
  
  message("Kõik failid on edukalt üles laetud.")
  
}, error = function(e) {
  # See plokk käivitub, kui mõni ülaltoodud SSH toiming ebaõnnestub
  message("SSH toiming ebaõnnestus: ", e$message)
  stop("SSH toiming ebaõnnestus, skript peatatakse.", call. = FALSE)
}, finally = {
  # see käivitatakse nii õnnestumisel kui veal
  if (exists("session") && inherits(session, "ssh_session")) {
    ssh_disconnect(session)
    message("SSH ühendus suletud.")
  }
})