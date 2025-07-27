library(ssh)

source("NAV_kasv.R")

system("quarto render turuylevaade.qmd")

session <- tryCatch(
  ssh_connect("virt135256@seppo.ai"),
  error = function(e) {
    message("Failed to establish SSH connection: ", e$message)
    stop("SSH connection failed", call. = FALSE)
  }
)
on.exit(ssh_disconnect(session))

tryCatch(
  scp_upload(session, "./aastane_tulu_tuleva_lhv.png", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/"),
  error = function(e) {
    message("Failed to upload aastane_tulu_tuleva_lhv.png: ", e$message)
    stop("Upload failed", call. = FALSE)
  }
)

tryCatch(
  scp_upload(session, "./turuylevaade.html", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/"),
  error = function(e) {
    message("Failed to upload turuylevaade.html: ", e$message)
    stop("Upload failed", call. = FALSE)
  }
)
tryCatch(
  scp_upload(session, "./koguturg_indeksfondid_osakaal.csv", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/"),
  error = function(e) {
    message("Failed to upload koguturg_indeksfondid_osakaal.csv: ", e$message)
    stop("Upload failed", call. = FALSE)
  }
)
tryCatch(
  scp_upload(session, "./koguturg_koguinfo.csv", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/"),
  error = function(e) {
    message("Failed to upload koguturg_koguinfo.csv: ", e$message)
    stop("Upload failed", call. = FALSE)
  }
)

system("quarto render tuleva.qmd")

tryCatch(
  scp_upload(session, "./tuleva.html", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/"),
  error = function(e) {
    message("Failed to upload tuleva.html: ", e$message)
    stop("Upload failed", call. = FALSE)
  }
)



ssh_disconnect(session)

