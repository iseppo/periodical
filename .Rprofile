source("renv/activate.R")
# Posit PPM binaarrepo valitakse jooksva Linux-distro järgi, et binaarpaketid
# (nt stringi, mis lingib ICU teegi vastu) sobiksid süsteemiga. Varem oli siin
# kõvasti "bookworm", mis lõhkus lokaalse arenduse mitte-bookworm masinatel.
# Tundmatu või mitte-Linux süsteem -> lähtekoodirepo (OS-agnostiline, kompileerib).
local({
  codename <- tryCatch({
    lines <- readLines("/etc/os-release", warn = FALSE)
    hit <- grep("^VERSION_CODENAME=", lines, value = TRUE)
    if (length(hit) == 0) NA_character_
    else gsub('"', "", sub("^VERSION_CODENAME=", "", hit[1]))
  }, error = function(e) NA_character_)

  known <- c("noble", "jammy", "focal", "bookworm", "bullseye", "trixie")
  repo <- if (isTRUE(codename %in% known)) {
    sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename)
  } else {
    "https://packagemanager.posit.co/cran/latest"
  }
  options(repos = c(CRAN = repo))
})
# NB: OutDec on viidud skriptidesse (NAV_kasv.R + .qmd setup-chunkidesse), sest
# CI lülitab .Rprofile välja — nii on väljund local ja CI vahel ühtne.
