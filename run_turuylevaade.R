library(ssh)

source("NAV_kasv.R")
generate_nav_charts()

system("quarto render turuylevaade.qmd")

session <- ssh_connect("virt135256@seppo.ai")

scp_upload(session, "./aastane_tulu_tuleva_lhv.png", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/")

scp_upload(session, "./turuylevaade.html", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/")
scp_upload(session, "./koguturg_indeksfondid_osakaal.csv", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/")
scp_upload(session, "./koguturg_koguinfo.csv", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/tuleva/")

system("quarto render tuleva.qmd")

scp_upload(session, "./tuleva.html", to = "/data03/virt135256/domeenid/www.seppo.ai/htdocs/kihlveod/")



ssh_disconnect(session)

