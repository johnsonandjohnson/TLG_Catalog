## Deploy to idarconnect
server <- 'idarconnect.jnj.com'

rsconnect::deployApp(
  appDir = "_book",
  #appName = "jjcs_tlgs_r_catalog",
  appId    = 959,
  #quarto = FALSE,
  forceUpdate = FALSE,
  appMode = "static",
  server = server,
  account  = rsconnect::accounts(server)$name
)