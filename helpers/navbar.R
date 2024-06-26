## Links
link_home <-  tags$a(HTML("Contato"), href = "https://monitoramento.sepe.pe.gov.br")
link_git <- tags$a(shiny::icon("github"), href = "https://github.com/StrategicProjects", target = "_blank")
link_logout <- actionLink(inputId = "logout", icon = shiny::icon("arrow-right-from-bracket"), label = "") # , href = "https://monitoramento.sepe.pe.gov.br", target = "_self")
