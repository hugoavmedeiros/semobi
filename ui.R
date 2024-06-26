ui <-
  tagList(
    page_navbar(
      fillable = FALSE,
      lang = "pt",
      window_title = "Monitoramento &mdash; Dashboard",
      title = fn_navbar(link = "https://monitoramento.sepe.pe.gov.br",
                        class = "govpe-logo",
                        #image = "images/icon_settings.svg",
                        image = "images/markdown.png",
                        width = "50px"),
      id = "navbar",
      footer = fn_footer("Este aplicativo foi desenvolvido pela <a href='https://www.javierorracadeatcu.com'>Secretaria Executiva de Monitoramento Estratégica</a> com R + Shiny"),
      ui_Licitacoes("licitacoes"),
      ui_Sobre("about"),
      # Itens do menu à direita (Links úteis)
      nav_spacer(),
      # Links definidos no arquivo helpers/navbar.R
      nav_item(link_home),
      nav_item(link_git),
      nav_item(link_logout)
    )
  )

##### Important

secure_front(ui)