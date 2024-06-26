function(input, output, session) {
  #### auth prod ----
  auth <- secure_server(
    check_credentials = vialactea::check_creds(sepe_datalake2, key, session)
  )
  
  #### auth dev ----
  # auth <- secure_server(
  #   check_credentials = vialactea::check_creds(sepe_datalake, key, session)
  # )
  
  # Avisa se o aplicativo se encontra na "Autenticação" ou "Aplicação"
  observe({
    message(input$shinymanager_where)
  })
  
  rct_js_licitacoes <- reactive({
    input$js_licitacoes %>% as.data.frame()
  })
  
  # rct_js_portfolio <- reactive({
  #   input$js_portfolio %>% as.data.frame()
  # })
  # 
  # rct_js_obras <- reactive({
  #   input$js_obras %>% as.data.frame()
  # })
  
  server_Licitacoes("licitacoes", auth$user, rct_js_licitacoes)
  
  observe({
    if (input$shinymanager_where == "application" && !is.null(auth$user)) {
      message("Usuário autenticado: ", auth$user)
      showModal(modalDialog(
        title = "Acesso autorizado",
        paste0("Olá, ", auth$user, ", seja bem-vindo(a)."),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$logout, {
    session$reload()
  })
  
  session$onSessionEnded(function() {
    message("Sessão encerrada.")
  })
  
}
