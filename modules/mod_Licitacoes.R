lista_local <- c("DER", "DNIT", "SAD", "CAIXA", "PGE", "SEMOBI")

ui_Licitacoes <- function(id) {
  ns <- NS(id)
  nav_panel(
    spsGoTop(
      "default", 
      right = "3%",  
      bottom= "3%", 
      icon = icon("arrow-up"), 
      color = "#593196"),
    title = "Licitações",
      card(
        card_header(
          class = "d-flex justify-content-between",
          div(
            class = "d-flex align-items-center",
            actionButton(
              ns("toggle_mode"), 
              label = NULL,
              class = "btn btn-primary btn-circle",
              icon = icon("toggles"),
              style = "margin-right: 10px;"  
            ) %>% tooltip("Modo de visualização")
          ),
          div(
            class = "d-flex align-items-center",
            actionButton(
              ns("modal_add_licitacao"), 
              label = NULL, 
              class = "btn btn-primary btn-circle",
              icon = icon("plus"),
              style = "margin-right: 10px;"
            ) %>% tooltip("Adicionar licitação"),
            downloadButton(
              ns("downloadData"),
              NULL,
              class = "btn btn-primary btn-circle",
              icon = icon('download')
            ) %>% tooltip("Baixar dados")
          )
        ),
        card_body(
          shinycssloaders::withSpinner(
            uiOutput(ns("output_mode")),
            type = 8,
            color = '#5bc0de',
            size = 2
          )
        ),
        full_screen = TRUE
      )
    )
}

server_Licitacoes <- function(id, user, rct_js_licitacoes) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      dbtrigger <- makereactivetrigger()
      
      #### download ----
      ##### observe ----
      # observeEvent(input$js_licitacoes, {
      #   runjs("$('#downloadData')[0].click();")
      # })
      
      ##### btn ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("licitacoes_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          openxlsx::write.xlsx(rct_js_licitacoes(), file)
          #openxlsx::write.xlsx(input$js_licitacoes, file)
        }
      )
      
      #### reactive ----
      ##### licitações ----
      last_licitacoes <- reactiveVal(NULL)
      
      load_licitacoes <- function() {
        dbGetQuery(con_base, "SELECT * FROM tbl_semobi_licitacao WHERE excluir_flag = 0")
      }
      
      check_licitacoes <- function() {
        query <- "SELECT MAX(updated_at) AS max_id FROM tbl_semobi_licitacao"
        current_version <- dbGetQuery(con_base, query)$max_id
        
        if (is.null(last_licitacoes())) {
          last_licitacoes(current_version)
          return(TRUE)
        } else if (current_version > last_licitacoes()) {
          last_licitacoes(current_version)
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
      
      react_values_licitacoes <- reactivePoll(
        intervalMillis = 2000,  
        session = session,
        checkFunc = check_licitacoes,
        valueFunc = function() {
          load_licitacoes()
        }
      )
      
      ##### transações ----
      last_transacao <- reactiveVal(NULL)
      
      load_transacao <- function() {
        dbGetQuery(con_base, "SELECT * FROM tbl_semobi_licitacao_transacao WHERE excluir_flag = 0")
      }
      
      check_transacao <- function() {
        query <- "SELECT MAX(updated_at) AS max_id FROM tbl_semobi_licitacao_transacao"
        current_version <- dbGetQuery(con_base, query)$max_id
        
        if (is.null(last_licitacoes())) {
          last_licitacoes(current_version)
          return(TRUE)
        } else if (current_version > last_licitacoes()) {
          last_licitacoes(current_version)
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
      
      react_transacoes <- reactivePoll(
        intervalMillis = 2000,  
        session = session,
        checkFunc = check_transacao,
        valueFunc = function() {
          load_transacao()
        }
      )
      
      ##### local ----
      react_values_local <- reactiveValues(
        db = tbl_der_licitacoes_local %>% collect())
      
      ##### status ----
      react_values_status <- reactiveValues(
        db = tbl_der_licitacoes_status %>% collect())
      
      ##### prioridade ----
      react_values_prioridade <- reactiveValues(
        db = tbl_der_licitacoes_prioridade %>% collect())
      
      ##### objeto ----
      react_values_objeto <- reactiveValues(
        db = tbl_der_licitacoes_objeto %>% collect())
      
      ##### responsável ----
      react_values_responsavel <- reactiveValues(
        db = tbl_der_licitacoes_responsavel %>% collect())
      
      #### textos ----
      ##### bem vindo ----
      output$txt_bemvindo <- renderText({
        paste0(
          'Bem vindo(a), ',
          user
        )
      })
      
      #### render ui ----
      ##### cards x reactable licitações ----
      output$output_mode <- renderUI({
        
        #print(input$js_dat)
        
        if (modo() == "Cards") {
          
          dados <- react_values_licitacoes()
          
          gerar_cards(dados, 'objeto')
          
          
        } else if (modo() == "Tabela") {
          
          reactableOutput(ns("tbl_licitacoes"))
          
        }
      })
      
      ##### tbl ----
      output$tbl_licitacoes <- renderReactable({
        
        ###### conjunto principal ----
        dados1 <- react_values_licitacoes()  %>% 
          mutate(
            btn_editar = row_number(),
            btn_card = row_number(),
            btn_trans = row_number()
          ) %>% 
          relocate(
            btn_card, btn_editar, btn_trans, .before = id
          )
        
        ###### conjunto secundário ----
        dados2 <- react_transacoes() 
        
        ###### rectable principal ----
        tbl <- reactable(
          data = dados1,
          #elementId = "licitacoes-tbl_licitacoes",
          theme = reactableTheme(
            cellStyle = list(
              display = "flex",
              flexDirection = "column", 
              justifyContent = "center")
          ),
          language = br_react,
          fullWidth = TRUE,
          pagination = TRUE,
          defaultPageSize = 3,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(1, 3, 5, 10, 20, 50, 100),
          defaultSorted = c("objeto"),
          #defaultSortOrder = "desc",
          striped = TRUE,
          filterable = TRUE,
          searchable = F, 
          outlined = TRUE,
          highlight = TRUE,
          compact = TRUE,
          showSortable = FALSE,
          height = 400,
          style = list(
            fontSize = '80%'),
          defaultColDef = colDef(
            align = "center",
            headerVAlign = "center",
            na = '-'
          ),
          columns = list(
            id = colDef(
              show = F
            ),
            descricao = colDef(
              show = F
            ),
            observacao = colDef(
              show = F
            ),
            usuario = colDef(
              show = F
            ),
            excluir_flag = colDef(
              show = F
            ),
            created_at = colDef(
              show = F
            ),
            updated_at = colDef(
              show = F
            ),
            btn_editar = colDef(
              name = "", 
              width = 30,
              filterable = FALSE,
              sortable = FALSE,
              cell = function(value, index) {
                attr <- "bg-primary"
                btn <- htmltools::tags$button(
                  shiny::icon("user-pen"),
                  style = 
                    "width:22px; 
                      height:22px; 
                      padding: 0; 
                      margin: 0", 
                  class = 'btn btn-primary btn-circle'
                ) %>% 
                  bsplus::bs_embed_tooltip(
                    title = "Editar",
                    placement = 'right')
                return(btn)
              }
            ),
            btn_card = colDef(
              name = "", 
              width = 30,
              filterable = FALSE,
              sortable = FALSE,
              cell = function(value, index) {
                attr <- "bg-primary"
                btn <- htmltools::tags$button(
                  shiny::icon("circle-info"),
                  style = 
                    "width:22px; 
                    height:22px; 
                    padding: 0; 
                    margin: 0", 
                  class = 'btn btn-primary btn-circle'
                ) %>% bsplus::bs_embed_tooltip(title = "Detalhamento")
                return(btn)
              }
            ),
            btn_trans = colDef(
              name = "", 
              width = 30,
              filterable = FALSE,
              sortable = FALSE,
              cell = function(value, index) {
                attr <- "bg-primary"
                btn <- 
                  htmltools::tags$button(
                      icon("plus"),
                      style = 
                        "width:22px; height:22px; padding: 0; margin: 0", 
                      class = 'btn btn-primary btn-circle'
                ) %>% bsplus::bs_embed_tooltip(title = "Adicionar movimentação")
                return(btn)
              }
            )
          ),
          onClick = JS(
            sprintf(
              "function(rowInfo, column) {
    if ((column.id !== 'btn_editar') && (column.id !== 'btn_card') && (column.id !== 'btn_trans')) {return}
    if (column.id === 'btn_editar') {
      if (window.Shiny) {
        Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
      }
    }
    if (column.id === 'btn_card') {
      if (window.Shiny) {
        Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
      }
    }
    if (column.id === 'btn_trans') {
      if (window.Shiny) {
        Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
      }
    }
  }", ns("dtls_licitacao"), ns("vsl_licitacao"), ns("trans_licitacao"))
          ),
          details = function(index) {
            sec_lvl = dados2[dados2$id_licitacao == dados1$id[index], ]
            reactable(
              data = sec_lvl,
              compact    = TRUE,
              filterable = TRUE,
              bordered   = TRUE,
              resizable  = TRUE,
              style = list(
                fontSize = '14px'),
              columns = list(
                id = colDef(
                  show = F),
                id_licitacao = colDef(
                  show = F),
                usuario = colDef(
                  show = F),
                excluir_flag = colDef(
                  show = F),
                created_at = colDef(
                  show = F),
                updated_at = colDef(
                  show = F)
                ) # columns
            )
          }
        ) # fecha reactable
        
        htmlwidgets::onRender(tbl, 
                              "() => {
      Reactable.onStateChange('licitacoes-tbl_licitacoes', state => {
        var state = Reactable.getState('licitacoes-tbl_licitacoes');
        Shiny.setInputValue('js_licitacoes:xx', state.sortedData);
        console.log('input-criada');
      })
    }")
        
      })
      
      #### observe ----
      ##### modo ----
      modo <- reactiveVal("Cards")
      
      ##### alternador ----
      observeEvent(input$toggle_mode, {
        if (modo() == "Cards") {
          modo("Tabela")
        } else {
          modo("Cards")
        }
      })
      
      ##### atualizar btn ----
      observe({
        if (modo() == "Cards") {
          updateActionButton(
            session, 
            "toggle_mode", 
            label = NULL,
            icon = icon('toggle-off'))
        } else if (modo() == "Tabela") {
          updateActionButton(
            session, 
            "toggle_mode",
            label = NULL,
            icon = icon('toggle-on'))
        }
      })
      
      ##### modal: cancelar ----
      observeEvent(input$cancelar_modal, {
        removeModal()
      })
      
      ##### modal: btn cancelar ----
      observeEvent(input$btn_cancelar, {
        removeModal()
      })
      
      #### crud ----
      ##### add licitação ----
      ###### form add licitação ----
      observeEvent(input$modal_add_licitacao, {
        
        if (user %in% editores) {
        
        showModal(
          modalDialog(
            size = 'xl',
            easyClose = TRUE,
            fade = TRUE,
            title = "Nova Licitação",
            layout_column_wrap(
              width = 1/4,
              textInput(
                ns("objeto"), 
                label = tags$b("Objeto"), 
                placeholder = "Objeto de forma resumida", 
                width = "100%"),
              textInput(
                ns("subobjeto"), 
                label = tags$b("Subobjeto"), 
                placeholder = "Subobjeto de forma resumida", 
                width = "100%"),
              textAreaInput(
                ns("descricao"), 
                label = tags$b("Descrição"), 
                placeholder = "Detalhamento do objeto", 
                width = "100%",
                rows = 3, 
                resize = "none"),
              textInput(
                ns("prioridade"), 
                label = tags$b("Prioridade"), 
                placeholder = "Prioridade da licitação", 
                width = "100%"),
              textInput(
                ns("encaminhamento"), 
                label = tags$b("Encaminhamento"), 
                placeholder = "Encaminhamento atual", 
                width = "100%"),
              textInput(
                ns("observacao"), 
                label = tags$b("Observação"), 
                placeholder = "Encaminhamento atual", 
                width = "100%"),
              textInput(
                ns("subacao"), 
                label = tags$b("Subação"), 
                placeholder = "Informa a subação", 
                width = "100%"),
              selectInput(
                ns("status"), 
                label = tags$b("Status"),
                choices = react_values_status$db$status,
                width = "100%"),
              textInput(
                ns("sei"), 
                label = tags$b("Processo SEI"), 
                placeholder = "Número do SEI", 
                width = "100%"),
              selectInput(
                ns("responsavel"), 
                label = tags$b("Responsável"),
                choices = react_values_responsavel$db$responsavel,
                width = "100%"),
              selectInput(
                ns("matriz_risco"), 
                label = tags$b("Matriz de Risco?"),
                choices = c("NÃO", 'SIM'),
                width = "100%"),
              numericInput(
                ns("valor"), 
                label = tags$b("Valor da Licitação"),
                1,
                step = 10000
              ),
              numericInput(
                ns("valor_proposta"), 
                label = tags$b("Valor do Vencedor"),
                1,
                step = 10000
              )
            ),
            footer = tagList(
              actionButton(
                ns("clear_modal"), 
                label = "Limpar",
                width = '20%', 
                class = "m-1 btn-info"),
              actionButton(
                ns("confirmar_modal"), 
                label = "Confirmar",
                width = '20%', 
                class = "m-1 btn-success"),
              actionButton(
                ns("cancelar_modal"), 
                label = "Cancelar",
                width = '20%', 
                class = "m-1 btn-danger")
            )
          )
        )
          
        } else {
          showModal(
            modalDialog(
              easyClose = TRUE,
              fade = TRUE,
              tagList(
                tags$b("Adição não habilitada")
              ),
              footer = tagList(
                actionButton(
                  ns("btn_cancelar"), 
                  label = "Cancelar",
                  width = '20%', 
                  class = "m-1 btn-danger")
              )
            )
          )
        }
          
      })
      
      ###### btn add licitação ----
      observeEvent(input$confirmar_modal, {
        
        # nova base de dados
        new_data <- tibble(
          objeto = input$objeto,
          subobjeto = input$subobjeto,
          descricao = input$descricao,
          prioridade = input$prioridade,
          encaminhamento = input$encaminhamento,
          observacao = input$observacao,
          subacao = input$subacao,
          status = input$status,
          sei = input$sei,
          responsavel = input$responsavel,
          matriz_risco = input$matriz_risco,
          valor = input$valor,
          valor_proposta = input$valor_proposta,
          excluir_flag = 0
        )
        
        glimpse(new_data)
        
        # comando sql
        new_data_sql <- paste0(
          "INSERT INTO tbl_semobi_licitacao (",
          "\"objeto\", \"subobjeto\", \"descricao\", \"prioridade\", \"encaminhamento\", \"observacao\", \"subacao\", \"status\", \"sei\", ",
          "\"responsavel\", \"matriz_risco\", \"valor\", \"valor_proposta\", \"excluir_flag\", \"usuario\", \"updated_at\") VALUES ('",
          new_data$objeto, "', '",
          new_data$objeto, "', '",
          new_data$descricao, "', '",
          new_data$prioridade, "', '",
          new_data$encaminhamento, "', '",
          new_data$observacao, "', '",
          new_data$subacao, "', '", 
          new_data$status, "', '",
          new_data$sei, "', '",
          new_data$responsavel, "', '",
          new_data$matriz_risco, "', '",
          new_data$valor, "', '",
          new_data$valor_proposta, "', '",
          new_data$excluir_flag, "', '",
          user, "', CURRENT_TIMESTAMP);"
        )
        
        conn <- poolCheckout(con_base)
        
        # commit
        result <- tryCatch({
          dbWithTransaction(conn, {
            dbExecute(conn, new_data_sql)
          })
        }, error = function(err) {
          err
        })
        
        # mensagem de erro / sucesso
        if ( inherits(result, 'error') ) {
          showNotification(result$message)
        } else {
          dbtrigger$trigger()
          removeModal()
          showNotification("Nova licitação cadastrada com sucesso.")
        }
        ##
        
        poolReturn(conn)
        
      })
      
      ##### add transação ----
      ###### form add transação ----
      observeEvent(input$trans_licitacao, {
        
        if (user %in% editores) {
          
          licitacao <- react_values_licitacoes() %>% 
            filter(
              row_number() == as.integer(input$trans_licitacao)
              )
          
          showModal(
            modalDialog(
              size = 'xl',
              easyClose = TRUE,
              fade = TRUE,
              title = "Nova Transação",
              layout_column_wrap(
                width = 1/4,
                radioButtons(
                  ns("local"), 
                  label = tags$b("Local"),
                  choices = lista_local,
                  width = "100%"),
                dateInput(
                  inputId = ns("data"),
                  label = tags$b("Data"),
                  value = lubridate::today() + years(),
                  min = lubridate::today() + days(),
                  max = NULL,
                  format = "dd/mm/yyyy",
                  language = "pt-BR"
                )
              ),
              footer = tagList(
                actionButton(
                  ns("clear_modal"), 
                  label = "Limpar",
                  width = '20%', 
                  class = "m-1 btn-info"),
                actionButton(
                  ns("confirmar_transacao"), 
                  label = "Confirmar",
                  width = '20%', 
                  class = "m-1 btn-success"),
                actionButton(
                  ns("cancelar_modal"), 
                  label = "Cancelar",
                  width = '20%', 
                  class = "m-1 btn-danger")
              )
            )
          )
          
        } else {
          showModal(
            modalDialog(
              easyClose = TRUE,
              fade = TRUE,
              tagList(
                tags$b("Adição não habilitada")
              ),
              footer = tagList(
                actionButton(
                  ns("btn_cancelar"), 
                  label = "Cancelar",
                  width = '20%', 
                  class = "m-1 btn-danger")
              )
            )
          )
        }
        
      })
      
      ###### btn add transação ----
      observeEvent(input$confirmar_transacao, {
        
        licitacao <- react_values_licitacoes() %>% 
          filter(
            row_number() == as.integer(input$trans_licitacao)
          )
        
        new_data <- tibble(
          id_licitacao = licitacao$id,
          local = input$local,
          data = handle_date(input$data),
          excluir_flag = 0
        )
        
        glimpse(new_data)
        
        # comando sql
        new_data_sql <- paste0(
          "INSERT INTO tbl_semobi_licitacao_transacao (",
          "\"id_licitacao\", \"local\", \"data\", \"excluir_flag\", \"usuario\", \"updated_at\") VALUES ('", new_data$id_licitacao , "', 
          '",new_data$local, "', 
          ", handle_null(new_data$data), ", 
          '",new_data$excluir_flag, "', '",
          user, "', CURRENT_TIMESTAMP);"
        )
        
        conn <- poolCheckout(con_base)
        
        # commit
        result <- tryCatch({
          dbWithTransaction(conn, {
            dbExecute(conn, new_data_sql)
          })
        }, error = function(err) {
          err
        })
        
        # mensagem de erro / sucesso
        if ( inherits(result, 'error') ) {
          showNotification(result$message)
        } else {
          dbtrigger$trigger()
          removeModal()
          showNotification("Nova transação cadastrada com sucesso.")
        }
        ##
        
        poolReturn(conn)
        
      })
      
      ##### edit licitação ----
      ###### form edit licitação ----
      observeEvent(input$dtls_licitacao, {
        
        # cria a tbl da linha selecionada
        licitacao <- react_values_licitacoes() %>% 
          filter(row_number() == as.integer(input$dtls_licitacao)) 
        
        modal_corpo <- if (user %in% editores) {
          layout_column_wrap(
            width = 1/4,
            textInput(
              ns("objeto_edt"), 
              label = tags$b("Objeto"), 
              placeholder = "Objeto de forma resumida",
              value = licitacao$objeto,
              width = "100%"),
            textInput(
              ns("subobjeto_edt"), 
              label = tags$b("Subobjeto"), 
              placeholder = "Subobjeto de forma resumida",
              value = licitacao$subobjeto,
              width = "100%"),
            textAreaInput(
              ns("descricao_edt"), 
              label = tags$b("Descrição"), 
              placeholder = "Detalhamento do objeto", 
              value = licitacao$descricao,
              width = "100%",
              rows = 3, 
              resize = "none"),
            textInput(
              ns("prioridade_edt"), 
              label = tags$b("Prioridade"), 
              placeholder = "Prioridade da licitação",
              value = licitacao$prioridade,
              width = "100%"),
            textInput(
              ns("encaminhamento_edt"), 
              label = tags$b("Encaminhamento"), 
              placeholder = "Encaminhamento atual",
              value = licitacao$encaminhamento,
              width = "100%"),
            textInput(
              ns("observacao_edt"), 
              label = tags$b("Observação"), 
              placeholder = "Observação",
              value = licitacao$observacao,
              width = "100%"),
            textInput(
              ns("subacao_edt"), 
              label = tags$b("Subação"), 
              placeholder = "Subação",
              value = licitacao$subacao,
              width = "100%"),
            selectInput(
              ns("status_edt"), 
              label = tags$b("Status"),
              choices = react_values_status$db$status,
              selected = licitacao$status,
              width = "100%"),
            textInput(
              ns("sei_edt"), 
              label = tags$b("Processo SEI"), 
              placeholder = "Número do SEI", 
              value = licitacao$sei,
              width = "100%"),
            selectInput(
              ns("responsavel_edt"), 
              label = tags$b("Responsável"),
              choices = react_values_responsavel$db$responsavel,
              selected = licitacao$responsavel,
              width = "100%"),
            selectInput(
              ns("matriz_risco_edt"), 
              label = tags$b("Matriz de Risco?"),
              choices = c("NÃO", 'SIM'),
              selected = licitacao$matriz_risco,
              width = "100%"),
            numericInput(
              ns("valor_edt"), 
              label = tags$b("Valor da Licitação"),
              licitacao$valor,
              step = 10000
            ),
            numericInput(
              ns("valor_proposta_edt"), 
              label = tags$b("Valor do Vencedor"),
              licitacao$valor_proposta,
              step = 10000
            )
          )
        } else {
          tagList(
            tags$b("Edição não habilitada")
          )
        }
        
        modal_rodape <- if (user %in% validadores) {
          tagList(
            actionButton(
              ns("btn_deletar"),
              label = "Remover",
              width = '20%',
              class = "m-1 btn-danger"),
            actionButton(
              ns("btn_validar"),
              label = "Bloquear licitação",
              width = '20%',
              class = "m-1 btn-warning"),
            actionButton(
              ns("saveuser_btn"),
              label = "Salvar",
              width = '20%',
              class = "m-1 btn-success"),
            actionButton(
              ns("btn_cancelar"),
              label = "Cancelar",
              width = '20%')
          )
        } else if (user %in% editores) {
          tagList(
            actionButton(
              ns("btn_deletar"),
              label = "Remover",
              width = '20%',
              class = "m-1 btn-danger"),
            actionButton(
              ns("btn_cancelar"),
              label = "Cancelar",
              width = '20%'),
            actionButton(
              ns("saveuser_btn"),
              label = "Salvar",
              width = '20%',
              class = "m-1 btn-success")
          )
        } else {
          tagList(
            actionButton(
              ns("btn_cancelar"),
              label = "Cancelar",
              width = '20%')
          )
        }
        
        # modal para inserção dos dados de edição
          showModal(
            modalDialog(
              size = 'xl',
              easyClose = TRUE,
              fade = TRUE,
              title = if (user %in% editores) "Editar" else "Acesso restrito",
              modal_corpo,
              footer = modal_rodape
            )
          )
        
      })
      
      ###### btn editar licitação ----
      # btn e lógica para editar registro
      observeEvent(input$saveuser_btn, {
        
        # cria a tbl da linha selecionada
        licitacao <- react_values_licitacoes() %>% 
          filter(row_number() == as.integer(input$dtls_licitacao))
        
        # guarda infos para id registro a ser atualizado
        del_data <- tibble(
          id = licitacao$id,
          excluir_flag = licitacao$excluir_flag
        )
        
        # tbl intermediária com os dados de edição
        # neste ponto que os dados recebem os handles
        edt_data <- tibble(
          objeto = input$objeto_edt,
          subobjeto = input$subobjeto_edt,
          descricao = input$descricao_edt,
          prioridade = input$prioridade_edt,
          encaminhamento = input$encaminhamento_edt,
          observacao = input$observacao_edt,
          subacao = input$subacao_edt,
          status = input$status_edt,
          sei = input$sei_edt,
          responsavel = input$responsavel_edt,
          matriz_risco = input$matriz_risco_edt,
          valor = handle_numeric(input$valor_edt),
          valor_proposta = handle_numeric(input$valor_proposta_edt),
          excluir_flag = licitacao$excluir_flag
        )
        
        # comando sql
        edt_data_sql <- paste0(
          "UPDATE tbl_semobi_licitacao SET ",
          "objeto = '", edt_data$objeto, "', ",
          "subobjeto = '", edt_data$subobjeto, "', ",
          "descricao = '", edt_data$descricao, "', ",
          "prioridade = '", edt_data$prioridade, "', ",
          "encaminhamento = '", edt_data$encaminhamento, "', ",
          "observacao = '", edt_data$observacao, "', ",
          "subacao = '", edt_data$subacao, "', ",
          "status = '", edt_data$status, "', ",
          "sei = '", edt_data$sei, "', ",
          "responsavel = '", edt_data$responsavel, "', ",
          "matriz_risco = '", edt_data$matriz_risco, "', ",
          "valor = '", edt_data$valor, "', ",
          "valor_proposta = '", edt_data$valor_proposta, "', ",
          "excluir_flag = '", edt_data$excluir_flag, "', ",
          "usuario = '", user, "', ",
          "updated_at = CURRENT_TIMESTAMP ",
          "WHERE ",
          "id = '", del_data$id, "';")  
        
        conn <- poolCheckout(con_base)
        
        result <- tryCatch({
          dbWithTransaction(conn, {
            dbExecute(conn, edt_data_sql)
          })
        }, error = function(err) {
          err
        })
        
        if ( inherits(result, 'error') ) {
          showNotification(result$message)
        } else {
          removeModal()
          showNotification("Licitação Atualizada")
        }
        
        poolReturn(conn)
        
      })
      
      #### deletar licitação ----
      ##### btn deletar licitação ----
      observeEvent(input$btn_deletar, {
        
        licitacao <- react_values_licitacoes() %>% 
          filter(row_number() == as.integer(input$dtls_licitacao)) 
        
        del_data <- tibble(
          id = licitacao$id
        )
        
        new_data_sql <- paste0(
          "UPDATE tbl_semobi_licitacao SET ",
          "excluir_flag = 1, ",
          "usuario = '", user, "', ",
          "updated_at = CURRENT_TIMESTAMP ",  
          "WHERE id = '", del_data$id, "';"
        )  
        
        conn <- poolCheckout(con_base)
        
        result <- tryCatch({
          dbWithTransaction(conn, {
            dbExecute(conn, new_data_sql)
          })
        }, error = function(err) {
          err
        })
        
        if ( inherits(result, 'error') ) {
          showNotification(result$message)
        } else {
          dbtrigger$trigger()
          removeModal()
          showNotification("Licitação removida.")
        }
        
        poolReturn(conn)
        
      })
      
      #### detalhamento licitação ----
      ##### modal detalhamento licitação ----
      observeEvent(input$vsl_licitacao, {
        
        # cria a tbl da linha selecionada
        dados <- react_values_licitacoes() %>%
          filter(
            row_number() == as.integer(input$vsl_licitacao)
          ) %>%
          select(
            -c(
              excluir_flag, usuario, created_at, updated_at, id
            )
          ) 
        
        modal_corpo <- gerar_cards(dados, 'objeto')
        
        modal_rodape <- 
          tagList(
            actionButton(
              ns("btn_cancelar"),
              label = "Cancelar",
              width = '20%')
          )
        
        showModal(
          modalDialog(
            size = 'xl',
            easyClose = TRUE,
            fade = TRUE,
            title = "Detalhamento",
            modal_corpo,
            footer = modal_rodape
          )
        )
        
      })
      
    } # fecha função
  ) # fecha módulo
} # fecha servidor