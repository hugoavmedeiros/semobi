#### biblios ----
library(arrow) # Leitura de arquivos Parquet e Feather
library(bsicons)# Ícones do Bootstrap para uso em Shiny
library(bslib) # Suporte a temas Bootstrap para Shiny
library(DBI) # Interface de banco de dados para R
library(dbplyr) # Traduz consultas dplyr em SQL
library(fst) # Leitura e escrita rápida de dados
library(glue) # Interpolação de strings
library(httr) # Ferramentas para trabalhar com HTTP
library(htmltools) # Ferramentas para manipulação de HTML
library(jsonlite) # Trabalho com JSON
library(lubridate) # Para manipulação de datas
library(pals) # Paletas de cores
library(plotly) # Gráficos interativos
library(pikchr) # Desenho de diagramas em texto
library(pool) # Agrupamento de conexões de banco de dados
library(reactable) # Tabelas interativas para Shiny
library(reactable.extras) # Extras para reactable
library(rlang) # Ferramentas essenciais para a linguagem R
#library(RMySQL) # Ferramentas essenciais para a linguagem R
library(safer) # Criptografia para R
library(scales) # Escalas para ggplot2
library(shiny) # Para criação de aplicativos web interativos
library(shinyjs) # Uso de JavaScript no Shiny
library(shinymanager) # Autenticação para aplicativos Shiny
library(shinycssloaders) # Carregadores CSS para Shiny
library(spsComps) # 
library(stringi) # Manipulação de strings
library(tidyverse) # Coleção de pacotes para ciência de dados
library(tippy) # Dicas de ferramentas para Shiny
library(vialactea) # Funções do modelo da SEPE/SEME
library(viridis) # Paletas de cores para mapas e gráficos
library(waiter) # Para carregamento e progresso em Shiny
library(writexl) # Escrever dados em arquivos Excel

#### YAML ----

status <- config::get("status")
sepe_datalake <- config::get(paste0("sepedatalake_", status))
sepe_datalake2 <- config::get(paste0("sepedatalake_", 'localhost'))
app_name <- config::get("app_name")
seed <- read_rds("data/semente.rds")
key <- safer::keypair(seed = seed)
# read keys
keys <- read_rds("data/key_app_users.rds")

#### 3. Funções ----
##### Tratar Números ----
handle_numeric <- function(value) {
  if (is.null(value) || is.na(value) || (is.character(value) && value == "") || length(value) == 0) {
    return(0)
  } else {
    return(value)
  }
}

###### Tratar Datas ----
handle_date <- function(value) {
  if (is.null(value) || is.na(value) || (is.character(value) && value == "") || length(value) == 0) {
    return('Sem data')
  } else {
    return(value)
  }
}

###### Tratar para Null ----
handle_null <- function(value) {
  if (is.null(value) || is.na(value) || (is.character(value) && (value == "" || value == "Sem data")) || length(value) == 0) {
    return('NULL')
  } else if (is.numeric(value)) {
    return(as.character(value))
  } else if (inherits(value, "Date")) {
    return(paste0("'", value, "'"))
  } else {
    return(paste0("'", value, "'"))
  }
}

###### Formatar pra R$ ----
formatar_real <- dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")

###### Criar input ----
registerInputHandler(
  "xx",
  function(data, ...){
    fromJSON(
      toJSON(
        data,
        null = 'null',
        na = 'string')
      )
  },
  force = TRUE
)

###### Gerar cards ----
gerar_cards <- function(dados, nome_coluna) {

  dados <- dados[order(dados[[nome_coluna]]), ]
  
  cards <- list()
  
  for (i in 1:nrow(dados)) {
    
    card_name <- as.character(dados[i, nome_coluna])
    
    card <- paste(
      "<div style='border: 0px solid black; padding: 5px;'>",
      #"<b>", card_name, "</b><br/>",
      #paste(names(dados), dados[i, ], sep = ": ", collapse = "<br/>"),
      paste(
        "<span style='font-weight:bold;'>",
        names(dados),
        "</span>",
        dados[i, ], 
        sep = " ", 
        collapse = " "),
      "</div>"
    )
    
    cards[[i]] <- card(
      card_header(
        card_name
      ),
      card_body(
        HTML(card)
        )
      )
    
  }
  
  # Retorna a lista de cards como um htmlOutput
  tagList(cards)
}

#### Auth ----
load_svg <- HTML('<svg version="1.1" id="L7" width="100" height="100" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" viewBox="0 0 100 100" enable-background="new 0 0 100 100" xml:space="preserve">
 <path fill="#1F64FF" d="M31.6,3.5C5.9,13.6-6.6,42.7,3.5,68.4c10.1,25.7,39.2,38.3,64.9,28.1l-3.1-7.9c-21.3,8.4-45.4-2-53.8-23.3
  c-8.4-21.3,2-45.4,23.3-53.8L31.6,3.5z">
      <animateTransform attributeName="transform" attributeType="XML" type="rotate" dur="2s" from="0 50 50" to="360 50 50" repeatCount="indefinite"></animateTransform>
  </path>
 <path fill="#93E600" d="M42.3,39.6c5.7-4.3,13.9-3.1,18.1,2.7c4.3,5.7,3.1,13.9-2.7,18.1l4.1,5.5c8.8-6.5,10.6-19,4.1-27.7
  c-6.5-8.8-19-10.6-27.7-4.1L42.3,39.6z">
      <animateTransform attributeName="transform" attributeType="XML" type="rotate" dur="1s" from="0 50 50" to="-360 50 50" repeatCount="indefinite"></animateTransform>
  </path>
 <path fill="#FF6400" d="M82,35.7C74.1,18,53.4,10.1,35.7,18S10.1,46.6,18,64.3l7.6-3.4c-6-13.5,0-29.3,13.5-35.3s29.3,0,35.3,13.5
  L82,35.7z">
      <animateTransform attributeName="transform" attributeType="XML" type="rotate" dur="2s" from="0 50 50" to="360 50 50" repeatCount="indefinite"></animateTransform>
  </path>
</svg>')

secure_front <- function(ui, logo = "images/logo_sepe.png") {
  secure_app(
    language = "pt-BR",
    theme = fn_custom_theme(),
    fab_position = "none",

    # Cabeçalho da página de autenticação
    tags_top = tags$div(
      tags$img(src = logo, width = '80%')
    ),

    # Informação adicional no rodapé da página de autenticação
    tags_bottom = tags$div(
      tags$p(
        "Qualquer dúvida, entrar em contato com o ",
        tags$a(
          href = "mailto:andre.leite@sepe.pe.gov.br?Subject=SEPE Apps",
          target="_top", "administrador"
        )
      )
    ),

    # Estilo de fundo para a página de autenticação
    background = "radial-gradient(circle at 22.4% 21.7%, rgb(238, 130, 238) 0%, rgb(127, 0, 255) 100.2%);",

    # Inclusão de cabeçalhos de autenticação
    head_auth = tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "login.css")
    ),

    page(
      title = app_name, 
      tags$head(
        tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200")
      ),

      useShinyjs(),

      useWaiter(),
      
      bsplus::use_bs_tooltip(),

      includeScript("www/script.js"),

      theme = fn_custom_theme(), 
      
      ui
    )
  )
}


set_labels(
  language = "pt-BR",
  "Please authenticate" = "",
  "Username:" = "Nome",
  "Password:" = "Senha"
)



#### ETL ----
##### pool ----
create_pool <- function() {
  dbPool(
  drv = RPostgres::Postgres(),
  dbname = sepe_datalake$database,
  host = sepe_datalake$server,
  user = sepe_datalake$uid,
  password = sepe_datalake$pwd
)
}

con_base <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = sepe_datalake$database,
  host = sepe_datalake$server,
  user = sepe_datalake$uid,
  password = sepe_datalake$pwd
)

message("Pool de conexões criado.")

onStop(function() {
  message("Antes?", DBI::dbIsValid(con_base))
  pool::poolClose(con_base)
  message("Depois?", DBI::dbIsValid(con_base))
})

##### tbls ----
tbl_semobi_licitacao <- dbGetQuery(con_base, "SELECT * FROM tbl_semobi_licitacao")
tbl_semobi_licitacao_transacao <- dbGetQuery(con_base, "SELECT * FROM tbl_semobi_licitacao_transacao")

tbl_der_licitacoes_local <- dbGetQuery(con_base, "SELECT * FROM tbl_der_licitacoes_local")
tbl_der_licitacoes_status <- dbGetQuery(con_base, "SELECT * FROM tbl_der_licitacoes_status")
tbl_der_licitacoes_prioridade <- dbGetQuery(con_base, "SELECT * FROM tbl_der_licitacoes_prioridade")
tbl_der_licitacoes_objeto <- dbGetQuery(con_base, "SELECT * FROM tbl_der_licitacoes_objeto")
tbl_der_licitacoes_responsavel <- dbGetQuery(con_base, "SELECT * FROM tbl_der_licitacoes_responsavel")

#### listas ----
validadores <- c(
  'rivaldo.melo')

editores <- c(
  'ana.machado',
  'daniela.medeiros', 
  'hayanne.carneiro',
  'hugo.medeiros',
  'karime.bezerra',
  'rivaldo.melo')

#### Módulos ----
file_paths <- fs::dir_ls(c("modules", "helpers"))  # Lista todos os arquivos nos diretórios 'modules' e 'helpers'.

map(file_paths, function(x) {
  source(x)  # Carrega cada arquivo individualmente.
})