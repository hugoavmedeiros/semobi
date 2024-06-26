
fn_custom_theme <- function() {
  bslib::bs_theme(
    version = "5",  
    base_font = sass::font_link(
      "Jost",  
      href="https://fonts.googleapis.com/css2?family=Jost:ital,wght@0,100..900;1,100..900&display=swap"
    ),
    preset = "pulse",  
    `enable-gradients` = TRUE,  # Habilita gradientes
    `enable-shadows` = TRUE,  # Habilita sombras
    `enable-rounded` = TRUE  # Habilita bordas arredondadas
    # As seguintes opções foram comentadas e não estão ativas no momento:
    # fg = "rgb(16, 100, 176)",  # Cor de primeiro plano
    # bg = "#fff",  # Cor de fundo
    # primary = "#142755",  # Cor primária
    # secondary = "#0708F5",  # Cor secundária
    # success = "#76DF43",  # Cor para sucesso
    # info = "#54B2F9",  # Cor para informações
    # warning = "#F0B13D",  # Cor para avisos
    # danger = "#D83831"  # Cor para perigo
  ) |>
    # Adiciona regras personalizadas do Sass a partir de um arquivo externo
    bs_add_rules(sass::sass_file("www/styles.scss"))
}

# Definições de idioma para o pacote reactable
br_react <- reactable::reactableLang(
  searchPlaceholder = "Pesquisar...",  # Texto do placeholder da pesquisa
  noData = "Nenhum item encontrado.",  # Texto quando não há dados
  pageSizeOptions = "Mostrar {rows}",  # Opções de tamanho da página
  pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",  # Informação da página
  pagePrevious = "\u276e",  # Símbolo para página anterior
  pageNext = "\u276f",  # Símbolo para próxima página
  pagePreviousLabel = "Pág. anterior",  # Label para página anterior
  pageNextLabel = "Pág. seguinte"  # Label para próxima página
)

# Função para criar um tema temático para saídas do ggplot2 (comentada)
# fn_thematic_theme <- function() {
#   thematic::thematic_theme(
#     bg = "#ffffff",  # Cor de fundo
#     fg = "#1d2d42",  # Cor de primeiro plano
#     accent = "#f3d436",  # Cor de destaque
#     font = font_spec(sass::font_google("Open Sans"), scale = 1.75)  # Fonte e escala
#   )
# }
