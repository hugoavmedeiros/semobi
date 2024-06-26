pacman::p_load(
  DBI,
  pool,
  RPostgres,
  tidyverse)

#### yaml ----
status <- config::get("status")
sepe_datalake <- config::get(paste0("sepedatalake_", status))
sepe_datalake2 <- config::get(paste0("sepedatalake_", 'localhost'))
app_name <- config::get("app_name")
seed <- read_rds("data/semente.rds")
key <- safer::keypair(seed = seed)
# read keys
keys <- read_rds("data/key_app_users.rds")

#### pool ----
create_pool <- function() {
  dbPool(
    drv = RPostgres::Postgres(),
    dbname = sepe_datalake$database,
    host = sepe_datalake$server,
    user = sepe_datalake$uid,
    password = sepe_datalake$pwd
  )
}

con_ingestao <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = sepe_datalake$database,
  host = sepe_datalake$server,
  user = sepe_datalake$uid,
  password = sepe_datalake$pwd
)

conn <- poolCheckout(con_ingestao)

#### crt ----
sql_criar_tbl_licitacao <- "
CREATE TABLE tbl_semobi_licitacao (
  id SERIAL PRIMARY KEY,
  objeto TEXT,
  subobjeto TEXT,
  descricao TEXT,
  prioridade TEXT,
  encaminhamento TEXT,
  observacao TEXT,
  subacao TEXT,
  status TEXT,
  sei TEXT,
  responsavel TEXT,
  matriz_risco TEXT,
  valor INTEGER,
  valor_proposta INTEGER,
  usuario TEXT,
  excluir_flag SMALLINT CHECK (excluir_flag IN (0, 1)) DEFAULT 0,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP
);
"

dbWithTransaction(conn, {
  dbExecute(conn, sql_criar_tbl_licitacao)
})

#### tbl licitações transações ----
sql_criar_tbl_licitacao_transacao <- "
CREATE TABLE tbl_semobi_licitacao_transacao (
  id SERIAL PRIMARY KEY,
  id_licitacao INT,
  local TEXT,
  data DATE,
  usuario TEXT,
  excluir_flag SMALLINT CHECK (excluir_flag IN (0, 1)) DEFAULT 0,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP,
  FOREIGN KEY (id_licitacao) REFERENCES tbl_semobi_licitacao(id)
);
"

dbWithTransaction(conn, {
  dbExecute(conn, sql_criar_tbl_licitacao_transacao)
})

#### rmv ----
dbWithTransaction(conn, {
  dbExecute(conn, "DROP TABLE IF EXISTS tbl_semobi_licitacao_transacao;")
})

dbWithTransaction(conn, {
  dbExecute(conn, "DROP TABLE IF EXISTS tbl_semobi_licitacao;")
})

#### load ----
tbl_semobi_licitacao <- dbGetQuery(con_ingestao, "SELECT * FROM tbl_semobi_licitacao")
tbl_semobi_licitacao_transacao <- dbGetQuery(con_ingestao, "SELECT * FROM tbl_semobi_licitacao_transacao")

teste_integridade <- tbl_teste_filho %>% 
  left_join(
    tbl_teste_pai,
    by = c('id_pai' = 'id'))

poolReturn(conn)
