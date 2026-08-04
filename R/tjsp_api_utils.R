# Helpers internos do cliente da API mobile do TJSP (api.tjsp.jus.br/processo).
#
# Espelha o cliente Python `TJSPAPIeSAJClient` (videre/tribunais/tjsp/api_esaj.py):
# autenticação leve por CPF+senha (sem 2FA), busca pública por vários critérios,
# e rotas de detalhe que exigem o processo previamente "aberto" via dadosbasicos.
#
# Não exportado. Usa `httr` (não `httr2`) para reaproveitar o cookie jar implícito
# do pacote, do mesmo jeito que tjsp_autenticar()/check_login() fazem para
# esaj.tjsp.jus.br -- sem precisar passar cookies_path entre chamadas.

.tjsp_api_base <- "https://api.tjsp.jus.br/processo"

.tjsp_api_graus <- c("cpopg", "cposg", "cpocr")

.tjsp_api_criterios <- c(
  "numproc", "docparte", "nmparte", "nmadvogado",
  "numoab", "numcda", "precatorio", "docdeleg"
)

.tjsp_api_pasta_instancias <- c("pg", "sg", "cr")

.tjsp_api_grau_por_instancia <- c(pg = "cpopg", sg = "cposg", cr = "cpocr")

.tjsp_api_sem_acesso <- "SEMACESSODETALHES"

# Estado da sessão (credenciais para reautenticar, cd_processo aberto).
# O cookie CAS em si fica no cookie jar implícito do httr, não aqui.
.tjsp_api_state <- new.env(parent = emptyenv())

.tjsp_api_login <- function(cpf, senha) {
  resp <- httr::POST(
    paste0(.tjsp_api_base, "/sajcas/login"),
    query = list(usuario = cpf, senha = senha),
    httr::add_headers("Content-Length" = "0")
  )
  httr::status_code(resp) == 200 &&
    tolower(trimws(httr::content(resp, "text", encoding = "UTF-8"))) == "true"
}

.tjsp_api_reautenticar <- function() {
  cpf <- get0("cpf", envir = .tjsp_api_state)
  senha <- get0("senha", envir = .tjsp_api_state)
  if (is.null(cpf) || is.null(senha)) {
    return(FALSE)
  }
  .tjsp_api_login(cpf, senha)
}

.tjsp_api_autenticado <- function() {
  !is.null(get0("cpf", envir = .tjsp_api_state))
}

# Busca anônima em `{grau}/search/{criterio}/{valor}`. Retorna tibble
# (vazia quando não há resultado ou a resposta não é 200).
.tjsp_api_buscar <- function(criterio, valor, grau = "cpopg") {
  if (!grau %in% .tjsp_api_graus) {
    stop("grau inválido: '", grau, "'. Use um de: ", paste(.tjsp_api_graus, collapse = ", "))
  }
  if (!criterio %in% .tjsp_api_criterios) {
    stop("critério inválido: '", criterio, "'. Use um de: ", paste(.tjsp_api_criterios, collapse = ", "))
  }

  url <- paste0(.tjsp_api_base, "/", grau, "/search/", criterio, "/", utils::URLencode(as.character(valor), reserved = TRUE))
  resp <- httr::GET(url)

  if (httr::status_code(resp) != 200) {
    return(tibble::tibble())
  }

  texto <- httr::content(resp, "text", encoding = "UTF-8")
  data <- tryCatch(jsonlite::fromJSON(texto), error = function(e) NULL)

  if (is.null(data) || length(data) == 0) {
    return(tibble::tibble())
  }
  if (is.data.frame(data)) {
    return(tibble::as_tibble(data))
  }
  # objeto único (dict) em vez de lista: normaliza para 1 linha
  tibble::as_tibble(as.data.frame(data, stringsAsFactors = FALSE))
}

# Renomeia os campos crus da API (camelCase) para o padrão já usado em
# tjsp_api_ler(): nume_processo, cd_processo, classe, assunto, foro,
# data_recebimento, tipo (+ nome/documento/cda/... quando presentes).
.tjsp_api_consulta_padronizar <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      nume_processo = character(), cd_processo = character(), classe = character(),
      assunto = character(), foro = character(), data_recebimento = character(),
      tipo = character()
    ))
  }
  if ("nmForo" %in% names(df)) {
    df <- dplyr::rename(df, foro = "nmForo")
  }
  df <- janitor::clean_names(df)
  if ("nume_processo" %in% names(df)) {
    df$nume_processo <- stringr::str_remove_all(df$nume_processo, "\\D")
  }
  df
}

# Aceita cd interno (ex. "1HZX1T0BT0000") ou número CNJ (resolve via busca pública).
.tjsp_api_resolver_cd <- function(numero_ou_cd, grau = "cpopg") {
  digitos <- stringr::str_remove_all(numero_ou_cd, "\\D")
  if (nchar(digitos) != 20) {
    return(as.character(numero_ou_cd))
  }
  df <- .tjsp_api_buscar("numproc", digitos, grau)
  if (nrow(df) == 0 || !"cdProcesso" %in% names(df)) {
    stop("Processo não encontrado: ", numero_ou_cd)
  }
  as.character(df$cdProcesso[[1]])
}

# Abre o processo na sessão via dadosbasicos (POST) -- pré-requisito para
# partes/movimentacao/pastadigital. Guarda o cd aberto para não reabrir à toa.
.tjsp_api_abrir <- function(cd, grau = "cpopg") {
  url <- paste0(.tjsp_api_base, "/", grau, "/dadosbasicos/", cd)
  resp <- httr::POST(url, body = "null", encode = "raw", httr::content_type_json())

  if (!httr::status_code(resp) %in% c(200, 403)) {
    return(NULL)
  }

  dados <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  status <- if (is.list(dados)) dados$status else NULL
  if (!is.null(dados) && !identical(status, .tjsp_api_sem_acesso)) {
    assign("cd_aberto", cd, envir = .tjsp_api_state)
  }
  dados
}

# GET numa rota que exige o processo previamente aberto. Em 400 (sessão
# expirada), reautentica, reabre e repete uma vez -- como no cliente Python.
.tjsp_api_detalhe_de_processo_aberto <- function(recurso, cd, grau = "cpopg") {
  aberto <- get0("cd_aberto", envir = .tjsp_api_state)
  if (is.null(aberto) || aberto != cd) {
    .tjsp_api_abrir(cd, grau)
  }

  url <- paste0(.tjsp_api_base, "/", grau, "/", recurso, "/", cd)
  resp <- httr::GET(url, httr::content_type_json())

  if (httr::status_code(resp) == 400 && .tjsp_api_reautenticar()) {
    assign("cd_aberto", NULL, envir = .tjsp_api_state)
    .tjsp_api_abrir(cd, grau)
    resp <- httr::GET(url, httr::content_type_json())
  }

  if (httr::status_code(resp) != 200) {
    return(NULL)
  }
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
}

# Árvore da pasta digital (lista de documentos/volumes) de um cd_processo.
.tjsp_api_pasta_tree <- function(cd, instancia) {
  if (!instancia %in% .tjsp_api_pasta_instancias) {
    stop("instancia inválida: '", instancia, "'. Use 'pg', 'sg' ou 'cr'.")
  }
  aberto <- get0("cd_aberto", envir = .tjsp_api_state)
  if (is.null(aberto) || aberto != cd) {
    .tjsp_api_abrir(cd, .tjsp_api_grau_por_instancia[[instancia]])
  }
  url <- paste0(.tjsp_api_base, "/pastadigital/", instancia, "/", cd)
  resp <- httr::GET(url, httr::content_type_json())
  if (httr::status_code(resp) != 200) {
    return(tibble::tibble())
  }
  data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  if (is.data.frame(data)) tibble::as_tibble(data) else tibble::tibble()
}
