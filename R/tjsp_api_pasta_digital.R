#' Lista os documentos da pasta digital de um processo na API mobile do TJSP
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param instancia "pg" (1º grau, padrão), "sg" (2º grau) ou "cr" (criminal).
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e habilitação nos
#'    autos. Cada linha é um documento/volume da pasta; `n_arquivos` é a
#'    quantidade de arquivos que o compõem -- em volumes escaneados, 1 por
#'    página; em processos digitais, normalmente 1 (a peça inteira).
#'
#' @return tibble com colunas `cd_processo`, `cd_documento`, `titulo`, `n_arquivos`.
#' @export
#'
tjsp_api_pasta_digital <- function(numero_ou_cd, instancia = "pg") {

  grau <- .tjsp_api_grau_por_instancia[[instancia]]
  cd <- .tjsp_api_resolver_cd(numero_ou_cd, if (is.null(grau)) "cpopg" else grau)

  arvore <- .tjsp_api_pasta_tree(cd, instancia)

  if (nrow(arvore) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(
    cd_processo = cd,
    cd_documento = as.character(arvore$cdDocumento),
    titulo = trimws(arvore$title),
    n_arquivos = purrr::map_int(arvore$childrenDto, function(x) if (is.data.frame(x)) nrow(x) else length(x))
  )
}

#' Encontra documentos da pasta digital cujo título casa com um termo
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param termo Termo a buscar no título dos documentos (sem distinção de
#'    maiúsculas/minúsculas ou acentos).
#' @param instancia "pg" (1º grau, padrão), "sg" (2º grau) ou "cr" (criminal).
#'
#' @details Útil para localizar peças por tipo, ex.:
#'    `tjsp_api_encontrar_documentos(processo, "petição")` -- no SAJ a peça
#'    inaugural costuma vir rotulada apenas como "Petição".
#'
#' @return tibble no mesmo formato de [tjsp_api_pasta_digital()] (pode ser vazio).
#' @export
#'
tjsp_api_encontrar_documentos <- function(numero_ou_cd, termo, instancia = "pg") {

  docs <- tjsp_api_pasta_digital(numero_ou_cd, instancia)

  if (nrow(docs) == 0) {
    return(docs)
  }

  alvo <- tolower(remover_acentos(termo))

  docs[stringr::str_detect(tolower(remover_acentos(docs$titulo)), stringr::fixed(alvo)), ]
}
