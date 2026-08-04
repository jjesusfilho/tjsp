#' Busca processos por número de OAB na API mobile do TJSP (pública, sem login)
#'
#' @param numero_oab Número de inscrição na OAB.
#' @param uf UF da OAB (ex. "SP").
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @return tibble com um processo por linha (pode ser vazio).
#' @export
#'
tjsp_api_buscar_oab <- function(numero_oab, uf, grau = "cpopg") {

  valor <- paste0(stringr::str_remove_all(numero_oab, "\\s"), uf)

  .tjsp_api_buscar("numoab", valor, grau) |>
    .tjsp_api_consulta_padronizar()
}
