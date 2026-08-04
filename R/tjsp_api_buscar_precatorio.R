#' Busca processos por número de precatório na API mobile do TJSP (pública, sem login)
#'
#' @param numero Número do precatório.
#' @param grau "cpopg" (1º grau cível, padrão) -- único grau com essa rota no contrato da API.
#'
#' @return tibble com um processo por linha (pode ser vazio).
#' @export
#'
tjsp_api_buscar_precatorio <- function(numero, grau = "cpopg") {

  .tjsp_api_buscar("precatorio", numero, grau) |>
    .tjsp_api_consulta_padronizar()
}
