#' Busca processos por Certidão de Dívida Ativa (CDA) na API mobile do TJSP (pública, sem login)
#'
#' @param numero_cda Número da CDA.
#' @param grau "cpopg" (1º grau cível, padrão) -- único grau com essa rota no contrato da API.
#'
#' @return tibble com um processo por linha (pode ser vazio).
#' @export
#'
tjsp_api_buscar_cda <- function(numero_cda, grau = "cpopg") {

  .tjsp_api_buscar("numcda", numero_cda, grau) |>
    .tjsp_api_consulta_padronizar()
}
