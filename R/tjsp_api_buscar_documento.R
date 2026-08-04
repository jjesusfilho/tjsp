#' Busca processos por CPF/CNPJ de parte na API mobile do TJSP (pública, sem login)
#'
#' @param cpf_cnpj CPF ou CNPJ da parte, com ou sem pontuação.
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @return tibble com um processo por linha (pode ser vazio).
#' @export
#'
tjsp_api_buscar_documento <- function(cpf_cnpj, grau = "cpopg") {

  doc <- stringr::str_remove_all(cpf_cnpj, "\\D")

  .tjsp_api_buscar("docparte", doc, grau) |>
    .tjsp_api_consulta_padronizar()
}
