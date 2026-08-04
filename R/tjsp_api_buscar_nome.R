#' Busca processos por nome de parte ou advogado na API mobile do TJSP (pública, sem login)
#'
#' @param nome Nome (ou trecho do nome) da parte ou advogado.
#' @param tipo "parte" (padrão) ou "advogado".
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @return tibble com um processo por linha (pode ser vazio).
#' @export
#'
tjsp_api_buscar_nome <- function(nome, tipo = c("parte", "advogado"), grau = "cpopg") {

  tipo <- match.arg(tipo)

  criterio <- if (tipo == "advogado") "nmadvogado" else "nmparte"

  .tjsp_api_buscar(criterio, nome, grau) |>
    .tjsp_api_consulta_padronizar()
}
