#' Busca processo por número CNJ na API mobile do TJSP (pública, sem login)
#'
#' @param numero Número do processo (CNJ), com ou sem pontuação.
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @details Consulta anônima (sem captcha) via
#'    `GET https://api.tjsp.jus.br/processo/{grau}/search/numproc/{numero}`.
#'    Retorna dados básicos e o `cd_processo` interno do SAJ, necessário para
#'    [tjsp_api_dados_basicos()], [tjsp_api_partes()], [tjsp_api_movimentacao()]
#'    e a pasta digital.
#'
#' @return tibble de uma linha.
#' @export
#'
tjsp_api_buscar_processo <- function(numero, grau = "cpopg") {

  digitos <- stringr::str_remove_all(numero, "\\D")

  df <- .tjsp_api_buscar("numproc", digitos, grau) |>
    .tjsp_api_consulta_padronizar()

  if (nrow(df) == 0) {
    stop("Processo não encontrado: ", numero)
  }

  encontrado <- which(df$nume_processo == digitos | df$nume_processo == "")

  if (length(encontrado) == 0) {
    stop("Processo não encontrado: ", numero)
  }

  df[encontrado[1], ]
}
