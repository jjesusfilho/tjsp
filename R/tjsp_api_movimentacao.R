#' Lista a movimentação de um processo na API mobile do TJSP (autenticado)
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e habilitação nos autos.
#'
#' @return tibble com colunas `cd_processo`, `dt_mov`, `movimento`, `descricao`.
#' @export
#'
tjsp_api_movimentacao <- function(numero_ou_cd, grau = "cpopg") {

  cd <- .tjsp_api_resolver_cd(numero_ou_cd, grau)

  data <- .tjsp_api_detalhe_de_processo_aberto("movimentacao", cd, grau)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }

  data |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::rename(dt_mov = "dt_movimento") |>
    dplyr::mutate(dt_mov = lubridate::dmy(.data$dt_mov, quiet = TRUE)) |>
    tibble::add_column(cd_processo = cd, .before = 1)
}
