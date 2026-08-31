#' Lista incidentes, recursos e execuções de sentença de um processo na API mobile do TJSP
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @details Equivalente à seção "Incidentes, ações incidentais, recursos e
#'    execuções de sentenças" do cpopg web -- inclui, por exemplo, Apelação
#'    Criminal, Recurso em Sentido Estrito e Execução de Sentença vinculados
#'    ao processo principal. Não exige login em processo público; em processo
#'    de acesso restrito sem habilitação nos autos, volta tibble vazia (como
#'    [tjsp_api_movimentacao()] e [tjsp_api_partes()]).
#'
#' @return tibble com colunas `cd_processo`, `codigo`, `classe`, `data`.
#' @export
#'
tjsp_api_incidentes <- function(numero_ou_cd, grau = "cpopg") {

  cd <- .tjsp_api_resolver_cd(numero_ou_cd, grau)

  data <- .tjsp_api_detalhe_de_processo_aberto("incidente", cd, grau)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }

  data |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    tibble::add_column(cd_processo = cd, .before = 1)
}
