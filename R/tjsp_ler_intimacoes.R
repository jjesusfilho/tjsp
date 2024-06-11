#' Lê csv baixado com tjsp_baixar_intimacoes
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente, informar diretório.
#'
#' @return Tibble
#' @export
#'
tjsp_ler_intimacoes <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "csv$")

  }


  purrr::map_dfr(arquivos, purrr::possibly(~{

 s<- .x |>
  readr::read_delim(delim = ";",
             escape_double = FALSE,
             locale = readr::locale(encoding = "ISO-8859-1"),
             trim_ws = TRUE) |>
  janitor::clean_names() |>
  tibble::add_column(arquivo = .x, .before = 1)

  }, NULL))
}
