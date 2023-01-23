#' Baixa petição inicial, ajuizamento (jec) ou denúncia
#'
#' @param processo Número do processo
#' @param diretorio Diretório onde armazenar a inicial
#'
#' @return PDF?
#' @export
#'
tjsp_baixar_inicial <- function(processo, diretorio){

  dir <- tempdir()

  tjsp_cpopg_baixar_tabela_docs(processo, diretorio = dir)

  tabela <- tjsp_ler_tabela_docs(diretorio = dir)

  doc <- tabela |>
       dplyr::mutate(doc_name = abjutils::rm_accent(doc_name) |> tolower()) |>
       dplyr::filter(stringr::str_detect(doc_name, "(peticao|ajuizamento|denncia)")) |>
       dplyr::slice_head(n = 1)

  unlink(dir, recursive= TRUE, force = TRUE)

  tjsp_baixar_docs(doc$processo, id_doc = doc$id_doc, urls = doc$url_doc, diretorio= diretorio)

}
