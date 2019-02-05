#' Lê dados do cpopg já serializados
#'
#' @param arquivo arquivo rds serializado
#' @param wide lógico.long, default, ou wide.
#'
#' @return tibble em long ou wide format.
#' @export
#'
ler_dados_cpopg_serializados <- function(arquivo,
                                   wide = FALSE) {
  arquivo <- readRDS(arquivo)

  conexao <- names(arquivo) %>%
    stringr::str_extract("\\d.+") %>%
    stringr::str_replace(".html", ".rds")

  dir <- tempdir()
  system.time(arquivos <- purrr::map2(arquivo, conexao,  ~ {
    writeBin(.x, paste0(dir, "/", .y))

    readRDS(paste0(dir, "/", .y)) %>%
      unclass()
  }))

  unlink(dir)

  processos <- stringr::str_extract(names(arquivos), "\\d{20}")



  dados <- purrr::map2_dfr(arquivos,
                           processos,
                           purrr::possibly( ~ {
                             resposta <- .x %>%
                               xml2::read_html()

                             digital <-
                               xml2::xml_find_first(resposta, "boolean(//*[@class='linkPasta'])")


                             variavel <- resposta %>%
                               xml2::xml_find_all("//table[@class='secaoFormBody']//label[@class='labelClass']") %>%
                               xml2::xml_text()

                             valor <- resposta %>%
                               xml2::xml_find_all(
                                 "//table[@class='secaoFormBody']//label[@class='labelClass']/../following-sibling::td"
                               ) %>%
                               xml2::xml_text() %>%
                               stringr::str_squish()


                             tibble::tibble(processo = .y, digital, variavel, valor)
                           }, NULL), .progress = TRUE)

  if (wide == TRUE) {
    dados <- dados %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id)

  }

  return(dados)
}
