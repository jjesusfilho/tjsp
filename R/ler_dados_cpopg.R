#' Lê os metadados de processos de primeira instância.
#'
#' @param diretorio Diretório onde se encontram os htmls.
#' @param wide o padrão é o formado longo, com apenas quatro colunas:
#'      processo, digital, variavel, valor.
#' @return tibble com os metadados
#' @export
#'
#' @examples
#' \dontrun{
#' ler_dados_cpopg()
#' }
#'
ler_dados_cpopg <-
  function(diretorio = ".",
           wide = FALSE) {
    arquivos <- list.files(path = diretorio,
                           pattern = ".html",
                           full.names = TRUE)

    processos <- stringr::str_extract(arquivos, "\\d{20}")



    dados <- purrr::map2_dfr(arquivos,
                                    processos,
                                    purrr::possibly(~ {
                                      resposta <- .x %>%
                                        xml2::read_html()

                                      digital <-
                                        xml2::xml_find_first(resposta, "boolean(//*[@class='linkPasta'])")


                                      variavel <- resposta %>%
                                        xml2::xml_find_all("//table[@class='secaoFormBody']//label[@class='labelClass']") %>%
                                        xml2::xml_text() %>%
                                        stringr::str_squish()

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