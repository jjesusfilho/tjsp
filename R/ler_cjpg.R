#' Lê decisões de primeira instância do TJSP baixadas por meio da função baixar_cjpg.
#'
#' @param diretorio objeto ou diretório onde se encontram as páginas.
#' @param arquivos Se NULL, informar diretorio
#' @return uma tibble com nove colunas: processo, classe, assunto, magistrado,comarca, foro, vara,
#'     disponibilizacao e julgado (texto da decisão).
#' @export
#'
#' @examples
#' \dontrun{
#' ler_cjpg()
#' }
ler_cjpg<-function (arquivos = NULL, diretorio = ".")
{

  if (is.null(arquivos)){

    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  df <- purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    resposta <- .x %>% xml2::read_html(encoding = "UTF-8") %>%
      xml2::xml_find_all(xpath = "//*[@id='divDadosResultado']/table//td//td[@align='left']") %>%
      xml2::xml_text(trim = TRUE) %>% stringr::str_squish() %>%
      paste0(collapse = "\n") %>% stringi::stri_split_regex("\n(?=\\d{4,})") %>%
      unlist()
    processo <- stringi::stri_extract_first_regex(resposta,
                                                  "\\d+-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}", omit_no_match = F)
    classe <- stringi::stri_extract_first_regex(resposta,
                                                "Classe:.*", omit_no_match = F)
    assunto <- stringi::stri_extract_first_regex(resposta,
                                                 "Assunto:.*", omit_no_match = F)
    magistrado <- stringi::stri_extract_first_regex(resposta,
                                                    "Magistrado:.*", omit_no_match = F)
    comarca <- stringi::stri_extract_first_regex(resposta,
                                                 "Comarca:.*", omit_no_match = F)
    foro <- stringi::stri_extract_first_regex(resposta, "Foro:.*",
                                              omit_no_match = F)
    vara <- stringi::stri_extract_first_regex(resposta, "Vara:.*",
                                              omit_no_match = F)
    disponibilizacao <- stringi::stri_extract_first_regex(resposta,
                                                          "Data\\s+de\\s+Disponibilização:.*", omit_no_match = F)
    julgado <- stringi::stri_extract_last_regex(resposta,
                                                "(?<=\n).*", omit_no_match = F)
    tibble::tibble(processo, classe, assunto, magistrado,
                   comarca, foro, vara, disponibilizacao, julgado)
  }, NULL)) %>%
    dplyr::mutate(dplyr::across(2:8,~stringi::stri_replace_first_regex(., ".*:\\s?",""))) %>%

    dplyr::mutate(dplyr::across(dplyr::everything(),stringr::str_squish)) %>%
    rm_duplicados(processo) %>%
    dplyr::mutate(disponibilizacao = lubridate::dmy(disponibilizacao),
                                              processo = stringr::str_remove_all(processo, "\\D+"))
}
