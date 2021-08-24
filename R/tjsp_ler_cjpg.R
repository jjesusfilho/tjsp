#' Lê decisões de primeira instância do TJSP baixadas por meio da função baixar_cjpg.
#'
#' @param diretorio objeto ou diretório onde se encontram as páginas.
#' @param arquivos Se NULL, informar diretorio
#' @return uma tibble com nove colunas: processo, classe, assunto, magistrado,comarca, foro, vara,
#'     disponibilizacao e julgado (texto da decisão).
#' @details Esta função é experimental. Eventualmente, substituirá a função ler_cjpg
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_ler_cjpg()
#' }
tjsp_ler_cjpg<-function (arquivos = NULL, diretorio = ".")
{

  if (is.null(arquivos)){

    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  df <- purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    x <- .x %>% xml2::read_html(encoding = "UTF-8")

    cd_doc <- x %>%
      xml2::xml_find_all("//a[@title='Visualizar Inteiro Teor']") %>%
      xml2::xml_attr("name")

    resposta <- x %>%
      xml2::xml_find_all(xpath = "//*[@id='divDadosResultado']/table//td//td[@align='left']") %>%
      xml2::xml_text(trim = TRUE) %>% stringr::str_squish() %>%
      paste0(collapse = "\n") %>% stringi::stri_split_regex("\n(?=\\d{4,})") %>%
      unlist()

    pagina <- stringr::str_extract(.x, "(?<=pagina_)\\d+") %>%
      as.integer()

    hora_coleta <- stringr::str_extract(.x, "\\d{4}.+(?=_p)") %>%
      lubridate::ymd_hms(tz="America/Sao_Paulo")

    processo <- stringi::stri_extract_first_regex(resposta,
                                                  "\\d+-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}")

    classe <- stringi::stri_extract_first_regex(resposta,
                                                "Classe:.*")

    assunto <- stringi::stri_extract_first_regex(resposta,
                                                 "Assunto:.*")

    magistrado <- stringi::stri_extract_first_regex(resposta,
                                                    "Magistrado:.*")

    comarca <- stringi::stri_extract_first_regex(resposta,
                                                 "Comarca:.*")

    foro <- stringi::stri_extract_first_regex(resposta, "Foro:.*")


    vara <- stringi::stri_extract_first_regex(resposta, "Vara:.*")

    disponibilizacao <- stringi::stri_extract_first_regex(resposta,
                                                          "Data\\s+de\\s+Disponibiliza\u00e7\u00e3o:.*")

    julgado <- x %>%
      xml2::xml_find_all("//div[@align='justify'][@style='display: none;']") %>%
      xml2::xml_text()


    tibble::tibble(processo, pagina, hora_coleta,classe, assunto, magistrado,
                   comarca, foro, vara, disponibilizacao, julgado, cd_doc)
  }, NULL)) %>%

    dplyr::mutate(dplyr::across(4:10, ~stringi::stri_replace_first_regex(., ".*:\\s?",""))) %>%

    dplyr::mutate_if(is.character, stringr::str_squish) %>%

    dplyr::mutate(disponibilizacao = lubridate::dmy(disponibilizacao),
                  processo = stringr::str_remove_all(processo, "\\D+")) %>%

    dplyr::mutate(duplicado = vctrs::vec_duplicate_detect(processo), .after = hora_coleta)
}

#' @rdname tjsp_ler_cjpg
#' @export
ler_cjpg <- tjsp_ler_cjpg
