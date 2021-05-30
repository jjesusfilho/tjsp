#' Lê os metadados de processos de primeira instância.
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar apenas se não informou arquivos
#' @param wide Padrão para FALSE. Colocar TRUE se quiser manter em formato wide
#'
#' @return tibble
#' @export
#'
tjsp_ler_dados_cpopg <- function(arquivos = NULL, diretorio = ".", wide = FALSE) {

  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }
  processos <- stringr::str_extract(arquivos, "\\d{20}")

  pb <- progress::progress_bar$new(total = length(processos))

  dados <- purrr::map2_dfr(arquivos, processos, purrr::possibly(~ {

    pb <- pb$tick()

    resposta <- .x %>% xml2::read_html()

    digital <- resposta %>% xml2::xml_find_first("boolean(//*[@id='linkPasta'] |//*[@id='linkConsultaSG'])")

    situacao <- resposta %>%
      xml2::xml_find_first("//span[@id='labelSituacaoProcesso']") %>%
      xml2::xml_text()

    codigo <- resposta %>%
      xml2::xml_find_all("//a[contains(@href,'processo.codigo')]/@href|//form[contains(@action,'processo.codigo')]/@action") %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    if (length(codigo) > 1) {
      codigo <- duplicated(codigo) %>%
        which() %>%
        codigo[.] %>%
        unique() %>%
        stringr::str_c(collapse = "\n")
    }


    variavel <- resposta %>%
      xml2::xml_find_all("//div//span[@class='unj-label']") %>%
      xml2::xml_text() %>%
      stringr::str_squish()

    valor <- resposta %>%
      xml2::xml_find_all("//div[span[@class='unj-label']]/div") %>%
      xml2::xml_text() %>%
      stringr::str_squish()

    tibble::tibble(
      processo = .y, codigo_processo = codigo, digital, situacao, variavel, valor
    )
  }, NULL))
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
