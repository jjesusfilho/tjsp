#' Lê os metadados de processos de primeira instância.
#'
#' @param arquivos Opcional. Informar se não informar o diretório
#' @param diretorio  Diretório onde se encontram os htmls.
#' @param wide o padrão é o formado longo, com apenas quatro colunas:
#'      processo, digital, variavel, valor.
#'
#' @details Esta função será descontinuada. Provavelmente você
#'     quer usar a função tjsp_ler_dados_cpopg
#' @return tibble com os metadados
#' @export
#'
#' @examples
#' \dontrun{
#' ler_dados_cpopg()
#' }
#'
ler_dados_cpopg <- function(arquivos = NULL, diretorio = ".", wide = FALSE) {

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
    digital <- resposta %>% xml2::xml_find_first("boolean(//*[@class='linkPasta'] |//*[@class='linkConsultaSG'])")
    codigo <- resposta %>%
      xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") %>%
      xml2::xml_attr("href") %>%
      stringr::str_extract("(?<=processo.codigo=)\\w+")
    if (length(codigo) > 1) {
      codigo <- duplicated(codigo) %>%
        which() %>%
        codigo[.] %>%
        unique()
    }
    cdProcesso <- resposta %>%
      xml2::xml_find_first("//*[@name='cdProcesso']") %>%
      xml2::xml_attr("value")
    variavel <- resposta %>%
      xml2::xml_find_all("//table[@id=''][@class='secaoFormBody']//*[@width='150']") %>%
      xml2::xml_text() %>%
      stringr::str_squish()
    valor <- resposta %>%
      xml2::xml_find_all("//table[@id=''][@class='secaoFormBody']//*[@width='150']/following-sibling::td") %>%
      xml2::xml_text() %>%
      stringr::str_squish()
    tibble::tibble(
      processo = .y, codigo_processo = codigo,
      cd_processo = cdProcesso, digital, variavel, valor
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
