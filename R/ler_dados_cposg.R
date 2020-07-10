#' Lê metadados dos processos de segunda instância
#'
#' @param arquivos Vetor de arquivos. Se NULL, informar diretório.
#' @param diretorio Diretório onde se encontram os htmls. Informar
#'     apenas se os arquivos não forem informados.
#'
#' @return tabela com dados do processo
#' @export
#'
#' @examples
#' \dontrun{
#' dados <- ler_dados_cposg()
#' }
ler_dados_cposg <- function(arquivos = NULL, diretorio = ".") {

  if(is.null(arquivos)){
  arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )
}

 pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    resposta <- xml2::read_html(.x)

    nomes <- resposta %>%
      xml2::xml_find_all("//span[@class='unj-label']") %>%
      xml2::xml_text(trim=TRUE)

    digital <- resposta %>%
      xml2::xml_find_first("boolean(//*[@title='Pasta Digital'] |//*[@class='linkConsultaSG'])")

    cdProcesso <- resposta %>%
      xml2::xml_find_first("//*[@name='cdProcesso']") %>%
      xml2::xml_attr("value")

    valores <- resposta %>%
      xml2::xml_find_all("//span[@class='unj-label']/following-sibling::div") %>%
      xml2::xml_text()

    tibble::tibble(processo = processo, cd_processo = cdProcesso, variavel = nomes,
                   valor = valores)

  }, otherwise = NULL)) %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names()
}
