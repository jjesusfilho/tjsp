#' Ler metadados das decisões
#'
#' @param arquivos Caminhos para os htmls
#' @param diretorio Diretório onde se encontram os htmls,
#'       se arquivos for NULL.
#'
#' @return tabela com metadados jurisprudenciais.
#' @export
#'
#' @examples
#' \dontrun{
#' cjsg <- tjsp_ler_cjsg()
#' }
#'
tjsp_ler_cjsg <- function(arquivos = NULL, diretorio = ".") {

  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio,
      pattern = ".html",
      full.names = T
    )
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    resposta <- xml2::read_html(.x, encoding = "UTF-8")

    aC <-
      xml2::xml_find_all(resposta, '//*[@class="assuntoClasse"]') %>%
      xml2::xml_text(trim = T) %>%
      stringr::str_match("(?:Classe.Assunto.\\s+)(\\w.*?)(?: / )(.*)")
    classe <- aC[, 2]
    assunto <- aC[, 3]
    relator <-
      xml2::xml_find_all(resposta, '//tr[2][@class="ementaClass2"][1]') %>%
      xml2::xml_text(trim = T)
    comarca <-
      xml2::xml_find_all(resposta, '//*[@class="ementaClass2"][2]') %>%
      xml2::xml_text(trim = T)
    orgao_julgador <-
      xml2::xml_find_all(resposta, '//*[@class="ementaClass2"][3]') %>%
      xml2::xml_text(trim = T)
    data_julgamento <-
      xml2::xml_find_all(resposta, '//*[@class="ementaClass2"][4]') %>%
      xml2::xml_text(trim = T)
    data_publicacao <-
      xml2::xml_find_all(resposta, '//*[@class="ementaClass2"][5]') %>%
      xml2::xml_text(trim = T)
    ementa <-
      xml2::xml_find_all(resposta, '//*[@class="mensagemSemFormatacao"]') %>%
      xml2::xml_text(trim = T)
    processo <-
      xml2::xml_find_all(resposta, '//*[@class="esajLinkLogin downloadEmenta"]') %>%
      xml2::xml_text(trim = T)
    cdacordao <- xml2::xml_find_all(resposta, "//a[1]/@cdacordao") %>%
      xml2::xml_text()
    tibble::tibble(
      classe,
      assunto,
      relator,
      comarca,
      orgao_julgador,
      data_julgamento,
      data_publicacao,
      processo,
      ementa,
      cdacordao
    ) %>%
     # dplyr::mutate(dplyr::across(1:7, ~iconv(.x, "UTF-8","latin1//TRANSLIT"))) %>%
      dplyr::mutate(dplyr::across(3:7,~stringr::str_remove(.x,".+:\\s?") %>%
                                    stringr::str_trim())) %>%
      dplyr::mutate(dplyr::across(6:7,~lubridate::dmy(.x))) %>%
      dplyr::mutate(processo = stringr::str_remove_all(processo,"\\D+"))


  }, otherwise = NULL))
}


#' @rdname tjsp_ler_cjsg
#' @export
ler_cjsg <- tjsp_ler_cjsg


