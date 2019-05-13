#' Lê metadados dos processos de segunda instância
#'
#' @param diretorio diretório onde se encontram os htmls baixados.
#'
#' @return tabela com dados do processo
#' @export
#'
#' @examples
#' \dontrun{
#' dados <- ler_dados_cposg()
#' }
ler_dados_cposg <- function(diretorio = ".") {
  a <- list.files(path = diretorio, pattern = ".html", full.names = T)

  processo <- stringr::str_extract(a, "\\d{20}")


  purrr::map_dfr(a, purrr::possibly(~ {
    resposta <- xml2::read_html(.x)

    nomes <- resposta %>%
      xml2::xml_find_all("//label[@class='labelClass']") %>%
      xml2::xml_text() %>%
      stringr::str_extract_all("^.*?(?=:)") %>%
      stringr::str_trim() %>%
      stringr::str_squish()

    digital <- resposta %>%
      xml2::xml_find_first("boolean(//*[@class='linkPasta'] |//*[@class='linkConsultaSG'])")

    cdProcesso <- resposta %>%
      xml2::xml_find_first("//*[@name='cdProcesso']") %>%
      xml2::xml_attr("value")

    valores <- resposta %>%
      xml2::xml_find_all("//label[@class='labelClass']/parent::td/following-sibling::td") %>%
      xml2::xml_text() %>%
      stringr::str_trim() %>%
      stringr::str_squish()

    nomes2 <- resposta %>%
      xml2::xml_find_all("//span[@class='labelClass']") %>%
      xml2::xml_text() %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      paste0("ultima_carga_", .)

    valores2 <- resposta %>%
      xml2::xml_find_all("//span[@class='labelClass']/following-sibling::text()") %>%
      xml2::xml_text() %>%
      stringr::str_trim() %>%
      stringr::str_squish() %>%
      {
        if (length(.) == 0) . <- NA_character_ else . <- .
      }


    as.list(c(valores, valores2)) %>%
      setNames(c(nomes, nomes2)) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      tidyr::separate(processo, c("processo", "situacao"), sep = "\\w+$", extra = "merge") %>%
      tibble::add_column(cd_processo = cdProcesso) %>%
      tibble::add_column(digital=digital)
  }, otherwise = NULL))
}
