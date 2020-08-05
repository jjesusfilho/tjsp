#' Lê dados cposg, em uma das interfaces.
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório, quando arquivos não informados.
#'
#' @return dataframe
#' @export
#'
ler_dados_cposg2 <- function(arquivos = NULL, diretorio = ".") {

 if (is.null(arquivos)){
  arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )
 }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~ {

    pb$tick()

    processo <- stringr::str_extract(arquivos, "\\d{20}")

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
      tibble::add_column(digital = digital)
  }, otherwise = NULL))
}
