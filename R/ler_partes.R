#' Aplica parser para extrair informações sobre as partes do
#'     processo.
#'
#' @param diretorio diretório onde se encontram os htmls baixados.
#'
#' @return tabela com informações das partes.
#' @export
#'
ler_partes <- function(diretorio = ".") {
  diretorio <- normalizePath(diretorio)

  arquivos <- list.files(
    path = diretorio,
    pattern = ".html",
    full.names = T
  )

  processos <-
    stringr::str_extract(arquivos, "\\d{20}") %>%
    abjutils::build_id(.)

  purrr::map2_dfr(arquivos, processos, purrr::possibly(~ {

    ## Esta primeira parte seleciona a tabela que contêm as partes e a converte em
    ## texto. Infelizmente o html não é muito consistente. Temos de usar regex.
    resposta <- xml2::read_html(.x) %>%
      xml2::xml_find_first("//*[@id='tablePartesPrincipais']") %>%
      xml2::xml_text(trim = T) %>%
      stringr::str_squish()

    ## monta um regex padrão de extração a partir das partes.
    padrao <- resposta %>%
      stringr::str_extract_all("\\w+(\\.\\s)?/?\\w+?\\:") %>%
      unlist() %>%
      paste0(collapse = "|") %>%
      paste0("(", ., ")", ".+?(?=", ., "|$)")

    # Extrai nome e nome da parte, converte em tibble, separa os dois.
    stringr::str_extract_all(resposta, padrao) %>%
      unlist() %>%
      stringr::str_trim() %>%
      tibble::tibble() %>%
      setNames("parte") %>%
      tidyr::separate(parte, c("parte", "parte_nome"), ":\\s?") %>%
      dplyr::mutate(processo = .y) %>%
      dplyr::select(processo, dplyr::everything())
  }, otherwise = NULL))
}
