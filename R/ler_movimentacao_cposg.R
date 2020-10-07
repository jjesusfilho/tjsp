#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'
ler_movimentacao_cposg <- ler_movimentacao_cpopg <- function(arquivos = NULL,diretorio = ".") {

   if (is.null(arquivos)){
   arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )
}

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{


    pb$tick()

    processo <- stringr::str_extract(.x, "\\d{20}")

    texto <- xml2::read_html(.x) %>%
      xml2::xml_find_first(xpath = "//table/tbody[@id='tabelaTodasMovimentacoes']")


    data <- xml2::xml_find_all(texto, ".//td[@width='120']") %>%
      xml2::xml_text(trim = TRUE) %>%
      lubridate::dmy()

    mov <- xml2::xml_find_all(texto, ".//td[@style='vertical-align: top; padding-bottom: 5px']") %>%
      xml2::xml_text(trim = TRUE)


    tibble::tibble(processo = processo, data = data, movimentacao = mov)
  }, otherwise = NULL))
}
