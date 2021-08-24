#' Lê o dispositivo das decisões de segunda instância a partir dos htmls
#'
#' @param arquivos Informar arquivos ou diretório.
#' @param diretorio  Diretório onde se encontram os htmls baixados
#'
#' @return tibble com as os numéros dos processos e respectivos dispositivos
#' @export
#'
#' @examples
#' \dontrun{
#' decisoes <- ler_decisoes_cposg()
#' }
ler_decisoes_cposg <- function (arquivos = NULL, diretorio = ".")
{
  ""
  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }

  processo <- stringr::str_extract(arquivos, "\\d{20}") %>%
    pontuar_cnj()

  lista <- listenv::listenv()

  tentativa <- function(x, y) {
    tryCatch({
      x[ii] %>% xml2::read_html() %>%
        xml2::xml_find_all("//div/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=2]") %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        stats::setNames(c("data_julgamento",  "situacao_julgamento", "dispositivo")) %>%
        cbind(processo = y[ii], ., stringsAsFactors = F)

    }, error = function(e) {
      x[ii] %>% xml2::read_html() %>% xml2::xml_find_all("//div/h2[@class='subtitle'][contains(.,'Julgamentos')]/following::table[position()=2]") %>%
        rvest::html_text() %>% stringr::str_trim() %>%
        tibble::tibble(processo = y[ii], data_julgamento = NA_character_,
                       situacao_julgamento = ., dispositivo = "a decisao n\u00e3o foi disponibilizada no andamento")
    })
  }
  pb <- progress::progress_bar$new(total = length(arquivos))

  for (ii in seq_along(arquivos)) {
    pb$tick()
    lista[[ii]] <- tentativa(arquivos, processo)
  }
  lista <- as.list(lista)
  do.call(rbind, lista)
}
