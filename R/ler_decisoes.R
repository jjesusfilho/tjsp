#' Ler decisoes
#'
#' @param arquivos Caminho para os htmls baixados. Se NULL,
#'     usar diretório
#' @param diretorio Diretório onde se encontram os htmls baixados
#'
#' @return tibble com as os numéros dos processos e respectivas decisões
#' @export
#'
#' @examples
#' \dontrun{
#' ler_decisoes()
#' }
ler_decisoes <- function (arquivos = NULL, diretorio = ".")
{
  ""
  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }

  processo <- stringr::str_extract(arquivos, "\\d{20}") %>%
    abjutils::build_id()

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
