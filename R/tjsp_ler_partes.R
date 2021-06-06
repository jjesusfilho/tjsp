#' Ler Partes do cpopg ou cposg
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório se não informar arquivos
#' @details São criadas quatros colunas : processo,
#'     tipo_parte, parte e representante. Esta última
#'     pode ser tanto o procurador quanto o representante
#'     legal.
#' @return tibble
#' @export
#'
tjsp_ler_partes <- function(arquivos = NULL,diretorio = ".") {

  if (is.null(arquivos)) {

    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }

  processos <-
    stringr::str_extract(arquivos, "\\d{20}")

  pb <- progress::progress_bar$new(total= length(arquivos))



    lista <- purrr::map2_dfr(arquivos,processos,purrr::possibly(~{
      pb$tick()
      x <- xml2::read_html(.x)

      if (
      xml2::xml_find_first(x,"boolean(//table[@id='tableTodasPartes'])")
      ) {

      x %>%
        xml2::xml_find_first("//table[@id='tableTodasPartes']") %>%
        rvest::html_table() %>%
        setNames(c("tipo_parte","parte")) %>%
        tidyr::separate(parte,c("parte","representante"),sep = "(?<=\\S)\\s{10,}", extra = "merge") %>%
        tibble::add_column(processo = .y, .before = 1)
      } else {

        x %>%
          xml2::xml_find_first("//table[@id='tablePartesPrincipais']") %>%
          rvest::html_table() %>%
          setNames(c("tipo_parte","parte")) %>%
          tidyr::separate(parte,c("parte","representante"),sep = "(?<=\\S)\\s{10,}", extra = "merge") %>%
          tibble::add_column(processo = .y, .before = 1)

      }

    },NULL))

}

#' @rdname tjsp_ler_partes
#' @export
ler_partes <- tjsp_ler_partes


