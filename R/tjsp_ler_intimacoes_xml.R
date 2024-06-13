#' Ler intimações em xml
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório como alternativa.
#'
#' @return tibble
#' @export
#'
tjsp_ler_intimacoes_xml <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "xml$")
  }

purrr::map_dfr(arquivos, purrr::possibly(~{

    x <- .x |>
         xml2::read_html() |>
       xml2::xml_find_all("//e")

  df1 <-  purrr::map_dfr(x, ~{

     e <- .x |>
         xml2::xml_find_first(".") |>
         xml2::xml_attr("i")

     n <- .x |>
       xml2::xml_find_all(".//r") |>
       xml2::xml_attr("n")

     v <- .x|>
       xml2::xml_find_all(".//r") |>
       xml2::xml_attr("v")

     tibble::tibble(e, n, v)

   }) |>
     dplyr::mutate(n = snakecase::to_snake_case(n, transliterations = "Latin-ASCII")) |>
     tidyr::pivot_wider(names_from = n, values_from = v) |>
     tibble::add_column(arquivo = basename(.x), .before = 1)


   df2 <- purrr::map2_dfr(df1$e, df1$ato_tarjas_json, purrr::possibly(~{

     .y |>
       jsonlite::fromJSON() |>
       dplyr::pull(deTarja) |>
       stringr::str_c(collapse = "|") |>
       tibble::tibble(tarjas = _) |>
       tibble::add_column(e = .x)
   }, NULL))

   df1 |>
     dplyr::left_join(df2, by = dplyr::join_by("e")) |>
     dplyr::relocate(tarjas, .before = ato_tarjas_json) |>
     dplyr::mutate(ato_tarjas_json = NULL) |>
     dplyr::mutate(hora_coleta = Sys.time())


},NULL),  .progress = TRUE)

}




