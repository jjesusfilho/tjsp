#' Lê decisões de primeira instância do TJSP baixadas por meio da função tjsp_baixar_cjpg.
#'
#' @param diretorio objeto ou diretório onde se encontram as páginas.
#' @param arquivos Se NULL, informar diretorio
#' @return uma tibble com nove colunas: processo, classe, assunto, magistrado,comarca, foro, vara,
#'     disponibilizacao e julgado (texto da decisão).
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_ler_cjpg()
#' }
tjsp_ler_cjpg <- function(arquivos = NULL, diretorio = ".")
{
  if (is.null(arquivos)) {
    arquivos <- list.files(path = diretorio, pattern = ".html",
                           full.names = TRUE)
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    pagina <- stringr::str_extract(.x, "(?<=pagina_)\\d+") %>%
      as.integer()

    suppressWarnings(
      hora_coleta <- stringr::str_extract(.x, "\\d{4}[\\d_/]+") %>%
        lubridate::ymd_hms(tz = "America/Sao_Paulo")

    )

    x <- .x |>  xml2::read_html(encoding = "UTF-8")

    fundo_cinza <- x |>
      xml2::xml_find_all(xpath = "//tr[@class='fundocinza1']")

    processo <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/a/span[@class='fonteNegrito']") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    cd_doc <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/a[@title='Visualizar Inteiro Teor']") |>
                   xml2::xml_attr("name")) |>
      unlist()

    assunto <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Assunto:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    classe <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Classe:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    comarca <-fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Comarca:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    foro <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Foro:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    vara <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Vara:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    magistrado <- fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Magistrado:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()


    disponibilizacao <-fundo_cinza |>
      purrr::map(~xml2::xml_child(.x, "/tr/td//strong[contains(.,'Data de Disponibiliza\u00E7\u00E3o:')]/following-sibling::text()") |>
                   xml2::xml_text(trim = TRUE)) |>
      unlist()

    julgado <- x %>% xml2::xml_find_all("//div[@align='justify'][@style='display: none;']") %>%
      xml2::xml_text(trim = TRUE)

    tibble::tibble(processo, pagina, hora_coleta, classe,
                   assunto, magistrado, comarca, foro, vara, disponibilizacao,
                   julgado, cd_doc)

  }, NULL)) %>%
    dplyr::mutate(dplyr::across(4:10, ~stringi::stri_replace_first_regex(.,".*:\\s?", ""))) %>% dplyr::mutate_if(is.character, stringr::str_squish) %>%
    dplyr::mutate(disponibilizacao = lubridate::dmy(disponibilizacao),
                  processo = stringr::str_remove_all(processo, "\\D+")) %>%
    dplyr::mutate(duplicado = vctrs::vec_duplicate_detect(processo),
                  .after = hora_coleta)
}

