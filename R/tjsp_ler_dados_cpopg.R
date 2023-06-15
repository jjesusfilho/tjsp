#' Extrai a tabela de incidentes
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#' @param wide Colocar em formato wide? Padrão TRUE
#' @return tibble
#' @export
tjsp_ler_dados_cpopg <- function(arquivos = NULL, diretorio = ".", wide = TRUE) {

  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  dados <- purrr::map_dfr(arquivos, purrr::possibly(~ {

    pb <- pb$tick()

    resposta <- .x |>  xml2::read_html()


    processo <- resposta |>
      xml2::xml_find_first("//span[contains(@class,'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^\\d+\\s]") |>
      stringr::str_trim()


    digital <- resposta |>
      xml2::xml_find_first("boolean(//*[@id='linkPasta'] |//*[@id='linkConsultaSG'])")

    situacao <- resposta |>
      xml2::xml_find_first("//span[@id='labelSituacaoProcesso']") |>
      xml2::xml_text()

   codigo <- resposta |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |> 
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    if (length(codigo) > 1) {
      codigo <- duplicated(codigo) |>
        which() |>
        (function(.)
          codigo[.])()  |>
        unique() |>
        stringr::str_c(collapse = "\n")
    }


    variavel <- resposta |>
      xml2::xml_find_all("//div//span[@class='unj-label']") |>
      xml2::xml_text() |>
      stringr::str_squish()

    valor <- resposta |>
      xml2::xml_find_all("//div[span[@class='unj-label']]/div") |>
      xml2::xml_text() |>
      stringr::str_squish()

    tibble::tibble(
      processo, codigo_processo = codigo, digital, situacao, variavel, valor
    )
  }, NULL))


  if (nrow(dados) == 0){

    return(NULL)

  }

  if (wide == TRUE) {

    dados <- dados |>
      dplyr::group_by_at(dplyr::vars(-valor)) |>
      dplyr::mutate(row_id = 1:dplyr::n()) |>
      dplyr::ungroup() |>
      tidyr::spread(key = variavel, value = valor) |>
      dplyr::select(-row_id) |>
      janitor::clean_names()
  }


  return(dados)

}
