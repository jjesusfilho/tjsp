#' Lê dados da api
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório
#'
#' @return Tibble
#' @export
#'
tjsp_api_ler <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = T, pattern = "json$")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()


    df <-  jsonlite::fromJSON(.x) |>
      tibble::add_column(processo = stringr::str_extract(.x, "\\d{20}") , .before = 1)

    if (!is.data.frame(df)){

      df <- tibble::tibble(
        processo = stringr::str_extract(.x, "\\d{20}"),
        nume_processo = NA_character_,
        cd_processo = NA_character_,
        classe =  NA_character_,
        assunto =  NA_character_,
        foro =  NA_character_,
        data_recebimento =  NA_character_,
        tipo =  NA_character_
      )
    }

    df |>
      setNames(c("processo","nume_processo","cd_processo", "classe","assunto","foro", "data_recebimento","tipo")) |>
      dplyr::mutate(nume_processo = stringr::str_remove_all(nume_processo,"\\D"))
  },NULL))

}
