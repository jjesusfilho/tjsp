#' Cria vetor informando se o voto por por unanimidade ou por maioria
#'
#' @param x  Vetor de dispositivos
#'
#' @return Vetor
#' @export
#'
tjsp_votacao <- function(x = NULL) {


    dplyr::case_when(
      stringr::str_detect(x, "(?i)(v\\.\\s?u\\.?|un.ni)") ~ "unanime",
      stringr::str_detect(x, "(?i)v\\.?\\s?u\\.?\\)?\\.?$") ~ "unanima",
      stringr::str_detect(x, "(?i)maioria") ~ "maioria",
      TRUE ~ NA_character_
    )
}


#' @rdname tjsp_votacao
#' @export
votacao <- tjsp_votacao
