#' Cria vetor informando se o voto por por unanimidade ou por maioria
#'
#' @param x  Vetor de dispositivos
#'
#' @return Vetor
#' @export
#'
votacao <- function(x = NULL) {


    dplyr::case_when(
      stringr::str_detect(x, "(?i)(v\\.\\s?u\\.?|un.ni)") ~ "un\\u00e2nime",
      stringr::str_detect(x, "(?i)v\\.?\\s?u\\.?\\)?\\.?$") ~ "un\\u00e2nime",
      stringr::str_detect(x, "(?i)maioria") ~ "maioria",
      TRUE ~ NA_character_
    )
}
