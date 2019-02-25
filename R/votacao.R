#' Cria coluna no dataframe informando se o voto por por unanimidade
#'
#' @param df base lida por ler_dados
#' @param x  coluna lida com dispositivo da decisão.
#'
#' @return mesmo dataframe adicionado da coluna votacao
#' @export
#'
votacao <- function(df,x=NULL){

  x1 <- rlang::enquo(x)

  df %>%
    dplyr::mutate(votacao = dplyr::case_when(
      stringr::str_detect(!!x1,"(?i)(v\\.\\s?u\\.?|un.ni)") ~ "unânime",
      stringr::str_detect(!!x1,"(?i)v\\.?\\s?u\\.?\\)?\\.?$") ~ "unânime",
      stringr::str_detect(!!x1,"(?i)maioria") ~ "maioria",
      TRUE  ~ "indefinido"))
}
