#' Filtra tabela (dataframe) com base numa expressão regular.
#'
#' @param x tibble ou data.frame
#' @param coluna coluna
#' @param expressao expressão regular
#' @details Esta função dispensa o uso de aspas para a coluna(NSE). Ela equivale a dplyr::filter(df,str_detect(coluna,expressao))
#' @keywords regex,filter
#' @export
#' @examples
#' df<-data.frame(a = c("furto", "roubo","tráfico"),
#'    b = c("roubo", "homicidio culposo","lesão corporal"),
#'    c = c("roubo", "ameaça","tráfico"))
#'    filtrar(df,c,"roubo")

filtrar <- function(x, coluna, expressao) {
  coluna <- rlang::enexpr(coluna)

  x[stringr::str_which(x[rlang::quo_name(coluna)], expressao), ]

}
