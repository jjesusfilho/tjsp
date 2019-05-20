#' Remove as linhas de um dataframe, quando há duplicidade na coluna especificada.
#'
#' @param x data.frame
#' @param y coluna com células a serem identificadas como duplicadas.
#' @keywords duplicated, remove
#' @export
#' @examples
#'
#' rm_duplicados(iris, Species)
rm_duplicados <- function(x, y) {
  y <- rlang::enexpr(y)
  x <- x[which(duplicated(x[, rlang::quo_name(y)]) == FALSE), , drop = FALSE]
  return(x)
}
