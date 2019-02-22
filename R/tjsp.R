#' \code{tjsp} package
#'
#' Text mining of Brazilian judicial decisions
#'
#'
#' @docType package
#' @name tjsp

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")

  utils::globalVariables(c(".", ".x", ".y",":=","processo","disponibilizacao","valor","variavel","row_id","size","alternativa"))
