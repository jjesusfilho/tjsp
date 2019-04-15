#' \code{tjsp} package
#'
#' Text mining of Brazilian judicial decisions
#'
#'
#' @docType package
#' @name tjsp
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")

  utils::globalVariables(c(".", ".x", ".y",":=","processo","disponibilizacao","valor","variavel","row_id",
                           "size","alternativa","vara","vara2","codigo_processo",
                           "digital","v1","assunto","classe","distribuicao","juiz",
                           "filter","branco","v2","area","vara","prioritaria",
                           ""))

