#' Procura a palavra ou frase do segundo vetor que melhor
#'    se aproxima do primeiro. Particularmente útil para
#'    comparar nomes de municípios.
#'
#' @param x Vetor de strings supostamente erradas
#' @param y Vetor de strings suportamente corretas
#'
#' @return vetor com as strings de y próximos
#'     de x.
#' @export
#'
#' @examples
#' wrong_names<-c("Mogi das Cruses","Sao Paulo","CANTA GALLO")
#' correct_names<-c("Canta Galo","Mogi das Cruzes","São Paulo")
#' fuzzy_search(x=wrong_names,y=correct_names)


fuzzy_search<-function(x=NULL,y=NULL){

  if (is.null(x)){
    stop("You have a string vector for x and y")
  }

  if (is.null(y)){
    stop("You have a string vector for x and y")
  }

  x1 <-x %>%
    stringi::stri_trans_general("latin-ascii") %>%
    stringi::stri_trans_tolower() %>%
    stringi::stri_trim_both() %>%
    stringi::stri_replace_all_regex("\\s+","_")
  y1 <-y %>%
    stringi::stri_trans_general("latin-ascii") %>%
    stringi::stri_trans_tolower() %>%
    stringi::stri_trim_both() %>%
    stringi::stri_replace_all_regex("\\s+","_")

  purrr::map(x1,~{
    a<- stringdist::stringdist(.x,y1)
    b<-which.min(a)
    d<-y[b]
  }) %>%
    unlist()

}
