#' Função limpar
#'
#' Esta função limpa o texto em português: retira acentos, retira pontuação, coloca em caixa baixa
#'
#' @param texto

#' @keywords limpar, stop words,
#' @import stringi
#' @import stringr
#' @import tm
#' @import stringr
#' @export
#' @examples
#' limpa(texto)

limpar<-function(texto){
  text<-stringi::stri_trans_general(texto,"Latin-ASCII")
  text<-tolower(text)
  text<-stringr::str_replace_all(text,"\\s+"," ")
  text<-tm::removePunctuation(text)
  text<-tm::stripWhitespace(text)
  text<-tm::removeNumbers(text)
  return(text)
  }
