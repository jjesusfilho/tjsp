#' Função clearPt
#'
#' Esta função limpa o texto em português: retira acentos, retira pontuação, coloca em minúsculo
#'     Remove stopwords, retira caracteres ordinais.
#'
#' @param x objeto texto a ser submetido à conversão
#' @param lower Lógico. Colocar em caixa baixa?
#' @param stopwords Lógico. Remover stopwords
#' @param accent  Lógico. Remover acentos?
#' @param punctuation Lógico. Remover pontuação
#' @param whitespace  Lógico. Remover espaços em branco
#' @param numbers     Lógico. Remover números
#' @param ordinal    Leogico. Remover indicadores de ordinais
#' @keywords clean, stopwords,
#' @import stringi
#' @import stringr
#' @import tm
#' @export
#' @examples
#' clearPt(texto)

clearPt<-function(x,lower=T,stopwords=T,accent=T,punctuation=T,whitespace=T,numbers=T,ordinal=T){
  if (lower)
    x<-tolower(x)
  if (stopwords)
    x<=tm::removeWords(x,tm::stopwords(kind="pt"))
  if (accent)
    x<-stringi::stri_trans_general(x,"Latin-ASCII")
  if (punctuation)
    x = str_replace_all(x,"[[:punct:]]", "")
  if (whitespace) {
    x = str_replace_all(x,"[ \t]{2,}", " ")
    x = str_replace_all(x,"^\\s+|\\s+$", "")
  }
  if(numbers)
    x<-str_replace_all(x,"[[:digit:]]","")

  if (ordinal)
    x<-str_replace_all(x,"(º|ª)","")

  return(x)
}
