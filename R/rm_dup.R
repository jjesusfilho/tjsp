#' Função rm_dup
#'
#' Esta função remove as linhas de um data.frame, quando há duplicidade em uma coluna.
#'
#' @param x data.frame
#' @param y coluna a serem idenficiados as duplicacões.
#' @keywords duplicated, remove
#' @export
#' @examples
#' rm_dup(df,"docname")

rm_dup<-function(x,y){
  x<-x[which(duplicated(x[,y])==FALSE),]
  return(x)
}
