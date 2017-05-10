#' Function filtra
#'
#' This functions uses regex to filter a data frame based on a column's content
#'
#' @param x data.frame
#' @param column column name where to search for string
#' @param string string to be found in the mentioned column
#' @keywords regex,filter
#' @export
#' @examples
#' filtra(df,"column","string")

filtra<-function(df,column,string){
  d<-df[str_which(df[[var]],string),]
  return(d)
}
