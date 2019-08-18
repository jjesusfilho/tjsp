#' Applies functions baixar_cjpg from package tjsp and sends data to psql db 
#'
#' @description The dbAppendTable() method assumes that the table has been created beforehand, 
#' @param con PostgreSQL db connection
#' @param tbl Table name. It requires that have already created it.
#' @param ... Other arguments passed to `tjsp::baixar_cjpg`, except directory.
#'
#' @return It doesn't return anything.
#' @export
#'
#' @examples
#' \dontrun{
#' con<-dbx::dbxConnect()
#' DBI::dbCreateTable(con,"cjpg",
#'                   fields=c("processo", "classe", "assunto", "magistrado", "comarca", "foro", 
#'                                "vara", "disponibilizacao", "julgado"))
#'  psql_cjpg(con,"cjpg",livre="feminicidio")                      
#' }
psql_cjpg <- function(con,tbl,...){
  
  d <- tempdir()
  tjsp::baixar_cjpg(diretorio=d,...)
  
  records<-tjsp::ler_cjpg(d)
  
  dbx::dbxInsert(conn=con,table=tbl,records=records,batch_size = 10000)
  
  
  unlink(d)
  dbx::dbxDisconnect(con)
}