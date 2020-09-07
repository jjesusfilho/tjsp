#' Baixar acórdãos do TJSP a partir do id (cdacordo)
#'
#' @param cdacordao Id do acórdão
#' @param diretorio Diretório
#'
#' @return pdf
#' @export
#'
tjsp_baixar_acordaos_cjsg <- function(cdacordao, diretorio = "."){

  url1 <- "https://esaj.tjsp.jus.br/cjsg/consultaCompleta.do?gateway=true"
  r1 <- httr::GET(url1)

  pb <- progress::progress_bar$new(total = length(cdacordao))

  purrr::walk(cdacordao,purrr::possibly(~{

    pb$tick()

     Sys.sleep(1)
    url <- paste0("https://esaj.tjsp.jus.br/cjsg/getArquivo.do?cdAcordao=",.x,"&cdForo=0")

    arquivo <-  file.path(diretorio,paste0("cdacordao_",.x,".pdf"))

    httr::GET(url,httr::write_disk(arquivo,overwrite = TRUE))



  },NULL))

}
