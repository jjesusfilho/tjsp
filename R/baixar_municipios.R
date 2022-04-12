#' Baixa municípios do interior de São Paulo com
#'     respectivos fóruns
#'
#' @param numeros Sequência numérica
#' @param diretorio Onde armazenar os htms
#'
#' @return htm
#' @export
#'
baixar_municipios <- function(numeros = 1:645, diretorio = "."){

  uri <- "https://www.emsampa.com.br/delpol/"

 muni <- numeros|>
   stringr::str_pad(3, "left", pad ="0")

 muni <- paste0("sp_", muni, ".htm")

 purrr::walk(muni, purrr::possibly(~{


   url <- paste0(uri, .x)

   arquivo <- file.path(diretorio,.x)

   httr::GET(url, httr::write_disk(arquivo, overwrite = T))


 },NULL))

 }
