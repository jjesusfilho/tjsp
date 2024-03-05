#' Baixa tabela de documentos com base no cd_processo
#'
#' @param cd_processo Número do processo
#' @param diretorio Diretório onde armazenar as tabelas
#'
#' @return html
#' @export
#'
tjsp_baixar_tabela_cd_processo <- function (cd_processo = NULL, diretorio = ".")
{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  pb <- progress::progress_bar$new(total = length(cd_processo))

  purrr::walk(cd_processo, purrr::possibly(~{

    arquivo <- file.path(diretorio,paste0("tabela_cd_processo_",.x,".html"))


    r1 <- .x |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
      httr::GET()


    if (

    r1 |>
      httr::content() |>
      xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")

    ) {

     cdProcesso <-  r1 |>
        httr::content() |>
        xml2::xml_find_first("//a[@class='linkConsultaSG btn btn-secondary btn-space']") |>
        xml2::xml_attr("href") |>
       stringr::str_extract("(?<=Sg.)\\w+")

    url2 <-  paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=",cdProcesso, "&gateway=true")


     r2 <- httr::GET(url2)

     url3  <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=",cdProcesso,"&conversationId=&_=1599440192646")



     arquivo <- file.path(diretorio, paste0("tabela_cd_processo_pg_", .x, "_cd_processo_sg_",cdProcesso,".html"))


     } else{

    url3 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",.x)

    arquivo <- file.path(diretorio,paste0("tabela_cd_processo_pg_",.x,".html"))


     }

    url3|>
      httr::GET() |>
      httr::content("text") |>
      httr::GET(httr::write_disk(arquivo,overwrite = TRUE))

  }, NULL))

}


