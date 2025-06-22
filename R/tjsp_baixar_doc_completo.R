#' Baixa documento completo com base no cd_processo
#'
#' @param cd_processo Número do processo
#' @param diretorio Diretório onde armazenar as tabelas
#'
#' @return pdf
#' @export
#'
tjsp_baixar_doc_completo <- function (cd_processo = NULL, diretorio = ".")
{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  purrr::walk(cd_processo,purrr::possibly(~{


    xx <- .x

    arquivo <- file.path(diretorio,paste0("doc_cd_processo_",.x,".pdf"))


    url1 <- .x |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true")

    if (stringr::str_detect(.x, "^DW")){
      url1 <- paste0(url1,"&consultaDeRequisitorios=true")
    }

    r1 <- url1 |>
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



      arquivo <- file.path(diretorio, paste0("doc_cd_processo_pg_", .x, "_cd_processo_sg_",cdProcesso,".pdf"))


    } else{

      url3 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",.x)

      arquivo <- file.path(diretorio,paste0("doc_cd_processo_pg_",.x,".pdf"))


    }

    documentos <-   url3 |>
      httr::GET() |>
      httr::content("text") |>
      httr::GET() |>
      httr::content("text") |>
      stringr::str_extract("(?<=requestScope = )\\X+?(?=;)") |>
      jsonlite::fromJSON() |>
      purrr::pluck("children") |>
      purrr::map(~{

        .x$data$parametros
      }) |>
      rlang::set_names("itensPdfSelecionados") |>
      purrr::list_assign(cdProcesso = xx, separarDocumentos = 'false', acessoPeloPetsg = "")


    url4 <- "https://esaj.tjsp.jus.br/pastadigital/salvarDocumentoPreparado.do"

    localizador  <- httr::POST(url4,
                               body = documentos, encode = "form"
    ) |>
      httr::content("text")

    Sys.sleep(5)

    localizacao <- list(localizador = localizador, cdProcesso = xx)

    url5 <- "https://esaj.tjsp.jus.br/pastadigital/buscarDocumentoFinalizado.do"

    httr::RETRY("POST", url5, body = localizacao, encode = "form", times = 5,
                pause_base = 5,
                pause_cap = 5,
                pause_min = 5 ) |>
      httr::content("text") |>
      httr::GET( httr::write_disk(arquivo, overwrite = T))


  },NULL), .progress = TRUE)

}



