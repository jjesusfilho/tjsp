#' Baixa documento completo com base no cd_processo
#'
#' @param cd_processo Número do processo
#' @param diretorio Diretório onde armazenar as tabelas
#' @param tentativas Número de tentantivas de recuperação do arquivo finalizado.
#'        Não é possível saber de antemão quanto tempo domora para gerar o(s) arquivo(s).
#'        Fixamos em 10 tentativas de 5 segundos cada.
#' @param separar_documentos TRUE para zip, FALSE para pdf
#'
#' @return zip ou pdf a depender da escolha em separar_documentos
#' @export
#'
tjsp_baixar_doc_completo <- function (cd_processo = NULL, diretorio = ".", tentativas = 10, separar_documentos = TRUE)
{

  if(separar_documentos == TRUE){

    separar_documentos <-  'true'

  } else {

    separar_documentos <- "false"
  }

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

      xx <- cdProcesso
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
      purrr::flatten() |>
      as.list() |>
      rlang::set_names("itensPdfSelecionados") |>
      purrr::list_assign(cdProcesso = xx, separarDocumentos = separar_documentos, acessoPeloPetsg = "")



    url4 <- "https://esaj.tjsp.jus.br/pastadigital/salvarDocumentoPreparado.do"

    localizador  <- httr::POST(url4, body = documentos, encode = "form") |>
      httr::content("text")


    localizacao <- list(localizador = localizador, cdProcesso = xx)

    if(separar_documentos == "true"){

      arquivo <- stringr::str_replace(arquivo,"pdf$","zip")
    }

    url5 <- "https://esaj.tjsp.jus.br/pastadigital/buscarDocumentoFinalizado.do"

    r5 <- httr::POST(url5, body = localizacao, encode = "form") |>
      httr::content("text")

    i <- 0

    while(r5 == "" & i <=  tentativas ){

      Sys.sleep(5)

      r5 <- httr::POST(url5, body = localizacao, encode = "form") |>
        httr::content("text")

      i <- i+1

    }

    if (stringr::str_detect(r5,"http")){

      httr::GET(r5, httr::write_disk(arquivo, overwrite = T))

    } else {

      print("Não foi possível baixar o arquivo. Experimente aumentar o número de tentativas.")
    }


  },NULL), .progress = TRUE)

}



