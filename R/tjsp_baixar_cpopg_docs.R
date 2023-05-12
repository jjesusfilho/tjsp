#' Baixar petições e docs a partir da tabela lida com tjsp_ler_tabela_docs
#'
#' @param df dataframe com colunas processo, id_doc e url_doc
#' @param diretorio Diretório
#'
#' @return pdfs
#' @export
#'
tjsp_baixar_cpopg_docs <- function(df,diretorio = "."){



  lista <- df |>
    dplyr::mutate(p = stringr::str_remove_all(processo, "\\D+") |>
                    stringr::str_pad(width = 20, "left", "0") |>
                    pontuar_cnj()) |>
    dplyr::group_split(processo)


  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  uri1 <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

  pb <- progress::progress_bar$new(total = length(lista))


  purrr::walk(lista, purrr::possibly(~{

    pb$tick()

    p <- unique(.x$p)

    unificado <- p |> stringr::str_extract(".{15}")

    foro <- p |> stringr::str_extract("\\d{4}$")

    query1 <- list(conversationId = "", dadosConsulta.localPesquisa.cdLocal = "-1",
                   cbPesquisa = "NUMPROC", dadosConsulta.tipoNuProcesso = "UNIFICADO",
                   numeroDigitoAnoUnificado = unificado, foroNumeroUnificado = foro,
                   dadosConsulta.valorConsultaNuUnificado = p, dadosConsulta.valorConsulta = "",
                   uuidCaptcha = "")

    resposta1 <- httr::RETRY("GET", url = uri1, query = query1,
                             quiet = TRUE, httr::timeout(2))

    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {
      conteudo1 <- xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") |>
        xml2::xml_attr("href") |>
        xml2::url_absolute("https://esaj.tjsp.jus.br") |>
        purrr::map(~httr::RETRY("GET", .x, httr::timeout(2)) |>
                     httr::content())
    }  else {
      conteudo1 <- list(conteudo1)
    }

    dd <- .x

    purrr::walk(conteudo1, purrr::possibly(~{



      if (
        .x |>
        xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")
      ) {

        cd_processo <- .x |>
          xml2::xml_find_first("//a[@class='linkConsultaSG btn btn-secondary btn-space']") |>
          xml2::xml_attr("href") |>
          stringr::str_extract("(?<=cdProcessoSg=)\\w+")

        r1 <- cd_processo |>
          paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
          httr::GET()

        url1 <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=", cd_processo,"&conversationId=&_=1599440192646")

      } else {


        url1 <- .x |>
          xml2::xml_find_first("//a[@id='linkPasta']") |>
          xml2::xml_attr("href")  |>
          xml2::url_absolute("https://esaj.tjsp.jus.br")

      }

      r2 <-  url1 |>
        httr::GET() |>
        httr::content("text") |>
        httr::GET()




      tjsp_baixar_docs(dd$processo, dd$id_doc, dd$pagina,  dd$url_doc, diretorio)


    },NULL))

  },NULL))
}



#' Função interna do tjsp_baixar_cpopg_docs
#' @param processos Número do processo
#' @param id_doc Id do documento
#' @param pagina Pagina
#' @param urls Urls dos documentos
#' @param diretorio Diretório
#'
#' @return pdfs
#'
tjsp_baixar_docs <- function(processos  = NULL,
                             id_doc = NULL,
                             pagina = NULL,
                             urls = NULL,
                             diretorio = NULL){


  processos <- stringr::str_remove_all(processos,"\\D")

  id <- stringr::str_c(processos,
                       "_id_doc_",id_doc,
                       "_pagina_",pagina
  )

  pb <- progress::progress_bar$new(total = length(processos))


  purrr::walk2(urls,id,purrr::possibly(~{

    pb$tick()

    arquivo <- file.path(diretorio,paste0(.y,".pdf"))

    httr::GET(.x,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}
