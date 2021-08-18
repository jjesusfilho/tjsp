#' Baixa tabelas dos documentos do processo
#'
#' @param processos Número do processo
#' @param diretorio Diretório
#'
#' @return html
#' @export
#'
tjsp_cposg_baixar_tabela_docs <- function(processos, diretorio = "."){



  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos <- stringr::str_remove_all(processos, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>%
    abjutils::build_id()

  uri1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  pb <- progress::progress_bar$new(total = length(processos))

  purrr::walk(processos, purrr::possibly(~{

    pb$tick()

    r<-  httr::GET("https://esaj.tjsp.jus.br/cposg/open.do?gateway=true")

    p <- .x

    unificado <- p %>%
      stringr::str_extract(".{15}")

    foro <- p %>%
      stringr::str_extract("\\d{4}$")

    query1<-  list(conversationId = "", paginaConsulta = "1", localPesquisa.cdLocal = "-1",
                   cbPesquisa = "NUMPROC", tipoNuProcesso = "UNIFICADO", numeroDigitoAnoUnificado = unificado,
                   foroNumeroUnificado = foro, dePesquisaNuUnificado = p,
                   dePesquisa = "", uuidCaptcha = "", pbEnviar = "Pesquisar")

    resposta1 <- httr::RETRY("GET",
                             url = uri1, query = query1,
                             quiet = TRUE, httr::timeout(2)
    )

    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {
      conteudo1 <- xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") %>%
        xml2::xml_attr("href") %>%
        xml2::url_absolute("https://esaj.tjsp.jus.br") %>%
        purrr::map(~ httr::RETRY("GET", .x, httr::timeout(2)) %>%
                     httr::content())
    } else {
      conteudo1 <- list(conteudo1)
    }

    purrr::walk(conteudo1,purrr::possibly(~{



      cdProcesso <- .x %>%
        xml2::xml_find_all("//input[@name='cdProcesso']") %>%
        xml2::xml_attr("value")


      arquivo <- file.path(diretorio,paste0("tabela_cposg_docs_processo_",stringr::str_remove_all(p,"\\D"),".html"))

      url1  <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=",cdProcesso,"&conversationId=&_=1599440192646")

      url2 <- httr::GET(url1) %>%
        httr::content("text")



      httr::GET(url2,httr::write_disk(arquivo,overwrite=TRUE))


    },NULL))
  },NULL))

  }


#' @rdname tjsp_cposg_baixar_tabela_docs
#' @export
tjsp_baixar_tabela_docs <- tjsp_cposg_baixar_tabela_docs



