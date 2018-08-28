#' Baixa ac\\u00f3rd\\u00e3os do Tribunal de Justi\\u00e7a de S\\u00e3o Paulo
#'
#' @param processos vetor com o N\\u00famero do processo com os separadores
#' @param diretorio Diret\\u00f3rio. Default para o corrente.
#'
#' @return pdf do ac\\u00f3rd\\u00e3o com o n\\u00famero como nome.
#' @export
#'
baixar_acordao <- function(processos=NULL, diretorio = "."){
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  if (any(nchar(processos)!=25)){
   warning("Um ou mais n\u00fameros de processos n\u00e3o t\u00eam 25 caracteres,
           eles ser\u00e3o removidos")
   processos<- processos[which(nchar(processos)==25)]
  }

  uri1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"


  for (i in seq_along(processos)) {
    tryCatch({
      unificado <- processos[i] %>%
        stringr::str_extract(".{15}")
      foro <- processos[i] %>%
        stringr::str_extract("\\d{4}$")

      query1 <- list(
        cbPesquisa = "NUMPROC",
        conversationId = "",
        dePesquisa= "",
        dePesquisaNuUnificado= processos[i],
        foroNumeroUnificado= foro,
        localPesquisa.cdLocal= "-1",
        numeroDigitoAnoUnificado= unificado,
        paginaConsulta= "1",
        tipoNuProcesso = "UNIFICADO",
        uuidCaptcha= ""
      )

      resposta1 <- httr::RETRY("GET",
                               url=uri1,
                               query = query1,
                               quiet=TRUE,
                               httr::timeout(2))

      conteudo1 <- httr::content(resposta1)

      documento <- conteudo1 %>%
        xml2::xml_find_first("//tr/td/a[contains(.,'Acord\\u00e3o Finalizado')]") %>%
        xml2::xml_attr("href") %>%
        stringr::str_extract("\\d+")

      cdProcesso <- conteudo1 %>%
        xml2::xml_find_first("//input[@name='cdProcesso']") %>%
        xml2::xml_attr("value")

      tempo <- lubridate::now() %>%
        as.numeric() %>%
        magrittr::multiply_by(1000) %>%
        floor() %>%
        as.character()


      uri2 <-"https://esaj.tjsp.jus.br/cposg/verificarAcessoMovimentacao.do?"

      query2 <- list(
        cdDocumento = documento,
        origemRecurso = "M",
        cdProcesso = cdProcesso,
        conversationId = "",
        `_` = tempo
      )


      ## A sequ\\u00eancia abaixo cria a uri do pdf. A segunda requisi\\u00e7\\u00e3o precisa ocorrer imediatamente
      ## ap\\u00f3s ser gerada pela primeira.

      uri3 <- httr::RETRY("GET",
                          uri2,
                          query = query2,
                          httr::timeout(2),
                          quiet=TRUE) %>%
        httr::content("text") %>%
        unlist() %>%
        httr::RETRY("GET",
                    .,
                    httr::timeout(2),
                    quiet=TRUE) %>%
        httr::content("text") %>%
        stringr::str_extract("nuSeq.+?(?=.,)") %>%
        paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?", .)

      Sys.sleep(1)

      httr::GET(uri3, httr::write_disk(
        paste0(
          diretorio,
          "/",
          stringr::str_remove_all(processos[i], "\\D+"),
          ".pdf"
        )
      ))
    }, error = function(e) {
      e
    }, finally = {
      next
    })
  }

}

