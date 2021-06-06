#' Baixa acórdãos do Tribunal de Justiça de São Paulo a partir do andamento
#'
#' @param processos vetor com o número do processo com ou sem os separadores
#' @param diretorio Diretório. Default para o corrente.
#'
#' @return tabela com metadados do pdf e pdf do acórdão baixado com data
#'    do acordao e número do processo.
#' @export
#'

tjsp_baixar_acordaos_cposg2 <- function(processos = NULL,
                            diretorio = ".") {
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos <- stringr::str_remove_all(processos, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>%
    abjutils::build_id()

  uri1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  pb <- progress::progress_bar$new(total = length(processos))

  purrr::map_dfr(processos, purrr::possibly(~ {

    pb$tick()

    httr::GET("https://esaj.tjsp.jus.br/cposg/open.do?gateway=true")

    p <- .x

    unificado <- p %>%
      stringr::str_extract(".{15}")

    foro <- p %>%
      stringr::str_extract("\\d{4}$")

    query1 <- list(
      cbPesquisa = "NUMPROC", conversationId = "",
      dePesquisat = "", dePesquisaNuUnificado = p, foroNumeroUnificado = foro,
      localPesquisa.cdLocal = "-1", numeroDigitoAnoUnificado = unificado,
      paginaConsulta = "1", tipoNuProcesso = "UNIFICADO",
      uuidCaptcha = ""
    )

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

    conteudo1 %>%
      purrr::map_dfr(purrr::possibly(~ {
        cc <- .x

        doc <- cc %>% xml2::xml_find_all("//tr/td/a[contains(.,'Acord\\u00e3o Finalizado')]|//tr/td/a[contains(.,'Julgado virtualmente')]")

        doc_num <- doc %>%
          xml2::xml_attr("href") %>%
          stringr::str_extract("\\d+")

        doc_texto <- doc %>%
          xml2::xml_text(trim = TRUE)

        decisao <- doc %>%
          xml2::xml_find_all("following-sibling::span") %>%
          xml2::xml_text()

        data_decisao <- doc %>%
          xml2::xml_find_all("../preceding-sibling::td[2]") %>%
          xml2::xml_text(trim = TRUE) %>%
          lubridate::dmy()

        cdProcesso <- cc %>%
          xml2::xml_find_all("//input[@name='cdProcesso']") %>%
          xml2::xml_attr("value")

        tempo <- lubridate::now() %>%
          as.numeric() %>%
          magrittr::multiply_by(1000) %>%
          floor() %>%
          as.character()

        uri2 <- purrr::map_chr(doc_num, ~ {
          uri_parseada <- httr::parse_url("https://esaj.tjsp.jus.br/cposg/verificarAcessoMovimentacao.do?")
          uri_parseada$query <- list(
            cdDocumento = .x,
            origemRecurso = "M", cdProcesso = cdProcesso,
            conversationId = "", `_` = tempo
          )
          httr::build_url(uri_parseada)
        })


        uri3 <- purrr::map_chr(uri2, ~ {
          httr::RETRY("GET", .x, httr::timeout(2), quiet = TRUE) %>%
            httr::content("text") %>%
            unlist() %>%
            httr::RETRY("GET", ., httr::timeout(2), quiet = TRUE) %>%
            httr::content("text") %>%
            stringr::str_extract("nuSeq.+?(?=.,)") %>%
            paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?", .)
        })

        Sys.sleep(1)

        purrr::map2(uri3, data_decisao, ~ {
          httr::GET(.x, httr::write_disk(paste0(
            diretorio,
            "/", stringr::str_replace_all(.y, "/", "_"),
            "_", stringr::str_remove_all(p, "\\D+"), ".pdf"
          ),
          overwrite = TRUE
          ))
        })

        tibble::tibble(
          processo = p, data_jugalmento = data_decisao,
          doc_texto = doc_texto, decisao = decisao, doc_num = doc_num,
          url = uri3
        )
      }, otherwise = NULL))
  }, otherwise = NULL))
}

