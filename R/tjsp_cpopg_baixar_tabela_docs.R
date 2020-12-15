#' Baixa tabela de documentos dos processos do TJSP
#'
#' @param processos Número do processo
#' @param diretorio Diretório onde armazenar as tabelas
#'
#' @return html
#' @export
#'
tjsp_cpopg_baixar_tabela_docs <- function (processos = NULL, diretorio = ".")
{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  processos <- stringr::str_remove_all(processos, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>% abjutils::build_id()

  uri1 <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

  pb <- progress::progress_bar$new(total = length(processos))

  purrr::walk(processos, purrr::possibly(~{

    pb$tick()

    p <- .x
    unificado <- p %>% stringr::str_extract(".{15}")
    foro <- p %>% stringr::str_extract("\\d{4}$")
    query1 <- list(conversationId = "", dadosConsulta.localPesquisa.cdLocal = "-1",
                   cbPesquisa = "NUMPROC", dadosConsulta.tipoNuProcesso = "UNIFICADO",
                   numeroDigitoAnoUnificado = unificado, foroNumeroUnificado = foro,
                   dadosConsulta.valorConsultaNuUnificado = p, dadosConsulta.valorConsulta = "",
                   uuidCaptcha = "")
    resposta1 <- httr::RETRY("GET", url = uri1, query = query1,
                             quiet = TRUE, httr::timeout(2))
    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {
      conteudo1 <- xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") %>%
        xml2::xml_attr("href") %>% xml2::url_absolute("https://esaj.tjsp.jus.br") %>%
        purrr::map(~httr::RETRY("GET", .x, httr::timeout(2)) %>%
                     httr::content())
    } else {
      conteudo1 <- list(conteudo1)
    }
    purrr::walk(conteudo1,purrr::possibly(~{

      arquivo <- file.path(diretorio,paste0("tabela_cpopg_docs_processo_",stringr::str_remove_all(p,"\\D"),".html"))


      url1 <- .x %>%
        xml2::xml_find_first("//a[@id='linkPasta']") %>%
        xml2::xml_attr("href") %>%
        paste0("https://esaj.tjsp.jus.br",.)


      httr::GET(url1,httr::write_disk(arquivo,overwrite = TRUE))


    },NULL))
  }, NULL))
}
