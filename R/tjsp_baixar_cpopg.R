#' Baixar consulta processual de primeiro grau
#'
#' @param processos Número do processo (cnj)
#' @param sono Tempo em segundos entre requisições.
#' @param diretorio Diretório onde serão armazenados os htmls
#'
#' @details Assim como em [tjsp_baixar_cpopg_cd_processo()], as movimentações
#'     (carregadas sob demanda pelo e-SAJ) são sempre baixadas e gravadas no
#'     html, em `tbody#tabelaTodasMovimentacoes`.
#'
#' @return html com dados processuais
#' @export
#'

tjsp_baixar_cpopg <- function(processos = NULL, sono = 1, diretorio = "."){


  processos <- stringr::str_remove_all(processos, "\\D+") |>
    stringr::str_pad(width = 20, "left", "0") |>
    pontuar_cnj()

  purrr::walk(processos, purrr::possibly(~{

    tjsp_baixar_cpopg1(.x,diretorio)

    Sys.sleep(sono)

  },NULL), .progress = TRUE)


}






tjsp_baixar_cpopg1 <- function (processo = NULL, diretorio)
{

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  uri1 <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

    unificado <- processo |>  stringr::str_extract(".{15}")

    foro <- processo |>  stringr::str_extract("\\d{4}$")

    
     query1 <- list(conversationId = "", dadosConsulta.localPesquisa.cdLocal = "-1",
                   cbPesquisa = "NUMPROC", dadosConsulta.tipoNuProcesso = "UNIFICADO",
                   numeroDigitoAnoUnificado = unificado, foroNumeroUnificado = foro,
                   dadosConsulta.valorConsultaNuUnificado = processo, dadosConsulta.valorConsulta = "",
                   uuidCaptcha = "")

    if (foro == '0500'){
       query1$consultaDeRequisitorios = "true"
      }
     
    resposta1 <- httr::RETRY("GET", url = uri1, query = query1,
                             quiet = TRUE, httr::timeout(2))
     
    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {

      codigo_processo <-  xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") |>
        xml2::xml_attr("href") |>
        stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

      conteudo1 <-   codigo_processo |>
        paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
        purrr::map(~ httr::RETRY("GET", .x, httr::timeout(2)) |>
                     httr::content())

    } else if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='modalIncidentes'])")){


      codigo_processo <- conteudo1 |>
        xml2::xml_find_all("//input[@id='processoSelecionado']") |>
        xml2::xml_attr("value")

      conteudo1 <- codigo_processo |>
        purrr::map(~{
          .x |>
            paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
            httr::GET() |>
            httr::content()
        })
    } else {

      codigo_processo <- conteudo1 |>
        xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |>
        xml2::xml_text() |>
        stringr::str_extract("(?<=processo.codigo=)\\w+")


      conteudo1 <- list(conteudo1)

    }

    processo <- stringr::str_remove_all(processo, "\\D+")

    arquivo <- file.path(diretorio, paste0("cpopg_", format(Sys.Date(),
                                                            "%Y_%m_%d_"), processo,"_cd_processo_",codigo_processo,".html"))

    ## O e-SAJ carrega as movimentações sob demanda: busca-as e as insere
    ## no html (mesmo layout de tjsp_baixar_cpopg_cd_processo()). Se falhar,
    ## o arquivo não é gravado, para não ficar um html incompleto.
    purrr::pwalk(list(conteudo1, arquivo, codigo_processo), function(doc, arq, cd) {

      ok <- tryCatch({
        .tjsp_cpopg_incluir_movimentacoes_doc(doc, cd)
        TRUE
      }, error = function(e) {
        message("Falha ao baixar movimenta\u00E7\u00F5es de ", cd, ": ", conditionMessage(e))
        FALSE
      })

      if (ok) xml2::write_html(doc, arq)
    })

}
