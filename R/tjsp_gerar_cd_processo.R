
#' Gera cd_processo a partir de processo
#'
#' @param processo Vetor com números dos processos
#'
#' @return String
#' @export

tjsp_gerar_cd_processo <- function(processo) {

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

      conteudo1 |>
       xml2::xml_find_all("//a[@class='linkProcesso']") |>
        xml2::xml_attr("href") |>
        stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

    

    } else if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='modalIncidentes'])")){


      conteudo1 |>
        xml2::xml_find_all("//input[@id='processoSelecionado']") |>
        xml2::xml_attr("value")

    } else {

         conteudo1 |>
        xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |>
        xml2::xml_text() |>
        stringr::str_extract("(?<=processo.codigo=)\\w+")

    }
}