
#' Gera cd_processo a partir de processo
#'
#' A consulta processual de primeiro grau passou a exigir login. Autentique-se
#' antes com [tjsp_autenticar()] (por exemplo, `metodo = "certificado"`).
#'
#' @param processo Número do processo (número único CNJ)
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
                             quiet = TRUE, httr::timeout(30))

    texto1 <- httr::content(resposta1, "text")

    conteudo1 <- xml2::read_html(texto1)

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

      ## No layout atual, o código aparece em "codigoProcesso: '...'" e em
      ## links "cdProcesso=..."; "processo.codigo=" pode apontar para incidentes.
      padroes <- c("(?<=codigoProcesso:\\s{0,5}')\\w+",
                   "(?<=cdProcesso=)\\w+",
                   "(?<=processo\\.codigo=)\\w+")

      cd <- NA_character_

      for (padrao in padroes) {
        cd <- stringr::str_extract(texto1, padrao)
        if (!is.na(cd)) break
      }

      cd

    }
}