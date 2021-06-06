#' Baixa processo da consulta do cjpg
#'
#' @param processo número do processo
#' @param diretorio diretório
#'
#' @return html
#' @export
#'
tjsp_baixar_cjpg_processo <- function(processo = NULL, diretorio = ".") {
  dia <- Sys.Date() %>% stringr::str_replace_all("-", "_")

  pb <- progress::progress_bar$new(total = length(processo))

  purrr::walk(processo, purrr::possibly(~ {
    p <- .x %>%
      stringr::str_remove_all("\\D+") %>%
      stringr::str_pad(width = 20, "left", "0") %>%
      abjutils::build_id()

    unificado <- stringr::str_extract(p, ".+?(?=\\.8\\.26)")
    foro <- stringr::str_extract(p, "\\d{4}$")



    url_parseada <- list(
      scheme = "http", hostname = "esaj.tjsp.jus.br",
      port = NULL, path = "cjpg/pesquisar.do", query = list(
        conversationId = "",
        dadosConsulta.pesquisaLivre = "", tipoNumero = "UNIFICADO",
        numeroDigitoAnoUnificado = unificado, foroNumeroUnificado = foro,
        dadosConsulta.nuProcesso = p, dadosConsulta.nuProcessoAntigo = "",
        classeTreeSelection.values = "", classeTreeSelection.text = "",
        assuntoTreeSelection.values = "", assuntoTreeSelection.text = "",
        agenteSelectedEntitiesList = "", contadoragente = "0",
        contadorMaioragente = "", cdAgente = "",
        nmAgente = "", `dadosConsulta.agentes[0].cdAgente` = "",
        `dadosConsulta.agentes[0].nmAgente` = "", dadosConsulta.dtInicio = "",
        dadosConsulta.dtFim = "", varasTreeSelection.values = "",
        varasTreeSelection.text = "", dadosConsulta.ordenacao = "DESC"
      )
    )

    url_parseada$query$`dadosConsulta.agentes[0].cdAgente` <- NULL
    url_parseada$query$`dadosConsulta.agentes[0].nmAgente` <- NULL

    class(url_parseada) <- "url"

    url <- httr::build_url(url_parseada)

    arquivo <- stringr::str_c(diretorio, "/", dia, "_", .x, ".html")

    httr::GET(url, httr::write_disk(arquivo, overwrite = TRUE))
  }, otherwise = NULL))
}
