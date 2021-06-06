#' Baixar todas as publicações do dje São Paulo.
#'
#' @param processo número do processo com ou sem pontos e hífen.
#' @param diretorio default para o diretório atual.
#'
#' @details Esta função não é vetorizada.
#' @return publicações em html
#' @export
#'
baixar_publicacoes_dje <- function(processo, diretorio = ".") {

  ## Assegura que o o número do processo está no formato cnj.
  processo <- stringr::str_remove_all(processo, "\\D+") %>%
    stringr::str_pad(width = 20, "left", "0") %>%
    pontuar_cnj()

  ## Verifica o número de meses entre a data atual e o mês de janeiro do ano
  ## da distribuição do processo.
  inicio <- stringr::str_extract(processo, "(?<=.{11})\\d{4}") %>%
    paste0(., "-01-01") %>%
    lubridate::ymd() %>%
    lubridate::interval(lubridate::today()) %>%
    lubridate::time_length("month")

  ## Cria um vetor o primeiro dia de cada mês desde o início até o mês anterior
  ## ao atual.
  inicios <- lubridate::floor_date(lubridate::today(), "month") - months(1:inicio)

  ## Cria um vetor com o último dia de cada mês desde o início até o mês anterior
  ## ao mês atual.
  finais <- lubridate::ceiling_date(inicios, "months") - 1

  ## Converte para o formato dd/mm/yyyy
  inicios <- format(inicios, "%d/%m/%Y")
  ## idem
  finais <- format(finais, "%d/%m/%Y")

  ## cria objeto apenas com o número do processo para
  ## nomear o arquivo mais adiante.
  p <- stringr::str_remove_all(processo, "\\D+")

  ## Coloca aspas no número do processo.
  processo <- deparse(processo)

  ## Inicía a requisição

  pb <- progress::progress_bar$new(total = length(inicios))

  purrr::walk2(inicios, finais, ~ {

    pb$tick()

    body <- list(
      dadosConsulta.dtInicio = .x,
      dadosConsulta.dtFim = .y,
      dadosConsulta.cdCaderno = "-11",
      dadosConsulta.pesquisaLivre = processo,
      pagina = ""
    )

    ## Cria o nome do arquivo com a primeira dia do mês, último dia do mês
    ## e número do processo.
    i <- stringr::str_replace_all(.x, "/", "_")
    f <- stringr::str_replace_all(.y, "/", "_")

    arquivo <- sprintf("%s/%s_%s_%s.html", diretorio, i, f, p)


    content <-
      httr::POST(
        "http://www.dje.tjsp.jus.br/cdje/consultaAvancada.do",
        encode = "form",
        body = body
      ) %>%
      httr::content()

    ## Salva somente os arquivos em que constam publicações.
    if (xml2::xml_find_first(content, "boolean(//tr[@class='ementaClass'])")) {
      xml2::write_html(content, arquivo)
    }
  })
}
