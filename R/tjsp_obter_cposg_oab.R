#' Obter processos de segunda instância pela oab
#'
#' @param oab Número da OAB
#'
#' @return Tibble
#' @export
#'
tjsp_obter_cposg_oab <- function (oab = NULL)
{

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  url1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  url2 <- "https://esaj.tjsp.jus.br/cposg/trocarPagina.do?"

  query1 <- list(conversationId = "", dadosConsulta.localPesquisa.cdLocal = "-1",
                 cbPesquisa = "NUMOAB", dadosConsulta.tipoNuProcesso = "UNIFICADO",
                 dePesquisa = oab, uuidCaptcha = "")

  resposta <- httr::RETRY(verb = "GET", url = url1, query = query1,
                          httr::timeout(30))


  max_pag <- resposta %>%
    httr::content() %>%
    xml2::xml_find_all(xpath = "//span[@class='resultadoPaginacao unj-font unj-lh-32 unj-caption']") %>%
    xml2::xml_text(trim = TRUE) %>%
    .[[1]] %>%
    stringr::str_extract("\\d+") %>%
    as.numeric() %>%
    `/`(25) %>%
    ceiling()


  paginas <- 1:max_pag %>% as.character()

  pb <- progress::progress_bar$new(total = max_pag)

  purrr::map_dfr(paginas, purrr::possibly(~{

    pb$tick()

    query2 <- list(paginaConsulta = .x, conversationId = "",
                   dadosConsulta.localPesquisa.cdLocal = "-1",
                   cbPesquisa = "NUMOAB", dadosConsulta.tipoNuProcesso = "UNIFICADO",
                   dePesquisa = oab, uuidCaptcha = "")

    resposta <- httr::RETRY("GET", url = url2, query = query2,
                            quiet = TRUE, httr::timeout(2))

    x <- httr::content(resposta) |>
      xml2::xml_find_all("//div[@id='listagemDeProcessos']/ul/li")


    processo <- x |>
      purrr::map_chr(~xml2::xml_child(.x,"/div[@class='nuProcesso col-md-3']")|>
                       xml2::xml_text(trim = TRUE) |>
                       stringr::str_remove_all("\\D"))

    tipo_participacao <- x |>
      purrr::map_chr(~xml2::xml_child(.x,'/label[@class="unj-label tipoDeParticipacao"]') |>
                       xml2::xml_text(trim = T))

    nome_participante <- x |>
      purrr::map_chr(~xml2::xml_child(.x, '/div[@class="unj-base-alt nomeParticipante"]') |>
                       xml2::xml_text(trim = T))

    nome<- nome_participante |>
      stringr::str_extract(".+")

    oab <- nome_participante |>
      stringr::str_squish() |>
      stringr::str_extract("(?<=OAB ).+")

    classe <- x |>
      purrr::map_chr(~xml2::xml_child(.x, "/div[@class='classeProcesso']") |>
                       xml2::xml_text())


    assunto <- x |>
      purrr::map_chr(~xml2::xml_child(.x, "/div[@class='assuntoProcesso']") |>
                       xml2::xml_text())

    dl <- x |>
      purrr::map_chr(~xml2::xml_child(.x,"/div[@class='dataLocalDistribuicao']") |>
                       xml2::xml_text())

    tibble::tibble(processo, tipo_participacao,
                   nome, oab, classe, assunto, dl) |>
      tidyr::separate(dl, c("data_distribuicao","unidade"), sep = " - ") |>
      dplyr::mutate(data_distribuicao = lubridate::dmy(data_distribuicao)) |>
      dplyr::select(processo, data_distribuicao, unidade, dplyr::everything())

  }, NULL))
}
