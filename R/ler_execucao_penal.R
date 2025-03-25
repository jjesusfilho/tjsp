#' Baixa metadados da execução penal
#'
#' @param cdprocesso Código do processo
#' @param diretorio Diretório onde armazenar
#'
#' @returns html
#' @export
#'
tjsp_baixar_execucao_penal_cd_processo <- function(cdprocesso, diretorio = "."){

  cdprocesso <- stringr::str_extract(cdprocesso,"\\w+")


  purrr::walk(cdprocesso, purrr::possibly(~{


    arquivo <- file.path(diretorio, paste0("execucao_penal_cd_processo_",.x, ".html"))

    httr::GET(paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",.x,"&gateway=true")) |>
      httr::content() |>
      xml2::xml_find_first("//a[contains(@href,'nuSeqParte')]") |>
      xml2::xml_attr('href') |>
      xml2::url_absolute("https://esaj.tjsp.jus.br") |>
      httr::GET(httr::write_disk(arquivo, overwrite = T))

  }, NULL), .progress = TRUE)
}

#' Lê os metadados da execução penal
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório
#'
#' @returns tibble
#' @export
#'
#'
tjsp_ler_execucao_penal_cd_processo <- function(arquivos  = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "cd_processo")

  }

  purrr::map(arquivos, purrr::possibly(~{


    tjsp_ler_execucao_penal_cd_processo1(.x)

  },NULL))
}

#' Lêapenas um arquivo da execução
#'
#' @param arquivo Caminho para arquivo
#'
#' @returns tibble
#'
tjsp_ler_execucao_penal_cd_processo1 <- function(arquivo){


  x <- xml2::read_html(arquivo)

  cdprocesso <- arquivo |>
    stringr::str_extract("(?<=cd_processo_)\\w+")


  nome <- x |>
    xml2::xml_find_all("//h2[@class='subtitle tituloDoBloco']") |>
    xml2::xml_text() |>
    purrr::pluck(2)

  label <- x |>
    xml2::xml_find_all("//td[@class='label']|//td/div[@class='label']") |>
    xml2::xml_text() |>
    snakecase::to_snake_case(transliterations = "Latin-ASCII")

  valores <- x |>
    xml2::xml_find_all("//td[@class='label']/following-sibling::td|//td/div[@class='label']/following-sibling::text()") |>
    xml2::xml_text(trim = T)

  principal <- tibble::tibble(variavel = label, valor = valores) |>
    tibble::add_row(variavel = "nome", valor= nome, .before = 1) |>
    tibble::add_column(cdprocesso, .before = 1)

  documentos <- x |>
    xml2::xml_find_first("//div[h2[text()='Documentos']]/following-sibling::table") |>
    rvest::html_table(convert = F) |>
    stats::setNames(c("documento_sigla","documento","documento_expedidor")) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(),.fns = ~.x == "")) |>
    tibble::add_column(cdprocesso, .before = 1)

  historico <- x |>
    xml2::xml_find_first("//div[h2[text()='Hist\u00F3rico da parte']]/following-sibling::table") |>
    rvest::html_table(convert = F) |>
    stats::setNames(c("data_evento","evento","complemento")) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(),.fns = ~.x == "")) |>
    dplyr::mutate(data_evento = lubridate::dmy(data_evento))  |>
    tibble::add_column(cdprocesso, .before = 1)

  link <- x |>
    xml2::xml_find_first("//script/text()[contains(.,'PG5')]") |>
    xml2::xml_text() |>
    stringr::str_extract("cdProcesso.+(?=')") |>
    xml2::url_absolute("https://esaj.tjsp.jus.br/cpopg/consultarDadosParte.do?") |>
    tibble::tibble(cdprocesso, link = _)

  list(principal, documentos, historico, link)

}
