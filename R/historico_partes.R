#' Baixa metadados do histórico da parte
#'
#' @param cdprocesso Código do processo
#' @param diretorio Diretório onde armazenar
#'
#' @returns html
#' @export
#'
tjsp_baixar_historico_parte_cd_processo <- function(cdprocesso, diretorio = "."){

  cdprocesso <- stringr::str_extract(cdprocesso,"\\w+")


  purrr::walk(cdprocesso, purrr::possibly(~{

   urls <-  httr::GET(paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",.x,"&gateway=true")) |>
      httr::content() |>
      xml2::xml_find_first("//a[contains(@href,'nuSeqParte')]") |>
      xml2::xml_attr('href') |>
      xml2::url_absolute("https://esaj.tjsp.jus.br")

   cd_processo <- .x

   purrr::walk(urls, purrr::possibly(~{

     nu_seq_parte = stringr::str_extract(.x, "(?<=nuSeqParte=)\\d+")

     arquivo <- file.path(diretorio, paste0("historico_parte_cd_processo_",cd_processo,"_nu_seq_parte_",nu_seq_parte, ".html"))

      .x |>
      httr::GET(httr::write_disk(arquivo, overwrite = T))

   }, NULL))

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
tjsp_ler_historico_parte_cd_processo <- function(arquivos  = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "cd_processo")

  }

  purrr::map(arquivos, purrr::possibly(~{


    tjsp_ler_historico_parte_cd_processo1(.x)

  },NULL))
}

#' Lêapenas um arquivo da execução
#'
#' @param arquivo Caminho para arquivo
#'
#' @returns tibble
#'
tjsp_ler_historico_parte_cd_processo1 <- function(arquivo){


  x <- xml2::read_html(arquivo)

  cdprocesso <- arquivo |>
    stringr::str_extract("(?<=cd_processo_)[A-Z0-9]+")

  nu_seq_parte <- stringr::str_extract(arquivo,"(?<=nu_seq_parte_)\\d+")

  parte <- x |>
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

  link <- x |>
    xml2::xml_find_first("//script/text()[contains(.,'PG5')]") |>
    xml2::xml_text() |>
    stringr::str_extract("cdProcesso.+(?=')") |>
    xml2::url_absolute("https://esaj.tjsp.jus.br/cpopg/consultarDadosParte.do?") |>
    tibble::tibble(cdprocesso, nu_seq_parte, link = _)

  principal <- tibble::tibble(variavel = label, valor = valores) |>
    tibble::add_row(variavel = "nome", valor= parte, .before = 1) |>
    tibble::add_column(cdprocesso, nu_seq_parte = nu_seq_parte, .before = 1)

  documentos <- x |>
    xml2::xml_find_first("//div[h2[text()='Documentos']]/following-sibling::table") |>
    rvest::html_table(convert = F) |>
    stats::setNames(c("documento_sigla","documento","documento_expedidor")) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(),.fns = ~.x == "")) |>
    tibble::add_column(cdprocesso, nu_seq_parte = nu_seq_parte, .before = 1)

  historico <- x |>
    xml2::xml_find_first("//div[h2[text()='Hist\u00F3rico da parte']]/following-sibling::table") |>
    rvest::html_table(convert = F) |>
    stats::setNames(c("data_evento","evento","complemento")) |>
    dplyr::filter(!dplyr::if_all(dplyr::everything(),.fns = ~.x == "")) |>
    dplyr::mutate(data_evento = lubridate::dmy(data_evento))  |>
    tibble::add_column(cdprocesso, nu_seq_parte = nu_seq_parte, .before = 1)

  list(principal = principal, documentos = documentos, historico = historico, link  = link)

}
