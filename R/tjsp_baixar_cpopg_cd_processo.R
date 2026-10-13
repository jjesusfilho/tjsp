#' Baixa dados processuais com base no código do processo
#'
#' @details O e-SAJ carrega as movimentações sob demanda (botão "Exibir
#'     movimentações", via `carregarMovimentacoesAjax.do`), então o HTML de
#'     `show.do` vem sem elas. Por isso a função sempre busca todas as páginas
#'     de movimentações e as grava no próprio html, em
#'     `tbody#tabelaTodasMovimentacoes`, o mesmo layout que os leitores
#'     (`tjsp_ler_movimentacao()`) esperam. Se a busca das movimentações
#'     falhar, o arquivo é apagado para não ficar um html incompleto.
#'
#' @param cd_processo Código do processo. Possivelmente, a coluna
#'     cd_doc do cjpg obtida com tjsp_ler_cjpg pode ser usada.
#' @param diretorio Diretório onde armazenar os arquivos.
#'
#' @return htmls
#' @export
#'
tjsp_baixar_cpopg_cd_processo <- function(cd_processo, diretorio = "."){

  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  pb <- progress::progress_bar$new(total = length(cd_processo))

  purrr::walk(cd_processo, purrr::possibly(~{


    pb$tick()
    arquivo <- file.path(diretorio, paste0("cpopg_cd_processo_",.x, ".html"))

    resposta <- httr::GET(paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",.x,"&gateway=true"),
              httr::write_disk(arquivo, overwrite = T))

    if (httr::status_code(resposta) == 200) {
      tryCatch(
        .tjsp_cpopg_incluir_movimentacoes(arquivo, .x),
        error = function(e) {
          unlink(arquivo)
          message("Falha ao baixar movimentações de ", .x, ": ", conditionMessage(e))
        }
      )
    }

  }, NULL))
}

#' Busca todas as páginas de movimentações de um processo
#'
#' Replica o que o navegador faz ao clicar em "Exibir movimentações" e
#' depois em "Mais": a primeira chamada a `carregarMovimentacoesAjax.do`
#' devolve a primeira página e um cursor opaco; cada chamada seguinte, com
#' `cursor`, devolve as linhas da página seguinte e, se houver mais,
#' um novo cursor na linha oculta `tr#trCursorMovimentacoes`.
#'
#' @param cd_processo Código do processo.
#' @param max_paginas Limite de segurança de páginas.
#'
#' @return vetor de caracteres com o html de cada linha (`tr`), na ordem do
#'     e-SAJ (mais recente primeiro).
#' @noRd
.tjsp_cpopg_buscar_movimentacoes <- function(cd_processo, max_paginas = 500){

  url <- "https://esaj.tjsp.jus.br/cpopg/carregarMovimentacoesAjax.do"

  ler <- function(query) {
    r <- httr::RETRY("GET", url, query = query, times = 3, quiet = TRUE)
    httr::stop_for_status(r)
    httr::content(r, "text", encoding = "UTF-8")
  }

  html <- ler(list(`processo.codigo` = cd_processo))

  pagina <- xml2::read_html(html)

  linhas <- pagina |>
    xml2::xml_find_all("//tbody[@id='tabelaPrimeiraPaginaMovimentacoes']/tr") |>
    as.character()

  cursor <- pagina |>
    xml2::xml_find_first("//input[@id='cursorMovimentacoesPaginado']") |>
    xml2::xml_attr("value")

  n <- 1
  while (!is.na(cursor) && n < max_paginas) {

    n <- n + 1

    html <- ler(list(`processo.codigo` = cd_processo, cursor = cursor))

    pagina <- xml2::read_html(paste0("<table><tbody>", html, "</tbody></table>"))

    novas <- pagina |>
      xml2::xml_find_all("//tr[not(@id='trCursorMovimentacoes')]") |>
      as.character()

    linhas <- c(linhas, novas)

    cursor <- pagina |>
      xml2::xml_find_first("//tr[@id='trCursorMovimentacoes']") |>
      xml2::xml_attr("data-cursor")
  }

  linhas
}

#' Insere as movimentações no html baixado de `show.do`
#'
#' Troca o `div#containerMovimentacoes` vazio por uma tabela com
#' `tbody#tabelaTodasMovimentacoes`.
#'
#' @param arquivo Caminho do html.
#' @param cd_processo Código do processo.
#'
#' @return `arquivo`, invisivelmente.
#' @noRd
.tjsp_cpopg_incluir_movimentacoes <- function(arquivo, cd_processo){

  linhas <- .tjsp_cpopg_buscar_movimentacoes(cd_processo)

  html <- paste(readLines(arquivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  tabela <- paste0(
    '<div id="containerMovimentacoes"><table><tbody id="tabelaTodasMovimentacoes">',
    paste(linhas, collapse = "\n"),
    "</tbody></table></div>"
  )

  html <- sub('<div id="containerMovimentacoes"></div>', tabela, html, fixed = TRUE)

  writeLines(html, arquivo, useBytes = TRUE)

  invisible(arquivo)
}

#' Insere as movimentações num documento html já lido
#'
#' Versão de `.tjsp_cpopg_incluir_movimentacoes()` para quem tem o documento
#' em memória (`xml2`) em vez do arquivo, como `tjsp_baixar_cpopg()`.
#'
#' @param doc Documento html (`xml2_document`) de `show.do`.
#' @param cd_processo Código do processo.
#'
#' @return `doc`, invisivelmente (modificado por referência).
#' @noRd
.tjsp_cpopg_incluir_movimentacoes_doc <- function(doc, cd_processo){

  linhas <- .tjsp_cpopg_buscar_movimentacoes(cd_processo)

  tabela <- paste0(
    '<table><tbody id="tabelaTodasMovimentacoes">',
    paste(linhas, collapse = "\n"),
    "</tbody></table>"
  ) |>
    xml2::read_html() |>
    xml2::xml_find_first("//table")

  doc |>
    xml2::xml_find_first("//div[@id='containerMovimentacoes']") |>
    xml2::xml_add_child(tabela)

  invisible(doc)
}
