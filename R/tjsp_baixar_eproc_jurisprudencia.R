#' Baixa jurisprudência do sistema eproc de primeiro grau do TJSP
#'
#' @param pesquisa Texto livre para pesquisa
#' @param campo "I" para inteiro teor (padrão) ou "E" para somente ementa
#' @param agrupar Lógico. Agrupar resultados? Padrão TRUE.
#' @param precedente Lógico. Filtrar somente precedentes relevantes? Padrão FALSE.
#' @param colegio_recursal Lógico. Incluir somente Colégio Recursal? Padrão FALSE.
#' @param tipo_documento Vetor de caracteres com tipos de documento.
#' @param processo Número do processo.
#' @param classe Vetor de caracteres com nomes das classes processuais.
#' @param relator Vetor de caracteres com nomes dos relatores.
#' @param orgao Vetor de caracteres com nomes dos órgãos julgadores.
#' @param inicio Data inicial de decisão no formato "dd/mm/aaaa".
#' @param fim Data final de decisão no formato "dd/mm/aaaa".
#' @param inicio_pb Data inicial de publicação no formato "dd/mm/aaaa".
#' @param fim_pb Data final de publicação no formato "dd/mm/aaaa".
#' @param n Número máximo de páginas. Se NULL, baixa todas.
#' @param diretorio Diretório para salvar os arquivos HTML. Padrão é o diretório atual.
#'
#' @return Salva arquivos HTML com os resultados no diretório especificado.
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_eproc1g_jurisprudencia(pesquisa = "homicídio", n = 1, diretorio = tempdir())
#' }
tjsp_baixar_eproc1g_jurisprudencia <- function(
    pesquisa         = "",
    campo            = c("I", "E"),
    agrupar          = TRUE,
    precedente       = FALSE,
    colegio_recursal = FALSE,
    tipo_documento   = character(0),
    processo         = "",
    classe           = character(0),
    relator          = character(0),
    orgao            = character(0),
    inicio           = "",
    fim              = "",
    inicio_pb        = "",
    fim_pb           = "",
    n                = NULL,
    diretorio        = ".") {

  campo <- match.arg(campo)

  data_errada <- verificar_datas(inicio, fim, inicio_pb, fim_pb)
  if (data_errada) {
    stop("Verifique se:
         Colocou as datas no formato esperado;
         informou somente publicação ou julgamento;
         colocou uma data inferior ou igual à data de hoje.")
  }

  base_url <- "https://eproc1g.tjsp.jus.br/eproc/"

  if (inicio != "" && fim != "") {
    datas <- agrupar_datas(inicio, fim, "anual")
    purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~ {
      tjsp_baixar_eproc_jurisprudencia1(
        base_url = base_url,
        pesquisa = pesquisa, campo = campo, agrupar = agrupar,
        precedente = precedente, colegio_recursal = colegio_recursal,
        tipo_documento = tipo_documento, processo = processo,
        classe = classe, relator = relator, orgao = orgao,
        inicio = .x, fim = .y,
        inicio_pb = inicio_pb, fim_pb = fim_pb,
        n = n, diretorio = diretorio
      )
    }, NULL))
  } else if (inicio_pb != "" && fim_pb != "") {
    datas <- agrupar_datas(inicio_pb, fim_pb, "anual")
    purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~ {
      tjsp_baixar_eproc_jurisprudencia1(
        base_url = base_url,
        pesquisa = pesquisa, campo = campo, agrupar = agrupar,
        precedente = precedente, colegio_recursal = colegio_recursal,
        tipo_documento = tipo_documento, processo = processo,
        classe = classe, relator = relator, orgao = orgao,
        inicio = inicio, fim = fim,
        inicio_pb = .x, fim_pb = .y,
        n = n, diretorio = diretorio
      )
    }, NULL))
  } else {
    tjsp_baixar_eproc_jurisprudencia1(
      base_url = base_url,
      pesquisa = pesquisa, campo = campo, agrupar = agrupar,
      precedente = precedente, colegio_recursal = colegio_recursal,
      tipo_documento = tipo_documento, processo = processo,
      classe = classe, relator = relator, orgao = orgao,
      inicio = inicio, fim = fim,
      inicio_pb = inicio_pb, fim_pb = fim_pb,
      n = n, diretorio = diretorio
    )
  }
}


#' Baixa jurisprudência do sistema eproc de segundo grau do TJSP
#'
#' @inheritParams tjsp_baixar_eproc1g_jurisprudencia
#'
#' @return Salva arquivos HTML com os resultados no diretório especificado.
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_eproc2g_jurisprudencia(pesquisa = "dano moral", n = 1, diretorio = tempdir())
#' }
tjsp_baixar_eproc2g_jurisprudencia <- function(
    pesquisa       = "",
    campo          = c("I", "E"),
    agrupar        = TRUE,
    precedente     = FALSE,
    tipo_documento = character(0),
    processo       = "",
    classe         = character(0),
    relator        = character(0),
    orgao          = character(0),
    inicio         = "",
    fim            = "",
    inicio_pb      = "",
    fim_pb         = "",
    n              = NULL,
    diretorio      = ".") {

  campo <- match.arg(campo)

  data_errada <- verificar_datas(inicio, fim, inicio_pb, fim_pb)
  if (data_errada) {
    stop("Verifique se:
         Colocou as datas no formato esperado;
         informou somente publicação ou julgamento;
         colocou uma data inferior ou igual à data de hoje.")
  }

  base_url <- "https://eproc2g.tjsp.jus.br/eproc/"

  if (inicio != "" && fim != "") {
    datas <- agrupar_datas(inicio, fim, "anual")
    purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~ {
      tjsp_baixar_eproc_jurisprudencia1(
        base_url = base_url,
        pesquisa = pesquisa, campo = campo, agrupar = agrupar,
        precedente = precedente, colegio_recursal = FALSE,
        tipo_documento = tipo_documento, processo = processo,
        classe = classe, relator = relator, orgao = orgao,
        inicio = .x, fim = .y,
        inicio_pb = inicio_pb, fim_pb = fim_pb,
        n = n, diretorio = diretorio
      )
    }, NULL))
  } else if (inicio_pb != "" && fim_pb != "") {
    datas <- agrupar_datas(inicio_pb, fim_pb, "anual")
    purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~ {
      tjsp_baixar_eproc_jurisprudencia1(
        base_url = base_url,
        pesquisa = pesquisa, campo = campo, agrupar = agrupar,
        precedente = precedente, colegio_recursal = FALSE,
        tipo_documento = tipo_documento, processo = processo,
        classe = classe, relator = relator, orgao = orgao,
        inicio = inicio, fim = fim,
        inicio_pb = .x, fim_pb = .y,
        n = n, diretorio = diretorio
      )
    }, NULL))
  } else {
    tjsp_baixar_eproc_jurisprudencia1(
      base_url = base_url,
      pesquisa = pesquisa, campo = campo, agrupar = agrupar,
      precedente = precedente, colegio_recursal = FALSE,
      tipo_documento = tipo_documento, processo = processo,
      classe = classe, relator = relator, orgao = orgao,
      inicio = inicio, fim = fim,
      inicio_pb = inicio_pb, fim_pb = fim_pb,
      n = n, diretorio = diretorio
    )
  }
}


#' Worker interno para download de jurisprudência eproc
#'
#' @param base_url URL base do sistema eproc (1g ou 2g)
#' @inheritParams tjsp_baixar_eproc1g_jurisprudencia
#'
#' @return Salva arquivos HTML no diretório especificado.
#'
tjsp_baixar_eproc_jurisprudencia1 <- function(
    base_url,
    pesquisa,
    campo,
    agrupar,
    precedente,
    colegio_recursal,
    tipo_documento,
    processo,
    classe,
    relator,
    orgao,
    inicio,
    fim,
    inicio_pb,
    fim_pb,
    n,
    diretorio) {

  httr::set_config(httr::config(ssl_verifypeer = FALSE, accept_encoding = "latin1"))

  url_pesquisa <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/pesquisar")
  url_post     <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/listar_resultados")
  url_ajax     <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/ajax_paginar_resultado")

  r0 <- httr::GET(url_pesquisa)

  body <- list(
    txtPesquisa = pesquisa,
    rdoCampo    = campo,
    txtProcesso = processo
  )

  if (agrupar)    body[["chkAgruparResultados"]]   <- "on"
  if (precedente) body[["chkPrecedenteRelevante"]] <- "on"
  if (colegio_recursal) body[["selOrigem[]"]] <- "3"

  if (length(tipo_documento) > 0 && any(tipo_documento != ""))
    body[["selTipoDocumento[]"]] <- I(tipo_documento)
  if (length(classe) > 0 && any(classe != ""))
    body[["selClasse[]"]] <- I(classe)
  if (length(relator) > 0 && any(relator != ""))
    body[["selRelator[]"]] <- I(relator)
  if (length(orgao) > 0 && any(orgao != ""))
    body[["selOrgao[]"]] <- I(orgao)

  body[["dtDecisaoInicio"]]    <- inicio
  body[["dtDecisaoFim"]]       <- fim
  body[["hdnDecisaoInicio"]]   <- inicio
  body[["hdnDecisaoFim"]]      <- fim
  body[["dtPublicacaoInicio"]] <- inicio_pb
  body[["dtPublicacaoFim"]]    <- fim_pb
  body[["hdnPublicacaoInicio"]] <- inicio_pb
  body[["hdnPublicacaoFim"]]   <- fim_pb

  r1 <- httr::POST(
    url_post,
    encode = "form",
    body   = body,
    httr::set_cookies(unlist(r0$cookies)),
    httr::accept("text/html; charset=latin1;")
  )

  cookies <- c(unlist(r0$cookies), unlist(r1$cookies))

  if (is.null(n)) {
    total_node <- r1 |>
      httr::content() |>
      xml2::xml_find_first("//*[contains(@id,'totalResultados') or contains(@class,'totalResultados')]")

    total <- if (!is.na(total_node)) {
      xml2::xml_text(total_node, trim = TRUE) |>
        stringr::str_extract("\\d+") |>
        as.numeric()
    } else {
      NA_real_
    }

    if (is.na(total)) {
      warning("Não foi possível determinar o número de páginas. Baixando apenas a primeira.")
      paginas <- 1L
    } else {
      paginas <- seq_len(ceiling(total / 20))
    }
  } else {
    paginas <- seq_len(n)
  }

  purrr::walk(paginas, purrr::possibly(~ {
    arquivo <- formatar_arquivo(inicio, fim, inicio_pb, fim_pb, pagina = .x, diretorio)
    Sys.sleep(1)
    httr::GET(
      url_ajax,
      query = list(pagina = .x),
      httr::set_cookies(cookies),
      httr::accept("text/html; charset=latin1;"),
      httr::write_disk(arquivo, overwrite = TRUE)
    )
  }, NULL), .progress = TRUE)
}
