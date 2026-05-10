#' Baixa jurisprudência do sistema eproc de primeiro grau do TJSP
#'
#' @param pesquisa Texto livre para pesquisa.
#' @param campo "I" para inteiro teor (padrão) ou "E" para somente ementa.
#' @param origem Vetor com os códigos de origem: "3" = Colégio Recursal,
#'   "4" = Primeiro Grau (padrão), "5" = Segundo Grau.
#' @param agrupar Lógico. Agrupar resultados? Padrão TRUE.
#' @param precedente Lógico. Filtrar somente precedentes relevantes? Padrão FALSE.
#' @param tipo_documento Código do tipo de documento. Os valores dependem da origem:
#'   origem "3" (Colégio Recursal) e "5" (Segundo Grau): "1" = Acórdão, "2" = Decisão monocrática;
#'   origem "4" (Primeiro Grau): "5" = Sentença, "2" = Decisão monocrática.
#'   Aceita vetor para múltiplos tipos.
#' @param processo Número do processo.
#' @param classe Vetor com nomes das classes processuais (ex. "Apelação", "Agravo de Instrumento").
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
    pesquisa       = "",
    campo          = c("I", "E"),
    origem         = "4",
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

  base_url <- "https://eproc1g.tjsp.jus.br/eproc/"

  if (inicio != "" && fim != "") {
    datas <- agrupar_datas(inicio, fim, "anual")
    purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~ {
      tjsp_baixar_eproc_jurisprudencia1(
        base_url = base_url,
        pesquisa = pesquisa, campo = campo, origem = origem,
        agrupar = agrupar, precedente = precedente,
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
        pesquisa = pesquisa, campo = campo, origem = origem,
        agrupar = agrupar, precedente = precedente,
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
      pesquisa = pesquisa, campo = campo, origem = origem,
      agrupar = agrupar, precedente = precedente,
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
    origem         = "5",
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
        pesquisa = pesquisa, campo = campo, origem = origem,
        agrupar = agrupar, precedente = precedente,
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
        pesquisa = pesquisa, campo = campo, origem = origem,
        agrupar = agrupar, precedente = precedente,
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
      pesquisa = pesquisa, campo = campo, origem = origem,
      agrupar = agrupar, precedente = precedente,
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
#' @param base_url URL base do sistema eproc (1g ou 2g).
#' @inheritParams tjsp_baixar_eproc1g_jurisprudencia
#'
#' @return Salva arquivos HTML no diretório especificado.
#'
eproc_montar_form <- function(body) {
  pares <- lapply(seq_along(body), function(i) {
    chave  <- names(body)[i]
    valores <- as.character(body[[i]])
    paste0(chave, "=", utils::URLencode(valores, reserved = TRUE))
  })
  paste(unlist(pares), collapse = "&")
}

tjsp_baixar_eproc_jurisprudencia1 <- function(
    base_url,
    pesquisa,
    campo,
    origem,
    agrupar,
    precedente,
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
  dir.create(diretorio, showWarnings = FALSE, recursive = TRUE)

  url_pesquisa <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/pesquisar")
  url_post     <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/listar_resultados")
  url_ajax     <- paste0(base_url, "externo_controlador.php?acao=jurisprudencia@jurisprudencia/ajax_paginar_resultado")

  r0 <- httr::GET(url_pesquisa)

  body <- list(
    txtPesquisa                  = pesquisa,
    hdnExibirPesquisaAvancada    = "",
    hdnUrlCarregarListasCombobox = "externo_controlador.php?acao=jurisprudencia@jurisprudencia/ajax_carregar_listas_pesquisa",
    rdoCampo                     = campo,
    txtProcesso                  = processo,
    hdnUrlPaginar                = "externo_controlador.php?acao=jurisprudencia@jurisprudencia/ajax_paginar_resultado"
  )

  body[["selOrigem[]"]] <- as.character(origem)

  if (agrupar)    body[["chkAgruparResultados"]]   <- "on"
  if (precedente) body[["chkPrecedenteRelevante"]] <- "on"

  if (length(tipo_documento) > 0 && any(tipo_documento != ""))
    body[["selTipoDocumento[]"]] <- tipo_documento
  if (length(classe) > 0 && any(classe != ""))
    body[["selClasse[]"]] <- classe
  if (length(relator) > 0 && any(relator != ""))
    body[["selRelator[]"]] <- relator
  if (length(orgao) > 0 && any(orgao != ""))
    body[["selOrgao[]"]] <- orgao

  body[["dtDecisaoInicio"]]     <- inicio
  body[["dtDecisaoFim"]]        <- fim
  body[["hdnDecisaoInicio"]]    <- inicio
  body[["hdnDecisaoFim"]]       <- fim
  body[["dtPublicacaoInicio"]]  <- inicio_pb
  body[["dtPublicacaoFim"]]     <- fim_pb
  body[["hdnPublicacaoInicio"]] <- inicio_pb
  body[["hdnPublicacaoFim"]]    <- fim_pb

  r1 <- httr::POST(
    url_post,
    body   = eproc_montar_form(body),
    httr::content_type("application/x-www-form-urlencoded"),
    httr::set_cookies(unlist(r0$cookies)),
    httr::accept("text/html; charset=latin1;")
  )

  cookies <- c(unlist(r0$cookies), unlist(r1$cookies))
  content_r1 <- httr::content(r1)

  total_paginas <- content_r1 |>
    xml2::xml_find_first("//input[@id='hdnTotalPaginas']") |>
    xml2::xml_attr("value") |>
    as.integer()

  total_resultado <- content_r1 |>
    xml2::xml_find_first("//input[@id='hdnTotalResultado']") |>
    xml2::xml_attr("value") |>
    as.integer()

  if (is.null(n)) {
    if (is.na(total_paginas)) {
      warning("Não foi possível determinar o número de páginas. Baixando apenas a primeira.")
      paginas <- 1L
    } else {
      paginas <- seq_len(total_paginas)
    }
  } else {
    paginas <- seq_len(n)
  }

  body_pag <- body
  body_pag[["hdnTotalPaginas"]]   <- as.character(total_paginas)
  body_pag[["hdnTotalResultado"]] <- as.character(total_resultado)
  body_pag[["selTamanhoPagina"]]  <- "10"

  purrr::walk(paginas, purrr::possibly(~ {
    arquivo <- formatar_arquivo(inicio, fim, inicio_pb, fim_pb, pagina = .x, diretorio)
    Sys.sleep(1)
    body_p <- body_pag
    body_p[["hdnPaginaAtual"]] <- as.character(.x)
    httr::POST(
      url_ajax,
      body   = eproc_montar_form(body_p),
      httr::content_type("application/x-www-form-urlencoded"),
      httr::set_cookies(cookies),
      httr::accept("text/html; charset=latin1;"),
      httr::write_disk(arquivo, overwrite = TRUE)
    )
  }, NULL), .progress = TRUE)
}
