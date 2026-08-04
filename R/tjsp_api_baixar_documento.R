#' Baixa um documento da pasta digital de um processo na API mobile do TJSP
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param cd_documento `cd_documento` do documento (ver [tjsp_api_pasta_digital()]).
#' @param destino Caminho do PDF de destino.
#' @param instancia "pg" (1º grau, padrão), "sg" (2º grau) ou "cr" (criminal).
#' @param inicio Primeiro arquivo do documento a incluir (1-based, inclusive).
#'    Em volumes escaneados equivale a um intervalo de páginas. `NULL` (padrão)
#'    baixa desde o primeiro.
#' @param fim Último arquivo do documento a incluir (1-based, inclusive).
#'    `NULL` (padrão) baixa até o último.
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e habilitação nos
#'    autos. A API mobile não monta PDF no servidor: cada arquivo (`showpdf`)
#'    é baixado individualmente e depois juntado num único PDF com
#'    `qpdf::pdf_combine()`.
#'
#' @return Caminho de `destino`, retornado invisivelmente.
#' @export
#'
tjsp_api_baixar_documento <- function(numero_ou_cd, cd_documento, destino, instancia = "pg",
                                       inicio = NULL, fim = NULL) {

  grau <- .tjsp_api_grau_por_instancia[[instancia]]
  cd <- .tjsp_api_resolver_cd(numero_ou_cd, if (is.null(grau)) "cpopg" else grau)

  arvore <- .tjsp_api_pasta_tree(cd, instancia)
  node <- arvore[as.character(arvore$cdDocumento) == as.character(cd_documento), ]

  if (nrow(node) == 0) {
    stop("Documento ", cd_documento, " não encontrado na pasta digital de ", cd)
  }

  paginas <- node$childrenDto[[1]]$urlPDf

  if (!is.null(inicio) || !is.null(fim)) {
    paginas <- paginas[(if (is.null(inicio)) 1 else inicio):(if (is.null(fim)) length(paginas) else fim)]
  }

  if (length(paginas) == 0) {
    stop("Documento ", cd_documento, " sem páginas para baixar.")
  }

  arquivos_tmp <- purrr::map_chr(paginas, function(url) {
    tmp <- tempfile(fileext = ".pdf")
    resp <- httr::GET(url, httr::write_disk(tmp, overwrite = TRUE))
    if (httr::status_code(resp) != 200) {
      stop("Falha ao baixar página: ", url)
    }
    tmp
  })
  on.exit(unlink(arquivos_tmp), add = TRUE)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  qpdf::pdf_combine(arquivos_tmp, destino)

  invisible(destino)
}

#' Acha e baixa o 1º documento cujo título casa com um termo
#'
#' @inheritParams tjsp_api_baixar_documento
#' @param termo Termo a buscar no título dos documentos (ver [tjsp_api_encontrar_documentos()]).
#'
#' @return Caminho de `destino`, retornado invisivelmente.
#' @export
#'
tjsp_api_baixar_documento_por_titulo <- function(numero_ou_cd, termo, destino, instancia = "pg") {

  docs <- tjsp_api_encontrar_documentos(numero_ou_cd, termo, instancia)

  if (nrow(docs) == 0) {
    stop("Nenhum documento com título ~ '", termo, "'.")
  }

  tjsp_api_baixar_documento(numero_ou_cd, docs$cd_documento[1], destino, instancia)
}
