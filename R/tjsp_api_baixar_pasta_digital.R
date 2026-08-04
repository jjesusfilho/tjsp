#' Baixa a pasta digital inteira de um processo na API mobile do TJSP
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param diretorio Diretório onde salvar um PDF por documento.
#' @param instancia "pg" (1º grau, padrão), "sg" (2º grau) ou "cr" (criminal).
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e habilitação nos
#'    autos. Pode ser pesado -- um processo pode ter milhares de páginas
#'    (uma requisição por página). Documentos com falha no download são
#'    ignorados (aviso) em vez de interromper o restante da pasta.
#'
#' @return Vetor com os caminhos dos PDFs gerados, invisivelmente.
#' @export
#'
tjsp_api_baixar_pasta_digital <- function(numero_ou_cd, diretorio, instancia = "pg") {

  grau <- .tjsp_api_grau_por_instancia[[instancia]]
  cd <- .tjsp_api_resolver_cd(numero_ou_cd, if (is.null(grau)) "cpopg" else grau)

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

  docs <- tjsp_api_pasta_digital(cd, instancia)

  if (nrow(docs) == 0) {
    return(invisible(character()))
  }

  purrr::iwalk(docs$cd_documento, purrr::possibly(function(cd_documento, i) {

    titulo <- docs$titulo[i]
    if (is.na(titulo) || titulo == "") titulo <- cd_documento
    titulo <- stringr::str_trunc(stringr::str_replace_all(titulo, "[^[:alnum:].-]+", "_"), 60, ellipsis = "")

    destino <- file.path(diretorio, sprintf("%03d_%s.pdf", i, titulo))

    tjsp_api_baixar_documento(cd, cd_documento, destino, instancia)

  }, otherwise = NULL), .progress = TRUE)

  invisible(list.files(diretorio, pattern = "\\.pdf$", full.names = TRUE))
}
