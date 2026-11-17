is_defined <- function(sym) {
  sym <- deparse(substitute(sym))
  env <- parent.frame()
  exists(sym, env, inherits = FALSE)
}

#' Extrai a página inicial e a final do título de um nó da pasta digital
#'
#' Formatos: "Página 5.053", "Páginas 431 - 433" e "Sem efeito pags 357 - 358". O ponto
#' é separador de milhar. Nós que não são páginas (ex.: gravações de audiência) dão NA.
#'
#' @param titulo Vetor de títulos.
#' @return Lista com `inicial` e `final`, em texto.
#' @noRd
extrair_paginas <- function(titulo) {
  titulo <- stringr::str_remove_all(titulo, "(?<=\\d)\\.(?=\\d{3})")
  m <- stringr::str_match(titulo, "(?i)^\\s*(?:p[áa]ginas?|sem efeito pags?)\\s+(\\d+)(?:\\s*-\\s*(\\d+))?")
  list(inicial = m[, 2], final = dplyr::coalesce(m[, 3], m[, 2]))
}
