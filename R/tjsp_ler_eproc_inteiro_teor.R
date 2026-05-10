#' Lê o inteiro teor dos documentos baixados do eproc
#'
#' Parseia os HTMLs baixados por \code{tjsp_baixar_eproc_inteiro_teor} e
#' retorna um tibble com o texto de cada seção do documento.
#' Um mesmo arquivo pode conter múltiplos documentos (artigos); cada um
#' gera uma linha.
#'
#' @param arquivos Vetor de caminhos para os HTMLs. Se NULL, usa \code{diretorio}.
#' @param diretorio Diretório onde se encontram os HTMLs.
#'
#' @return Tibble com uma linha por documento (artigo) encontrado, contendo
#'   as colunas \code{id_jurisprudencia}, \code{id_documento},
#'   \code{cod_documento}, \code{processo}, \code{relator},
#'   \code{relatorio} e \code{voto}.
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_eproc_inteiro_teor(ids, grau = "2g", diretorio = "inteiros_teores")
#' df <- tjsp_ler_eproc_inteiro_teor(diretorio = "inteiros_teores")
#' }
tjsp_ler_eproc_inteiro_teor <- function(arquivos = NULL, diretorio = ".") {

  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, pattern = "\\.html$", full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(~ {

    id_jurisprudencia <- tools::file_path_sans_ext(basename(.x))
    doc  <- xml2::read_html(.x, encoding = "latin1")
    arts <- xml2::xml_find_all(doc, "//article")

    if (length(arts) == 0L) return(NULL)

    purrr::map_dfr(arts, ~ eproc_parsear_artigo(.x, id_jurisprudencia))

  }, NULL))
}


#' Parseia um artigo do inteiro teor do eproc
#'
#' @param art Nó xml2 de um artigo.
#' @param id_jurisprudencia ID do arquivo de origem.
#'
#' @return Tibble de uma linha.
#'
eproc_parsear_artigo <- function(art, id_jurisprudencia) {

  secao <- function(nome) {
    xml2::xml_find_first(art, paste0('.//section[@data-nome="', nome, '"]')) |>
      xml2::xml_text(trim = TRUE) |>
      stringr::str_squish()
  }

  id_documento  <- xml2::xml_attr(art, "data-id_documento")
  cod_documento <- xml2::xml_attr(art, "data-cod_documento")

  processo <- secao("identificacao_processo") |>
    stringr::str_extract("[\\d]{7}-[\\d]{2}\\.[\\d]{4}\\.[\\d]\\.[\\d]{2}\\.[\\d]{4}")

  relator <- secao("relator") |>
    stringr::str_remove("(?i)^RELATOR[A]?:\\s*") |>
    stringr::str_remove("(?i)^Desembargador[a]?\\s+") |>
    stringr::str_squish()

  tibble::tibble(
    id_jurisprudencia = id_jurisprudencia,
    id_documento      = id_documento,
    cod_documento     = cod_documento,
    processo          = processo,
    relator           = relator,
    relatorio         = secao("relatorio"),
    voto              = secao("voto")
  )
}
