#' Lê metadados da jurisprudência baixada do eproc
#'
#' Parseia os HTMLs baixados por \code{tjsp_baixar_eproc1g_jurisprudencia} ou
#' \code{tjsp_baixar_eproc2g_jurisprudencia} e retorna um tibble com os
#' metadados de cada documento. Para acórdãos, inclui a coluna
#' \code{id_jurisprudencia} que pode ser usada com
#' \code{tjsp_baixar_eproc_inteiro_teor}.
#'
#' @param arquivos Vetor de caminhos para os HTMLs. Se NULL, usa \code{diretorio}.
#' @param diretorio Diretório onde se encontram os HTMLs.
#'
#' @return Tibble com uma linha por documento encontrado.
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_eproc2g_jurisprudencia("homicídio", diretorio = "dados")
#' df <- tjsp_ler_eproc_jurisprudencia(diretorio = "dados")
#' }
tjsp_ler_eproc_jurisprudencia <- function(arquivos = NULL, diretorio = ".") {

  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, pattern = "\\.html$", full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(~ {

    doc   <- xml2::read_html(.x, encoding = "latin1")
    items <- xml2::xml_find_all(doc, "//div[contains(@class,'resultadoItem')]")

    if (length(items) == 0L) return(NULL)

    pagina <- stringr::str_extract(basename(.x), "(?<=pagina_)\\d+") |>
      as.integer()

    purrr::map_dfr(items, ~ eproc_parsear_item(.x, pagina))

  }, NULL), .id = NULL)
}


#' Parseia um item de resultado do eproc
#'
#' @param item Nó xml2 de um resultado individual.
#' @param pagina Número da página de origem.
#'
#' @return Tibble de uma linha.
#'
eproc_parsear_item <- function(item, pagina) {

  campo <- function(label) {
    xml2::xml_find_first(
      item,
      paste0(".//div[@class='resLabel'][contains(.,'", label, "')]/following-sibling::div[1]")
    ) |>
      xml2::xml_text(trim = TRUE)
  }

  id_jurisprudencia <- xml2::xml_find_first(
    item, ".//a[contains(@class,'inteiroTeor')]"
  ) |>
    xml2::xml_attr("data-link") |>
    stringr::str_extract("(?<=id_jurisprudencia=)[^&]+")

  tipo <- xml2::xml_find_first(
    item, ".//div[@class='resValueTipoJurisprudencia']"
  ) |>
    xml2::xml_text(trim = TRUE)

  processo <- xml2::xml_find_first(
    item, ".//a[contains(@class,'numero-processo')]"
  ) |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove("/TJSP$") |>
    stringr::str_trim()

  classe <- xml2::xml_find_first(
    item, ".//div[contains(@class,'resValue')]//span"
  ) |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove("^\\w+\\s+-\\s*") |>
    stringr::str_trim()

  orgao_julgador  <- campo("JULGADOR")
  data_julgamento <- campo("JULGAMENTO")
  data_publicacao <- campo("PUBLICA")
  relator         <- campo("RELAT")
  ementa          <- campo("EMENTA")

  decisao <- xml2::xml_find_first(
    item,
    ".//div[contains(@class,'resValue') and contains(@class,'completo') and @id[contains(.,'DECIS')]]"
  ) |>
    xml2::xml_text(trim = TRUE)

  if (is.na(decisao)) {
    decisao <- xml2::xml_find_first(
      item,
      ".//div[contains(@class,'resValue') and contains(@class,'limitado')]"
    ) |>
      xml2::xml_text(trim = TRUE)
  }

  tibble::tibble(
    pagina            = pagina,
    id_jurisprudencia = id_jurisprudencia,
    tipo              = tipo,
    processo          = processo,
    classe            = classe,
    orgao_julgador    = stringr::str_squish(orgao_julgador),
    data_julgamento   = lubridate::dmy(data_julgamento),
    data_publicacao   = lubridate::dmy(data_publicacao),
    relator           = stringr::str_squish(relator),
    ementa            = stringr::str_squish(ementa),
    decisao           = stringr::str_squish(decisao)
  )
}


#' Baixa o inteiro teor de documentos do eproc
#'
#' Usa os \code{id_jurisprudencia} obtidos por \code{tjsp_ler_eproc_jurisprudencia}
#' para baixar o inteiro teor dos documentos.
#'
#' @param id_jurisprudencia Vetor de IDs obtidos da coluna \code{id_jurisprudencia}.
#' @param grau "1g" ou "2g".
#' @param diretorio Diretório onde salvar os arquivos HTML.
#'
#' @return Salva arquivos HTML no diretório especificado.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- tjsp_ler_eproc_jurisprudencia(diretorio = "dados")
#' acordaos <- dplyr::filter(df, tipo == "Acórdão")
#' tjsp_baixar_eproc_inteiro_teor(acordaos$id_jurisprudencia, grau = "2g", diretorio = "inteiros_teores")
#' }
tjsp_baixar_eproc_inteiro_teor <- function(id_jurisprudencia,
                                           grau      = c("2g", "1g"),
                                           diretorio = ".") {

  grau     <- match.arg(grau)
  base_url <- paste0("https://eproc", grau, ".tjsp.jus.br/eproc/")
  url_base <- paste0(base_url,
    "externo_controlador.php?acao=jurisprudencia@jurisprudencia/download_inteiro_teor&id_jurisprudencia=")

  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  dir.create(diretorio, showWarnings = FALSE, recursive = TRUE)

  purrr::walk(id_jurisprudencia, purrr::possibly(~ {

    arquivo <- file.path(diretorio, paste0(.x, ".html"))
    Sys.sleep(1)

    httr::GET(
      paste0(url_base, .x),
      httr::accept("text/html; charset=latin1;"),
      httr::write_disk(arquivo, overwrite = TRUE)
    )

  }, NULL), .progress = TRUE)
}
