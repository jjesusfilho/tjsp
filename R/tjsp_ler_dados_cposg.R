#' Lê metadados dos processos de segunda instância
#'
#' @param arquivos Vetor de arquivos. Se NULL, informar diretório.
#' @param diretorio Diretório onde se encontram os htmls. Informar
#'     apenas se os arquivos não forem informados.
#' @param wide TRUE para colocar em formato largo.
#'
#' @return tabela com dados do processo
#' @export
#'
#' @examples
#' \dontrun{
#' dados <- tjsp_ler_dados_cposg()
#' }
tjsp_ler_dados_cposg <- function(arquivos = NULL, diretorio = ".", wide = TRUE) {

  if(is.null(arquivos)){
  arquivos <- list.files(
    path = diretorio, pattern = ".html",
    full.names = TRUE
  )
}

 pb <- progress::progress_bar$new(total = length(arquivos))

  dados <- purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")
    cd_processo <- stringr::str_extract(.x, "(?<=cd_processo_)\\w+")

    resposta <- xml2::read_html(.x)

    digital <- resposta |> xml2::xml_find_first("boolean(//*[@id='pbVisualizarAutos'] |//*[@id='linkConsultaSG'])")

    situacao <- resposta |> xml2::xml_find_first("//span[@id='situacaoProcesso']") |>
      xml2::xml_text()

    nomes <- resposta |>
      xml2::xml_find_all("//span[@class='unj-label']") |>
      xml2::xml_text(trim=TRUE)

    valores <- resposta |>
      xml2::xml_find_all("//span[@class='unj-label']/following-sibling::div") |>
      xml2::xml_text()

    tibble::tibble(processo = processo, digital, situacao, cd_processo, variavel = nomes,
                   valor = valores)

  }, otherwise = NULL))

  if (wide){
    dados <- dados |>
    dplyr::group_by_at(dplyr::vars(-valor)) |>
    dplyr::mutate(row_id = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::spread(key = variavel, value = valor) |>
    dplyr::select(-row_id) |>
    janitor::clean_names()
  }
    return(dados)
}
