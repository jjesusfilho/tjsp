#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'
#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'
tjsp_ler_movimentacao <- function (arquivos = NULL, diretorio = ".") {
  if (is.null(arquivos)) {
    arquivos <- list.files(
      path = diretorio, 
      pattern = ".html",
      full.names = TRUE
    )
  }
  pb <- progress::progress_bar$new(total = length(arquivos))
  
  purrr::map_dfr(arquivos, purrr::possibly(~{
    pb$tick()
    
    resposta <- xml2::read_html(.x)
    
    processo <- resposta |>
      xml2::xml_find_first("//span[contains(@class,'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_squish() |>
      stringr::str_remove_all("[^\\d+\\s]") |>
      stringr::str_trim()
    
    cd_processo <- resposta |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |> 
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")
    
    movs <- resposta |>
      xml2::xml_find_first("//table/tbody[@id='tabelaTodasMovimentacoes']") |> 
      xml2::xml_find_all("./tr")
    
    dt_mov <- movs |> 
      xml2::xml_find_first("./td[@class='dataMovimentacao']") |> 
      xml2::xml_text(trim = TRUE) |>
      lubridate::dmy()
    
    tem_anexo <- movs |>
      xml2::xml_find_first("./td[@class='descricaoMovimentacao']/a") |> 
      xml2::xml_attr("href") |> 
      is.na() == FALSE
    
    mov <- movs |> 
      xml2::xml_find_first("./td[@class='descricaoMovimentacao']") |>
      xml2::xml_text(trim=TRUE)
    
    tibble::tibble(
      processo, 
      cd_processo,
      dt_mov, 
      mov,
      tem_anexo
    ) |> 
      tidyr::separate(
        col = mov,
        into = c("movimento", "descricao"), 
        sep = "\n\\s+"
      )
  }, otherwise = NULL))
}
