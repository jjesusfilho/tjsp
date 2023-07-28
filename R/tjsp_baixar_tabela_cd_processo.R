#' Baixa tabela de documentos com base no cd_processo
#'
#' @param cd_processo Número do processo
#' @param diretorio Diretório onde armazenar as tabelas
#'
#' @return html
#' @export
#'
tjsp_baixar_tabela_cd_processo <- function (cd_processo = NULL, diretorio = ".")
{
  httr::set_config(httr::config(ssl_verifypeer = FALSE))


  pb <- progress::progress_bar$new(total = length(cd_processo))

  purrr::walk(cd_processo, purrr::possibly(~{

    arquivo <- file.path(diretorio,paste0("tabela_cd_processo_",.x,".html"))


    r1 <- .x |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
      httr::GET()


    url1 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",.x)

    url1 |>
      httr::GET() |>
      httr::content("text") |>
      httr::GET(httr::write_disk(arquivo,overwrite = TRUE))

    }, NULL))

}

