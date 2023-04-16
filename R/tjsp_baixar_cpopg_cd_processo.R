#' Baixa dados processuais com base no código do processo
#'
#' @param cd_processo Código do processo. Possivelmente, a coluna
#'     cd_doc do cjpg obtida com tjsp_ler_cjpg pode ser usada.
#' @param diretorio Diretório onde armazenar os arquivos.
#'
#' @return htmls
#' @export
#'
tjsp_baixar_cpopg_cd_processo <- function(cd_processo, diretorio = "."){

  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  purrr::walk(cd_processo, purrr::possibly(~{

    arquivo <- file.path(diretorio, paste0("cpopg_cd_processo_",.x, ".html"))

    httr::GET(paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=",.x),
              httr::write_disk(arquivo, overwrite = T))

  }, NULL))
}
