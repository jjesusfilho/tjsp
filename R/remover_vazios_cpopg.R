#' Remove cpopg htmls vazios, ou seja, htmls menores que 90Kb.
#'
#' @param diretorio Default para diretorio atual.
#'
#' @return Mantidos apenas arquivos com conteúdo
#' @export
#'
remover_vazios_cpopg <- function(diretorio = ".") {
  fs::file_info(fs::dir_ls(diretorio)) %>%
    .[c(1, 3)] %>%
    dplyr::filter(size <= fs::as_fs_bytes("86K")) %>%
    dplyr::pull("path") %>%
    fs::file_delete()
}
