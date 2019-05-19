#' Aplica uma função para baixar htmls e serializa os htmls em grupos de
#'     no máximo aproximadamente 300 Mb.
#'
#' @param lista lista de vetores de processos ou simplesmente um vetor de processos
#' @param dir  diretorio onde serão criados os subdiretorios
#' @param subdir subdiretórios onde serão armazenados  grupo da lista.
#' @param funcao Função de download do html, ex. esaj::download_cpopg
#'
#' @return Arquivo em rds baixado
#' @export
#'
baixar_serializar <- function(lista = NULL, dir = ".", subdir = NULL, funcao = NULL) {
  diretorios <- fs::dir_create(paste0(dir, "/", subdir))

  purrr::walk2(lista, diretorios, ~ {
    funcao(.x, .y)

    fs::file_info(fs::dir_ls(.y, glob = "*.html")) %>%
      .[c(1, 3)] %>%
      dplyr::filter(size <= fs::as_fs_bytes("86K")) %>%
      dplyr::pull("path") %>%
      fs::file_delete()

    serializar(
      origem = .y,
      destino = .y,
      prefix = "arquivo"
    )
    fs::dir_ls(.y, glob = "*.html") %>%
      fs::file_delete()
  })
}
