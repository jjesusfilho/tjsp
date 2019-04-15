#' Serializa htmls baixados.
#'
#' @param origem diretorio onde se encontram os htmls
#' @param destino diretorio para onde irão os htmls
#' @param prefix prefixar os arquivos com um nome.
#'
#' @return Salva htmls serializados
#' @export
serializar <- function(origem = NULL,
                       destino = ".",
                       prefix = NULL) {
  info <- fs::dir_ls(origem)

  if (length(info) >= 500000) {
    baldes <- split(info, 1:50)
  } else if (length(info) >= 100000) {
    baldes <- split(info, 1:20)
  } else if (length(info) >= 50000) {
    baldes <- split(info, 1:10)
  } else if (length(info) >= 20000) {
    baldes <- split(info, 1:5)
  } else if (length(info) >= 10000) {
    baldes <- split(info, 1:2)
  } else {
    baldes <- list(info)
  }


  arquivos <- paste0(destino, "/", prefix, seq_along(baldes), ".rds")



  purrr::walk2(baldes, arquivos, ~ {
    purrr::map(.x, ~ {
      .x %>%
        xml2::read_html() %>%
        xml2::xml_serialize(NULL)
    }) %>%
      saveRDS(.y)
  })
}
