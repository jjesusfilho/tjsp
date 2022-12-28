#' Combina docs baixados com tjsp_baixar_docs
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Diretório de origem, se não
#'     informar arquivos
#' @param dir_destino Diretório destino
#'
#' @return único pdf
#' @export
#'
tjsp_combinar_docs <- function(arquivos = NULL, dir_origem = ".",  dir_destino = NULL){

  if (is.null(dir_destino) || !dir.exists(dir_destino)){

    stop("Voc\u00ea deve informar um diret\u00f3rio existente")

  }

  if (is.null(arquivos)){

    arquivos <- list.files(dir_origem, full.names = TRUE, pattern = "pdf$")

  }




  lista <- tibble::tibble(arquivos) |>
    dplyr::mutate(processo = stringr::str_extract(arquivos,"\\d{20}"),
                  doc_id = stringr::str_extract(arquivos,'(?<=doc_id_)\\d+') |> as.integer(),
		  pagina_inicial = stringr::str_extract(arquivos, '(?<=inicial_)\\d+') |> as.integer()) |> # nolint
    dplyr::arrange(processo,doc_id,pagina_inicial) |>
    dplyr::group_split(processo)

  purrr::walk(lista, purrr::possibly(~{

    processo <- unique(.x$processo)

    qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,paste0(processo,".pdf")))

  },NULL))


}

