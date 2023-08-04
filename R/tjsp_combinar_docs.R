#' Combina docs baixados com tjsp_baixar_docs
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Diretório de origem, se não
#'     informar arquivos
#' @param dir_destino Diretório destino
#' @param nivel Juntar processo inteiro ou por documento?
#'
#' @return único pdf
#' @export
#'
tjsp_combinar_docs <- function(arquivos = NULL,
                               dir_origem = ".",
                               dir_destino = NULL,
                               nivel = c("processo", "doc")){

  if (is.null(dir_destino) || !dir.exists(dir_destino)){

    stop("Voc\u00ea deve informar um diret\u00f3rio existente")

  }

  if (is.null(arquivos)){

    arquivos <- list.files(dir_origem, full.names = TRUE, pattern = "pdf$")

  }

  nivel = nivel[1]


  lista <- tibble::tibble(arquivos) |>
    dplyr::mutate(processo = stringr::str_extract(arquivos,"\\d{20}"),
                  id_doc = stringr::str_extract(arquivos,'(?<=id_doc_)\\d+') |> as.integer(),
		  pagina_inicial = stringr::str_extract(arquivos, '(?<=inicial_)\\d+') |> as.integer()) |> # nolint
    dplyr::arrange(processo,id_doc,pagina_inicial)

  if (nivel == "processo"){

    lista <- dplyr::group_split(lista, processo)


    purrr::walk(lista, purrr::possibly(~{

      processo <- unique(.x$processo)

      qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,paste0(processo,".pdf")))

    },NULL))


  } else {

    lista <- dplyr::group_split(lista, processo, id_doc)

    purrr::walk(lista, purrr::possibly(~{
      processo <- unique(.x$processo)
      id_doc <- unique(.x$id_doc)
      suppressWarnings(
        qpdf::pdf_combine(.x$arquivos, file.path(dir_destino,
                                                 paste0(processo,"_id_doc_",id_doc, ".pdf")))
      )
    }, NULL))
  }




}

