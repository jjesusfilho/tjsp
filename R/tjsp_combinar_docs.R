#' Combina docs baixados com tjsp_baixar_docs
#'
#' @param arquivos Vetor de arquivos
#' @param dir_origem Diretório de origem, se não
#'     informar arquivos
#' @param dir_destino Diretório destino
#' @param nivel Juntar processo inteiro ou por documento?
#' @param forcar Padrão para TRUE. Documentos inválidos são excluídos
#'
#' @return Pdf(s) combinado(s)
#' @export
#'
tjsp_combinar_docs <- function(arquivos = NULL,
                               dir_origem = ".",
                               dir_destino = NULL,
                               nivel = c("processo", "doc"),
                               forcar = TRUE){

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

  if (forcar){

    lista <- lista |>
      dplyr::filter(is_pdf(arquivos))
  }

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


#' Verifica se um arquivo é pdf (vetorizado)
#'
#' @param x Vetor de arquivos
#'
#' @return Valor lógico
#'
is_pdf <- function(x) {
  purrr::map_lgl(x, is_pdf1)
}

#' Verifica se um arquivo é pdf (não vetorizado)
#'
#' @param arquivo Suposto pdf a ser verificado
#'
#' @return Valor lógico
#'
is_pdf1 <- function(arquivo) {
  if(file.exists(arquivo)) {
    res <- suppressMessages(try(pdftools::pdf_info(arquivo),
                                silent = TRUE))
    if(!methods::is(res, "try-error")) return(TRUE)
    warning(paste(arquivo, "Parece n\u00E3o se tratar de um pdf."))
    return(FALSE)
  }
  warning(paste(arquivo, "n\u00E3o existe."))
  return(FALSE)
}

