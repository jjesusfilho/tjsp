#' Listar documentos com respectivos códigos
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativamente informar diretório
#'
#' @return tibble
#' @export
#'
mni_listar_documentos <- function(arquivos = NULL, diretorio  = "."){


  if(is.null(arquivos)){


    arquivos <- list.files(diretorio, full.names = TRUE, pattern = "incluir_documentos$")

  }


  purrr::map_dfr(arquivos, purrr::possibly(~{


    processo <- stringr::str_extract(.x, "\\d{20}")

      .x |>
      xml2::read_xml() |> 
      xml2::xml_find_all("//ns2:dadosBasicos/following-sibling::ns2:documento") |>
      purrr::map_dfr(purrr::possibly(~{

        id_documento <-   .x |>
          xml2::xml_attr("idDocumento")

        tipo_documento <-   .x |>
          xml2::xml_attr("tipoDocumento")

        data_hora <-   .x |>
          xml2::xml_attr("dataHora") |>
          lubridate::ymd_hms(tz = "America/Sao_Paulo")

        mime_type <-   .x |>
          xml2::xml_attr("mimetype")

        nivel_sigilo <-   .x |>
          xml2::xml_attr("nivelSigilo")

        movimento <-   .x |>
          xml2::xml_attr("movimento")

        descricao <-   .x |>
          xml2::xml_attr("descricao")

        tipo_documento_local <-   .x |>
          xml2::xml_attr("tipoDocumentoLocal")

        tibble::tibble(processo,id_documento, tipo_documento, data_hora, mime_type, nivel_sigilo, movimento, descricao, tipo_documento_local)

      }, NULL))


  }, NULL), .progress = TRUE)

}
