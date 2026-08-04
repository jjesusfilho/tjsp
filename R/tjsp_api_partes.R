#' Lista as partes de um processo na API mobile do TJSP (autenticado)
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ.
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e habilitação nos
#'    autos. Os advogados de cada parte viram linhas próprias, com
#'    `tipo_parte = "Advogado"` e `representante` = nome da parte representada
#'    -- mesma convenção de colunas de [tjsp_ler_partes()]. A API mobile não
#'    traz CPF/CNPJ/OAB das partes; para documentos, use `tjsp-apollo`.
#'
#' @return tibble com colunas `cd_processo`, `tipo_parte`, `parte`, `representante`.
#' @export
#'
tjsp_api_partes <- function(numero_ou_cd, grau = "cpopg") {

  cd <- .tjsp_api_resolver_cd(numero_ou_cd, grau)

  data <- .tjsp_api_detalhe_de_processo_aberto("partes", cd, grau)

  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(data)), function(i) {

    participacao <- data$participacao[[i]]
    if (is.null(participacao) || is.na(participacao) || participacao == "") {
      participacao <- "Parte"
    }
    nome <- data$nome[[i]]
    if (is.null(nome) || is.na(nome)) nome <- ""

    linhas <- tibble::tibble(
      cd_processo = cd, tipo_parte = participacao, parte = nome, representante = NA_character_
    )

    advogados <- data$advogados[[i]]
    advogados <- advogados[!is.na(advogados) & advogados != ""]

    if (length(advogados) > 0) {
      linhas <- dplyr::bind_rows(
        linhas,
        tibble::tibble(
          cd_processo = cd, tipo_parte = "Advogado", parte = advogados, representante = nome
        )
      )
    }

    linhas
  })
}
