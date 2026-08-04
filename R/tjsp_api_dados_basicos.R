#' Dados básicos autenticados de um processo na API mobile do TJSP
#'
#' @param numero_ou_cd Número CNJ do processo ou o `cd_processo` interno do SAJ
#'    (obtido em [tjsp_api_buscar_processo()] e afins).
#' @param grau "cpopg" (1º grau cível, padrão), "cposg" (2º grau) ou "cpocr" (criminal).
#'
#' @details Requer login prévio ([tjsp_api_autenticar()]) e que o usuário esteja
#'    habilitado nos autos -- senão o único campo retornado é
#'    `status = "SEMACESSODETALHES"`. Esta chamada também **abre** o processo
#'    na sessão, pré-requisito para [tjsp_api_partes()], [tjsp_api_movimentacao()]
#'    e a pasta digital (essas funções abrem o processo automaticamente quando
#'    necessário).
#'
#' @return tibble de uma linha.
#' @export
#'
tjsp_api_dados_basicos <- function(numero_ou_cd, grau = "cpopg") {

  cd <- .tjsp_api_resolver_cd(numero_ou_cd, grau)

  dados <- .tjsp_api_abrir(cd, grau)

  if (is.null(dados)) {
    return(tibble::tibble(cd_processo = cd, status = NA_character_))
  }

  dados |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    tibble::add_column(cd_processo = cd, .before = 1)
}
