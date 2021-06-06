#' Ajustar os assuntos com base na tabela do TJSP
#'
#' @param df tibble obtido a partir da função organiza_dados_cpopg
#' @param excluir_cartas default para TRUE. Excluir cartas de ordem e pracatórias.
#'     Se você optar por FALSE, verifique a compatibilidade entre o assunto e a area.
#
#' @return mesmo df com assuntos ajustados e áreas e  subáreas acrescidas.
#' @export
#'
ajustar_assuntos <- function(df, excluir_cartas = TRUE) {
  suppressWarnings({
    df %>%
      dplyr::select(assunto) %>%
      dplyr::distinct() %>%
      dplyr::mutate(assunto_ = stringr::str_remove(assunto, "^\\d.+?\\- ") %>%
        stringi::stri_trans_general("latin-ascii") %>%
        stringr::str_to_lower() %>%
        stringr::str_squish()) %>%
      dplyr::left_join(tjsp::assuntos, by = "assunto_") %>%
      dplyr::select(assunto = assunto.x, subarea = area) %>%
      tidyr::separate(subarea, c("cod_subarea", "subarea"), sep = "(?<=\\d)\\s-\\s", extra = "merge") %>%
      dplyr::right_join(df, by = "assunto") %>%
      tidyr::separate(classe, c("classe", "classe_numero", "classe_situacao"), sep = "(\\s\\(|\\)\\s?)", extra = "merge") %>%
      tidyr::separate(processo_principal, c("processo_principal", "situacao"), sep = "\\s", extra = "merge") %>%
      dplyr::select(processo, assunto, area, cod_subarea, subarea, classe, classe_numero, classe_situacao, juiz, vara, foro, digital, dplyr::everything()) %>%
      dplyr::mutate(subarea = dplyr::coalesce(subarea, area)) %>%
      dplyr::filter(if (excluir_cartas == TRUE) stringr::str_detect(classe, "(?i)carta", negate = TRUE)) %>%
      dplyr::distinct(processo, .keep_all = TRUE)
  })
}
