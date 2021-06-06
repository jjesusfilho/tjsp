#' Corrigir nomes das varas
#'
#' @param df data.frame com a coluna dos nomes da varas a serem corrigidas
#' @param coluna nome da coluna
#'
#' @return mesmo data.frame adicionado da coluna vara2
#' @export
#'
tjsp_corrigir_varas <- function(df, coluna) {
  .x <- rlang::enquo(coluna)

  df %>%
    dplyr::select(!!.x) %>%
    dplyr::distinct() %>%
    dplyr::mutate(vara2 := stringr::str_to_lower(!!.x) %>%
      stringr::str_replace_all("\\W+", " ") %>%
      stringr::str_squish() %>%
      stringr::str_replace("\u00ba", "\u00aa") %>%
      stringr::str_replace("vara juizado", "vara do juizado") %>%
      stringr::str_to_sentence(),
     vara2 = dplyr::case_when(
      stringr::str_detect(vara2, "(mulher|dom\u00e9stica|fam.lia)") ~ "Vara de viol\u00eancia dom\u00e9stica",
      TRUE ~ vara2
    )
    ) %>%
    dplyr::right_join(df)
}
