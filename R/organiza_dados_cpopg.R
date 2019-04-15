#' Organiza metadados cpopg (apenas criminal por hora)
#'
#' @param df data.frame lido com ler_dados_cpopg
#' @param excluir informar vetor de frases a serem excluídas dos assuntos.
#'     Há uma sugestão de palavras no dataset 'excluir_assuntos_cpopg'
#'
#' @return data.frame limpo e organizado
#' @export
#'
organizar_dados_cpopg <- function(df, excluir = "") {
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(processo, codigo_processo, digital, v1, assunto, classe, distribuicao, juiz)

  processos_criminais <- df %>%
    dplyr::filter(v1 == "Área: Criminal") %>%
    dplyr::pull("processo") %>%
    unique()

  df %>%
    dplyr::filter(processo %in% processos_criminais) %>%
    dplyr::mutate(branco = dplyr::case_when(
      v1 == "Área: Criminal" ~ "area",
      v1 == "(Tramitação prioritária)" ~ "prioritaria",
      TRUE ~ "vara"
    )) %>%
    tidyr::unite("v2", assunto, v1, sep = "&", remove = FALSE) %>%
    tidyr::spread(branco, v2) %>%
    dplyr::mutate_at(dplyr::vars(area, vara), dplyr::funs(stringr::str_remove(., "NA&"))) %>%
    dplyr::mutate(prioritaria = stringr::str_remove(prioritaria, "&.+")) %>%
    dplyr::select(processo, codigo_processo, vara, digital, assunto, classe, distribuicao, juiz) %>%
    dplyr::mutate(vara = zoo::na.locf(vara, fromLast = T)) %>%
    dplyr::filter(!is.na(distribuicao) | !is.na(juiz) | !is.na(classe) | !is.na(assunto)) %>%
    dplyr::filter(!is.element(assunto, excluir)) %>%
    dplyr::mutate(
      data_distribuicao = stringr::str_extract(distribuicao, "\\d+/\\d+/\\d+") %>%
        lubridate::dmy(),
      horario_distribuicao = stringr::str_extract(distribuicao, "\\d{2}:\\d{2}"),
      tipo_distribuicao = stringr::str_extract(distribuicao, "(?<=-\\s).+"),
      distribuicao = NULL
    ) %>%
    tidyr::separate(vara, c("vara", "foro"), sep = " - ", extra = "merge")
}
