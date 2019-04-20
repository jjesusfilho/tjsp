#' Organiza metadados cpopg (apenas criminal por hora)
#'
#' @param df data.frame lido com ler_dados_cpopg
#' @param excluir informar vetor de frases a serem excluídas dos assuntos.
#'     Há uma sugestão de palavras no dataset 'excluir_assuntos_cpopg'
#' @return data.frame limpo e organizado
#' @export
#'
organiza_dados_cpopg <- function (df, excluir = "") {
  df <- df %>% janitor::clean_names()

  if (nrow(df)>0) {

    df <- df %>%
      dplyr::mutate(
        branco = dplyr::case_when(stringr::str_detect(v1, "(?i)[áa]rea") ~ "area",
                                  v1 == "(Tramitação prioritária)" ~ "prioritaria",
                                  TRUE ~ "vara" )
      ) %>%
      tidyr::unite("v2", assunto, v1, sep = "&",remove=FALSE) %>%
      tibble::rowid_to_column() %>%
      tidyr::spread(branco, v2) %>%
      dplyr::mutate_at(dplyr::vars(area, vara), list(~stringr::str_remove(., "NA&"))) %>%
      dplyr::mutate(prioritaria = stringr::str_remove(prioritaria, "&.+")) %>%
      dplyr::mutate(vara = zoo::na.locf(vara, fromLast = T,na.rm=FALSE)) %>%

      dplyr::filter(!is.na(distribuicao) | !is.na(juiz) | !is.na(classe) | !is.na(assunto)) %>%
      dplyr::filter(!is.element(assunto, excluir)) %>%
      dplyr::mutate(data_distribuicao = stringr::str_extract(distribuicao, "\\d+/\\d+/\\d+") %>%
                      lubridate::dmy(),
                    horario_distribuicao = stringr::str_extract(distribuicao, "\\d{2}:\\d{2}") %>%
                      lubridate::hm(),
                    tipo_distribuicao = stringr::str_extract(distribuicao, "(?<=-\\s).+"),
                    distribuicao = NULL) %>%
      dplyr::mutate(data_recebimento = stringr::str_extract(recebido_em, "\\d+/\\d+/\\d+") %>%
                      lubridate::dmy(),
                    recebido_em = NULL) %>%
      tidyr::separate(vara, c("vara", "foro"), sep = " - ", extra = "merge") %>%
      dplyr::mutate(area = stringr::str_remove_all(v1,"(?i)(Área|\\W+)"),
                    v1 = NULL) %>%
      dplyr::mutate(classe=dplyr::if_else(exists("execucao_de_sentenca") & is.na(classe)==TRUE,execucao_de_sentenca,classe)) %>%
      dplyr::mutate(classe = dplyr::if_else(exists("incidente") & is.na(classe)==TRUE,incidente,classe)) %>%
      dplyr::mutate(processo_principal = dplyr::if_else(exists("processo_2") & is.na(processo_principal)==TRUE,processo_2,processo_principal)) %>%
      dplyr::mutate(situacao = stringr::str_extract(processo_principal,"\\p{L}+$")) %>%
      dplyr::mutate(data_distribuicao = dplyr::if_else(is.na(data_distribuicao)==TRUE,data_recebimento,data_distribuicao)) %>%
      dplyr::mutate(rowid = NULL)

  }
  return(df)
}
