#' Organiza metadados cpopg (apenas criminal por ora)
#'
#' @param df data.frame lido com ler_dados_cpopg
#' @param excluir_assunto informar vetor de frases a serem excluídas dos assuntos.
#'     Há uma sugestão de palavras no dataset 'excluir_assuntos_cpopg'
#' @param excluir_classe informar vetor de frases a serem excluídas das classes.
#' @return data.frame limpo e organizado
#' @export
#'
organizar_dados_cpopg <- function (df, excluir_assunto = "", excluir_classe = "")
{
  df <- df %>% janitor::clean_names()
  suppressWarnings({

    df$na <- NA_character_

    df$distribuicao <- dplyr::coalesce(df$na, df$distribuicao,
                                       df$recebido_em)

    if (exists("execucao_de_sentenca",df, inherits = FALSE) & exists("incidente",df,inherits = FALSE)){


      df$classe <- dplyr::coalesce(df$na, df$classe, df$execucao_de_sentenca,
                                   df$incidente)

    } else if (exists("execucao_de_sentenca",df, inherits = FALSE)){

      df$classe <- dplyr::coalesce(df$na, df$classe, df$execucao_de_sentenca)

    } else {

      df$classe <- dplyr::coalesce(df$na, df$classe)

    }

    df$processo_principal <- dplyr::coalesce(df$na, df$processo_principal,
                                             df$processo_2)
    df$recebido_em <- NULL
    df$processo_2 <- NULL
    df$execucao_de_sentenca <- NULL
    df$incidente <- NULL
    df$na <- NULL

    if (nrow(df) > 0) {
      df <- df %>% dplyr::mutate(branco = dplyr::case_when(stringr::str_detect(v1,
                                                                               "(?i)[\\u00e1a]rea") ~ "area", v1 == "(Tramita\\u00e7\\u00e3o priorit\\u00e1ria)" ~
                                                             "prioritaria", TRUE ~ "vara")) %>% tidyr::unite("v2",
                                                                                                             assunto, v1, sep = "&", remove = FALSE) %>% tibble::rowid_to_column() %>%
        tidyr::spread(branco, v2) %>% dplyr::mutate_at(dplyr::vars(area,
                                                                   vara), list(~stringr::str_remove(., "NA&"))) %>%
        dplyr::mutate(vara = zoo::na.locf(vara, fromLast = T,
                                          na.rm = FALSE)) %>% dplyr::filter(!is.na(distribuicao) |
                                                                              !is.na(classe) | !is.na(assunto)) %>% dplyr::filter(!is.element(assunto,
                                                                                                                                              excluir_assunto)) %>% dplyr::mutate(data_recebimento = stringr::str_extract(distribuicao,
                                                                                                                                                                                                                          "\\d+/\\d+/\\d+") %>% lubridate::dmy(), horario_recebimento = stringr::str_extract(distribuicao,
                                                                                                                                                                                                                                                                                                             "\\d{2}:\\d{2}") %>% lubridate::hm(), tipo_recebimento = stringr::str_extract(distribuicao,
                                                                                                                                                                                                                                                                                                                                                                                           "(?<=-\\s).+"), distribuicao = NULL) %>% tidyr::separate(vara,
                                                                                                                                                                                                                                                                                                                                                                                                                                                    c("vara", "foro"), sep = " - ", extra = "merge") %>%
        dplyr::mutate(area = stringr::str_remove_all(v1,
                                                     "(?i)(\\u00c1rea|\\W+)"), v1 = NULL)
      df <- df %>% dplyr::filter(!is.element(classe, excluir_classe)) %>%
        dplyr::mutate(rowid = NULL)
      if (exists("prioritaria", df, inherits = FALSE)) {
        df <- dplyr::mutate(df, prioritaria = stringr::str_remove(prioritaria,
                                                                  "&.+"))
      }
      if (exists("valor_da_acao", df, inherits = FALSE)) {
        df <- dplyr::mutate(df, valor_da_acao = tjsp::numero(valor_da_acao))
      }
    }
  })
  return(df)
}
