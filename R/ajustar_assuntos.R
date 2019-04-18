#' Ajustar os assuntos com base na tabela do TJSP
#'
#' @param df tibble obtido a partir da função organiza_dados_cpopg
#'
#' @return mesmo df com assuntos ajustados e áreas acrescidas.
#' @export
#'
ajustar_assuntos <- function(df){

  df %>%
    dplyr::select(assunto) %>%
    dplyr::distinct() %>%
    dplyr::mutate(assunto_=stringr::str_remove(assunto,"^\\d.+?\\- ") %>%
                    abjutils::rm_accent() %>%
                    stringr::str_to_lower() %>%
                    stringr::str_squish()
    ) %>%
    dplyr::left_join(assuntos,by="assunto_") %>%
    dplyr::select(assunto = assunto.x,area2=area) %>%
    dplyr::right_join(df,by = "assunto")

}
