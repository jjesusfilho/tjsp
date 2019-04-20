#' Ajustar os assuntos com base na tabela do TJSP
#'
#' @param df tibble obtido a partir da função organiza_dados_cpopg
#' @param excluir_cartas default para TRUE. Excluir cartas de ordem e pracatórias.
#
#' @return mesmo df com assuntos ajustados e áreas acrescidas.
#' @export
#'
ajustar_assuntos <- function(df, excluir_cartas=TRUE){

 df %>%
    dplyr::select(assunto) %>%
    dplyr::distinct() %>%
    dplyr::mutate(assunto_= stringr::str_remove(assunto,"^\\d.+?\\- ") %>%
                    abjutils::rm_accent() %>%
                    stringr::str_to_lower() %>%
                    stringr::str_squish()
    ) %>%
    dplyr::left_join(tjsp::assuntos,by="assunto_") %>%
    dplyr::select(assunto= assunto.x,area2=area) %>%
    dplyr::right_join(df,by="assunto") %>%
    dplyr::select(processo,assunto,area,area2,classe,juiz,vara,foro,digital,dplyr::everything()) %>%
    dplyr::filter(if (excluir_cartas == TRUE)  stringr::str_detect(classe,"(?i)carta",negate=TRUE)) %>%
    dplyr::distinct(processo)
}

