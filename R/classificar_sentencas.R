#' Classifica decisões de primeiro grau
#'
#' @param x data.frame cjpg
#' @param sentenca nome da coluna, sem aspas,  com as sentenças (julgados)
#' @param decisao nome da coluna, entre aspas, a ser criada.
#'
#' @return cria uma nova coluna com a classificação das decisões
#' @export
#'
#' @examples
#' \dontrun{
#' df <- classificar_sentenca(df,julgado,"decisao")
#' }
classificar_sentenca<- function (x, sentenca, decisao)
{
  input <- rlang::enexpr(sentenca)
  decisao_out <- rlang::enexpr(decisao)

  y <-  x %>% dplyr::distinct(rlang::UQ(input)) %>%
    dplyr::mutate(alternativa = stringr::str_sub(rlang::UQ(input),-2000) %>%
                    tolower(.) %>%  stringi::stri_trans_general(., "latin-ascii"))

  y <- y %>% dplyr::mutate(`:=`(
    !!decisao_out,
    dplyr::case_when(
      stringr::str_detect(alternativa,"(?i)\\bparcial\\w+") ~ "parcial",
      stringr::str_detect(alternativa,"(?i)\\bprocecente em parte") ~ "parcial",
      str_detect(alternativa,"desistencia") ~ "desistência",
      str_detect(alternativa,"\\bhomologo\\b") ~  "homologação",
      stringr::str_detect(alternativa,"(?i)(\\bproced\\w+|condeno)") ~ "procedente",
      stringr::str_detect(alternativa,"(?i)\\bimproced\\w+") ~ "improcedente",
      stringr::str_detect(alternativa,"(?i)prejudicad[ao]") ~  "prejudicado",
      stringr::str_detect(alternativa,"(?i)(an)?nul[ao](do)?") ~ "nulo",
      stringr::str_detect(alternativa,"(?i)extin\\w+") ~ "extinto",
      TRUE ~ NA_character_
    )
  )) %>%
    dplyr::select(-alternativa)
  x %>% dplyr::left_join(y)
}
