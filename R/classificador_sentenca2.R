#' Classifica decisões de primeiro grau
#'
#' @param sentenca vetor de sentenças
#'
#' @return decisão
#' @export
#'
#' @examples
#' \dontrun{
#' x <- classificar_sentenca2(sentenca)
#' }
classificar_sentenca2<- function (sentenca)
{

 x %>%
     stringr::str_sub(-2000) %>%
      tolower() %>%
      stringi::stri_trans_general("latin-ascii") %>%
      dplyr::case_when(
      stringr::str_detect(x,"(?i)julgo\\sparcial\\w+") ~ "parcial",
      stringr::str_detect(x,"(?i)\\bparcial\\w+") ~ "parcial",
      stringr::str_detect(x,"(?i)julgo\\s+procecente em parte") ~ "parcial",
      stringr::str_detect(x,"(?i)\\bprocecente em parte") ~ "parcial",
      stringr::str_detect(x,"desistencia") ~ "desistência",
      stringr::str_detect(x,"\\bhomologo\\b") ~  "homologação",
      stringr::str_detect(x,"(?i)julgo\\s+procede\\w+") ~ "procedente",
      stringr::str_detect(x,"(?i)julgo\\simprocede\\w+") ~ "improcedente",
      stringr::str_detect(x,"(?i)\\bprocede\\w+") ~ "procedente",
      stringr::str_detect(x,"(?i)\\bimprocede\\w+") ~ "improcedente",
      stringr::str_detect(x,"(?i)prejudicad[ao]") ~  "prejudicado",
      stringr::str_detect(x,"(?i)(an)?nul[ao](do)?") ~ "nulo",
      stringr::str_detect(x,"(?i)extin\\w+") ~ "extinto",
      TRUE ~ NA_character_
    )
}
