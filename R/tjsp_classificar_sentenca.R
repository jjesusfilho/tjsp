#' Classifica decisões de primeiro grau
#'
#' @param x vetor de sentenças
#'
#' @return decisão
#' @export
#'
#' @examples
#' \dontrun{
#' x <- tjsp_classificar_sentenca(x = sentenca)
#' }
tjsp_classificar_sentenca <- function (x)
{

x<- x %>%
    # stringr::str_sub(-2000) %>%
      tolower() %>%
      stringi::stri_trans_general("latin-ascii")

      dplyr::case_when(
      re2::re2_detect(x,"(?i)julgo\\sparcial\\w+") ~ "parcial",
      re2::re2_detect(x,"(?i)\\bparcial\\w+") ~ "parcial",
      re2::re2_detect(x,"(?i)julgo\\s+procedente em parte") ~ "parcial",
      re2::re2_detect(x,"(?i)\\bprocedente em parte") ~ "parcial",
      re2::re2_detect(x,"(?i)\\bhomolog\\w+\\b") ~  "homologacao",
      re2::re2_detect(x,"(?i)julgo\\s+procede\\w+") ~ "procedente",
      re2::re2_detect(x,"(?i)julgo\\simprocede\\w+") ~ "improcedente",
      re2::re2_detect(x,"(?i)\\bproced\\w+") ~ "procedente",
      re2::re2_detect(x,"(?i)\\bimproced\\w+") ~ "improcedente",
      re2::re2_detect(x,"(?i)prejudicad[ao]") ~  "prejudicado",
      re2::re2_detect(x,"(?i)(an)?nul[ao](do)?") ~ "nulo",
      re2::re2_detect(x,"(?i)extin\\w+") ~ "extinto",
      re2::re2_detect(x,"(?i)rejeit\\w+ (os)? embargos") ~  "embargos rejeitados",
      re2::re2_detect(x,"(?i)n\u00e3o acolho os embargos") ~ "embargos rejeitados",
      re2::re2_detect(x,"(?i)\\bdefiro") ~ "deferido",
      re2::re2_detect(x,"(?i)\\bindefiro") ~ "indefiro",
      re2::re2_detect(x,"(?i)\\bdeneg\\w+") ~ "denegado",
      re2::re2_detect(x,"(?i)desistencia") ~ "desistencia",
      TRUE ~ NA_character_
    )
}

#' @rdname tjsp_classificar_sentenca
#' @export
classificar_sentenca2 <- tjsp_classificar_sentenca

