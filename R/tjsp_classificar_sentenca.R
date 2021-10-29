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
      re2::re2_detect(x,"julgo\\sparcial\\w+") ~ "parcial",
      re2::re2_detect(x,"\\bparcial\\w+") ~ "parcial",
      re2::re2_detect(x,"julgo\\s+procedente em parte") ~ "parcial",
      re2::re2_detect(x,"\\bprocedente em parte") ~ "parcial",
      re2::re2_detect(x,"desistencia") ~ "desist\u00eancia",
      re2::re2_detect(x,"\\bhomolog\\w+\\b") ~  "homologa\u00e7\u00e3o",
      re2::re2_detect(x,"julgo\\s+procede\\w+") ~ "procedente",
      re2::re2_detect(x,"julgo\\simprocede\\w+") ~ "improcedente",
      re2::re2_detect(x,"\\bprocede\\w+") ~ "procedente",
      re2::re2_detect(x,"\\bimprocede\\w+") ~ "improcedente",
      re2::re2_detect(x,"prejudicad[ao]") ~  "prejudicado",
      re2::re2_detect(x,"(an)?nul[ao](do)?") ~ "nulo",
      re2::re2_detect(x,"extin\\w+") ~ "extinto",
      re2::re2_detect(x,"rejeit\\w+ (os)? embargos") ~  "embargos rejeitados",
      re2::re2_detect(x,"não acolho os embargos") ~ "embargos rejeitados",
      re2::re2_detect(x,"\\bdefiro") ~ "deferido",
      re2::re2_detect(x, "\\bindefiro") ~ "indefiro",
      re2::re2_detect(x, "\\bdeneg\\w+") ~ "denegado",
      TRUE ~ NA_character_
    )
}

#' @rdname tjsp_classificar_sentenca
#' @export
classificar_sentenca2 <- tjsp_classificar_sentenca

