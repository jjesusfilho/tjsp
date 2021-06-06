#' Classificar writs(HC, MS, revisão criminal)
#'
#' @param x Texto da decisão
#'
#' @return classificação
#' @export
#'
tjsp_classificar_writ <- function(x){

  x<- x %>%
    stringr::str_sub(-2000) %>%
    tolower() %>%
    stringi::stri_trans_general("latin-ascii")


  dplyr::case_when(
  stringr::str_detect(x, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
  stringr::str_detect(x, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
  stringr::str_detect(x, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
  stringr::str_detect(x, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
  stringr::str_detect(x,"(prej|extin)") ~ "prejudicado/extinto",
  stringr::str_detect(x,"(indef|inder\\w+)") ~ "denegado",
  stringr::str_detect(x,"^defer") ~ "concedido",
  stringr::str_detect(x,",\\s+deferi\\w+") ~ "concedido",
  stringr::str_detect(x,"(desp|impr)") ~ "denegado",
  stringr::str_detect(x, "parcial\\w*\\sdeferi\\w+") ~ "parcial",
  stringr::str_detect(x,"(nao|nega\\w+)\\s+provi.*")~ "improvido",
  stringr::str_detect(x,"^prov") ~ "concedido",
  stringr::str_detect(x,"parcial\\sprov\\w+") ~ "concedido",
  stringr::str_detect(x,"absolv\\w+") ~ "concedido",
  stringr::str_detect(x, "acolher\\w+|\\bprocedente") ~ "concedido",
  stringr::str_detect(x,"de*neg") ~ "denegado",
  stringr::str_detect(x,"^conce\\w+") ~ "concedido",
  stringr::str_detect(x,"^conhe\\w+") ~ "concedido",
  stringr::str_detect(x, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
  stringr::str_detect(x,"^desconh\\w+") ~ "desconhecido",
  stringr::str_detect(x,"(homolog|desistencia)") ~ "desist\u00eancia",
  stringr::str_detect(x, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
  stringr::str_detect(x,"(,|e|votos)\\s+conce\\w+")~ "concedido",
  TRUE ~ NA_character_
)

}


#' @rdname tjsp_classificar_writ
#' @export
classificar_writ <- tjsp_classificar_writ
