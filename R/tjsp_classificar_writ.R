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
    stringi::stri_trans_general("latin-ascii")


  dplyr::case_when(
  stringr::str_detect(x, "(?i)(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
  stringr::str_detect(x, "(?i)(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
  stringr::str_detect(x, "(?i)(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
  stringr::str_detect(x, "(?i)(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
  stringr::str_detect(x, "(?i)(indef|inder\\w+)") ~ "indeferido",
  stringr::str_detect(x, "(?i)(?=.*(parcial|parte)\\w*)(?=.*\\bdefer\\w+)") ~ "parcial",
  stringr::str_detect(x, "(?i)(?=.*(parcial|parte)\\w*)(?=.*\\b(proced|prodec)\\w+)") ~ "parcial",
  stringr::str_detect(x, "(?i)indef[ei]riram") ~ "indeferido",
  stringr::str_detect(x, "(?i)\\bdef[ei]riram") ~ "deferido",
  stringr::str_detect(x, "(?i)^defer") ~ "deferido",
  stringr::str_detect(x, "(?i),\\s+defer\\w+") ~ "deferido",
  stringr::str_detect(x, "(?i)(improced|improdec)") ~ "improcedente",
  stringr::str_detect(x, "(?i)(desp|improv)") ~ "improvido",
  stringr::str_detect(x, "(?i)(nao|nega\\w+)\\s+provi.*")~ "improvido",
  stringr::str_detect(x, "(?i)^prov") ~ "provido",
  stringr::str_detect(x, "(?i)(acolher|deu|deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)\\sprovim") ~ "provido",
  stringr::str_detect(x, "(?i)(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)\\sprovim") ~ "improvido",
  stringr::str_detect(x, "(?i)parcial\\sprov\\w+") ~ "parcial",
  stringr::str_detect(x, "(?i)absolv\\w+") ~ "absolvido",
  stringr::str_detect(x, "(?i)(\\bprocedente|\\bprodecente|procedencia|prodecencia)") ~ "procedente",
  stringr::str_detect(x, "(?i)de*neg") ~ "denegado",
  stringr::str_detect(x, "(?i)^conce\\w+") ~ "concedido",
  stringr::str_detect(x, "(?i)^conhe\\w+") ~ "concedido",
  stringr::str_detect(x, "(?i)(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
  stringr::str_detect(x, "(?i)^desconh\\w+") ~ "desconhecido",
  stringr::str_detect(x, "(?i)(homolog|desistencia)") ~ "desist\u00eancia",
  stringr::str_detect(x, "(?i)diligencia") ~ "convers\u00e3o em dilig\u00eancia",
  stringr::str_detect(x, "(?i)(prej|extin)") ~ "prejudicado/extinto",
  stringr::str_detect(x, "(?i)(,|e|votos)\\s+conce\\w+")~ "concedido",
  TRUE ~ NA_character_
)

}


#' @rdname tjsp_classificar_writ
#' @export
classificar_writ <- tjsp_classificar_writ
