#' Classifica acórdãos
#'
#' @param x data.frame com a coluna a ser classificada
#' @param dispositivo Coluna onde se encontram os dispositivos do acórdão
#' @param decisao Nome da coluna a ser criada com a decisão
#' @param inteiro_teor Colocar TRUE se a coluna conter o inteiro teor
#' @return x adicionado da coluna decisão.
#' @export
#'
classificar_recurso <- function(x, dispositivo, decisao, inteiro_teor = FALSE) {

  ## Para reduzir o tempo de computação, eu optei por dar um count na coluna a ser classificada.
  ## Poderia usar somente a coluna, mas count rearranja por ordem alfabética, ou por outra ordem,
  ## mesmo que eu dê sort=FALSE. Diante dessa limitação, eu incluí o data.frame como input, para
  ## mais tarde dar um left_join. Alternativamente, pode-se converter o vetor original em tibble
  ## e dar um left_join mais tarde. Não sei o que é melhor.

  input <- rlang::enexpr(dispositivo)
  decisao_out <- rlang::enexpr(decisao)


  if (inteiro_teor == TRUE){
    y <-  x %>% dplyr::distinct(!!input) %>%
      dplyr::mutate(alternativa = stringr::str_sub(!!input,-2000) %>%
                      tolower(.) %>%  stringi::stri_trans_general(., "latin-ascii"))

  } else

    y <- x %>%
    dplyr::distinct(!!input) %>%
    dplyr::mutate(alternativa = tolower(!!input) %>%
                    stringi::stri_trans_general(., "latin-ascii"))


  y <- y %>%
    dplyr::mutate(!!decisao_out :=
      dplyr::case_when(
        stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "parcial\\w*\\sprovi\\w+") ~ "parcial",
        stringi::stri_detect_regex(alternativa, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "n\u00e3o conhecido",
        stringi::stri_detect_regex(alternativa, "desconh\\w+") ~ "desconhecido",
        stringi::stri_detect_regex(alternativa, "nao\\s+conhec\\w+") ~ "desconhecido",
        stringi::stri_detect_regex(alternativa, "(desprov\\w+|improv\\w+)") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "(nao|nega\\w+)\\s+provi\\X*") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "prove\\w+") ~ "provido",
        stringi::stri_detect_regex(alternativa, "mantiveram") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "acolh\\w+") ~ "provido",
        stringi::stri_detect_regex(alternativa, "(deu|deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)") ~ "provido",
        stringi::stri_detect_regex(alternativa, "(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "(homolog|desistencia)") ~ "desist\u00eancia",
        stringi::stri_detect_regex(alternativa, "(anular\\w*|nulo|nula|nulidade)") ~ "anulado",
        stringi::stri_detect_regex(alternativa, "diligencia") ~ "convers\u00e3o em dilig\u00eancia",
        stringi::stri_detect_regex(alternativa, "(prej|extin)") ~ "prejudicado/extinto",
        TRUE ~ "outros"
      )) %>%
    dplyr::select(-alternativa)

  x %>%
    dplyr::left_join(y)
}
