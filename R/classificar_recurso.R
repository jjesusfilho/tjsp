#' Classifica acórdãos
#'
#' @param x data.frame com a coluna a ser classificada
#' @param dispositivo Coluna onde se encontram os dispositivos do acórdão
#' @param decisao Nome da coluna a ser criada com a decisão
#'
#' @return x adicionado da coluna decisão.
#' @export
#'
classificar_recurso <- function(x, dispositivo, decisao) {

  ## Para reduzir o tempo de computação, eu optei por dar um count na coluna a ser classificada.
  ## Poderia usar somente a coluna, mas count rearranja por ordem alfabética, ou por outra ordem,
  ## mesmo que eu dê sort=FALSE. Diante dessa limitação, eu incluí o data.frame como input, para
  ## mais tarde dar um left_join. Alternativamente, pode-se converter o vetor original em tibble
  ## e dar um left_join mais tarde. Não sei o que é melhor.

  input <- rlang::enexpr(dispositivo)
  decisao_out <- rlang::enexpr(decisao)
  y <- x %>%
    dplyr::distinct(rlang::UQ(input)) %>%
    dplyr::mutate(alternativa = tolower(rlang::UQ(input)) %>%
      stringi::stri_trans_general(., "latin-ascii"))

  y <- y %>%
    dplyr::mutate(!!decisao_out :=
      dplyr::case_when(
        stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bneg[oa]\\w*\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bderam\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bneg[oa]\\w*\\b)(?=.*\\bprejudicado\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "(?=.*\\bacolh\\w+\\b)(?=.*\\bneg[ao]\\w*\\b)") ~ "duvida",
        stringi::stri_detect_regex(alternativa, "parcial\\w*\\sprovi\\w+") ~ "parcial",
        stringi::stri_detect_regex(alternativa, "(nao\\sconhec\\w+|nao\\sse\\sconhec\\w+)") ~ "não conhecido",
        stringi::stri_detect_regex(alternativa, "^desconh\\w+") ~ "desconhecido",
        stringi::stri_detect_regex(alternativa, "nao\\s+conhec\\w+") ~ "desconhecido",
        stringi::stri_detect_regex(alternativa, "^(desp|impr)") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "(nao|nega\\w+)\\s+provi\\X*") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "^prove\\w+") ~ "provido",
        stringi::stri_detect_regex(alternativa, "^mantiveram") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "acolh\\w+") ~ "provido",
        stringi::stri_detect_regex(alternativa, "(deram|da\\-*\\s*se|dando\\-*(se)*|comporta|\\bdou\\b|confere\\-se|se\\s*\\-*da|merece)") ~ "provido",
        stringi::stri_detect_regex(alternativa, "(nao\\sderam|nao\\smerece|se\\snega|nega\\-*\\s*se|negar\\-*\\s*lhe|nao\\scomporta|negram|negararam|nego|negar)") ~ "improvido",
        stringi::stri_detect_regex(alternativa, "(homolog|desistencia)") ~ "desistência",
        stringi::stri_detect_regex(alternativa, "(anular\\w*|nulo|nula|nulidade)") ~ "anulado",
        stringi::stri_detect_regex(alternativa, "diligencia") ~ "conversão em diligência",
        stringi::stri_detect_regex(alternativa, "(prej|extin)") ~ "prejudicado/extinto",
        TRUE ~ "outros"
      )) %>%
    dplyr::select(-alternativa)

  x %>%
    dplyr::left_join(y)
}
