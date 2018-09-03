#' Função tjpg
#'
#' Esta função extraí dados de busca livre por julgados de primeira instância no TJSP
#' @param url Faça primeiramente a busca na página do TJSP, depois copie e cole a url aqui.
#' @keywords tjsp primeira instância
#' @return Um data.frame com as informações do julgado
#' @export
#' @examples
#' tjsg(url)

tjpg<-function (url){
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  a <- httr::GET(url)
  b <- xml2::xml_find_first(content(a, as = "parsed"),xpath="//*[@bgcolor='#EEEEEE']")
  val <- xml2::xml_text(b, trim = T)
  val <- stringi::stri_extract_first_regex(val, "\\d+$")
  num <- as.numeric(val)
  max_pag <- ceiling(num/10)
  df <- data.frame()
  for (i in 1:max_pag) {
    tryCatch({
      c <- httr::GET(paste0("https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=",
                      i, "&conversationId="), httr::set_cookies(unlist(a$cookies)))
      d <- xml2::xml_find_all(content(c, as = "text"),xpath="//*[@id='divDadosResultado']/table//td//td[@align='left']")
      ## From now on I had to use regex because not all nine elements are present in every response.
      s <- stringi::stri_replace_all_regex(s, "\\s+", " ")
      s <- paste0(s, collapse = "\n")
      s <- unlist(stringi::stri_split_regex(s, "\n(?=\\d{4,})"))
      processo <- stringi::stri_extract_first_regex(s, "\\d+-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}",
                                           omit_no_match = F)
      classe <- stringi::stri_extract_first_regex(s, "Classe:.*",
                                         omit_no_match = F)
      assunto <- stringi::stri_extract_first_regex(s, "Assunto:.*",
                                          omit_no_match = F)
      magistrado <- stringi::stri_extract_first_regex(s, "Magistrado:.*",
                                             omit_no_match = F)
      comarca <- stringi::stri_extract_first_regex(s, "Comarca:.*",
                                          omit_no_match = F)
      foro <- stringi::stri_extract_first_regex(s, "Foro:.*", omit_no_match = F)
      vara <- stringi::stri_extract_first_regex(s, "Vara:.*", omit_no_match = F)
      disponibilizacao <- stringi::stri_extract_first_regex(s, "Data\\sde\\sDisponibilização:.*",
                                                   omit_no_match = F)
      julgado <- stringi::stri_extract_last_regex(s, "(?<=\n).*",
                                         omit_no_match = F)
      df1 <- data.frame(processo, classe, assunto, magistrado,
                        comarca, foro, vara, disponibilizacao, julgado,
                        stringsAsFactors = F)
      df1$pagina <- i
      df1[2:8] <- lapply(df1[2:8], function(x) stringi::stri_replace_first_regex(x,
                                                                        ".*:\\s?", ""))
      df1[] <- lapply(df1[], stringr::stri_trim)
      df <- rbind(df, df1)
    }, error = function(m) {
      m
    }, finally = {
      next
    })
    sys.sleep(1)
  }
  df <- df[duplicated(df$processo) == F, ]
  return(df)
}

