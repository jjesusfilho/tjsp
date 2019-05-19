#' Organiza metadados cpopg (apenas criminal por hora)
#'
#' @param df data.frame lido com ler_dados_cpopg
#' @param excluir_assunto informar vetor de frases a serem excluídas dos assuntos.
#'     Há uma sugestão de palavras no dataset 'excluir_assuntos_cpopg'
#' @param excluir_classe informar vetor de frases a serem excluídas das classes.
#' @return data.frame limpo e organizado
#' @export
#'
organizar_dados_cpopg <- function (df, excluir_assunto = "",excluir_classe="") {
  df <- df %>% janitor::clean_names()
  
  if (nrow(df)>0) {
    
    df <- df %>%
      dplyr::mutate(
        branco = dplyr::case_when(stringr::str_detect(v1, "(?i)[áa]rea") ~ "area",
                                  v1 == "(Tramitação prioritária)" ~ "prioritaria",
                                  TRUE ~ "vara" )
      ) %>%
      tidyr::unite("v2", assunto, v1, sep = "&",remove=FALSE) %>%
      tibble::rowid_to_column() %>%
      tidyr::spread(branco, v2) %>%
      dplyr::mutate_at(dplyr::vars(area, vara), list(~stringr::str_remove(., "NA&"))) %>% 
      dplyr::mutate(vara = zoo::na.locf(vara, fromLast = T,na.rm=FALSE)) %>%
      dplyr::filter(!is.na(distribuicao) | !is.na(juiz) | !is.na(classe) | !is.na(assunto)) %>%
      dplyr::filter(!is.element(assunto, excluir_assunto)) %>%
      dplyr::mutate(data_distribuicao = stringr::str_extract(distribuicao, "\\d+/\\d+/\\d+") %>%
                      lubridate::dmy(),
                    horario_distribuicao = stringr::str_extract(distribuicao, "\\d{2}:\\d{2}") %>%
                      lubridate::hm(),
                    tipo_distribuicao = stringr::str_extract(distribuicao, "(?<=-\\s).+"),
                    distribuicao = NULL) %>%
      { if (exists("recebido_em")) dplyr::mutate(data_recebimento = stringr::str_extract(recebido_em, "\\d+/\\d+/\\d+") %>%
                      lubridate::dmy(),
                    recebido_em = NULL) else . }  %>% 
      tidyr::separate(vara, c("vara", "foro"), sep = " - ", extra = "merge") %>%
      dplyr::mutate(area = stringr::str_remove_all(v1,"(?i)(Área|\\W+)"),
                    v1 = NULL)
    
      
      if (exists("execucao_de_sentenca",df,inherits=FALSE)){
        
        df <- dplyr::mutate(df,classe=dplyr::if_else(is.na(classe),execucao_de_sentenca,classe))
        
      }
    
    if (exists("incidente",df,inherits=FALSE)){
      
      df <- dplyr::mutate(df,classe=dplyr::if_else(is.na(classe),incidente,classe))
      
    }
    
    if (exists("processo_2",df,inherits=FALSE) & exists("processo_principal",df,inherits=FALSE)){
      
      df <- dplyr::mutate(df,processo_principal=dplyr::if_else(is.na(processo_principal),processo_2,processo_principal))
      
    } else if (exists("processo_2", df,inherits=FALSE) & exists("processo_principal",df,inherits=FALSE)==FALSE){
      
      df <- dplyr::mutate(df,processo_principal=processo_2,
                          processo_2=NULL)
      
    }
    
    
    if (exists("processo_principal",df,inherits=FALSE)){
      
     df<- dplyr::mutate(df,situacao = stringr::str_extract(processo_principal,"\\p{L}+$"))
        
    }
    
    if (exists("data_recebimento",df,inherits=FALSE)){
      
  df<-    dplyr::mutate(df,data_distribuicao = dplyr::if_else(is.na(data_distribuicao)==TRUE,data_recebimento,data_distribuicao))
        
    }
    
      df <- df %>% 
      dplyr::filter(!is.element(classe, excluir_classe)) %>%
      dplyr::mutate(rowid = NULL)
    
    if (exists("prioritaria",df,inherits=FALSE)) {
     df<- dplyr::mutate(df,prioritaria = stringr::str_remove(prioritaria, "&.+"))
    }
    
      if (exists("valor_da_acao",df,inherits = FALSE)){
        
        df<-dplyr::mutate(df,valor_da_acao=tjsp::numero(valor_da_acao))
        
      }
      
  }
  return(df)
}
