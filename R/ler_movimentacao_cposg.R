#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param diretorio diretório onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg<-ler_movimentacao_cposg()
#' andamento_cpopg<-ler_movimentacao_cpopg()
#' }

ler_movimentacao_cposg<-ler_movimentacao_cpopg<-function(diretorio="."){


  a<- list.files(path=diretorio,pattern=".html",full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}") %>%
            abjutils::build_id()


  future::plan("multiprocess")
  furrr::future_map2_dfr(a,processo,purrr::possibly(~{
    texto<-   xml2::read_html(.x) %>%
      rvest::html_node(xpath="//table/tbody[@id='tabelaTodasMovimentacoes']") %>%
      rvest::html_text()
    data<-stringr::str_extract_all(texto,"(?<=\t)\\d{2}/\\d{2}/\\d{4}") %>%
          unlist()
    mov<- stringr::str_extract_all(texto,"(?<=\t)[a-zA-Z].+") %>%
      unlist()

   tibble::tibble(processo=.y,data=data,movimentacao=mov)
  },otherwise=NULL))


}

