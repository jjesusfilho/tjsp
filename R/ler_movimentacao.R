ler_movimentacao<-function(path="."){

  a<- list.files(path=path,pattern=".html",full.names = T)

  processo<-stringr::str_extract(a,"\\d{20}")

    purrr::map2_dfr(a,processo,purrr::possibly(~{
    texto<-   xml2::read_html(.x) %>%
    rvest::html_node(xpath="//table/tbody[@id='tabelaTodasMovimentacoes']") %>%
    rvest::html_text()
  data<-str_extract_all(texto,"(?<=\t)\\d{2}/\\d{2}/\\d{4}")
  mov<- str_extract_all(texto,"(?<=\t)[a-zA-Z].+")


    cbind(processo=.y,data=data,mov=mov)
    },otherwise=NULL))

}
