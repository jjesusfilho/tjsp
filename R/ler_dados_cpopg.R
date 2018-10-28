#' Lê os metadados de processos de primeira instância.
#'
#' @param diretorio Diretório onde se encontram os htmls.
#'
#' @return tibble com os metadados
#' @export
#'
#' @examples
#' \dontrun{
#' ler_dados_cpopg()
#' }
#'
ler_dados_cpopg<-function(diretorio="."){

  arquivos<-list.files(path = diretorio,pattern=".html",full.names=TRUE)

  processos<-stringr::str_extract(arquivos,"\\d{20}") %>%
    as.numeric()


  future::plan("multiprocess")

  furrr::future_map2_dfr(arquivos,processos,purrr::possibly(~{

    .x %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath="//table[@class='secaoFormBody']") %>%
      rvest::html_table(fill=T) %>%
      purrr::pluck(2) %>%
      dplyr::filter(stringr::str_detect(X1,":")) %>%
      tidyr::unite(coluna,c("X1","X2"),sep="") %>%
      dplyr::select(coluna) %>%
      tidyr::separate(coluna,c("variavel","valor"),sep=":") %>%
      dplyr::mutate_all(dplyr::funs(stringr::str_trim(.) %>%
                               stringr::str_squish())) %>%
      dplyr::mutate(processo=rlang::UQ(rlang::quo(.y)),
                    valor=stringr::str_remove(valor,"NA$")) %>%
      dplyr::mutate_if(is.character,as.factor) %>%
      dplyr::select(processo,variavel,valor)

  },NULL),.progress = TRUE)

}
