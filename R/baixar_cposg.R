#' Baixa htmls dos processos de segunda instância do TJSP
#'
#' @param processos vetor com os números dos processos, com ou sem os separadores.
#' @param diretorio Diretório onde devem ser armazenados os htmls
#'
#' @return Arquivos htmls no formato aaaa_mm_dd_OOOOOOODDAAAAJTRNNNN.html indicando a data
#'      do download seguida pelo número do processo.
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_cposg("feminicídio")
#' }
baixar_cposg<-function(processos=NULL, diretorio = ".") {

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos<-stringr::str_remove_all(processos,"\\D+") %>%
    stringr::str_pad(width=20,"left","0") %>%
    abjutils::build_id()


  ##  URL básica
  uri <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  ### Inicía mapeamento dos processos
  purrr::map(processos,purrr::possibly(~{


    ## Monta a query para a busca do processo
    unificado <- .x %>%
      stringr::str_extract(".{15}")
    foro <- .x %>%
      stringr::str_extract("\\d{4}$")

    query <- list(
      cbPesquisa = "NUMPROC",
      conversationId = "",
      dePesquisat= "",
      dePesquisaNuUnificado= .x,
      foroNumeroUnificado= foro,
      localPesquisa.cdLocal= "-1",
      numeroDigitoAnoUnificado= unificado,
      paginaConsulta= "1",
      tipoNuProcesso = "UNIFICADO",
      uuidCaptcha= ""
    )

  arquivo<-  lubridate::today() %>%
      stringr::str_replace_all("-","_") %>%
      stringr::str_c("_") %>%
  stringr::str_c(.x) %>%
    stringr::str_remove_all("[\\.-]") %>%
    stringr::str_c(".html")

    ## Baixa o html do processo.
  resposta<-  httr::RETRY("GET",
                             url=uri,
                             query = query,
                             quiet=TRUE,
                             httr::timeout(2))




  ## Parsea a resposta
  conteudo <- httr::content(resposta)

  ## Verifica se, em vez do andamento do processo, aparece uma listagem de processos.
  if (
    xml2::xml_find_first(conteudo,"boolean(//div[@id='listagemDeProcessos'])")
  ) {

      conteudo %>%
      xml2::xml_find_all("//a[@class='linkProcesso']") %>%
      xml2::xml_attr("href") %>%
      xml2::url_absolute("https://esaj.tjsp.jus.br") %>%
      purrr::map(~httr::RETRY("GET",.x,
                              httr::timeout(2),
                              httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))


      ) } else {

        httr::RETRY("GET",
                    url=uri,
                    query = query,
                    quiet=TRUE,
                    httr::timeout(2),
                    httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))



      }

  },NULL))
}
