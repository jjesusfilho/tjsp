#' Ler tabelas baixadas com tjsp_baixar_tabela_docs
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar diretório
#'
#' @return tibble
#' @export
#'
tjsp_ler_tabela_docs <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)) {

    arquivos <- list.files(diretorio,full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    suppressMessages({
 df <-   .x %>%
      xml2::read_html() %>%
      xml2::xml_text() %>%
      stringr::str_extract("(?<=requestScope = )\\X+?(?=;)") %>%
      jsonlite::fromJSON() %>%
      tidyr::unnest_longer(children,names_repair="unique") %>%
      purrr::flatten_dfc() %>%
      dplyr::select(dplyr::matches("(title|parametros)")) %>%
      setNames(c("doc_name","paginas","url_doc")) %>%
      tibble::rownames_to_column("doc_id") %>%
      dplyr::mutate(pagina_inicial = stringr::str_extract(paginas,"\\d+"),
                    pagina_final = stringr::str_extract(paginas,"\\d+$"),
                    processo = processo,
                    url_doc = paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?",url_doc)) %>%
      dplyr::select(processo, doc_id,doc_name,paginas,pagina_inicial, pagina_final, url_doc)


})

    df

  },NULL))



}
