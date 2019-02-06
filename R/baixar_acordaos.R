#' Baixa acórdãos do Tribunal de Justiç de São Paulo a partir do andamento
#'
#' @param processos vetor com o número do processo com ou sem os separadores
#' @param diretorio Diretório. Default para o corrente.
#'
#' @return tabela com metadados do pdf e pdf do acórdão baixado o o número como nome.
#' @export
#'

baixar_acordaos<-function(processos=NULL, diretorio = "."){
  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos<-stringr::str_remove_all(processos,"\\D+") %>%
    stringr::str_pad(width=20,"left","0") %>%
    abjutils::build_id()


##  URL básica
uri1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"

### Inicía mapeamento dos processos
 purrr::map_dfr(processos,purrr::possibly(~{

  ## Atribui p ao processo para não gerar confusão na hora usar os vários .x
     p<-.x

  ## Monta a query para a busca do processo
      unificado <- p %>%
        stringr::str_extract(".{15}")
      foro <- p %>%
        stringr::str_extract("\\d{4}$")

      query1 <- list(
        cbPesquisa = "NUMPROC",
        conversationId = "",
        dePesquisat= "",
        dePesquisaNuUnificado= p,
        foroNumeroUnificado= foro,
        localPesquisa.cdLocal= "-1",
        numeroDigitoAnoUnificado= unificado,
        paginaConsulta= "1",
        tipoNuProcesso = "UNIFICADO",
        uuidCaptcha= ""
      )

      ## Faz a requisição do processo.
      resposta1 <- httr::RETRY("GET",
                               url=uri1,
                               query = query1,
                               quiet=TRUE,
                               httr::timeout(2))
 ## Parsea a resposta
      conteudo1 <- httr::content(resposta1)

 ## Verifica se, em vez do andamento do processo, aparece uma listagem de processos.
        if (
        xml2::xml_find_first(conteudo1,"boolean(//div[@id='listagemDeProcessos'])")
      ) {

      conteudo1 <-xml2::xml_find_all(conteudo1,"//a[@class='linkProcesso']") %>%
          xml2::xml_attr("href") %>%
          xml2::url_absolute("https://esaj.tjsp.jus.br") %>%
          purrr::map(~httr::RETRY("GET",.x,httr::timeout(2)) %>%
                     httr::content())

      }


  ## Extraí todos os processos em que há uma decisão. A decisão pode estar com o título Acórdão finalizado ou com Julgado virtualmente
  ## Além disso, a sequência abaixo pega o número do documento, a data de julgamento e o texto da decisão
  ## Como pode haver múltiplas decisões, é criado um vetor com os números dos documentos. Esses números
  ## serão posteriormente usados para extrair a url de cada pdf.

  conteudo1 %>%
    purrr::map_dfr(purrr::possibly(~{
 cc<-.x
    doc <- cc %>%
        xml2::xml_find_all("//tr/td/a[contains(.,'Acordão Finalizado')]|//tr/td/a[contains(.,'Julgado virtualmente')]")

    doc_num<-doc %>%
        xml2::xml_attr("href") %>%
        stringr::str_extract("\\d+")

    doc_texto<- doc %>%
      xml2::xml_text(trim=TRUE)

    decisao<-doc %>%
        xml2::xml_find_all("following-sibling::span") %>%
        xml2::xml_text()

   data_decisao<-doc %>%
     xml2::xml_find_all("../preceding-sibling::td[2]") %>%
     xml2::xml_text(trim=TRUE)

  cdProcesso <- cc %>%
        xml2::xml_find_all("//input[@name='cdProcesso']") %>%
        xml2::xml_attr("value")

  tempo <- lubridate::now() %>%
        as.numeric() %>%
        magrittr::multiply_by(1000) %>%
        floor() %>%
        as.character()

  ## A partir dos dados acima, especialmente o código do processo e o número do documento, cria as urls intermediárias que serão usadas
  ## para pegar as urls de cada um dos documentos.

    uri2<-purrr::map_chr(doc_num,~{

    uri_parseada<-httr::parse_url("https://esaj.tjsp.jus.br/cposg/verificarAcessoMovimentacao.do?")

      uri_parseada$query<- list(
        cdDocumento = .x,
        origemRecurso = "M",
        cdProcesso = cdProcesso,
        conversationId = "",
        `_` = tempo
      )
      httr::build_url(uri_parseada)
      })


     ## A sequência abaixo cria a uri do pdf. São realizadas duas requisições em ato contínuo.
       ## A segunda requisição precisa ocorrer imediatamente  após ser gerada pela primeira.
       ## Se demorar muito, a url é perdida.

      uri3 <- purrr::map_chr(uri2,~{

      httr::RETRY("GET",
                          .x,
                          httr::timeout(2),
                          quiet=TRUE) %>%
        httr::content("text") %>%
        unlist() %>%
        httr::RETRY("GET",
                    .,
                    httr::timeout(2),
                    quiet=TRUE) %>%
        httr::content("text") %>%
        stringr::str_extract("nuSeq.+?(?=.,)") %>%
        paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?", .)
      })

  ## Não sei se isso é necessário, fiz porque parece demorar um pouco entre uma
      ## requisição e outra.
  Sys.sleep(1)

## Agora que temos as urls de cada pdf, procedemos ao download. O nome do pdf será
  ## a data + o número do processo. Fiz isso porque se houver múltiplas decisões,
  ## cada uma receberá uma data distinta no nome.

   purrr::map2(uri3,data_decisao,~{
          httr::GET(.x, httr::write_disk(
        paste0(
          diretorio,
          "/",stringr::str_replace_all(.y,"/","_"), ## Coloca subscrito no lugar de barra
          "_",stringr::str_remove_all(p, "\\D+"), ## Remove o que não é número dos números dos processo.
          ".pdf"),overwrite = TRUE)
          )
        })

## por fim, cria um tibble com os metadados de cada um dos documentos(pdfs)

  tibble::tibble(processo=p,data_jugalmento=data_decisao,doc_texto=doc_texto,decisao=decisao,doc_num=doc_num,url=uri3)

  },otherwise = NULL))

 },otherwise=NULL))

 }

