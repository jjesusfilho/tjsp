#' Baixa consulta jurisprudencial do TJSP
#'
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param ementa palavra ou texto a ser buscado apenas nas ementas
#' @param aspas lógico. Colocar a expressão entre aspas?
#' @param classe Código da classe processual
#' @param assunto Código do assunto
#' @param orgao_julgador Código do órgão julgador
#' @param inicio  data inicial julgamento
#' @param fim  Data final julgamento
#' @param inicio_pb data inicial registro/publicação
#' @param fim_pb    data final registr/publicacao
#' @param sg  "T" para origem segundo grau
#' @param cr  "R" para origem colégio recursal
#' @param tipo "A" Para acórdãos, "D" para decisões monocráticas
#' @param n Número de páginas
#' @param diretorio Diretório onde serão armazenadas as páginas html
#' @keywords tjsp,acórdãos
#'
#' @return baixa os htmls das decisões de segunda instância
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_cjsg(livre = "Lei Maria da Penha")
#' }
#'
tjsp_baixar_cjsg <-
  function(livre = "",
           ementa = "",
           processo = "",
           aspas = FALSE,
           classe = "",
           assunto = "",
           orgao_julgador = "",
           inicio = "",
           fim = "",
           inicio_pb = "",
           fim_pb = "",
           sg = "T",
           cr = "",
           tipo = "A",
           n = NULL,
           diretorio = ".") {

  data_errada <-   verificar_datas(inicio, fim, inicio_pb, fim_pb)

  if (data_errada){

    stop("Verifique se:
         Colocou as datas no formato esperado;
         informou somente publica\u00E7\u00E3o ou julgamento;
         colocou uma data inferior ou igual \u00E0 data de hoje.")

  }

    if(inicio != "" && fim != ""){

      datas <- agrupar_datas(inicio, fim)


      purrr::walk2(datas$data_inicial, datas$data_final, purrr::possibly(~{

        tjsp_baixar_cjsg1(livre = livre,
                          ementa = ementa,
                          processo = processo,
                          aspas = aspas,
                          classe = classe,
                          assunto = assunto,
                          orgao_julgador = orgao_julgador,
                          inicio = .x,
                          fim = .y,
                          inicio_pb = inicio_pb,
                          fim_pb = fim_pb,
                          sg = sg,
                          cr = cr,
                          tipo = tipo,
                          n = n,
                          diretorio = diretorio
        )


      },NULL))

    } else if(inicio_pb != "" && fim_pb != ""){

      datas <- agrupar_datas(inicio_pb, fim_pb)

      purrr::walk2(datas$data_inicial, datas$data_final, ~{

        tjsp_baixar_cjsg1(livre,
                          ementa,
                          processo,
                          aspas,
                          classe,
                          assunto,
                          orgao_julgador,
                          inicio = inicio,
                          fim = fim,
                          inicio_pb = .x,
                          fim_pb = .x,
                          sg = sg,
                          cr = cr,
                          tipo,
                          n,
                          diretorio
        )


      })

    } else {

      tjsp_baixar_cjsg1(livre = livre,
                        ementa = ementa,
                        processo = processo,
                        aspas  = aspas,
                        classe,
                        assunto,
                        orgao_julgador,
                        inicio = inicio,
                        fim = fim,
                        inicio_pb = inicio_pb,
                        fim_pb = fim_pb,
                        sg = sg,
                        cr = cr,
                        tipo,
                        n,
                        diretorio)

    }

  }



#' Função para criar o nome do arquivo
#'
#' @param inicio  data inicial julgamento
#' @param fim  Data final julgamento
#' @param inicio_pb data inicial registro/publicação
#' @param fim_pb    data final registr/publicacao
#' @param pagina página
#' @param diretorio diretorio
#'
#' @return Arquivo
#'
formatar_arquivo <- function(inicio,
                            fim,
                            inicio_pb,
                            fim_pb,
                            pagina,
                            diretorio){


  hora <- stringr::str_replace_all(Sys.time(), "\\D", "_")


  if (inicio != "" & fim != ""){

    i <- lubridate::dmy(inicio) %>%
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(fim) %>%
      stringr::str_replace_all("\\D","_")

    arquivo <- file.path(diretorio,paste0(hora,"_inicio_",i,"_fim_",f,"_pagina_",pagina,".html"))



  } else  if (inicio_pb != "" & fim_pb != "") {

    i <- lubridate::dmy(inicio_pb) %>%
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(fim_pb) %>%
      stringr::str_replace_all("\\D","_")


    arquivo <- file.path(diretorio,paste0(hora,"_inicio_pb_",i,"_fim_pb_",f,"_pagina_",pagina,".html"))

  } else {

    arquivo <- file.path(diretorio,paste0(hora,"_pagina_",pagina,".html"))

  }

  return(arquivo)
}



#' Subdivide as data em intervalos de um ano
#'
#' @param data_inicial Data inicial dd/mm/aaaa
#' @param data_final Data final dd/mm/aaaa
#' @param formato Formato de retorno. Default para dd/mm/aaaa
#'
#' @return tibble com duas colunas com data inicial e final
#'
agrupar_datas <- function(data_inicial = NULL,
                          data_final = NULL,
                          formato = "%d/%m/%Y"){

  tibble::tibble(.datas = seq(lubridate::dmy(data_inicial),
                           lubridate::dmy(data_final),1)) |>
    dplyr::mutate(.ano = lubridate::year(.datas)) |>
    dplyr::group_split(.ano) |>
    purrr::map_dfr(~dplyr::pull(.x,".datas") |>
                     range() |>
                     setNames(c("data_inicial","data_final"))) |>
    dplyr::mutate_all(list(~as.Date(.,origin='1970-01-01') |>
                             format(formato)))

}


#' Verifica se está tudo ok com as datas.
#'
#' @param inicio Data inicial de julgamento
#' @param fim Data final de julgamento
#' @param inicio_pb Data inicial de publicação
#' @param fim_pb Data final de publicação
#'
#' @return Retorna TRUE se tem algo errado.
#' @export
#'
verificar_datas <- function(inicio, fim, inicio_pb, fim_pb){

  ### Verifica se nenhuma data é seperior à data atual

  datas <- lubridate::dmy(inicio, fim, inicio_pb, fim_pb)

  x <- any(datas > Sys.Date(), na.rm = TRUE)

  ### Verifica se o usuário colocou data de publicação e data de julgamento.

  y <-  all(inicio != "", fim != "", inicio_pb != "", fim_pb != "")


  ### verifica se o usuário escreveu a data em formato errado.

  z <- any(c(inicio, fim, inicio_pb, fim_pb) |> stringr::str_detect("(\\d{2}/\\d{2}/\\d{4}|^$)", negate =T ))

  any(x,y,z)

  }


#' Qualquer data
#'
#' @inheritParams tjsp_baixar_cjsg
#'
#' @return htmls
#'

tjsp_baixar_cjsg1 <- function (livre = "", ementa = "", processo = "", classe = "",
          assunto = "", orgao_julgador = "", inicio = "", fim = "", 
          inicio_pb = "", fim_pb = "", sg = "T", cr = "", tipo = "A", 
          n = NULL, diretorio = ".", aspas = FALSE) {
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE, accept_encoding = "latin1"))
  if (aspas == TRUE) livre <- deparse(livre)
  
  link_cjsg <- "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do"
  
  body <- list(
    dados.buscaInteiroTeor = livre, 
    dados.pesquisarComSinonimos = "S",
    dados.pesquisarComSinonimos = "S", 
    dados.buscaEmenta = ementa,
    dados.nuProcOrigem = processo, 
    dados.nuRegistro = "", 
    agenteSelectedEntitiesList = "",
    contadoragente = "0", 
    contadorMaioragente = "0", 
    codigoCr = "",
    codigoTr = "", 
    nmAgente = "", 
    juizProlatorSelectedEntitiesList = "",
    contadorjuizProlator = "0", 
    contadorMaiorjuizProlator = "0",
    codigoJuizCr = "", 
    codigoJuizTr = "", 
    nmJuiz = "", 
    classesTreeSelection.values = classe,
    classesTreeSelection.text = "", 
    assuntosTreeSelection.values = assunto,
    assuntosTreeSelection.text = "", 
    comarcaSelectedEntitiesList = "",
    contadorcomarca = "0", 
    contadorMaiorcomarca = "0", 
    cdComarca = "",
    nmComarca = "", 
    secoesTreeSelection.values = orgao_julgador,
    secoesTreeSelection.text = "", 
    dados.dtJulgamentoInicio = inicio,
    dados.dtJulgamentoFim = fim, 
    dados.dtRegistroInicio = inicio_pb,
    dados.dtRegistroFim = fim_pb,
    dados.origensSelecionadas = sg,
    dados.origensSelecionadas = cr, 
    tipoDecisaoSelecionados = tipo,
    dados.ordenacao = "dtPublicacao"
  )
  
  response <- httr::POST(link_cjsg, encode = "form", body = body, 
                         httr::accept("text/html; charset=latin1;"))
  if (!is.null(n)) {
    paginas <- 1:n
    pb <- progress::progress_bar$new(total = n)
  } else {
    max_pag <- response |>
      httr::content() |> 
      xml2::xml_find_all(xpath = "//*[@id='totalResultadoAba-A']|//*[@id='totalResultadoAba-D']") |> 
      xml2::xml_attrs() |> 
      purrr::pluck(1, 3) |> 
      as.numeric() |> 
      magrittr::divide_by(20) |> 
      ceiling()
    
    paginas <- 1:max_pag
    
    pb <- progress::progress_bar$new(total = max_pag)
  }
  if (tipo == "A") {
    purrr::walk(paginas, purrr::possibly(~{
      pb$tick()
        arquivo <- tjsp:::formatar_arquivo(inicio, fim, inicio_pb, 
                                  fim_pb, pagina = .x, diretorio)
      Sys.sleep(1)
      httr::GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=", .x), 
                httr::set_cookies(unlist(response$cookies)), httr::accept("text/html; charset=latin1;"), 
                httr::write_disk(arquivo, overwrite = TRUE))
    }, NULL))
  } else {
    purrr::walk(paginas, purrr::possibly(~{
      pb$tick()
      arquivo <- formatar_arquivo(inicio, fim, inicio_pb, 
                                  fim_pb, pagina = .x, diretorio)
      Sys.sleep(1)
      httr::GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=D&pagina=", 
                       .x), httr::set_cookies(unlist(response$cookies)), httr::write_disk(arquivo, 
                                                                                   overwrite = TRUE))
    }, NULL))
  }
}
