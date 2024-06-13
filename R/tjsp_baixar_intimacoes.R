#' Coleta intimações do TJSP
#'
#' @param cd_foro Código do Foro
#' @param cd_instancia Código da instância. Padrão: 1
#' @param cd_vara Código da vara.
#' @param dt_inicio Data do início no formato "dd/mm/aaaa".
#'      Se não informada, usa a data de hoje.
#' @param dt_fim Data do fim no formato "dd/mm/aaaa".
#'      Se não informada, uda a data de hoje.
#' @param situacao Opções: "cumprida", "pendente" ou "ambas". Padrão para "ambas".
#' @param natureza_comunicacao Opções: "intimação", "citação" ou "ambas". Padrão para "ambas".
#' @param versao Pode ser csv ou xml. Padrão para csv.
#' @param intervalo Padrão para trimestral, ou seja, máximo.
#' @param diretorio Padrão ".", ou seja, o atual.
#'
#' @return xml
#' @export
#'
tjsp_baixar_intimacoes <- function(cd_foro = "",
                                   cd_instancia = 1,
                                   cd_vara = "",
                                   dt_inicio = "",
                                   dt_fim = "",
                                   situacao = "ambas",
                                   natureza_comunicacao = "ambas",
                                   versao = c("xml","csv"),
                                   intervalo = "trimestral",
                                   diretorio = "."){


  cd_foro <- as.integer(cd_foro)

  ato_fl_cumprido <- switch(situacao,
                            ambas = "",
                            cumprida = "S",
                            pendente = "N"
  )

  if(dt_inicio == ""){

    dt_inicio <- (Sys.Date() - 1) |> format("%d/%m/%Y")
  }

  if(dt_fim == ""){

    dt_fim <- (Sys.Date() -1) |> format("%d/%m/%Y")
  }

  cd_tipo_ato <- switch(natureza_comunicacao,
                        ambas = "-1",
                        intimacao = "0",
                        citacao = "4"
  )

  versao <- versao[[1]]

  url1 <- switch(versao,

                 csv =  "https://esaj.tjsp.jus.br/intimacoesweb/exportarAtosRecebidosParaCsv.do",

                 xml= "https://esaj.tjsp.jus.br/intimacoesweb/consultarAtosRecebidos.do"

  )

  e <- switch(versao,
              csv = ".csv",
              xml = ".xml"
  )

  cd_usuario <- Sys.getenv("ESAJ_CD_USUARIO")


  datas <- agrupar_datas(dt_inicio, dt_fim, corte = intervalo)


  purrr::walk2(datas$data_inicial, datas$data_final, ~{

 di <- .x
 df <- .y

    corpo <-
      list(
        conversationId = "",
        entity.cdUsuario = cd_usuario,
        dadosConsulta.cdPerfil = "",
        cdInstancia = "1",
        entity.nmForo = "",
        entity.cdForo = cd_foro,
        contadorforoService = "0",
        entity.cdVara = cd_vara,
        entity.ato.especialidade.cdEspecialidade = "",
        entity.ato.especialidade.nmEspecialidade = "",
        entity.ato.cargo.cdCargo = "",
        entity.ato.cargo.deCargo = "",
        dadosConsulta.classesTreeSelection.values = "",
        dadosConsulta.classesTreeSelection.text = "",
        dadosConsulta.assuntosTreeSelection.values = "",
        dadosConsulta.assuntosTreeSelection.text = "",
        entity.nmForoOrigem = "",
        entity.cdForoOrigem = "",
        entity.nmVaraOrigem = "",
        entity.cdVaraOrigem = "",
        dadosConsulta.flArea = "",
        dadosConsulta.dtInicioPeriodo = di,
        dadosConsulta.dtFimPeriodo = df,
        entity.nuProcessoFormat = "",
        entity.cdProcesso = "",
        dadosConsulta.formaCienciaIntimacao = "",
        entity.cdTipoAto = cd_tipo_ato,
        entity.ato.flCumprido = ato_fl_cumprido
      )

    r1 <-  httr::POST(url1, body = corpo, encode = "form") |>
          httr::content()


   dividir <- `/`

   paginas <- r1 |>
              xml2::xml_find_first("//*[@id='textQtLinhasRow']/following-sibling::text()") |>
              xml2::xml_text() |>
              stringr::str_remove_all("\\D+") |>
              as.integer() |>
              dividir(20) |>
              ceiling()

   i <- lubridate::dmy(di) |>
     stringr::str_replace_all("\\D","_")

   f <- lubridate::dmy(df) |>
     stringr::str_replace_all("\\D","_")

   purrr::walk(0:paginas, ~{

     p  <- .x

   arquivo <- file.path(diretorio,paste0('pagina_',p,"_foro_", cd_foro,"_inicio_",i,"_fim_",f,e))

   q <- structure(
     list(
       scheme = "https",
       hostname = "esaj.tjsp.jus.br",
       port = NULL,
       path = "intimacoesweb/AjaxServlet.ajax",
       query = list(
         component = "gridPaginada",
         filterPaginationID = "/intimacoesweb_gridAtosUsuario",
         beanClass = "br.com.softplan.intimacoes.comum.AtoUsuario",
         objectPaginationHandler = "br.com.softplan.saj.intimacoesweb.grid.ConsultaAtosRecebidosPaginationHandler",
         ejbClass = "",
         ejbMethod = "",
         pageSize = "20",
         page = .x,
         currentPage = "0",
         paramSize = "0",
         orderBy = "null",
         orderByDirection = "null",
         column0Type = "null",
         column0 = "cdAto",
         column1Type = "null",
         column1 = "cdUsuario",
         column2Type = "null",
         column2 = "ato.cdProcesso",
         column3Type = "null",
         column3 = "ato.cdForo",
         column4Type = "null",
         column4 = "ato.especialidade.cdEspecialidade",
         column5Type = "null",
         column5 = "ato.cargo.cdCargo",
         column6Type = "null",
         column6 = "ato.flCumprido",
         column7Type = "null",
         column7 = "ato.cdTipoAto",
         column8Type = "null",
         column8 = "intimacaoAutomatica",
         column9Type = "null",
         column9 = "dtLeitura",
         column10Type = "null",
         column10 = "ato.dtInclusao",
         column11Type = "null",
         column11 = "dtIntimacao",
         column12Type = "null",
         column12 = "nuDiasPrazo",
         column13Type = "null",
         column13 = "ato.nuProcessoFormat",
         column14Type = "null",
         column14 = "classeEAssuntoPrincipaisFormatado",
         column15Type = "null",
         column15 = "usuarioIntimacao.nmUsuario",
         column16Type = "null",
         column16 = "ato.deTipoMvProcesso",
         column17Type = "null",
         column17 = "ato.especialidade.nmEspecialidade",
         column18Type = "null",
         column18 = "ato.cargo.deCargo",
         column19Type = "null",
         column19 = "ato.tarjasJson",
         columnsSize = "20",
         objSerializado = "null"
       ),
       params = NULL,
       fragment = NULL,
       username = NULL,
       password = NULL
     ),
     class = "url"
   )


   q$query$page <- p

   parseada <- httr::build_url(q)

   httr::GET(parseada, httr::write_disk(arquivo, overwrite = T))

})

  })
}
