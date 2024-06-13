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
#' @param versao Pode ser csv ou html. Padrão para csv.
#' @param intervalo Padrão para trimestral, ou seja, máximo.
#' @param diretorio Padrão ".", ou seja, o atual.
#'
#' @return csv
#' @export
#'
tjsp_baixar_intimacoes <- function(cd_foro = "",
                                   cd_instancia = 1,
                                   cd_vara = "",
                                   dt_inicio = "",
                                   dt_fim = "",
                                   situacao = "ambas",
                                   natureza_comunicacao = "ambas",
                                   versao = c("csv","html"),
                                   intervalo = "trimestral",
                                   diretorio = "."){


  cd_foro <- as.integer(cd_foro)

  ato_fl_cumprido <- switch(situacao,
                            ambas = "",
                            cumprida = "S",
                            pendente = "N"
  )

  if(dt_inicio == ""){

    dt_inicio <- Sys.Date() |> format("%d/%m/%Y")
  }

  if(dt_fim == ""){

    dt_fim <- Sys.Date() |> format("%d/%m/%Y")
  }

  cd_tipo_ato <- switch(natureza_comunicacao,
                        ambas = "-1",
                        intimacao = "0",
                        citacao = "4"
  )

  versao <- versao[[1]]

  url1 <- switch(versao,

                 csv =  "https://esaj.tjsp.jus.br/intimacoesweb/exportarAtosRecebidosParaCsv.do",

                 html = "https://esaj.tjsp.jus.br/intimacoesweb/consultarAtosRecebidos.do"

  )

  e <- switch(versao,
              csv = ".csv",
              html = ".html"
  )

  cd_usuario <- Sys.getenv("ESAJ_CD_USUARIO")


  datas <- agrupar_datas(dt_inicio, dt_fim, corte = intervalo)


  purrr::walk2(datas$data_inicial, datas$data_final, ~{


    i <- lubridate::dmy(.x) |>
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(.y) |>
      stringr::str_replace_all("\\D","_")

    arquivo <- file.path(diretorio,paste0("foro_", cd_foro,"_inicio_",i,"_fim_",f,e))

    body <-
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
        dadosConsulta.dtInicioPeriodo = .x,
        dadosConsulta.dtFimPeriodo = .x,
        entity.nuProcessoFormat = "",
        entity.cdProcesso = "",
        dadosConsulta.formaCienciaIntimacao = "",
        entity.cdTipoAto = cd_tipo_ato,
        entity.ato.flCumprido = ato_fl_cumprido
      )

    httr::POST(url1, body = body, encode = "form", httr::write_disk(arquivo, overwrite = T))
  })
}
