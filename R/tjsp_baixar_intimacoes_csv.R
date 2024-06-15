#' Coleta intimações do TJSP
#'
#' @param cd_foro Código do Foro
#' @param cd_instancia Código da instância. Padrão: 1
#' @param cd_vara Código da vara.
#' @param dt_inicio Data do início no formato "dd/mm/aaaa"
#' @param dt_fim Data do fim no formato "dd/mm/aaaa"
#' @param situacao Opções: "cumprida", "pendente" ou "ambas". Padrão para "ambas".
#' @param natureza_comunicacao Opções: "intimação", "citação" ou "ambas". Padrão para "ambas".
#' @param intervalo Padrão para semestral. Pode ser mensal também.
#' @param diretorio Padrão ".", ou seja, o atual.
#'
#' @return csv
#' @export
#'
tjsp_baixar_intimacoes_csv <- function(cd_foro = "",
                                   cd_instancia = 1,
                                   cd_vara = "",
                                   dt_inicio = "",
                                   dt_fim = "",
                                   situacao = "ambas",
                                   natureza_comunicacao = "ambas",
                                   intervalo = "trimestral",
                                   diretorio = "."){


  cd_foro <- as.integer(cd_foro)

  ato_fl_cumprido <- switch(situacao,
                     ambas = "",
                     cumprida = "S",
                     pendente = "N"
                     )



  cd_tipo_ato <- switch(natureza_comunicacao,
                            ambas = "-1",
                            intimacao = "0",
                            citacao = "4"
  )


  cd_usuario <- Sys.getenv("ESAJ_CD_USUARIO")


  datas <- agrupar_datas(dt_inicio, dt_fim, corte = intervalo)

  url1 <- "https://esaj.tjsp.jus.br/intimacoesweb/exportarAtosRecebidosParaCsv.do"

  purrr::walk2(datas$data_inicial, datas$data_final, ~{


    i <- lubridate::dmy(.x) |>
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(.y) |>
      stringr::str_replace_all("\\D","_")

    arquivo <- file.path(diretorio,paste0("foro_", cd_foro,"_inicio_",i,"_fim_",f,".csv"))

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
      dadosConsulta.dtFimPeriodo = .y,
      entity.nuProcessoFormat = "",
      entity.cdProcesso = "",
      dadosConsulta.formaCienciaIntimacao = "",
      entity.cdTipoAto = cd_tipo_ato,
      entity.ato.flCumprido = ato_fl_cumprido
    )

  httr::POST(url1, body = body, encode = "form", httr::write_disk(arquivo, overwrite = T))
  })
}
