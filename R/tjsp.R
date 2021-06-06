#' \code{tjsp} package
#'
#' Baixa  e organiza decisões do TJSP
#'
#'
#' @docType package
#' @name tjsp
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", ".x", ".y", ":=", "processo", "disponibilizacao", "valor", "variavel", "row_id",
    "size", "alternativa", "vara", "vara2", "codigo_processo",
    "digital", "v1", "assunto", "classe", "distribuicao", "juiz",
    "filter", "branco", "v2", "area", "vara", "prioritaria", "assuntos", "assunto.x",
    "", "foro", "cod_subarea", "subarea", "data_distribuicao",
    "data_recebimento", "processo_2", "processo_principal",
    "valor_da_acao", "incidente", "execucao_de_sentenca", "recebido_em",
    "classe_numero", "classe_situacao","number","n","comarca","imoveis", "lat","lng",
    "id0","dispositivo","id5","name0","name5", "pagina", "hora_coleta"
  ))
}
