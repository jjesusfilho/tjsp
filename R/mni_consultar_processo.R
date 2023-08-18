#' Consulta dados processuais pelo Modelo de interoperabilidade (MNI)
#'
#' @param tribunal Padrão "tjsp"
#' @param usuario Informar usuario ou variável de ambiente
#'     TJSPMNIUSUARIO
#' @param senha Informar senha ou variável de ambiente
#'     TJSPMNISENHA
#' @param processo Processo (número único CNJ)
#' @param cabecalho TRUE para pegar cabeçalho
#' @param movimentos TRUE para pegar movimentacao
#' @param documentos TRUE para pegar documentos.
#' @param diretorio Diretório
#'
#' @return xmls
#' @export
#'
mni_consultar_processo <- function(tribunal = "tjsp",
                                   usuario = NULL,
                                   senha = NULL,
                                   processo = "",
                                   cabecalho = TRUE,
                                   movimentos = FALSE,
                                   documentos = FALSE,
                                   diretorio = "."){

  usuario_key <- toupper(paste0(tribunal,"MNIUSUARIO"))

  senha_key <- toupper(paste0(tribunal,"MNISENHA"))

  if (is.null(usuario) || is.null(senha)) {
    usuario <- Sys.getenv(usuario_key)
    senha <- Sys.getenv(senha_key)
    if (usuario == "" || senha == "") {
      usuario <- as.character(getPass::getPass(msg = "Entre com usu\u00E1rio: "))
      senha <- as.character(getPass::getPass(msg = "Entre com senha: "))
    }
  }

x <- c(movimentos = movimentos, cabecalho = cabecalho, documentos = documentos) |>
     ifelse("true","false")

processo <- stringr::str_remove_all(processo,"\\D")

url <-"http://esaj.tjsp.jus.br/mniws/servico-intercomunicacao-2.2.2/intercomunicacao?wsdl"

pb <- progress::progress_bar$new(total = length(processo))

purrr::walk(processo, purrr::possibly(~{

  pb$tick()

## A op\u00E7\u00E3o por documentos n\u00E3o convive com cabe\u00E7alho e movimentos.

corpo <- criar_corpo(
                     cabecalho = x[["cabecalho"]],
                     movimentos = x[["movimentos"]],
                     documentos = x[["documentos"]],
                     processo = .x,
                     usuario = usuario,
                     senha = senha
                     )
if (documentos) {
arquivo <- file.path(diretorio, paste0(tribunal,"_mni_documentos_", .x,".xml"))

} else {

  arquivo <- file.path(diretorio, paste0(tribunal,"_mni_", .x,".xml"))

}

httr::POST(url, body = corpo, httr::write_disk(arquivo, overwrite = T))

}, NULL))

}


criar_corpo <- function(cabecalho, movimentos, documentos, processo, usuario , senha){

if (documentos){

corpo <- glue::glue('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://www.cnj.jus.br/servico-intercomunicacao-2.2.2/" xmlns:tip="http://www.cnj.jus.br/tipos-servico-intercomunicacao-2.2.2">
<soapenv:Header/>
<soapenv:Body>
<ser:consultarProcesso>
<tip:idConsultante>{usuario}</tip:idConsultante>
<tip:senhaConsultante>{senha}</tip:senhaConsultante>
<tip:numeroProcesso>{processo}</tip:numeroProcesso>
<tip:incluirDocumentos>{documentos}</tip:incluirDocumentos>
</ser:consultarProcesso>
</soapenv:Body>
</soapenv:Envelope>')

} else {

corpo <- glue::glue('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ser="http://www.cnj.jus.br/servico-intercomunicacao-2.2.2/" xmlns:tip="http://www.cnj.jus.br/tipos-servico-intercomunicacao-2.2.2">
<soapenv:Header/>
<soapenv:Body>
<ser:consultarProcesso>
<tip:idConsultante>{usuario}</tip:idConsultante>
<tip:senhaConsultante>{senha}</tip:senhaConsultante>
<tip:numeroProcesso>{processo}</tip:numeroProcesso>
<tip:incluirCabecalho>{cabecalho}</tip:incluirCabecalho>
<tip:movimentos>{movimentos}</tip:movimentos>
</ser:consultarProcesso>
</soapenv:Body>
</soapenv:Envelope>')
}
  return(corpo)

}
