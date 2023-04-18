#' Baixa informações sobre as partes
#'
#' @param cd_processo Vetor com os códigos dos processos
#' @param instancia Uma dessas c("PG","SG","CR"), primeiro grau, segundo grau, colégio
#' @param diretorio Diretório onde armazenar os jsons.
#'
#' @details Para usar esta função, você tem de estar autenticado com a função `tjsp_autenticar` e
#'       criar variáveis de ambiente para cd_usuario,
#'      usuario_solicitante, documento_usuario e cd_perfil.
#'      Você pode usar `usethis::edit_r_environ` para criá-las permanentemente.
#'      Estas são as variável que você deve criar: ESAJ_CD_USUARIO,
#'      ESAJ_USUARIO_SOLICITANTE, ESAJ_DOCUMENTO_USUARIO, ESAJ_CD_PERFIL.
#'      Alternativamente,  usa a função `tjsp_partes_credenciais` para criá-las somente para
#'      a presente sessão do R.
#'
#' @return Arquivo json
#' @export

tjsp_baixar_partes_docs <- function(cd_processo,
                                    instancia = "PG",
                                    diretorio = "."
){


  cd_usuario <- Sys.getenv("ESAJ_CD_USUARIO")

  usuario_solicitante <- Sys.getenv("ESAJ_USUARIO_SOLICITANTE")

  documento_usuario <- Sys.getenv("ESAJ_DOCUMENTO_USUARIO")

  cd_perfil <- Sys.getenv("ESAJ_CD_PERFIL")

  documento_usuario <- documento_usuario |>
    stringr::str_remove_all("\\D+")

  if (!nchar(documento_usuario) %in% c(11,14)){

    stop("N\u00FAmero de digitos inv\u00E1lido para o documento do usu\u00E1rio")

  } else if(nchar(documento_usuario) == 11){

    document_usuario <- stringr::str_replace(documento_usuario, "(\\d{3})(\\d{3})(\\d{3})(\\d{2})","\\1.\\2.\\3-\\4")

  } else {

    document_usuario <- stringr::str_replace(documento_usuario, "(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})","\\1.\\2.\\3/\\4-\\5")

  }


  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  pb <- progress::progress_bar$new(total = length(cd_processo))

  purrr::walk(cd_processo, purrr::possibly(~{


  pb$tick()
  
    url <- paste0("https://esaj.tjsp.jus.br/petpg/api/processos/", .x,"/partes?instancia=", instancia,"&cd_perfil=",cd_perfil,"&cd_usuario=",cd_usuario,"&cd_usuario_solicitante=",usuario_solicitante,"&documento_usuario=", document_usuario)

    arquivo <- file.path(diretorio, paste0("tjsp_cd_processo_", .x, "_doc_partes", ".json"))

    httr::GET(url, httr::write_disk(arquivo, overwrite = T))

  }, NULL))

}

#' Cria variáveis de embiente temporárias para o usuário esaj.
#'
#' @param cd_usuario Código do usuário. Aceita envvar ESAJ_CD_USUARIO.
#' @param usuario_solicitante Código do solicitante. Aceita envvar ESAJ_USUARIO_SOLICITANTE.
#' @param documento_usuario CPF ou CNPJ do usuário. Aceita envvar ESAJ_DOCUMENTO_USUARIO.
#' @param cd_perfil Perfil do usuário. Aceita envvar ESAJ_CD_PERFIL
#' @return Arquivo json
#' @export

tjsp_partes_credenciais <- function(cd_usuario = NULL,
                                    usuario_solicitante = NULL,
                                    documento_usuario = NULL,
                                    cd_perfil = NULL){


Sys.setenv(
ESAJ_CD_PERFIL=cd_perfil,
ESAJ_CD_USUARIO=cd_usuario,
ESAJ_USUARIO_SOLICITANTE=usuario_solicitante,
ESAJ_DOCUMENTO_USUARIO=documento_usuario
)
}
