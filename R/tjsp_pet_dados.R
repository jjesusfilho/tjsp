#' Baixa informações sobre as partes
#'
#' @param processo Vetor com os códigos dos processos
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

tjsp_pet_dados <- function(processo,
                                    instancia = "pg",
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


  processo <- processo |>
    stringr::str_remove_all("\\D") |>
    stringr::str_pad(width = 20,side = "left", pad = "0") |>
    stringr::str_replace("(.{7})(.{2})(.{4})(.{1})(.{2})(.{4})","\\1-\\2.\\3.\\4.\\5.\\6")



  purrr::walk(processo, purrr::possibly(~{



    p <- stringr::str_remove_all(.x, "\\D")

    url1 <- glue::glue("https://esaj.tjsp.jus.br/pet{instancia}/api/processos?cd_perfil={cd_perfil}&cd_usuario={cd_usuario}&cd_usuario_solicitante={usuario_solicitante}&documento_usuario={document_usuario}&numero_processo={.x}&permite_definicao_tipo_distribuicao=false&tipo_peticao=2&usar_outro_numero=false")


    arquivo <- file.path(diretorio, paste0("tjsp_pet",instancia,"_", p, "_dados", ".json"))

    httr::GET(url1, httr::write_disk(arquivo, overwrite = T))

}, NULL), .progress =TRUE)

}
