#' Função tjsg_meta
#'
#' Esta função ajuda a baixar os arquivos
#' @param url
#' @keywords baixar acordao
#' @import tuneR
#' @import httr
#' @import stringr
#' @export
#' @examples
#' tjsg_meta(livre='\"lei maria da penha\"',classes="",inicio="20/06/2012", fim="01/08/2012")


processo<-function(url) {
  arq_img<-"imagem.png"
  arq_aud<-"audio_mpg"
  folder_img <- dirname(arq_img)
  folder_aud <- dirname(arq_aud)
  handle_reset('https://esaj.tjsp.jus.br/cjsg')
  u0<-url
  r0 <- GET(u0)
  u_aud <- 'https://esaj.tjsp.jus.br/cjsg/somCaptcha.do'
  u_img <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
  GET(u_img, write_disk(arq_img, overwrite = TRUE))
  GET(u_aud, write_disk(arq_aud, overwrite = TRUE))
  res <- decifrar(arq_aud)
  u1 <- paste0(u0, '&vlCaptcha=', res)
  r1 <- GET(u1)$all_headers[[1]]$headers[['content-type']]
  passou <- r1 == "application/pdf;charset=UTF-8"
  return(c(passou, res))
}
