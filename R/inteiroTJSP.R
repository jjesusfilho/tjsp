#' Função inteiroTJSP
#'
#' This function predicts captcha letters
#' @param url a vector of url strings to access the decisions.
#' @keywords tjsp court decisions
#' @import httr
#' @import stringr
#' @import captchaSajAudio
#' @return pdf of the decision downloaded to the working directory.
#' @export
testar_processo<-function(url) {
  arq_img<-"imagem.png"
  arq_aud<-"audio.mpg"
  folder_img <- dirname(arq_img)
  folder_aud <- dirname(arq_aud)
  httr::handle_reset('https://esaj.tjsp.jus.br/cjsg')
  u0<-url
  r0 <- httr::GET(u0)
  u_aud <- 'https://esaj.tjsp.jus.br/cjsg/somCaptcha.do'
  u_img <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
  httr::GET(u_img, httr::write_disk(arq_img, overwrite = TRUE))
  httr::GET(u_aud, httr::write_disk(arq_aud, overwrite = TRUE))
  res <- captchaSajAudio::decifrar(arq_aud)
  u1 <- paste0(u0, '&vlCaptcha=', res)
  r1 <- httr::GET(u1)$all_headers[[1]]$headers[['content-type']]
  passou <- r1 == "application/pdf;charset=UTF-8"
  return(c(passou, res))
}


#' This function downloads complete decisions from São Paulo state High Court.
#' @param url a vector of url strings to access the decisions.
#' @param cdacordao a vector of decisions ids/codes to give as names to the downloading files.
#' @keywords tjsp court decisions
#' @import httr
#' @import stringr
#' @import captchaSajAudio
#' @return pdf of the decision downloaded to the working directory.
#' @export
inteiroTJSP<-function(url=NULL,cdacordao=NULL){

  for(i in seq_along(url)){
    POST(url[i],
         body<-list(testar_processo(url[i])[2]),
         write_disk(paste0(cdacordao[i],".pdf"),overwrite = T))
    Sys.sleep(1)
  }
  file.remove("imagem.png","audio.mpg")
}

#' @examples
#' inteiroTJSP(url)
