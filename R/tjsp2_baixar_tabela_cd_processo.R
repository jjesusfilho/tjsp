#' Baixa tabela de documentos com base no cd_processo
#'
#' @param cd_processo Número do processo
#' @param cookies_path informar o caminho para o cookie.
#' @param diretorio Diretório onde armazenar as tabelas
#'
#' @return html
#' @export
#'
tjsp2_baixar_tabela_cd_processo <- function (cd_processo = NULL, cookies_path = NULL,diretorio = ".")
{

  if (is.null(cookies_path)){

    cookies <- httr2::last_request()$options$cookiefile

  } else {

    cookies <- cookies_path

  }
  purrr::walk(cd_processo, purrr::possibly(~{

    arquivo <- file.path(diretorio,paste0("tabela_cd_processo_",.x,".html"))


    url1 <- .x |>
      paste0("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo=", ... = _, "&gateway=true")

    if (stringr::str_detect(.x, "^DW")){
      url1 <- paste0(url1,"&consultaDeRequisitorios=true")
    }

    r1 <- url1 |>
      httr2::request() |>
      httr2::req_cookie_preserve(cookies) |>
      httr2::req_options(ssl_verifypeer = FALSE) |>
      httr2::req_perform()


    if (

      r1 |>
      httr2::resp_body_html() |>
      xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")

    ) {

      cdProcesso <-  r1 |>
        httr2::resp_body_html() |>
        xml2::xml_find_first("//a[@class='linkConsultaSG btn btn-secondary btn-space']") |>
        xml2::xml_attr("href") |>
        stringr::str_extract("(?<=Sg.)\\w+")

      url2 <-  paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=",cdProcesso, "&gateway=true")


      r2 <- url2 |>
        httr2::request() |>
        httr2::req_cookie_preserve(cookies) |>
        httr2::req_options(ssl_verifypeer = FALSE) |>
        httr2::req_perform()

      url3  <- paste0("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso=",cdProcesso,"&conversationId=&_=1599440192646")



      arquivo <- file.path(diretorio, paste0("tabela_cd_processo_pg_", .x, "_cd_processo_sg_",cdProcesso,".html"))


    } else{

      url3 <- paste0("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo=",.x)

      arquivo <- file.path(diretorio,paste0("tabela_cd_processo_pg_",.x,".html"))


    }

    url3 |>
      httr2::request() |>
      httr2::req_cookie_preserve(cookies) |>
      httr2::req_options(ssl_verifypeer = FALSE) |>
      httr2::req_perform(path = arquivo)

  }, NULL), .progress = TRUE)

}
