#' Autenticar no tjsp
#'
#' @param login cpf
#' @param password senha
#' @param metodo Informar se "email" (login e senha com token por email) ou
#'    "certificado" (certificado digital A1)
#' @param certificado Caminho para o arquivo do certificado A1 (".pfx" ou ".p12").
#'    Necessário apenas quando `metodo = "certificado"`.
#' @param senha_certificado Senha do certificado A1. Se não for informada e
#'    `metodo = "certificado"`, será solicitada interativamente, ou pode ser
#'    fornecida pela variável de ambiente "SENHACERTIFICADO".
#' @param email_provider Informar se "gmail" ou "outlook"
#' @param outlook Informar se "personal" ou "business"
#' @param email_from Origem do gmail. Eventualmente, em ambiente corporativo, o email pode ser redistribuído
#'     a partir de outro email.
#' @param wait_email Tempo, em segundos, para aguardar antes de conferir se
#'    se o código chegou no email.
#' @param tz Fuso horário. Padrão para o da máquina. Recomentado: "America/Sao_Paulo",
#' @param check_login Verificar se já está logado?
#'
#' @return Estabelece uma sessão, não é necessário salvar.
#' @export
#'
#' @details Você pode informar as credenciais nos argumentos ou
#'      criar variáveis de ambiente: "LOGINADV" e "PASSWORDADV", ou
#'      chamar a função e aguardar o prompt para informar
#'      login e password. Para autenticar com certificado A1, informe
#'      `metodo = "certificado"` e o caminho do arquivo em `certificado`.
tjsp_autenticar <- function(login = NULL,
                            password = NULL,
                            metodo = c("email", "certificado"),
                            certificado = NULL,
                            senha_certificado = NULL,
                            email_provider = NA,
                            outlook = "business",
                            email_from = "esaj@tjsp.jus.br",
                            wait_email = 5,
                            tz = "",
                            check_login = TRUE
                            ) {


  print("Aguarde, pode demorar at\u00E9 dois minutos para retornar a resposta.")

    if(tz==""){

    tz <-Sys.timezone()

    }

  metodo <- match.arg(metodo, choices = c("email", "certificado"))

  # Check if isn't already logged in
  if(check_login){
  if (check_login()) {
    return(TRUE)
  }
  }

  if (metodo == "certificado") {
    return(tjsp_autenticar_certificado(certificado, senha_certificado))
  }

  email_provider <- match.arg(email_provider, choices = c("gmail","outlook"))

  outlook <- match.arg(outlook, choices = c("personal","business"))

  # Prompt for information if necessary
  if (is.null(login) || is.null(password)) {

    login <- Sys.getenv("LOGINADV")
    password <- Sys.getenv("PASSWORDADV")

    if ( login =="" || password == "") {

    login <- as.character(getPass::getPass(msg = "Enter your login: "))
    password <- as.character(getPass::getPass(msg = "Enter your password: "))
    }

    }

  # Initial access
  base <- "https://esaj.tjsp.jus.br/"
  httr::GET(stringr::str_c(base, "esaj/portal.do?servico=740000"), httr::config(ssl_verifypeer = FALSE))

  hora_gmail <- lubridate::now(tzone = tz) |>
    as.POSIXct() |>
    as.integer()

  hora_outlook <- format(lubridate::with_tz(lubridate::now(tzone = tz), "UTC"), "%Y-%m-%dT%H:%M:%SZ")

if(email_provider=='outlook'){

    hora <- hora_outlook

} else{

  hora <- hora_gmail
  }

  f_login <- stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) |>
    httr::GET(httr::config(ssl_verifypeer = FALSE))


  # Get parameters for POST
  lt <- f_login |>
    httr::content("text") |>
    xml2::read_html() |>
    xml2::xml_find_first("//input[@name='lt']") |>
    xml2::xml_attr("value")

  e2 <- f_login |>
    httr::content("text") |>
    xml2::read_html() |>
    xml2::xml_find_first("//input[@name='execution']") |>
    xml2::xml_attr("value")


  # Create POST quert
  query_post <- list(
    username = login,
    password = password,
    lt = lt,
    execution = e2,
    "_eventId" = "submit",
    pbEntrar = "Entrar",
    signature = "",
    certificadoSelecionado = "",
    certificado = ""
  )



  # Primeira etapa
 stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) |>
    httr::POST(body = query_post, httr::config(ssl_verifypeer = FALSE), encode = "form")

  Sys.sleep(wait_email)

 taxa <- purrr::rate_delay(pause = 15, max_times = 12)

 get_email_token_insistente <- purrr::insistently(get_email_token1, taxa, quiet = FALSE)

 get_token <- purrr::possibly(get_email_token_insistente, otherwise = "N\u00E3o foi poss\u00EDvel gerar o token")

 token <- get_token(email_provider,outlook,hora, email_from)

 if(stringr::str_detect(token,"\\d", negate = T)){

   stop(token)
 }

  query_post_token <- list(username = login, password = password,
                           lt = lt, execution = e2, token = token, `_eventId` = "submit",
                           signature = "", certificadoSelecionado = "", certificado = "")


  stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) |>
    httr::POST(body = query_post_token, httr::config(ssl_verifypeer = FALSE), encode = "form")


  # Message
  flag <- check_login()
  if (flag) {
    message("You're logged in")
  }
  else {
    message("Login failed")
  }

  return(flag)
}

check_login <- function() {
  "https://esaj.tjsp.jus.br/" |>
    stringr::str_c("sajcas/verificarLogin.js") |>
    httr::GET(httr::config(ssl_verifypeer = FALSE)) |>
    httr::content("text") |>
    stringr::str_detect("true")
}

#' Autenticar no tjsp com certificado digital A1
#'
#' @param certificado Caminho para o arquivo do certificado A1 (".pfx" ou ".p12").
#' @param senha_certificado Senha do certificado. Se `NULL`, busca a variável
#'    de ambiente "SENHACERTIFICADO" ou solicita interativamente.
#'
#' @return `TRUE`/`FALSE` indicando se a sessão foi autenticada.
#' @noRd
tjsp_autenticar_certificado <- function(certificado, senha_certificado = NULL) {

  if (is.null(certificado)) {
    stop("Informe o caminho do certificado A1 (.pfx ou .p12) em 'certificado'.")
  }

  if (!file.exists(certificado)) {
    stop("Arquivo de certificado não encontrado: ", certificado)
  }

  if (is.null(senha_certificado)) {

    senha_certificado <- Sys.getenv("SENHACERTIFICADO")

    if (senha_certificado == "") {
      senha_certificado <- as.character(getPass::getPass(msg = "Enter your certificate password: "))
    }
  }

  p12 <- openssl::read_p12(certificado, password = senha_certificado)

  cert_pem <- tempfile(fileext = ".pem")
  key_pem <- tempfile(fileext = ".pem")
  cert_der <- tempfile(fileext = ".der")
  hash_bin <- tempfile(fileext = ".bin")
  sig_bin <- tempfile(fileext = ".bin")
  on.exit(unlink(c(cert_pem, key_pem, cert_der, hash_bin, sig_bin)), add = TRUE)

  openssl::write_pem(p12$cert, cert_pem)
  openssl::write_pem(p12$key, key_pem)

  system2("openssl", c("x509", "-in", cert_pem, "-outform", "DER", "-out", cert_der))
  certificado_base64 <- readBin(cert_der, "raw", file.info(cert_der)$size) |>
    openssl::base64_encode()

  config_ssl <- httr::config(ssl_verifypeer = FALSE)

  base <- "https://esaj.tjsp.jus.br/"

  httr::GET(stringr::str_c(base, "esaj/portal.do?servico=740000"), config_ssl)

  url_login <- stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  )

  pagina_texto <- httr::GET(url_login, config_ssl) |>
    httr::content("text")

  pagina <- pagina_texto |> xml2::read_html()

  lt <- pagina |> xml2::xml_find_first("//input[@name='lt']") |> xml2::xml_attr("value")
  e2 <- pagina |> xml2::xml_find_first("//input[@name='execution']") |> xml2::xml_attr("value")

  hash_desafio <- stringr::str_match(pagina_texto, "hashDesafio\\s*=\\s*'([^']+)'")[, 2]

  if (is.na(hash_desafio)) {
    stop("Não foi possível obter o desafio de assinatura do TJSP.")
  }

  writeBin(openssl::base64_decode(hash_desafio), hash_bin)

  system2("openssl", c(
    "pkeyutl", "-sign",
    "-inkey", key_pem,
    "-in", hash_bin,
    "-out", sig_bin,
    "-pkeyopt", "digest:sha256"
  ))

  assinatura_base64 <- readBin(sig_bin, "raw", file.info(sig_bin)$size) |>
    openssl::base64_encode()

  query_post <- list(
    lt = lt,
    execution = e2,
    "_eventId" = "submit",
    token = "",
    certificadoSelecionado = certificado_base64,
    signature = assinatura_base64
  )

  httr::POST(url_login, body = query_post, config_ssl, encode = "form")

  flag <- check_login()

  if (flag) {
    message("You're logged in")
  } else {
    message("Login failed")
  }

  return(flag)
}

#' @rdname tjsp_autenticar
#' @export
autenticar <- tjsp_autenticar


#' Obt\u00EAm tokens do email
#'
#' @param email_provider "gmail" ou "outlook"
#' @param outlook Se outlook, informar se "business" ou
#'    "personal"
#' @param hora Par\u00E2metro n\u00E3o usado, por ora.
#' @param email_from Origem do gmail. Eventualmente, em ambiente corporativo, o email pode ser redistribuído
#'     a partir de outro email.
#'
#' @returns token
#'
get_email_token1 <- function(email_provider, outlook, hora, email_from) {

  filtro_gmail <- glue::glue("from:{email_from} AND subject:Valida\u00E7\u00E3o AND after:{hora}")

  filtro_outlook <- glue::glue("from/emailAddress/address eq '{email_from}' and contains(subject,'Valida\u00E7\u00E3o') and receivedDateTime ge {hora}")

  if(email_provider == "outlook"){

    if(outlook=="business"){

    outl <- Microsoft365R::get_business_outlook()

    } else{
      outl <- Microsoft365R::get_personal_outlook()
    }

    email <- outl$list_emails(
      filter = filtro_outlook, by='received ASC', n=1)


    token <- email[[1]]$properties$bodyPreview |>
      stringr::str_extract("\\d{6}")

  } else {

  token <- gmailr::gm_threads(search= filtro_gmail) |>
  purrr::pluck(1,"threads",1,"snippet") |>
  stringr::str_extract("\\d+")

}

  if(rlang::is_empty(token)){

    stop("Token vazio")
  }

  return(token)

}


