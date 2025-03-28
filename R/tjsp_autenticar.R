#' Autenticar no tjsp
#'
#' @param login cpf
#' @param password senha
#' @param email_provider Informar se "gmail" ou "outlook"
#' @param outlook Informar se "personal" ou "business"
#' @param wait_email Tempo, em segundos, para aguardar antes de conferir se
#'    se o código chegou no email.
#' @param tz Fuso horário. Padrão para o da máquina. Recomentado: "America/Sao_Paulo"
#'
#' @return Estabelece uma sessão, não é necessário salvar.
#' @export
#'
#' @details Você pode informar as credenciais nos argumentos ou
#'      criar variáveis de ambiente: "LOGINADV" e "PASSWORDADV", ou
#'      chamar a função e aguardar o prompt para informar
#'      login e password
tjsp_autenticar <- function(login = NULL,
                            password = NULL,
                            email_provider = NA,
                            outlook = "business",
                            wait_email = 5,
                            tz = ""
                            ) {


  print("Aguarde, pode demorar até dois minutos para retornar a resposta.")

    if(tz==""){

    tz <-Sys.timezone()

    }

  email_provider <- match.arg(email_provider, choices = c("gmail","outlook"))

  outlook <- match.arg(outlook, choices = c("personal","business"))

  # Check if isn't already logged in
  if (check_login()) {
    return(TRUE)
  }

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

 taxa <- purrr::rate_delay(pause = 10, max_times = 10)

 get_email_token_insistente <- purrr::insistently(get_email_token1, taxa, quiet = FALSE)

 get_token <- purrr::possibly(get_email_token_insistente, otherwise = "N\u00E3o foi poss\u00EDvel gerar o token")

 token <- get_token(email_provider,outlook,hora)

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

#' @rdname tjsp_autenticar
#' @export
autenticar <- tjsp_autenticar


#' Obtêm tokens do email
#'
#' @param email_provider "gmail" ou "outlook"
#' @param outlook Se outlook, informar se "business" ou
#'    "personal"
#' @param hora Parâmetro não usado, por ora.
#'
#' @returns token
#'
get_email_token1 <- function(email_provider, outlook, hora) {

  filtro_gmail <- glue::glue("from:esaj@tjsp.jus.br AND subject:Valida\u00E7\u00E3o AND after:{hora}")

  filtro_outlook <- glue::glue("from/emailAddress/address eq 'esaj@tjsp.jus.br' and contains(subject,'Validação') and receivedDateTime ge {hora}")

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


