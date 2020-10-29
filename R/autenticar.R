#' Autenticar no tjsp
#'
#' @param login cpf
#' @param password senha
#' @return Estabelece uma sessão, não é necessário salvar.
#' @export
#'
#' @details Você pode informar as credenciais nos argumentos ou
#'      criar variáveis de ambiente: "LOGINADV" e "PASSWORDADV", ou
#'      chamar a função e aguardar o prompt para informar
#'      login e password
autenticar <- function(login = NULL, password = NULL) {

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

  # Get login page file
  f_login <- stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE))

  # Get parameters for POST
  lt <- f_login %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//input[@name='lt']") %>%
    xml2::xml_attr("value")

  e2 <- f_login %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//input[@name='execution']") %>%
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

  # Try to login
 stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) %>%
    httr::POST(body = query_post, httr::config(ssl_verifypeer = FALSE), encode = "form")

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
  "https://esaj.tjsp.jus.br/" %>%
    stringr::str_c("sajcas/verificarLogin.js") %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content("text") %>%
    stringr::str_detect("true")
}
