#' Autenticar na API mobile do TJSP (api.tjsp.jus.br)
#'
#' @param cpf CPF do advogado (com ou sem pontuação).
#' @param senha Senha.
#' @param check_login Se `TRUE` (padrão), reaproveita a autenticação já feita
#'    nesta sessão do R em vez de logar de novo.
#'
#' @details Autentica contra `POST https://api.tjsp.jus.br/processo/sajcas/login`,
#'    o backend REST usado pelo app **TJSP Mobile** (mesmo `cpopgmobile` do
#'    e-SAJ). É uma autenticação mais leve que [tjsp_autenticar()]: **sem
#'    2FA por e-mail**, só CPF e senha.
#'
#'    Se não informados, `cpf`/`senha` são lidos das variáveis de ambiente
#'    `LOGINADV`/`PASSWORDADV` (as mesmas usadas por [tjsp_autenticar()]) ou
#'    solicitados interativamente.
#'
#'    **Não suporta certificado digital A1.** O contrato real dessa rota
#'    (conferido em `https://api.tjsp.jus.br/processo/swagger/v1/swagger.json`)
#'    só aceita os parâmetros `usuario` e `senha` -- ao contrário do CAS web
#'    usado por [tjsp_autenticar(metodo = "certificado")], não há parâmetros
#'    de certificado/assinatura nessa rota. Para consultas por certificado,
#'    use os demais clientes do pacote.
#'
#'    A autenticação só libera detalhes (`tjsp_api_dados_basicos()`,
#'    `tjsp_api_partes()`, `tjsp_api_movimentacao()`, pasta digital) nos
#'    processos em que o usuário logado está habilitado nos autos.
#'
#' @return `TRUE` se autenticado.
#' @export
#'
tjsp_api_autenticar <- function(cpf = NULL, senha = NULL, check_login = TRUE) {

  if (check_login && .tjsp_api_autenticado()) {
    return(TRUE)
  }

  if (is.null(cpf) || is.null(senha)) {

    cpf <- Sys.getenv("LOGINADV")
    senha <- Sys.getenv("PASSWORDADV")

    if (cpf == "" || senha == "") {
      cpf <- as.character(getPass::getPass(msg = "Enter your CPF: "))
      senha <- as.character(getPass::getPass(msg = "Enter your password: "))
    }
  }

  digitos <- stringr::str_remove_all(cpf, "\\D")

  ok <- .tjsp_api_login(digitos, senha)

  if (ok) {
    assign("cpf", digitos, envir = .tjsp_api_state)
    assign("senha", senha, envir = .tjsp_api_state)
    assign("cd_aberto", NULL, envir = .tjsp_api_state)
    message("You're logged in")
  } else {
    stop("Login recusado pela API do TJSP (usuário/senha inválidos).")
  }

  invisible(ok)
}
