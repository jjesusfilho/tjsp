#' Instalar certificado digital A1 em local padrão
#'
#' Copia o arquivo do certificado A1 (".pfx" ou ".p12") para um diretório
#' padrão do sistema operacional (obtido via \code{rappdirs::user_data_dir()},
#' válido tanto para Mac, quanto para Linux e Windows) e registra o caminho
#' na variável de ambiente "CERTIFICADOTJSP" no arquivo .Renviron do usuário.
#' Assim, \code{tjsp_autenticar(metodo = "certificado")} localiza o
#' certificado automaticamente em qualquer sessão futura, sem precisar
#' informar o caminho a cada chamada.
#'
#' @param certificado Caminho para o arquivo do certificado A1 (".pfx" ou ".p12")
#'    a ser instalado.
#'
#' @return Caminho do certificado no diretório padrão (invisível).
#' @export
#'
#' @details Após a instalação, é necessário reiniciar a sessão do R para que
#'      a variável de ambiente "CERTIFICADOTJSP" entre em vigor.
tjsp_instalar_certificado <- function(certificado) {

  if (!file.exists(certificado)) {
    stop("Arquivo de certificado não encontrado: ", certificado)
  }

  diretorio <- rappdirs::user_data_dir("tjsp", "tjsp")
  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)

  destino <- file.path(diretorio, basename(certificado))

  file.copy(certificado, destino, overwrite = TRUE)
  Sys.chmod(destino, mode = "0600")

  tjsp_atualizar_renviron("CERTIFICADOTJSP", destino)

  message("Certificado instalado em: ", destino)
  message("Reinicie a sessão do R para que a variável CERTIFICADOTJSP tenha efeito.")

  invisible(destino)
}

#' Atualizar ou criar uma variável no .Renviron do usuário
#'
#' @param chave Nome da variável de ambiente.
#' @param valor Valor a ser atribuído.
#'
#' @returns Caminho do arquivo .Renviron atualizado.
#' @noRd
tjsp_atualizar_renviron <- function(chave, valor) {

  renviron <- Sys.getenv("R_ENVIRON_USER")

  if (renviron == "") {
    renviron <- file.path(Sys.getenv("HOME"), ".Renviron")
  }

  linhas <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()

  padrao <- paste0("^", chave, "\\s*=")

  linhas <- linhas[!grepl(padrao, linhas)]

  linhas <- c(linhas, paste0(chave, "=", valor))

  writeLines(linhas, renviron)

  invisible(renviron)
}
