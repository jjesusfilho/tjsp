#' Lê busca por paramêtro diferente do processo
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Alternativa aos arquivos
#'
#' @return tibble
#' @export
#'
tjsp_ler_cpopg_par <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = T, pattern = "html$")

  }


pb <- progress::progress_bar$new(total = length(arquivos))


purrr::map_dfr(arquivos, purrr::possibly(~{

  pb$tick()


consulta <- stringr::str_extract(.x, "(?<=consulta_).+?(?=_)")

parametro <- stringr::str_extract(.x, "(?<=parametro_).+?(?=_)")

distribuidor <- stringr::str_extract(.x, "(?<=distribuidor_).+?(?=_)")

x <- .x |>
     xml2::read_html()

processo<- xml2::xml_find_all(x,"//a[@class='linkProcesso']") |>
  xml2::xml_text(trim=TRUE) |>
  stringr::str_remove_all("\\s.+")


tipo_participacao <- xml2::xml_find_all(x,"//li//div[@class='nuProcesso col-md-3']/following-sibling::div[1]") |>
  purrr::map(~xml2::xml_child(.x, "/label[@class='unj-label tipoDeParticipacao']") |>
                  xml2::xml_text(trim = TRUE) |>
                  stringr::str_remove_all("\\:.*")) |>
      unlist()



nome_parte <- xml2::xml_find_all(x,"//li//div[@class='nuProcesso col-md-3']/following-sibling::div[1]") |>
  purrr::map(~xml2::xml_child(.x,"/div[@class='unj-base-alt nomeParte']") |>
  xml2::xml_text(trim=T) |>
  stringr::str_remove_all("\\:.*")) |>
  unlist()

recebimento <- xml2::xml_find_all(x,"//div/label[@class='unj-label labelRecebidoEm']/following-sibling::div") |>
  xml2::xml_text(trim=T) |>
  stringr::str_remove_all("\\:.*")

 data_distribuicao <- recebimento |>
                     stringr::str_extract("\\S+") |>
                     lubridate::dmy()

 local_distribuicao <- recebimento |>
                      stringr::str_extract("(?<=- ).+")



tibble::tibble(processo,consulta, parametro, distribuidor,
               tipo_participacao,
               nome_parte,
               data_distribuicao,
               local_distribuicao)


}, NULL))
}
