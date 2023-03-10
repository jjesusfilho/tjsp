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


coluna<- xml2::xml_find_all(x,"//div[@class='espacamentoLinhas']/span") |>
  xml2::xml_text(trim=T) |>
  stringr::str_remove_all("\\:.*")

valor<- xml2::xml_find_all(x,"//div[@class='espacamentoLinhas']") |>
  xml2::xml_text(trim=TRUE) |>
  stringr::str_extract("(?<=:\\s{1,5}).+") |>
  stringr::str_squish()

tibble::tibble(processo,consulta, parametro, distribuidor, coluna,valor) |>
  tidyr::pivot_wider(names_from=coluna, values_from = valor) %>%
  janitor::clean_names() %>%
  tidyr::separate(recebido_em,c("data_entrada","unidade"), sep = " - ") %>%
  dplyr::mutate(data_entrada= lubridate::dmy(data_entrada)) %>%
  dplyr::select(processo,data_entrada, unidade, dplyr::everything())

}, NULL))
}
