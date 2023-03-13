#' Lista varas e distribuidores do tjsp
#'
#' @return dataframe
#' @export
#'
tjsp_varas <- function(){


  url1 <- "https://esaj.tjsp.jus.br/cjpg/varasTreeSelect.do"

  conteudo <- xml2::read_html(url1, encoding = "UTF-8")

  fn <- conteudo |>
    xml2::xml_find_all("//li[@class=' open']/span") |>
    xml2::xml_attr("searchid") |>
    stats::na.omit()

  foro <- conteudo |>
    xml2::xml_find_all("//li[@class=' open']/span") |>
    xml2::xml_attr("searchvalue") |>
    stats::na.omit()

  df <- data.frame(fn, foro)

  varas <- conteudo |>
    xml2::xml_find_all("//li[@class='leafItem']/span") |>
    xml2::xml_attr("searchid") |>
    stats::na.omit()


  valor <- conteudo |>
    xml2::xml_find_all("//li[@class='leafItem']/span") |>
    xml2::xml_attr("value") |>
    stats::na.omit()

  texto <-  conteudo |>
    xml2::xml_find_all("//li[@class='leafItem']/span") |>
    xml2::xml_text() |>
    stringr::str_subset("^\\s+$",negate = T)


  data.frame(varas, valor,texto) |>
    dplyr::mutate(distribuidor = stringr::str_extract(valor, "\\d+")) |>
    dplyr::left_join(df, by = c("distribuidor"="fn"))

}
