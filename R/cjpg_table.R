#' Extrai assuntos e classes
#'
#' @param tipo classe ou assunto
#'
#' @return tibble com classe ou assunto
#' @export
#'
#' @examples
#' \dontrun{
#' df <- cjpg_table(tipo="classe")
#' }
cjpg_table <- function (tipo)
{

  tipo <- switch(tipo, classes = "classe", subjects = "assunto")

  stringr::str_c("https://esaj.tjsp.jus.br/cjpg/",
                 tipo,
                 "TreeSelect.do?campoId=",
                 tipo) %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class='treeView']") %>%
    purrr::modify(xml2::as_list) %>%
    dplyr::first() %>%
    dplyr::nth(2) %>%
    purrr::keep(~is.list(.x)) %>%
  tree_to_tibble() %>%
  dplyr::mutate(name0 = ifelse(is.na(name0),name5, name0),
                  id0 = ifelse(is.na(id0), id5, id0)) %>%
    dplyr::select(
      dplyr::ends_with("0"),
      dplyr::ends_with("1"),
      dplyr::ends_with("2"),
      dplyr::ends_with("3"),
      dplyr::ends_with("4"),
      dplyr::ends_with("5")
    )



}


tree_to_tibble <- function (tree, n = 0)
{
  names <-
    tree %>% purrr::map(purrr::pluck, 2, 1) %>%
    purrr::compact() %>%
    magrittr::extract(. != "") %>%
    purrr::flatten_chr()

    ids <- tree %>%
    purrr::map(purrr::pluck, 2) %>%
    purrr::map(attr,"value") %>%
    purrr::compact() %>%
    magrittr::extract(. != "") %>%
    purrr::flatten_chr()

 purrr::imap_dfr(lengths(tree, FALSE), function(len, i){
    if (len == 3) {
      dplyr::tibble(name5 = names[i], id5 = ids[i])
    }
    else {
      tree %>%
        purrr::pluck(i, 4) %>%
        magrittr::extract(names(.) =="li") %>%
        tree_to_tibble(n + 1) %>%
        dplyr::mutate(`:=`(!!stringr::str_c("name", n), names[i]),ids[i])
    }
  })
}
