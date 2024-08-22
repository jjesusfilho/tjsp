#' Extrai foros e respectivos códigos da página de intimações do Esaj.
#'
#' @return tibble
#' @export
#'
tjsp_intimacoes_foros <- function(){


  url1 <- "https://esaj.tjsp.jus.br/intimacoesweb/AjaxServlet.ajax?tagId=foroService&cdUsuarioReferencia=251219&cdTipoAto=-1&cdInstancia=1&filtroValue=&component=inputSelect&bind_nmForo=entity.nmForo&bind_cdForo=entity.cdForo&useAction=false&SpwInputSelectRequestOrigin=InputSelectSearchGrid&currentTime=1724319940295"

  r1 <- httr::GET(url2)

  url2 <- "https://esaj.tjsp.jus.br/intimacoesweb/jsp/search/searchForo.jsp?nmForo=&reference=entity.nmForo&reference=entity.nmForo&entity.nmForo=&entity.nmForo=&=&=&=&idObjRetorno=foroService&multiselection=false&cdUsuarioReferencia=251219&cdTipoAto=-1&cdInstancia=1&height=282&inputSelectId=foroService&CurrentMultSelecaoId=foroService&multSelecaoProperty=null&gotInputParam=false&bind_nmForo=entity.nmForo&bind_cdForo=entity.cdForo&useAction=false&requesterUrl=jsp/search/searchForo.jsp?nmForo=&SpwInputSelectRequestOrigin=InputSelectSearchGrid&desabilitarSelecionados=false"

  c2 <- httr::GET(url2) |>
        httr::content()

  foro <- c2 |>
    xml2::xml_find_all("//table[@id='tabelaResultado']//td[@class='spwCelulaGrid '][@oculto='false']") |>
    xml2::xml_attr("title")


  codigo_foro <- c2 |>
    xml2::xml_find_all("//table[@id='tabelaResultado']//td[@class='spwCelulaGrid '][@oculto='true']") |>
    xml2::xml_attr("beanvalue")


  tibble::tibble(foro, codigo_foro)

}
