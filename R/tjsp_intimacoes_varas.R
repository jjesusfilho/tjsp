#' Obtêm códigos das varas a partir do código do foro.
#'
#' @param codigo_foro Código do foro obtido com tjsp_intimacoes_foros
#' @details
#'    Esta função requer que você tenha atribuído o código de usuário
#'    à variável de ambiente  ESAJ_CD_USUARIO
#'
#' @return tibble
#' @export
#'
tjsp_intimacoes_varas <- function(codigo_foro){

  cd_usuario <- Sys.getenv("ESAJ_CD_USUARIO")

  parseada <-   structure(
    list(
      scheme = "https",
      hostname = "esaj.tjsp.jus.br",
      port = NULL,
      path = "intimacoesweb/AjaxServlet.ajax",
      query = list(
        entity.cdForo = codigo_foro,
        tagId = "varaService",
        cdUsuarioReferencia = cd_usuario,
        cdTipoAto = "-1",
        cdInstancia = "1",
        cdForo = codigo_foro,
        filtroValue = glue::glue("entity.cdForo={codigo_foro}"),
        component = "inputSelect",
        bind_nmVara = "entity.nmVara",
        bind_cdVara = "entity.cdVara",
        useAction = "false",
        SpwInputSelectRequestOrigin = "InputSelectSearchGrid"
      ),
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL
    ),
    class = "url"
  )


  url1 <- httr::build_url(parseada)

  r1 <- httr::GET(url1)



parseada <- structure(
  list(
    scheme = "https",
    hostname = "esaj.tjsp.jus.br",
    port = NULL,
    path = "intimacoesweb/jsp/search/searchVara.jsp",
    query = list(
      nmVara = "",
      reference = "entity.nmVara",
      reference = "entity.nmVara",
      entity.nmVara = "",
      entity.nmVara = "",
      entity.cdForo = codigo_foro,
      entity.cdForo = codigo_foro,
      idObjRetorno = "varaService",
      multiselection = "false",
      cdUsuarioReferencia = cd_usuario,
      cdTipoAto = "-1",
      cdInstancia = "1",
      cdForo = codigo_foro,
      height = "282",
      inputSelectId = "varaService",
      CurrentMultSelecaoId = "varaService",
      multSelecaoProperty = "null",
      gotInputParam = "false",
      bind_nmVara = "entity.nmVara",
      bind_cdVara = "entity.cdVara",
      useAction = "false",
      requesterUrl = "jsp/search/searchVara.jsp?nmVara=",
      SpwInputSelectRequestOrigin = "InputSelectSearchGrid",
      desabilitarSelecionados = "false"
    ),
    params = NULL,
    fragment = NULL,
    username = NULL,
    password = NULL
  ),
  class = "url"
)

url2 <- httr::build_url(parseada)

c2 <- httr::GET(url2) |>
      httr::content()

vara <- c2 |>
  xml2::xml_find_all("//table[@id='tabelaResultado']//td[@class='spwCelulaGrid '][@oculto='false']") |>
  xml2::xml_attr("title")


codigo_vara <- c2 |>
  xml2::xml_find_all("//table[@id='tabelaResultado']//td[@class='spwCelulaGrid '][@oculto='true']") |>
  xml2::xml_attr("beanvalue")


tibble::tibble(codigo_foro = codigo_foro, codigo_vara = codigo_vara, vara = vara)

}
