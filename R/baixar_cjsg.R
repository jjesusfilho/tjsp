#' Baixa consulta jurisprudencial do TJSP
#'
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param aspas lógico. Colocar a expressão entre aspas?
#' @param classe Código da classe processual
#' @param assunto Código do assunto
#' @param inicio  data inicial
#' @param fim  Data final
#' @param tipo "A" Para acórdãos, "D" para decisões monocráticas.
#' @param diretorio Diretório onde serão armazenadas as páginas html.
#' @keywords tjsp,acórdãos
#'
#' @return baixa os htmls das decisões de segunda instância
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_cjsg(livre = "Lei Maria da Penha")
#' }
#' 
baixar_cjsg <-
  function(livre = "",
             aspas = FALSE,
             classe = "",
             assunto = "",
             inicio = "",
             fim = "",
             tipo = "A",
             diretorio = ".") {
    httr::set_config(httr::config(
      ssl_verifypeer = FALSE,
      accept_encoding = "latin1"
    ))
    body <-
      list(
        dados.buscaInteiroTeor = livre,
        dados.pesquisarComSinonimos = "S",
        dados.pesquisarComSinonimos = "S",
        dados.buscaEmenta = "",
        dados.nuProcOrigem = "",
        dados.nuRegistro = "",
        agenteSelectedEntitiesList = "",
        contadoragente = "0",
        contadorMaioragente = "0",
        codigoCr = "",
        codigoTr = "",
        nmAgente = "",
        juizProlatorSelectedEntitiesList = "",
        contadorjuizProlator = "0",
        contadorMaiorjuizProlator = "0",
        codigoJuizCr = "",
        codigoJuizTr = "",
        nmJuiz = "",
        classesTreeSelection.values = classe,
        classesTreeSelection.text = "",
        assuntosTreeSelection.values = assunto,
        assuntosTreeSelection.text = "",
        comarcaSelectedEntitiesList = "",
        contadorcomarca = "0",
        contadorMaiorcomarca = "0",
        cdComarca = "",
        nmComarca = "",
        secoesTreeSelection.values = "",
        secoesTreeSelection.text = "1",
        dados.dtJulgamentoInicio = inicio,
        dados.dtJulgamentoFim = fim,
        dados.dtRegistroInicio = "",
        dados.dtRegistroFim = "",
        dados.origensSelecionadas = "T",
        tipoDecisaoSelecionados = tipo,
        # tipoDecisaoSelecionados = "", tipoDecisaoSelecionados = "",
        dados.ordenacao = "data"
      )
    if (aspas == TRUE) livre <- deparse(livre)

    a <-
      httr::POST(
        "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do",
        encode = "form",
        body = body,
        httr::accept("text/html; charset=latin1;")
      )

    max_pag <- a %>%
      httr::content() %>%
      xml2::xml_find_all(xpath = "//*[@id='totalResultadoAba-A']|//*[@id='totalResultadoAba-D']") %>%
      xml2::xml_attrs() %>%
      .[[1]] %>%
      .[3] %>%
      as.numeric() %>%
      `/`(20) %>%
      ceiling()

    paginas <- 1:max_pag



    if (tipo == "A") {
      purrr::map(paginas, purrr::possibly(~ {
        httr::GET(
          paste0(
            "https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=",
            .x
          ),
          httr::set_cookies(unlist(a$cookies)),
          httr::accept("text/html; charset=latin1;"),
          httr::write_disk(paste0(diretorio, "/pagina_", .x, ".html"),
            overwrite = TRUE
          )
        )
      }, NULL))
    } else {
      purrr::map(paginas, purrr::possibly(~ {
        httr::GET(
          paste0(
            "https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=D&pagina=",
            .x
          ),
          httr::set_cookies(unlist(a$cookies)),
          httr::write_disk(paste0(diretorio, "/pagina_", .x, ".html"),
            overwrite = TRUE
          )
        )
        # httr::write_disk(paste0(diretorio, "/pagina_", .x,".html"), overwrite = TRUE)
      }, NULL))
    }
  }
