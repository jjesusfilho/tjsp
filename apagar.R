library(tjsp)


tjsp_baixar_eproc2g_jurisprudencia("homicídio",n = 1,  tipo_documento = "1", diretorio = "data-raw" )

df <- tjsp_ler_eproc_jurisprudencia(diretorio = "data-raw")

# Filtrar só acórdãos e baixar inteiros teores
df |>
  dplyr::filter(tipo == "Acórdão") |>
  dplyr::distinct(id_jurisprudencia, .keep_all = TRUE) |>
  dplyr::pull(id_jurisprudencia) |>
  tjsp_baixar_eproc_inteiro_teor(grau = "2g", diretorio = "data-raw")


body <- list(txtPesquisa = "homic\xeddio", hdnExibirPesquisaAvancada = "",
    hdnUrlCarregarListasCombobox = "externo_controlador.php?acao=jurisprudencia@jurisprudencia/ajax_carregar_listas_pesquisa",
    `selOrigem[]` = "5", `selTipoDocumento[]` = "1", chkAgruparResultados = "on",
    rdoCampo = "I", txtProcesso = "", dtDecisaoInicio = "", dtDecisaoFim = "",
    hdnDecisaoInicio = "", hdnDecisaoFim = "", dtPublicacaoInicio = "",
    dtPublicacaoFim = "", hdnPublicacaoInicio = "", hdnPublicacaoFim = "")



url1 <- "https://eproc1g.tjsp.jus.br/eproc/externo_controlador.php?acao=jurisprudencia@jurisprudencia/listar_resultados"

r1 <- httr::POST(url1, body = body, encode = "form")
writeBin(r1$content, "data-raw/r1.html")