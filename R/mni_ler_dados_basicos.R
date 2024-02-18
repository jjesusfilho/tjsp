#' Lê dados básicos xml mni
#'
#' @param arquivos Vetor de xmls
#' @param diretorio Alternativamente informar diretório.
#'
#' @return tibble
#' @export
#'
mni_ler_dados_basicos <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)
  }

  purrr::map(arquivos, purrr::possibly(~{

    processo <- stringr::str_extract(.x, "\\d{20}")


    dados_basicos <- .x |>
      xml2::read_xml() |>
      xml2::xml_find_first("//ns2:dadosBasicos")

    competencia <- dados_basicos |>
      xml2::xml_attr("competencia")

    classe_processual <- dados_basicos |>
      xml2::xml_attr("classeProcessual")

    codigo_localidade <- dados_basicos |>
      xml2::xml_attr("codigoLocalidade")


    nivel_sigilo <- dados_basicos |>
      xml2::xml_attr("nivel_sigilo")

    intervencao_mp <- dados_basicos |>
      xml2::xml_attr("intervencaoMP") |>
      as.logical()

    data_ajuizamento <- dados_basicos |>
      xml2::xml_attr("dataAjuizamento") |>
      lubridate::ymd_hms(tz = "America/Sao_Paulo")

    assunto_principal <- dados_basicos |>
      xml2::xml_find_first("./ns2:assunto") |>
      xml2::xml_attr("principal") |>
      as.logical()

    assunto_codigo_nacional <- dados_basicos |>
      xml2::xml_find_first("./ns2:assunto/ns2:codigoNacional") |>
      xml2::xml_text()

    valor_causa <- dados_basicos |>
      xml2::xml_find_first("./ns2:valorCausa") |>
      xml2::xml_text()


    orgao_julgador_codigo <- dados_basicos |>
      xml2::xml_find_first("./ns2:orgaoJulgador") |>
      xml2::xml_attr("codigoOrgao")


    orgao_julgador_nome <- dados_basicos |>
      xml2::xml_find_first("./ns2:orgaoJulgador") |>
      xml2::xml_attr("nomeOrgao")

    orgao_julgador_instancia <- dados_basicos |>
      xml2::xml_find_first("./ns2:orgaoJulgador") |>
      xml2::xml_attr("instancia")

    orgao_julgador_codigo_municipio_ibge <- dados_basicos |>
      xml2::xml_find_first("./ns2:orgaoJulgador") |>
      xml2::xml_attr("codigoMunicipioIBGE")


    tibble::tibble(processo,competencia, classe_processual,
                   codigo_localidade,
                   nivel_sigilo,
                   intervencao_mp,
                   data_ajuizamento,
                   assunto_principal,
                   assunto_codigo_nacional,
                   valor_causa,
                   orgao_julgador_codigo,
                   orgao_julgador_nome,
                   orgao_julgador_instancia,
                   orgao_julgador_codigo_municipio_ibge
    )

  }, NULL), .progress = TRUE) |>
    purrr::list_rbind()


}
