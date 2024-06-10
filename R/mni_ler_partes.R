#' Lê informações das partes do xml baixado com mni_consultar_processo
#'
#' @param arquivos Vetor de arquivos xml.
#' @param diretorio Alternativamente informar o diretório.
#'
#' @return Tibble
#' @export
#'
mni_ler_partes <- function(arquivos = NULL, diretorio = "."){
  
  
  if(is.null(arquivos)){
    
    arquivos <- list.files(diretorio, full.names = TRUE)
  }
  
  
  purrr::map(arquivos, ~{
    
    processo <- stringr::str_extract(.x, "\\d{20}")
    
    polos <- xml2::read_xml(.x) |>
      xml2::xml_find_all("//ns2:polo") |>
      xml2::xml_attr("polo")
    
    x <- .x |>
      xml2::read_xml()
    
    purrr::map(polos, ~{
      
      xpath <- glue::glue("//ns2:polo[@polo='{.x}']")
      
      p1 <- x |>
        xml2::xml_find_first(xpath)
      
      polo <- .x
      
     k <- p1 |>
        xml2::xml_find_all("./ns2:parte") |>
        
        purrr::imap(~{
          
          assistencia_judiciaria <- .x |>
            xml2::xml_find_first("./@assistenciaJudiciaria") |>
            xml2::xml_text()
          
          
          intimacao_pendente <- .x |>
            xml2::xml_find_first("./@intimacaoPendente") |>
            xml2::xml_text()
          
          
          
          
          relacionamento_processual <- .x |>
            xml2::xml_find_first("./@relacionamentoProcessual") |>
            xml2::xml_text()
          
          
          nome <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("nome")
          
          sexo <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("sexo")
          
          nome_genitor <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("nomeGenitor")
          
          nome_genitora <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("nomeGenitora")
          
          data_nascimento <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("dataNascimento") |>
            lubridate::ymd()
          
          numero_documento_principal <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("numeroDocumentoPrincipal")
          
          tipo_pessoa <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("tipoPessoa")
          
          cidade_natural <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("cidadeNatural")
          
          nacionalidade <- .x |>
            xml2::xml_find_first("./ns2:pessoa") |>
            xml2::xml_attr("nacionalidade")
          
          
          codigo_documento <- .x |>
            xml2::xml_find_first("./ns2:pessoa/ns2:documento") |>
            xml2::xml_attr("codigoDocumento")
          
          
          
          emissor_documento <- .x |>
            xml2::xml_find_first("./ns2:pessoa/ns2:documento") |>
            xml2::xml_attr("emissorDocumento")
          
          
          
          tipo_documento <- .x |>
            xml2::xml_find_first("./ns2:pessoa/ns2:documento") |>
            xml2::xml_attr("tipoDocumento")
          
          
          endereco <- .x |>
            xml2::xml_find_all("./ns2:pessoa/ns2:endereco")
          
          
          endereco_variavel <- endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_find_all("./*") |>
                xml2::xml_name()
              
            })
          
          endereco_valor <- endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_find_all("./*") |>
                xml2::xml_text()
              
            })
          
          
          cep <- endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_attr("cep")
              
            })
          
          
          end <- purrr::pmap_chr(list(x = endereco_variavel, y=  endereco_valor, z = cep), function(x,y,z){
            
            purrr::map2_chr(x,y, ~stringr::str_c(.x, .y, sep = ": ")) |>
              stringr::str_c(collapse = ", "  ) |>
              stringr::str_c(dplyr::coalesce(z, ""), sep = ", cep: ")
            
          } ) |> 
            stringr::str_remove("logradouro: ")
          
          advogado_nome <- .x |>
            xml2::xml_find_all("./ns2:advogado") |>
            xml2::xml_attr("nome")
          
          advogado_inscricao <- .x |>
            xml2::xml_find_all("./ns2:advogado") |>
            xml2::xml_attr("inscricao")
          
          advogado_numero_documento_principal <-  .x |>
            xml2::xml_find_all("./ns2:advogado") |>
            xml2::xml_attr("numeroDocumentoPrincipal")
          
          
          advogado_intimacao <-  .x |>
            xml2::xml_find_all("./ns2:advogado") |>
            xml2::xml_attr("intimacao") |> 
            as.logical()
          
          advogado_tipo_representante <-  .x |>
            xml2::xml_find_all("./ns2:advogado") |>
            xml2::xml_attr("tipoRepresentante")
          
          
          
          advogado_endereco <- .x |>
            xml2::xml_find_all("./ns2:advogado/ns2:endereco")
          
          
          advogado_endereco_variavel <- advogado_endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_find_all("./*") |>
                xml2::xml_name()
              
            })
          
          advogado_endereco_valor <- advogado_endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_find_all("./*") |>
                xml2::xml_text()
              
            })
          
          
          advogado_cep <- advogado_endereco |>
            purrr::map(~{
              .x |>
                xml2::xml_attr("cep")
              
            })
          
          advogado_end <- purrr::pmap_chr(list(x = advogado_endereco_variavel, y=  advogado_endereco_valor, z = advogado_cep), function(x,y,z){
            
            purrr::map2_chr(x,y, ~stringr::str_c(.x, .y, sep = ": ")) |>
              stringr::str_c(collapse = ", "  ) |>
              stringr::str_c(dplyr::coalesce(z, ""), sep = ", cep: ")
            
          } ) |> 
            stringr::str_remove("logradouro: ")
          
          
        list(polo = polo, indice = .y, assistencia_judiciaria = assistencia_judiciaria,
                        intimacao_pendente = intimacao_pendente, 
                        relacionamento_processual = relacionamento_processual,
                         nome  = nome, sexo  = sexo,nome_genitor = nome_genitor, nome_genitora = nome_genitora, 
                       data_nascimento = data_nascimento, numero_documento_principal = numero_documento_principal,
                       tipo_pessoa = tipo_pessoa, cidade_natural = cidade_natural, 
                    nacionalidade  = nacionalidade,
                    codigo_documento = codigo_documento,
                    emissor_documento = emissor_documento,
                    tipo_documento = tipo_documento,
                    endereco = list(end) |> purrr::map_if(rlang::is_empty, \(x) NA_character_),
                    advogado_nome = advogado_nome,
                    advogado_inscricao = advogado_inscricao, 
                    advogado_intimacao = advogado_intimacao,
                    advogado_tipo_representante = advogado_tipo_representante,
                    advogado_endereco = list(advogado_end) |> purrr::map_if(rlang::is_empty, \(x) NA_character_)) |> 
           purrr::map_if(rlang::is_empty, \(x) NA) |> 
           tibble::as_tibble()
           
         
          
        }) |>
        purrr::list_rbind()
    }) |>
      purrr::list_rbind() |>
      tibble::add_column(processo = processo, .before = 1)
    
  }, .progress = TRUE) |>
    purrr::list_rbind()
  
  
}
