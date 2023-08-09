#' Baixar processos de segunda instância do tjsp
#'
#' @param processos processos de segunda instância
#' @param diretorio diretório
#'
#' @details
#' Alguns processos podem baixar também os incidentes ou
#'     dependências ou desdobramentos. O cd_processo serve
#'     para distingui-los. Baixe os seguintes processos para ver:
#'     c("10006289720178260673",
#'     "00000066120098260270",
#'     "15014338820228260617")
#'
#'
#' @return html com dados processuais
#' @export
#'
tjsp_baixar_cposg <- function(processos = NULL,
                         diretorio = ".") {

  httr::set_config(httr::config(ssl_verifypeer = FALSE))

  processos <- stringr::str_remove_all(processos, "\\D+") |>
    stringr::str_pad(width = 20, "left", "0") |>
    pontuar_cnj()

  uri1 <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  pb <- progress::progress_bar$new(total = length(processos))

  purrr::walk(processos, purrr::possibly(~{

    pb$tick()

  r <-  httr::GET("https://esaj.tjsp.jus.br/cposg/open.do?gateway=true")

     p <- .x


    unificado <- p |>
      stringr::str_extract(".{15}")

    foro <- p |>
      stringr::str_extract("\\d{4}$")

  query1<-  list(conversationId = "", paginaConsulta = "1", localPesquisa.cdLocal = "-1",
         cbPesquisa = "NUMPROC", tipoNuProcesso = "UNIFICADO", numeroDigitoAnoUnificado = unificado,
         foroNumeroUnificado = foro, dePesquisaNuUnificado = p,
         dePesquisa = "", uuidCaptcha = "", pbEnviar = "Pesquisar")

    resposta1 <- httr::RETRY("GET",
      url = uri1, query = query1,
      quiet = TRUE, httr::timeout(2)
    )

    conteudo1 <- httr::content(resposta1)

    if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='listagemDeProcessos'])")) {

   codigo_processo <-  xml2::xml_find_all(conteudo1, "//a[@class='linkProcesso']") |>
        xml2::xml_attr("href") |>
        stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

   conteudo1 <-   codigo_processo |>
     paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
        purrr::map(~ httr::RETRY("GET", .x, httr::timeout(2)) |>
          httr::content())

    } else if (xml2::xml_find_first(conteudo1, "boolean(//div[@id='modalIncidentes'])")){


       codigo_processo <- conteudo1 |>
         xml2::xml_find_all("//input[@id='processoSelecionado']") |>
         xml2::xml_attr("value")

      conteudo1 <- codigo_processo |>
            purrr::map(~{
                .x |>
               paste0("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo=", ... = _, "&gateway=true") |>
         httr::GET() |>
             httr::content()
    })
    } else {

      codigo_processo <- conteudo1 |>
                xml2::xml_find_first("//input[@name='cdProcesso']") |>
                 xml2::xml_attr("value")

     conteudo1 <- list(conteudo1)

    }

    processo <- .x |>
        stringr::str_remove_all("\\D+")

    arquivo <- file.path(diretorio, paste0("cposg_", format(Sys.Date(),
                                                  "%Y_%m_%d_"), processo,"_cd_processo_",codigo_processo,".html"))

    purrr::walk2(conteudo1, arquivo, ~xml2::write_html(.x, .y))

  }, NULL))
}

