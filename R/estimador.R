#' Estima quantidade de processos com base na api do tjsp
#'
#' @param inicio Valor inicial
#' @param fim Valor final
#' @param ano Ano
#' @param distribuidor código do distribuidor
#'
#' @return valor
#'
tjsp_api_quantidade <- function (inicio = NULL, fim = NULL, ano = NULL, distribuidor = NULL)

{


  while (fim - inicio > 7) {

    inicio <- mean(c(inicio, fim))

    intervalo <- round(inicio + -3:3) %>% range()

    df <-   cnj_sequencial(intervalo[1], intervalo[2], ano, segmento = 8,
                           uf = 26, distribuidor) %>%
      tjsp_api_extrair()


    if (nrow(df) == 0) {
      inicio <- inicio - (fim - inicio)
      fim <- mean(c(inicio, fim))
    }
  }
  return(inicio)
}



#' Extrai dataframe da api do TJSP, função auxiliar.
#'
#' @param processos Vetor de processos
#'
#' @return data.frame
#'
tjsp_api_extrair <- function(processos){



  purrr::map_dfr(processos, ~{

    paste0("https://api.tjsp.jus.br/processo/cpopg/search/numproc/",.x) |>
      jsonlite::fromJSON()


  })

}
