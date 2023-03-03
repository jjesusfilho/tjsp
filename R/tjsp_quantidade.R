#' Calcula quantidade de processos distribuídos por fórum
#'
#' @param inicio Número inicial da sequência. Depende muito do
#'     distribuidor e da classe processual.
#' @param fim Inteiro grande o suficiente
#' @param ano Ano
#' @param distribuidor Verifique os códigos em codigos2
#' @param diretorio Um diretório vazio para downloads provisórios
#'
#' @return vetor
#' @export
#'
tjsp_quantidade <-
  function(inicio = NULL,
           fim=NULL,
           ano = NULL,
           distribuidor = NULL,
           diretorio = ".") {
    ## Para encontrar o maior número do processo do ano, eu usei a lógica da busca binária.
    ## fim pode ser qualquer número grande o bastante para ser superior ao número total de processos
    ## distribuídos.


    # O loop abaixo faz requisição ao modo de busca binária. Pode haver uma pequena diferença de 2.

    while (`-`(fim, inicio) > 7) {
      inicio <- mean(c(inicio,fim)) ## Calculo a média, mas não vejo necessidade de arrendondar.


      # Todas as funções para baixar htmls dos processos, de todos os pacotes,
      # possuem um argumento para o vetor de processos (ids) e outro para o
      # diretório ou path. Assim, criamos um diretorio temporário para guardar
      # os arquivos:



      ## Criamos um intervalo de oito números em torno de y
      ## para assegurar que ao menos um deles existirá caso o último seja
      ## superior ou igual a y.
      intervalo <- round(inicio + -3:3) %>%
        range()

      ## aqui eu uso a função cnj_sequencial para criar a numeracao conforme o CNJ,
      ## aplico a função para baixar e verifico se os cinco são simultaneamente nulos,
      ## somando os objetos lógicos. Se a soma for cinco, ou seja, TRUE, TRUE, TRUE, TRUE, TRUE
      ## o último processo é menor que inicio.


     cnj_sequencial(intervalo[1], intervalo[2], ano, segmento =8, uf=26, distribuidor) %>%
        tjsp_baixar_cpopg(diretorio = diretorio)

    arquivos <- list.files(diretorio, full.names = TRUE)

    ### Retorna 1 se todos os arquivos forem do mesmo tamanho. Se forem, tudo indica que são vazios.

    tamanhos_iguais <- arquivos |>
      file.size() |>
      unique() |>
      length()



      file.remove(arquivos) ## remove o diretório.

      ## Se inicio for maior que o último processo, substituímos inicio atual pelo y anterior,
      ## e fim se torna o atual inicio, isto é a média entre inicio e fim.
      ## Se o último for maior que inicio, fim é preservado e inicio passa a ser
      ## a média entre inicio e fim.

      ### Se todos estão vazios, o fim cai pela metade e o inicio volta a ser o que era imediamente antes.

      if (tamanhos_iguais == 1) {
        inicio <- inicio - (fim - inicio)
        fim <- mean(c(inicio, fim))
      }

    }

    return(inicio)
  }
