#' Lê e processa um arquivo mp3.
#'
#' Le um arquivo mp3, com o pacote \code{tuneR}.
#'
#' @param arq_mp3 arquivo mp3 a ser lido.
#'
#' @return um vetor de inteiros com a soma dos valores absolutos das frequencias
#' de cada trecho identificado. Esses numeros podem ser considerados como
#' uma fingerprint da letra, pois nenhuma outra letra possui a mesma soma
#' de frequencias.
#'
#' @export
ler <- function(arq_mp3) {
  x <- tuneR::readMP3(arq_mp3)@left
  colado <- paste(x, collapse = ',')
  spl_tot <- strsplit(colado,  '(0,){50,}')[[1]]
  spl_num <- lapply(spl_tot, function(x) as.numeric(unlist(strsplit(x, ','))))
  spl_sum <- sapply(spl_num, function(x) sum(x), USE.NAMES = FALSE)
  spl_sum <- abs(spl_sum[-length(spl_sum)][-1])
  spl_sum <- spl_sum[spl_sum > 1000]
  spl_sum
}


#' Decifra o audio
#'
#' Le um arquivo de audio e tenta decifrar quais sao as letras.
#'
#' @param arq_mp3 arquivo que contém o audio.
#'
#' @return character vector de tamanho 1 com as letras preditas.
#'
#' @export
decifrar <- function(arq_mp3) {
  somas <- as.character(ler(arq_mp3))
  paste(as.character(tab[somas]), collapse = '')
}
