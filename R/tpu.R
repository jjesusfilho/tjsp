#' @title tpu
#' @description Tabelas processuais unificadas do CNJ
#' @format dataframe com 7755 obs e 5 colunas:
#' \describe{
#'   \item{\code{cod_item}}{integer código do ítem}
#'   \item{\code{cod_item_pai}}{integer código do pai do ítem}
#'   \item{\code{tipo_tiem}}{character tipo do ítem: A, C, D ou M}
#'   \item{\code{nome}}{character autor do ato}
#'   \item{\code{situacao}}{character situação}
#' }
#' @source \url{https://www.cnj.jus.br/sgt/consulta_publica_classes.php}
"tpu"
