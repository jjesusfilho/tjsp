#' Modelar decisão
#'
#' @param ... Fórmula conforme modelo
#' @param data base
#' @param y variável dependente
#' @param metodo c("glm","gbm","xgboost","rf","catboost")
#'
#' @return objeto caret
#' @export
#'
modelar_decisao <- function(...,data,y=NULL, metodo="glm"){

  if (metodo =="gbm"){

    grid <- expand.grid(interaction.depth=5, n.trees = 250,
                        shrinkage=0.1,
                        n.minobsinnode=10)
    metodo <- "gbm"

  } else if (metodo =='xgb') {

    metodo <- "xgbLinear"
    grid <- NULL


  } else if (metodo == "catboost") {

    grid <- expand.grid(depth = 8,
                        learning_rate =0.01,
                        iterations = 1000,
                        l2_leaf_reg = 0.1,
                        rsm = 0.5,
                        border_count = 64)

  } else

    grid <- NULL


  decisao <- rlang::enquo(y)

  data_split <- rsample::initial_split(data, strata = !!decisao)
  treino <- rsample::training(data_split)
  teste <- rsample::testing(data_split)

  x <- stats::model.matrix(!!decisao ~ ., data = treino)



  ctrl <- caret::trainControl(method = "repeatedcv", # Para resampling usa validação cruzada repetica
                              number = 10, ## Número de folds a serem computados
                              repeats = 5, ## Número de iterações
                              summaryFunction = caret::twoClassSummary, ## Função para computar métricas de desempenho na validação cruzada
                              classProbs = TRUE, ## Computa as probabilidades das classes/etiquetas
                              savePredictions = TRUE, ## salva as predições no resampling
                              sampling="down", ## Equilibra as classes para baixo
                              allowParallel = TRUE ## autoriza paralelização.
  )



  caret::train(x=x,y=decisao,trControl=ctrl)

}
