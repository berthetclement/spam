
# prediction avec broom ----
#' @export
#' @title Generalised predict function
#' @description Generic function of prediction with function [augment()] of package {broom}.
#' @seealso [augment()] for initial function.
#' @param modele broom-compatible model.
#' @param data model data, class "data.frame".
#' @param new_data Put data frame "test".
#' @param type Default at "response" or see [predict.glm()], the type of prediction required.
#' @importFrom  broom augment
#' @examples
#' library(broom)
#' fit <- glm(Ozone~., data= airquality)
#' predict_broom(modele= fit, data= airquality, new_data= airquality[2:50,], type= "response")

# Fonction pour généralisation de la prédiciton
# broom ne marche pas avec RandomForest
# non prévue pour pour modele Arima()
# non prévu pour neurones package ANN2

predict_broom <- function(modele, data, new_data, type="response"){

  broom::augment(modele, data, newdata=new_data, type.predict=type)

}






