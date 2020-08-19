
# prediction glm ----
#' @export
#' @title Generalised predict function
#' @description Generic function of prediction with function [augment()] of package {broom}.
#' @seealso [augment()] for initial function.
#' @param modele broom-compatible model.
#' @param data model data, class "data.frame".
#' @param new_data Put data frame "test".
#' @param type Default at "response" or see [predict.glm()], the type of prediction required.
#' @importFrom  broom augment
#' @family predicting functions
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


# prediction glmnet ----

#' @export
#' @title Generalised predict function for [glmnet()] models.
#' @description Put data frame (train or test).
#' @param modele glmnet objects.
#' @param data_df Data, class "data.frame".
#' @param y_name The variable cible (Y) in characters.
#' @param x_name List of explanatory variables (list of characters).
#' @importFrom  stats model.matrix as.formula predict
#' @family predicting functions

pred_glm_net <- function(data_df, y_name, x_name, modele){
 mod_formula <- sprintf(paste0(y_name,"~ %s"), paste0(x_name, collapse = "+"))

 x <- model.matrix(as.formula(mod_formula), data_df)[,-1]

 pred <- predict(modele, x, type="response")
 pred
}


# prediction rpart ----

#' @export
#' @title Vector of predictions for [rpart()] models.
#' @description Put data frame (train or test).
#' @param data_df Data, class "data.frame".
#' @param modele rpart objects.
#' @importFrom  stats predict
#' @family predicting functions

pred_rpart <- function(data_df, modele){

 pred <- predict(modele, data_df, type="class")
 pred
}


# prediction KERAS ----

#' @export
#' @title Vector of predictions for keras [fit()] models.
#' @description Put data frame (train or test).
#' @param keras_model Keras model object.
#' @param test Data frame.
#' @param name_response Name of response variable.
#' @importFrom  stats model.matrix as.formula predict
#' @importFrom magrittr "%>%"
#' @family predicting functions

pred_keras <- function(keras_model, test, name_response){

  nom_col <- setdiff(names(test), name_response)
  mod_formula <- sprintf(paste0(name_response,"~ %s"), paste0(nom_col, collapse = "+"))
  x_test <- model.matrix(as.formula(mod_formula), test)[,-1]

  pred <- keras_model %>%
    predict(x_test)

  pred
}



