

# construire un train/test a partir d'un data frame ----

#' @export
#' @title Make data set train/test.
#' @description data set train/test with seed and prop (0.6).
#'
#' Data input is data frame.
#' @param data_df Data frame.
#' @param seed_init Put a value for seed.
#' @param prop Put a proportion for dimension train/test.
#' @return List of data frame (train + test).

make_train_test_list <- function(data_df, seed_init=1234, prop=0.6667){
 index <- seq_len(nrow(data_df))
 set.seed(seed_init)

 trainindex <- sample(index, trunc(length(index) * prop))

 res <- list(train=data_df[trainindex,],
             test=data_df[-trainindex,])
 res
}



#' @export
#' @title glm model
#' @description Fitting Generalized Linear Models.
#' Glm is used to fit generalized linear models \code{...}
#' @param data_df Model data, class "data.frame".
#' @param y_name The variable cible (Y) in characters.
#' @param x_name List of explanatory variables (list of characters).
#' @param ... Add arguments from [glm()].
#' @importFrom stats glm as.formula binomial
#' @family modeling functions
#' @seealso [glm()] for initial function
#' @examples
#'nom_col <- setdiff(names(iris), "Species")
#'glm_logit(iris, "Species", nom_col)

# modele logistique glm_logit ----
glm_logit <- function(data_df, y_name, x_name, ...) {
 mod_formula <- sprintf(paste0(y_name, "~ %s"), paste0(x_name, collapse = "+"))

 reg <- glm(as.formula(mod_formula),
            family = binomial(link="logit"), data=data_df, ...)
 reg
}


#' @export
#' @title glm model summary
#' @description Quelques infos sur le modele.
#' La convergence du modele, le R2 de Mc Fadden et la distribution $p_{xi}$ \code{...}
#' @param modele Object type glm.
#' @param train_response La variable reponse du train (spam de type facteur).
#' @family Analyse modele glm_logit.
#' @return List of 3 elements : boolean, num et data frame.

analyse_modele <- function(modele, train_response){
        converge <- modele$converged
        R2.mf = 1-(modele$deviance/modele$null.deviance)
        df <- data.frame(
                modele$fitted.values, train_response
        )
        names(df) <- c("pi", "spam")

        l <- list(conv = converge,
                  r2 = R2.mf,
                  df_pi = df)
        l

}



#' @export
#' @title Matrice de confusion.
#' @description Sensibilite, specificite \code{...}
#' @param var_response Vecteur de la variable reponse sur le test (type factor pour [table()]).
#' @param var_estime Vecteur des Y estimés sur test.
#' @param nb_individus Longueur du data frame test (nombre reel).
#' @family Analyse modele glm_logit.
#' @return Data frame.

conf_mat <- function(var_response, var_estime, nb_individus){
        reco <- ifelse(var_estime >.5, "spam","mail")

        confusion.mat = table(var_response, reco)

        fauxneg = confusion.mat[2,1]
        fauxpos = confusion.mat[1,2]
        vraisneg = confusion.mat[1,1]
        vraispos = confusion.mat[2,2]
        txerr = (fauxneg+fauxpos) / nb_individus
        sensibilite <- vraispos / (vraispos + fauxneg)
        precision <- vraispos / (vraispos + fauxpos)
        specificite <- vraisneg / (vraisneg + fauxpos)

        df <- data.frame(fauxneg,
                         fauxpos ,
                         vraisneg ,
                         vraispos ,
                         txerr ,
                         sensibilite ,
                         precision ,
                         specificite
        )
        df
}



#' @export
#' @title Donnees de la matrice de confusion [flextable()].
#' @description Pour affichage des valeurs dans un tableau.
#' @param df_conf_mat Data frame.
#' @family Analyse modele glm_logit.
#' @importFrom flextable flextable
#' @return Un tableau en image (viewer) et un objet de type list().

ft_conf_tab <- function(df_conf_mat){
        flextable(round(df_conf_mat,3))
}


# glm net ----

#' @export
#' @title Cross-validation for glmnet
#' @description Does k-fold cross-validation for glmnet, produces a plot \code{...}
#' By default, nfolds = 10
#' @param data_df Model data, class "data.frame".
#' @param y_name The variable cible (Y) in characters.
#' @param x_name List of explanatory variables (list of characters).
#' @param alpha_ 0 for Ridge and 1 for Lasso.
#' @param ... Add arguments from [cv.glmnet()].
#' @importFrom glmnet cv.glmnet
#' @importFrom  stats model.matrix as.formula
#' @family modeling functions
#' @seealso [cv.glmnet()] for initial function

cv_glm_net <- function(data_df, y_name, x_name, alpha_, ...) {
        # x matrix pour glmnet
        mod_formula <- sprintf(paste0(y_name,"~ %s"), paste0(x_name, collapse = "+"))

        x <- model.matrix(as.formula(mod_formula), data_df)[,-1]

        cv_modele <- cv.glmnet(x, data_df$spam, alpha = alpha_, type.measure = "class",
                               family="binomial", ...)

        cv_modele
}



#' @export
#' @title fit a GLM with Lasso or Ridge or elasticnet regularization
#' @description Fit a generalized linear model via penalized maximum likelihood \code{...}
#'
#' @param data_df Model data, class "data.frame".
#' @param y_name The variable cible (Y) in characters.
#' @param x_name List of explanatory variables (list of characters).
#' @param alpha_ 0 for Ridge and 1 for Lasso.
#' @param lambda_cv Lambda min from [cv_glm_net()]
#' @param ... Add arguments from [glmnet()].
#' @importFrom glmnet glmnet
#' @importFrom  stats model.matrix as.formula
#' @family modeling functions
#' @seealso [glmnet()] for initial function

glm_net <- function(data_df, y_name, x_name, alpha_, lambda_cv, ...){

        mod_formula <- sprintf(paste0(y_name,"~ %s"), paste0(x_name, collapse = "+"))

        x <- model.matrix(as.formula(mod_formula), data_df)[,-1]

        fit_lasso <-glmnet(x, data_df$spam, alpha = alpha_, lambda = lambda_cv,
                           family="binomial", ...)
        fit_lasso

}


# rpart ----

#' @export
#' @title Recursive Partitioning and Regression Trees
#' @description Fit a rpart model then prune.rpart {rpart} model.
#'
#' @param data_df Model data, class "data.frame".
#' @param y_name The variable cible (Y) in characters.
#' @param x_name List of explanatory variables (list of characters).
#' @param ... Add arguments from [rpart()].
#' @importFrom rpart rpart rpart.control prune
#' @importFrom  stats model.matrix as.formula
#' @family modeling functions
#' @seealso [rpart()] and [prune()]for initial function

tree_rpart <- function(data_df, y_name, x_name, ...){
        mod_formula <- sprintf(paste0(y_name, "~ %s"), paste0(x_name, collapse = "+"))

        fit <- rpart(as.formula(mod_formula), data_df,
                     control=rpart.control(minsplit=5,cp=0), ...)
        # 1-se
        se = min(fit$cptable[, "xerror"]) + (1*fit$cptable[ which.min(fit$cptable[, "xerror"]), "xstd"])

        cp_opti <- fit$cptable[fit$cptable[,"xerror"]<se , "CP"][1]

        # prune
        fit_prune = rpart::prune(fit, cp=cp_opti)

        fit_prune

}


#' @export
#' @title Matrice de confusion pour rpart.
#' @description Sensibilite, specificite \code{...}
#' @param var_response Vecteur de la variable reponse sur le test (type factor pour [table()]).
#' @param class_estime Vecteur (factor) des Y estimés sur test.
#' @param nb_individus Longueur du data frame test (nombre reel).
#' @return Data frame.

conf_mat_rpart <- function(var_response, class_estime, nb_individus){

        confusion.mat = table(var_response, class_estime)

        fauxneg = confusion.mat[2,1]
        fauxpos = confusion.mat[1,2]
        vraisneg = confusion.mat[1,1]
        vraispos = confusion.mat[2,2]
        txerr = (fauxneg+fauxpos) / nb_individus
        sensibilite <- vraispos / (vraispos + fauxneg)
        precision <- vraispos / (vraispos + fauxpos)
        specificite <- vraisneg / (vraisneg + fauxpos)

        df <- data.frame(fauxneg,
                         fauxpos ,
                         vraisneg ,
                         vraispos ,
                         txerr ,
                         sensibilite ,
                         precision ,
                         specificite
        )
        df
}


# neurones KERAS ----

#' @export
#' @title Define neuronal model with Keras
#' @description Type of model is sequential.
#'
#' @param input_neuro Number to define "input shape".
#' @param nb__neuro_cc Number of neurons of blind layer.
#' @param fct_activ Name of activation function.
#' @param name_loss Name of loss function.
#' @param name_optimizer Name of optimizer parameter.
#' @param name_metrics Name of metrics.
#' @importFrom keras keras_model_sequential compile layer_dense
#' @importFrom magrittr "%>%"
#' @family modeling functions
#' @seealso [keras_model_sequential()], [compile.keras.engine.training.Model()].

# neurone statique a 1 cc

def_model_keras <- function(input_neuro, nb__neuro_cc, fct_activ,
                            name_loss, name_optimizer, name_metrics){


        fit_k <- keras_model_sequential() %>%
                layer_dense(units=nb__neuro_cc,input_shape=c(input_neuro),activation=fct_activ) %>%
                layer_dense(units = 1, activation = fct_activ)

        # Compiling is done with the compile function:

        fit_k %>%
                compile(
                        loss = name_loss,
                        optimizer = name_optimizer,
                        metrics = name_metrics
                )

        fit_k

}


#' @export
#' @title Execute neuronal model with Keras
#' @description Type of model is sequential.
#'
#' @param keras_model Keras model object to fit.
#' @param train Data frame.
#' @param name_response Name of response variable.
#' @param nb_epochs Number of epochs.
#' @param param_batch_size Number of samples per gradient update.
#' @importFrom keras fit
#' @importFrom magrittr "%>%"
#' @importFrom stats model.matrix as.formula
#' @family modeling functions
#' @seealso [fit()].

# execution modele

fit_keras <- function(keras_model, train, name_response, nb_epochs, param_batch_size){

        nom_col <- setdiff(names(train), name_response)
        mod_formula <- sprintf(paste0(name_response,"~ %s"), paste0(nom_col, collapse = "+"))

        x_train <- model.matrix(as.formula(mod_formula), train)[,-1]
        # Outcome variable
        y_train <- as.numeric(levels(train$spam))[train$spam]

        keras_model %>%
                fit(
                        x = x_train, y = y_train,
                        epochs=nb_epochs,
                        batch_size=param_batch_size
                )
}




