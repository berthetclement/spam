
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
            family = binomial(link="logit"), data=data_df)
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

        cv_modele <- cv.glmnet(x, data_df$spam, alpha = alpha_, type.measure = "class", family="binomial")

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

        fit_lasso <-glmnet(x, data_df$spam, alpha = alpha_, lambda = lambda_cv, family="binomial")
        fit_lasso

}


