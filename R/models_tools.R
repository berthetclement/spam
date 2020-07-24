
# construire un train/test a partir d'une data frame ----

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




