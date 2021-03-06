% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_tools.R
\name{fit_keras}
\alias{fit_keras}
\title{Execute neuronal model with Keras}
\usage{
fit_keras(keras_model, train, name_response, nb_epochs, param_batch_size)
}
\arguments{
\item{keras_model}{Keras model object to fit.}

\item{train}{Data frame.}

\item{name_response}{Name of response variable.}

\item{nb_epochs}{Number of epochs.}

\item{param_batch_size}{Number of samples per gradient update.}
}
\description{
Type of model is sequential.
}
\seealso{
\code{\link[=fit]{fit()}}.

Other modeling functions: 
\code{\link{cv_glm_net}()},
\code{\link{def_model_keras}()},
\code{\link{glm_logit}()},
\code{\link{glm_net}()},
\code{\link{tree_rpart}()}
}
\concept{modeling functions}
