% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forcasting_tools.R
\name{pred_keras}
\alias{pred_keras}
\title{Vector of predictions for keras \code{\link[=fit]{fit()}} models.}
\usage{
pred_keras(keras_model, test, name_response)
}
\arguments{
\item{keras_model}{Keras model object.}

\item{test}{Data frame.}

\item{name_response}{Name of response variable.}
}
\description{
Put data frame (train or test).
}
\seealso{
Other predicting functions: 
\code{\link{pred_glm_net}()},
\code{\link{pred_rpart}()},
\code{\link{predict_broom}()}
}
\concept{predicting functions}
