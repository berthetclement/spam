% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_tools.R
\name{make_train_test_list}
\alias{make_train_test_list}
\title{Make data set train/test.}
\usage{
make_train_test_list(data_df, seed_init = 1234, prop = 0.6667)
}
\arguments{
\item{data_df}{Data frame.}

\item{seed_init}{Put a value for seed.}

\item{prop}{Put a proportion for dimension train/test.}
}
\value{
List of data frame (train + test).
}
\description{
data set train/test with seed and prop (0.6).

Data input is data frame.
}
