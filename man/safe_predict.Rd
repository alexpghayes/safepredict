% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safepredict-package.R
\name{safe_predict}
\alias{safe_predict}
\title{Safely predict from a model object}
\usage{
safe_predict(object, new_data, type = NULL, ..., level = 0.95,
  std_error = FALSE)
}
\arguments{
\item{object}{An object or model you would like to get predictions from.}

\item{new_data}{\strong{Required}. Data in the same format as required for
the \code{predict()} (or relevant method) for \code{object}. We do our best to
support missing data specified via \code{NA}s in \code{new_data} although this
is somewhat dependent on the underlying \code{predict()} method.

\code{new_data}:
\itemize{
\item does not need to contain the model outcome
\item can contain additional columns not used for prediction
\item can have one or more rows when specified via a table-like object such
as a \code{\link[tibble:tibble]{tibble::tibble()}}, \code{\link[=data.frame]{data.frame()}} or \code{\link[=matrix]{matrix()}}.
}}

\item{type}{A character vector indicating what kind of predictions you
would like.

Options are:
\itemize{
\item \code{"response"}: continuous/numeric predictions
\item \code{"class"}: hard class predictions
\item \code{"prob"}: class or survival probabilities
\item \code{"link"}: predictors on the linear scale (GLMs only)
\item \code{"conf_int"}: confidence intervals for means of continuous predictions
\item \code{"pred_int"}: prediction intervals for continuous outcomes
}

In most cases, only a subset of these options are available.}

\item{...}{Unused. \code{safe_predict()} checks that all arguments in \code{...} are
evaluated via the \code{ellipsis} package. The idea is to prevent silent errors
when arguments are mispelled. This feature is experimental and feedback
is welcome.}

\item{level}{A number strictly between \code{0} and \code{1} to use as the
confidence level when calculating confidence and prediction intervals.
Setting \code{level = 0.90} correspondings to a 90 percent confidence interval.
Ignored except when \code{type = "conf_int"} or \code{type = "pred_int"}. Defaults
to \code{0.95}.}

\item{std_error}{Logical indicating whether or not calculate standard
errors for the fit at each point. Not available for all models, and can
be computationally expensive to compute. The standard error is \emph{always}
the standard error for the mean, and \emph{never} the standard error for
predictions. Standard errors are returned in a column called \code{.std_error}.
Defaults to \code{FALSE}.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} with one row for each row of \code{new_data}.
Predictions for observations with missing data will be \code{NA}. Returned
tibble has different columns depending on \code{type}:
\itemize{
\item \code{"response"}:
\itemize{
\item univariate outcome: \code{.pred} (numeric)
\item multivariate outcomes: \code{.pred_{outcome name}} (numeric) for each
outcome
}
\item \code{"class"}: \code{.pred_class} (factor)
\item \code{"prob"}: \code{.pred_{level}} columns (numerics between 0 and 1)
\item \code{"link"}: \code{.pred} (numeric)
\item \code{"conf_int"}: \code{.pred}, \code{.pred_lower}, \code{.pred_upper} (all numeric)
\item \code{"pred_int"}: \code{.pred}, \code{.pred_lower}, \code{.pred_upper} (all numeric)
}

If you request standard errors with \code{std_error = TRUE}, an additional
column \code{.std_error}.

For interval predictions, the tibble has additional attributes \code{level}
and \code{interval}. The \code{level} is the same as the \code{level} argument and is
between 0 and 1. \code{interval} is either \code{"confidence"} or \code{"prediction"}.
Some models may also set a \code{method} attribute to detail the method
used to calculate the intervals.
}
\description{
Safely predict from a model object
}
\section{Confidence vs prediction intervals}{


For details on the difference between confidence and prediction intervals,
see the \href{TODO}{online documentation}. This is also available as a vignette
that you can access with:\preformatted{vignette("intervals", package = "safepredict")
}
}

\section{Factors and novel factor levels}{


We recommend using \code{\link[recipes:recipe]{recipes::recipe()}}s to consistently handle categorical
(factor) predictors. For details see:\preformatted{vignette("novel-factor-levels", package = "safepredict")
}

Currently we do not have a robust way to check for novel factor levels
from within \code{safepredict}. In practice this would require storing
information about predictors in the model object at fitting time,
and \code{safepredict} is largely at the mercy of package writers in
that regard. Using \code{recipes} to preprocess your data should take care
of the issue.
}

\section{Recommended implementations}{


The goal of \code{safepredict} is to make prediction as painless and consistent
as possible across a wide variety of model objects. In some cases, the
existing intrafrastructure is insufficient to provide a consistent and
feature-rich prediction interface. As a result, we support a number of
model objects that we do not actually recommend using. In these cases,
we try to link to better and more feature-rich implementations.
}

