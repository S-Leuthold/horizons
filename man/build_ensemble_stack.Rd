% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stack-build_model_ensemble.R
\name{build_ensemble_stack}
\alias{build_ensemble_stack}
\title{Build, Blend, and Evaluate a Stacked Ensemble from Top Workflows}
\usage{
build_ensemble_stack(
  top_workflows,
  input_data,
  variable,
  test_prop = 0.2,
  cv_folds = 5
)
}
\arguments{
\item{input_data}{A \code{data.frame} or \code{tibble} containing the full dataset, including predictors and the outcome variable.}

\item{variable}{Character. The name of the outcome variable (unquoted) to use for stratified resampling and evaluation.}

\item{test_prop}{Numeric. Proportion of the data to allocate to the holdout test set (default = \code{0.2}).}

\item{cv_folds}{Integer. Number of cross-validation folds used for resampling during tuning (default = \code{5}).}

\item{top_workflows}{A tibble containing the candidate workflows. Must include columns:
\code{wflow_id} (character) and \code{workflow} (list-column of \code{workflow()} objects).}
}
\value{
A named list with the following elements:
\itemize{
\item \code{model_stack}: A fitted \code{stacks} ensemble model.
\item \code{predictions}: A tibble with observed and predicted values on the test set.
\item \code{evaluation_metrics}: A tibble of holdout performance metrics (RMSE, R², MAE, rRMSE).
\item \code{tuned_models}: A tibble of models successfully tuned and refit on CV resamples.
}
}
\description{
This function performs end-to-end ensemble modeling using a set of pre-defined workflows.
It splits the input dataset into training and testing subsets, performs stratified cross-validation
with Bayesian hyperparameter tuning for each workflow, refits each finalized model on resamples,
adds them to a stack, blends the stack using penalized regression, and evaluates predictions
on the holdout test set.
}
\details{
The function assumes that all workflows in \code{top_workflows} are complete and contain
both preprocessing and model specification steps. The response variable is renamed internally
to \code{Response} for compatibility.
}
\examples{
\dontrun{
model_results <- build_ensemble_stack(
  top_workflows = filtered_workflows,
  input_data    = my_data,
  variable      = "SOC_pct"
)

model_results$evaluation_metrics
model_results$predictions
}

}
\seealso{
\code{\link[stacks]{stacks}}, \code{\link[stacks]{add_candidates}},
\code{\link[stacks]{blend_predictions}}, \code{\link[stacks]{fit_members}},
\code{\link[tune]{tune_bayes}}, \code{\link[tune]{fit_resamples}},
\code{\link[yardstick]{rmse}}, \code{\link[rsample]{initial_split}}
}
