% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models-test_finalized_models.R
\name{evaluate_final_models}
\alias{evaluate_final_models}
\title{Evaluate Finalized Workflows on Hold-Out Data}
\usage{
evaluate_final_models(finalized_wf_sets, holdout_data)
}
\arguments{
\item{finalized_wf_sets}{A tibble of fitted workflows, typically from \code{full_model_evaluation()},
containing at least \code{fitted_wf} (workflow object) and \code{wflow_id} (identifier).}

\item{holdout_data}{A data frame containing \code{Sample_ID}, \code{Response}, and any required covariates
or predictors. Used for final model evaluation.}
}
\value{
A tibble with one row per model/workflow ID, containing:
\describe{
\item{wflow_id}{Identifier string describing the model configuration.}
\item{rsq}{Coefficient of determination (R²).}
\item{rmse}{Root Mean Squared Error.}
\item{rrmse}{Relative RMSE as a percentage.}
}
}
\description{
Applies each finalized model workflow to a held-out evaluation dataset, computes predictions,
performs any required back-transformations (e.g., inverse log or square-root), and evaluates
model performance using a standard set of regression metrics: R², RMSE, and RRMSE.
}
\details{
This function is designed to be called after \code{fit()} has been applied to workflows created
via \code{workflow_set()} and filtered using \code{filter_workflows()}. It assumes a consistent
structure of fitted workflows with a \code{Response} column as the target.
}
\examples{
\dontrun{
evaluate_final_models(finalized_wf_sets = finalized_models,
                      holdout_data = test_data)
}

}
\seealso{
\code{\link[yardstick]{metric_set}}, \code{\link[yardstick]{rsq}},
\code{\link[yardstick]{rmse}}, \code{\link{rrmse_vec}}, \code{\link{full_model_evaluation}}
}
\keyword{internal}
