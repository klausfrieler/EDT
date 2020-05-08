library(tidyverse)
library(psychTestR)
library(psychTestRCAT)
source("data_raw/EDT_dict.R")
source("data_raw/EDT_item_bank.R")
source("R/options.R")
#source("R/practice.R")
#source("R/instructions.R")
source("R/main_test.R")
source("R/item_page.R")
source("R/feedback.R")
source("R/utils.R")

#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' EDT
#'
#' This function defines a EDT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the EDT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the EDT, consider using \code{\link{EDT_demo}()}.
#' For a standalone implementation of the EDT,
#' consider using \code{\link{EDT_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param take_training (Logical scalar) Whether to include the training phase.
#' @param label (Character scalar) Label to give the EDT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param next_item.criterion (Character scalar)
#' Criterion for selecting successive items in the adaptive test.
#' See the \code{criterion} argument in \code{\link[catR]{nextItem}} for possible values.
#' Defaults to \code{"bOpt"}.
#' @param next_item.estimator (Character scalar)
#' Ability estimation method used for selecting successive items in the adaptive test.
#' See the \code{method} argument in \code{\link[catR]{thetaEst}} for possible values.
#' \code{"BM"}, Bayes modal,
#' corresponds to the setting used in the original MPT paper.
#' \code{"WL"}, weighted likelihood,
#' corresponds to the default setting used in versions <= 0.2.0 of this package.
#' @param next_item.prior_dist (Character scalar)
#' The type of prior distribution to use when calculating ability estimates
#' for item selection.
#' Ignored if \code{next_item.estimator} is not a Bayesian method.
#' Defaults to \code{"norm"} for a normal distribution.
#' See the \code{priorDist} argument in \code{\link[catR]{thetaEst}} for possible values.
#' @param next_item.prior_par (Numeric vector, length 2)
#' Parameters for the prior distribution;
#' see the \code{priorPar} argument in \code{\link[catR]{thetaEst}} for details.
#' Ignored if \code{next_item.estimator} is not a Bayesian method.
#' The dfeault is \code{c(0, 1)}.
#' @param final_ability.estimator
#' Estimation method used for the final ability estimate.
#' See the \code{method} argument in \code{\link[catR]{thetaEst}} for possible values.
#' The default is \code{"WL"}, weighted likelihood.
#' #' If a Bayesian method is chosen, its prior distribution will be defined
#' by the \code{next_item.prior_dist} and \code{next_item.prior_par} arguments.
#' @param constrain_answers (Logical scalar)
#' If \code{TRUE}, then item selection will be constrained so that the
#' correct answers are distributed as evenly as possible over the course of the test.
#' We recommend leaving this option disabled.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
EDT <- function(num_items = 18L,
                with_welcome = TRUE,
                with_finish = TRUE,
                label = "EDT",
                feedback = EDT_feedback_with_score(),
                dict = EDT::EDT_dict) {
  audio_dir <- "https://media.gold-msi.org/test_materials/EDT"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) EDT_welcome_page(),
    psychTestR::new_timeline({
      main_test(label = label, num_items_in_test = num_items, audio_dir = audio_dir, dict = dict)
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) EDT_finished_page(),
    psychTestR::end_module())
}
