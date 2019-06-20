library(tidyverse)
library(psychTestR)
library(psychTestRCAT)
source("data_raw/RAT_dict.R")
source("data_raw/RAT_item_bank.R")
source("R/options.R")
source("R/practice.R")
source("R/instructions.R")
source("R/main_test.R")
source("R/item_page.R")
source("R/feedback.R")
source("R/utils.R")

#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' RAT
#'
#' This function defines a RAT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the RAT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#' For demoing the RAT, consider using \code{\link{RAT_demo}()}.
#' For a standalone implementation of the RAT,
#' consider using \code{\link{RAT_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param take_training (Logical scalar) Whether to include the training phase.
#' @param label (Character scalar) Label to give the RAT results in the output file.
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
RAT <- function(num_items = 15L,
                take_training = FALSE,
                with_welcome = TRUE,
                label = "RAT",
                feedback = RAT_feedback_with_score(),
                next_item.criterion = "bOpt",
                next_item.estimator = "BM",
                next_item.prior_dist = "norm",
                next_item.prior_par = c(0, 1),
                final_ability.estimator = "WL",
                constrain_answers = FALSE,
                dict = RAT::RAT_dict) {
  audio_dir = "http://media.gold-msi.org/test_materials/RAT2/audio"
  training_dir = "http://media.gold-msi.org/test_materials/RAT2/audio"
  img_dir = "http://media.gold-msi.org/test_materials/RAT2/img_inv"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_logical(take_training),
            purrr::is_scalar_character(audio_dir),
            purrr::is_scalar_character(training_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)
  training_dir <- gsub("/$", "", training_dir)
  img_dir <- gsub("/$", "", img_dir)

  psychTestR::new_timeline({
    c(
      if (with_welcome) psychTestR::new_timeline(
        psychTestR::one_button_page(
        body = shiny::div(
          shiny::h4(psychTestR::i18n("WELCOME"), style = "text-align:center"),
          shiny::p(psychTestR::i18n("INTRO"),
                   style = "margin-left:20%;margin-right:20%;text-align:justify")
         ),
         button_text = psychTestR::i18n("CONTINUE")
        ), dict = dict),

      if (take_training) psychTestR::new_timeline(instructions(audio_dir, img_dir), dict = dict),
      main_test(label = label, audio_dir = audio_dir, img_dir = img_dir, num_items = num_items,
                next_item.criterion = next_item.criterion,
                next_item.estimator = next_item.estimator,
                next_item.prior_dist = next_item.prior_dist,
                next_item.prior_par = next_item.prior_par,
                final_ability.estimator = final_ability.estimator,
                constrain_answers = constrain_answers),
      feedback
    )},
    dict = dict)
}
