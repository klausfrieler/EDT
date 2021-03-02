library(tidyverse)
library(psychTestR)
library(psychTestRCAT)

#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' EDT
#'
#' This function defines a EDT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the EDT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the EDT, consider using \code{\link{EDT_demo}()}.
#' For a standalone implementation of the EDT,
#' consider using \code{\link{EDT_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to  TRUE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to  TRUE
#' @param label (Character scalar) Label to give the EDT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param adaptive (Scalar boolean) Indicates whether you want to use the adaptive EDT2 (TRUE)
#' or the non-adaptive EDT (FASLE). Default is adaptive = TRUE.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export

EDT <- function(num_items_in_test = 18L,
                with_welcome = TRUE,
                with_finish = TRUE,
                label = "EDT",
                feedback = EDT_feedback_with_score(),
                dict = EDT::EDT_dict,
                adaptive = TRUE,
                next_item.criterion = "bOpt",
                next_item.estimator = "BM",
                next_item.prior_dist = "norm",
                next_item.prior_par = c(0, 1),
                final_ability.estimator = "WL",
                constrain_answers = FALSE
                ) {
  audio_dir <- "https://media.gold-msi.org/test_materials/EDT"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items_in_test) || purrr::is_scalar_double(num_items_in_test),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) EDT_welcome_page(),
    psychTestR::new_timeline(
      main_test(label = label,
                num_items_in_test = num_items_in_test,
                audio_dir = audio_dir,
                dict = dict,
                next_item.criterion = next_item.criterion,
                next_item.estimator = next_item.estimator,
                next_item.prior_dist = next_item.prior_dist,
                next_item.prior_par = next_item.prior_par,
                final_ability.estimator = final_ability.estimator,
                constrain_answers = constrain_answers,
                adaptive = adaptive
                ),
      dict = dict),
    if (!adaptive) scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) EDT_finished_page(),
    psychTestR::end_module())
}
