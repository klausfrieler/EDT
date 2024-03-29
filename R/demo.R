#' Demo EDT
#'
#' This function launches a demo for the EDT.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgoldstudy@gmail.com},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}), German (\code{"de"}), formal German (\code{"de_f"}), Russian (\code{"ru"}), Nederlands (\code{"nl"}), Espanol \code{"es"}, and Italiano (\code{"it"}).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{EDT}()}.
#' @export
#'
EDT_demo <- function(num_items = 3L,
                     feedback = EDT::EDT_feedback_with_score(),
                     admin_password = "demo",
                     researcher_email = "longgoldstudy@gmail.com",
                     dict = EDT::EDT_dict,
                     language = "en",
                     adaptive = TRUE,
                     ...) {
  elts <- psychTestR::join(
    EDT_welcome_page(dict = dict),
    EDT::EDT(num_items = num_items,
             with_welcome = FALSE,
             feedback = feedback,
             dict = dict,
             adaptive = adaptive,
             ...),
      EDT_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Emotion Discrimination Test",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
