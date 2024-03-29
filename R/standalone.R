#source("R/EDT.R")
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone EDT
#'
#' This function launches a standalone testing session for the EDT
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed.  Defaults to  TRUE
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}),
#' German (\code{"de"}), formal German (\code{"de_f"}),  Russian (\code{"ru"}), Nederlands (\code{"nl"}), Espanol \code{"es"}, and Italiano ((\code{"it"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param adaptive (Scalar boolean) Indicates whether you want to use the adaptive EDT2 (TRUE)
#' or the non-adaptive EDT (FASLE). Default is adaptive = TRUE.
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param autoplay (Scalar boolean) Indicates whether you want to have autoplay for item pages (instruction pages always not-autoplay)
#' @param ... Further arguments to be passed to \code{\link{EDT}()}.
#' @export

EDT_standalone  <- function(title = NULL,
                           num_items = 18L,
                           with_id = TRUE,
                           with_feedback = FALSE,
                           with_welcome = TRUE,
                           admin_password = "conifer",
                           researcher_email = "longgoldstudy@gmail.com",
                           languages = c("en", "de", "de_f", "ru", "nl", "it", "es", "zh_cn"),
                           dict = EDT::EDT_dict,
                           validate_id = "auto",
                           adaptive = TRUE,
                           take_training = FALSE,
                           autoplay = TRUE,
                           ...) {
  feedback <- NULL
  if(with_feedback) {
    feedback <- EDT::EDT_feedback_with_graph()
    #feedback <- EDT::EDT_feedback_with_score()
  }
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    if(take_training)
      EDT::EDT(num_items = num_items,
               with_welcome =  FALSE,
               with_finish = FALSE,
               feedback = feedback,
               dict = dict,
               take_training = TRUE,
               adaptive = adaptive,
               autoplay = autoplay,
               ...)
    else
      EDT::EDT(num_items = num_items,
             with_welcome = with_welcome,
             with_finish = FALSE,
             feedback = feedback,
             dict = dict,
             adaptive = adaptive,
             autoplay = autoplay,
             ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::code_block(function(state, ...){
      res <- get_results(state, complete = T)
      #browser()
    }),
    EDT_final_page(dict = dict)
  )
  browser()
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <-
      EDT::EDT_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title)) %>% str_replace("zh.cn", "zh_cn")
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}
