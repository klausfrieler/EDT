scoring <- function(){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()

    sum_score <- sum(purrr::map_lgl(results$EDT, function(x) x$correct))
    num_question <- length(results$EDT)
    perc_correct <- sum_score/num_question
    psychTestR::save_result(place = state,
                 label = "score",
                 value = perc_correct)
    psychTestR::save_result(place = state,
                             label = "num_questions",
                             value = num_question)

  })
}

get_eligible_first_items_EDT <- function(){
  lower_sd <- mean(EDT::EDT2_item_bank$difficulty) - stats::sd(EDT::EDT2_item_bank$difficulty)
  upper_sd <- mean(EDT::EDT2_item_bank$difficulty) + stats::sd(EDT::EDT2_item_bank$difficulty)
 which(EDT::EDT2_item_bank$difficulty >= lower_sd  &
         EDT::EDT2_item_bank$difficulty <= upper_sd)
}

main_test <- function(label,
                      num_items,
                      audio_dir,
                      dict = EDT::EDT_dict,
                      adaptive = TRUE,
                      next_item.criterion,
                      next_item.estimator,
                      next_item.prior_dist,
                      next_item.prior_par,
                      final_ability.estimator,
                      constrain_answers, ...) {
  if(adaptive) {
    item_bank <- EDT::EDT2_item_bank
    psychTestRCAT::adapt_test(
      label = label,
      item_bank = item_bank,
      show_item = show_item(audio_dir),
      stopping_rule = psychTestRCAT::stopping_rule.num_items(n = num_items),
      opt = EDT_options(next_item.criterion = next_item.criterion,
                        next_item.estimator = next_item.estimator,
                        next_item.prior_dist = next_item.prior_dist,
                        next_item.prior_par = next_item.prior_par,
                        final_ability.estimator = final_ability.estimator,
                        constrain_answers = constrain_answers,
                        eligible_first_items = get_eligible_first_items_EDT(),
                        item_bank = item_bank)
    )
  } else {
    elts <- c()
    item_bank <- EDT::EDT_item_bank
    item_sequence <- sample(1:nrow(item_bank), num_items)
    for(i in 1:length(item_sequence)){
      item <- EDT::EDT_item_bank[item_sequence[i],]
      emotion <- psychTestR::i18n(item[1,]$emotion_i18)
      item_page <- EDT_item(label = item$item_number[1],
                            correct_answer = item$correct[1],
                            prompt = get_prompt(i,
                                                num_items,
                                                emotion),
                            audio_file = item$audio_file[1],
                            audio_dir = audio_dir,
                            save_answer = TRUE,
                            adaptive = adaptive)
      elts <- psychTestR::join(elts, item_page)
    }
    elts
  }
}

item_page <- function(item_number, item_id, num_items, audio_dir,
                      dict = EDT::EDT_dict) {
  item <- EDT::EDT_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  EDT_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items, emotion),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
  #psychTestR::audio_NAFC_page(label = sprintf("%s-%s", item_number, num_items_in_test),
  #                promp = get_prompt(item_number, num_items_in_test, emotion),
  #                choices = c("1", "2"),
  #                url = file.path(audio_dir, item$audio_file[1]))
} # Is this function used anywhere? NR

get_prompt <- function(item_number, num_items, emotion,
                       dict = EDT::EDT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items))
                     "?" else
                       num_items)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION",
                       sub = list(emotion = emotion)),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

EDT_welcome_page <- function(dict = EDT::EDT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "margin-left:0%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

EDT_finished_page <- function(dict = EDT::EDT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

EDT_final_page <- function(dict = EDT::EDT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

show_item <- function(audio_dir) {
  function(item, ...) {
    #stopifnot(is(item, "item"), nrow(item) == 1L)
    item_number <- psychTestRCAT::get_item_number(item)
    num_items <- psychTestRCAT::get_num_items_in_test(item)
    emotion <- psychTestR::i18n(item[1,]$emotion_i18)
    messagef("Showing item %s", item_number)
    EDT_item(
      label = paste0("q", item_number),
      emotion = emotion,
      audio_file = item$audio_file,
      correct_answer = item$answer,
      adaptive = TRUE,
      prompt = get_prompt(item_number, num_items, emotion),
      audio_dir = audio_dir,
      save_answer = TRUE,
      get_answer = NULL,
      on_complete = NULL,
      instruction_page = FALSE
    )
  }
}
