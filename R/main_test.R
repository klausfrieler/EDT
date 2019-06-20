

main_test <- function(label, num_items_in_test, audio_dir, dict = EDT::EDT_dict) {
  elts <- c()
  for(i in 1:num_items_in_test){
    elts <- c(elts, item_page(item_number = i,
                              num_items_in_test = num_items_in_test,
                              audio_dir = audio_dir,
                              dict = dict))
  }
  psychTestR::new_timeline({
    elts
  }, dict = dict)
}


item_page <- function(item_number, num_items_in_test, audio_dir, dict = EDT::EDT_dict) {
  item <- EDT::EDT_item_bank[item_number, ]
  emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  EDT_item(label = sprintf("%s-%s", item_number, num_items_in_test),
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items_in_test, emotion),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
  #psychTestR::audio_NAFC_page(label = sprintf("%s-%s", item_number, num_items_in_test),
  #                promp = get_prompt(item_number, num_items_in_test, emotion),
  #                choices = c("1", "2"),
  #                url = file.path(audio_dir, item$audio_file[1]))
}

get_prompt <- function(item_number, num_items_in_test, emotion, dict = EDT::EDT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                     "?" else
                       num_items_in_test)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION",
                       sub = list(emotion = emotion)),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

get_welcome_page <- function(dict = EDT::EDT_dict){
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

get_final_page <- function(dict = EDT::EDT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block")
      )
    ), dict = dict)
}
