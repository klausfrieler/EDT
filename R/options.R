EDT_options <- function(next_item.criterion,
                        next_item.estimator,
                        next_item.prior_dist = next_item.prior_dist,
                        next_item.prior_par = next_item.prior_par,
                        final_ability.estimator,
                        constrain_answers = FALSE,
                        eligible_first_items = NULL,
                        item_bank) {
  #if(!is.null(eligible_first_items))
  #   printf("Length eligible_first_items %d", length(eligible_first_items))
  psychTestRCAT::adapt_test_options(
    next_item.criterion = next_item.criterion,
    next_item.estimator = next_item.estimator,
    next_item.prior_dist = next_item.prior_dist,
    next_item.prior_par = next_item.prior_par,
    final_ability.estimator = final_ability.estimator,
    #cb_control = list(names = c("4", "8", "16"), props = c(.2, .4, .4)),
    #cb_group = item_bank$type,
    eligible_first_items = eligible_first_items,
    constrain_answers = constrain_answers,
    avoid_duplicates = NULL
  )
}
