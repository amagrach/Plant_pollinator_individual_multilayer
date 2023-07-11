
pollinator_model_exp_coef <- function(model_pollinator_i,pollinator_i){
  
  coefficients_pollinator_i <- coefficients(model_pollinator_i) %>% as.data.frame() %>%
    filter(!is.na(`.`))
  coefficients_pollinator_i <- mutate(coefficients_pollinator_i, term = rownames(coefficients_pollinator_i))
  colnames(coefficients_pollinator_i) <- c("exp_term","term")
  coefficients_pollinator_i_exp <- mutate(coefficients_pollinator_i, exp_term = exp(exp_term)) %>%
    dplyr::select(term, exp_term)
  rownames(coefficients_pollinator_i_exp) <- 1:nrow(coefficients_pollinator_i_exp)
  
  confint_coef_pollinator_i <- confint(model_pollinator_i) %>% as.data.frame() %>%
    filter(!is.na(`.`)) %>%   mutate(term = coefficients_pollinator_i_exp$term)
  
  colnames(confint_coef_pollinator_i) <- c("lower_ci","upper_ci","term")
  confint_coef_pollinator_i_exp <- mutate(confint_coef_pollinator_i, lower_ci = exp(lower_ci),
                                          upper_ci = exp(upper_ci)) %>%
    dplyr::select(term, lower_ci, upper_ci)
  rownames(confint_coef_pollinator_i_exp) <- 1:nrow(confint_coef_pollinator_i_exp)
  
  pollinator_i_exp_coef <- left_join(coefficients_pollinator_i_exp, confint_coef_pollinator_i_exp, by = "term") %>%
    mutate(pollinator = pollinator_i)
  
  return(pollinator_i_exp_coef)
  
}
