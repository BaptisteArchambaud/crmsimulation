#' Graph of prior and posterior toxicity probabilities
#'
#' @param modelfit
#' @param target
#'
#' @return
#'
#' @examples
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 aes geom_hline geom_line geom_point ggplot labs scale_color_manual scale_y_continuous theme_light
ggplot_tox_probas <- function(modelfit, target = 0.25){

  #skeleton probabilities
  data_plot <- tibble(dose_level = 1:length(modelfit$skeleton),
                      all_tox_probas = modelfit$skeleton,
                      proba_id = "Skeleton")

  #if at least 1 patient included then also add posterior probabilities
  if(nrow(modelfit$df) > 0){
    data_plot <- data_plot %>%
      bind_rows(
        tibble(dose_level = 1:length(modelfit$skeleton),
               all_tox_probas = modelfit$dfcrm_fit$ptox,
               proba_id = "Posterior probabilities")
      )
  }

  data_plot %>% ggplot(aes(x = dose_level, y = all_tox_probas, color = proba_id)) +
    scale_color_manual(values = c("Skeleton" = "#F8766D", "Posterior probabilities" = "#619CFF")) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = target) +
    labs(x = "Dose level", y = "Probability of toxicity", color = "") +
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
    theme_light()

}



#' Formatting history of patient toxicities into a vector suited for escalation::fit
#'
#' @param data
#' @param dose_level
#' @param tox_probas
#'
#' @return
#'
#' @examples
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
tox_df_to_vector <- function(data, dose_level, tox_probas){
  leveltox <- paste0(
    pull(data[sym(dose_level)]),
    str_replace_all(pull(data[sym(tox_probas)]), c("0" = "N", "1" = "T")),
    collapse = " "
    )
  return(leveltox)
}
