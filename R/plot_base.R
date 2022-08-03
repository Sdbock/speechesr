#' plot_base
#'
#' @keywords internal
#' @noRd
#'
plot_base <- function(data, terms, years = c(1952,2020), ...) {
  data %>%
    ungroup() %>%
    filter(word %in% terms,
           between(election,years[1],years[2])) %>%
    ggplot(aes(x = election, y = frequency, group = factor(word))) +
    scale_x_continuous(limits = c(years[1],years[2] + 4),breaks = seq(years[1],years[2],4)) +
    scale_y_continuous(labels = scales::percent_format()) +
    ggthemes::theme_clean(base_size = 18, base_family = "Helvetica") +
    labs(
      y = "Frequency",
      x = "Election year"
    ) +
    theme(
      plot.background = element_rect(color = "white"),
      axis.text.x = element_text(angle = 90)
    )
}
