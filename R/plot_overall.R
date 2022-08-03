
#' plot_overall
#'
#'
#' @keywords internal
#' @noRd
#'


plot_overall <- function(...) {
  plot_base(data = ngrams_overall,...) +
    geom_line(aes(color = word), show.legend = FALSE) +
    labs(color = NULL) +
    ggrepel::geom_text_repel(data = . %>% filter(election == max(election)),
                             aes(label = word, color = word),
                             arrow = grid::arrow(angle = 30, length = unit(0.07, "inches") ),
                             segment.color = "black",
                             segment.size = .25,
                             nudge_x = 2,
                             direction = "y",
                             show.legend = FALSE) +
    ggthemes::scale_color_economist()
}
