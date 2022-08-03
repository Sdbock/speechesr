
#' plot_party
#'
#'
#' @keywords internal
#' @noRd
#'


plot_party <- function(...) {

  dems <- "#016a8e"
  reps <- "#b1283a"

  plot_base(data = ngrams_byparty,...) +
    geom_line(data = . %>% filter(party == "Democrat"), aes(color  = party)) +
    geom_line(data = . %>% filter(party == "Republican"), aes(color = party)) +
    scale_color_manual(values = c(dems,reps)) +
    ggrepel::geom_text_repel(data = . %>% group_by(party, word) %>% filter(election == max(election)),
                             aes(label = word,
                                 color = party),
                             arrow = grid::arrow(angle = 30, length = unit(0.07, "inches") ),
                             segment.color = "black",
                             segment.size = .25,
                             #min.segment.length = .1,
                             nudge_x = 2,
                             direction = "y",
                             show.legend = FALSE) +
    labs(
      color = NULL,
    ) +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.background = element_rect(color = "white"))

}
