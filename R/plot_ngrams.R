#' plot_ngrams
#'
#'
#'
#' @NoRd




plot_ngrams <- function(byparty = TRUE, facet_ngram = FALSE, facet_party = FALSE,...) {

  if(byparty == TRUE) {
    plot <- plot_party(...)
  }

  else{
    plot <- plot_overall(...)
  }

  if(facet_ngram == FALSE) {
    plot <- plot
  }

  else {
    plot <- plot +
      facet_wrap(~word, ncol = 2) +
      theme(
        strip.text.x = element_blank()
      )
  }

  if(byparty == TRUE & facet_party ==  TRUE){
    plot <- plot +
      facet_wrap(~party,ncol = 2) +
      theme(
        strip.text.x = element_blank()
      )
  }
  else{
    plot <- plot
  }
  if(byparty == TRUE & facet_party == TRUE & facet_ngram == TRUE) {
    plot <- plot +
      facet_grid(party ~ word) +
      theme(
        strip.text.x = element_blank(),
        strip.text.y = element_blank()
      )
  }
  plot
}
