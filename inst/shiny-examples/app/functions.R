



#ngrams_overall <- read_feather("ngrams_overall")
#ngrams_byparty <- read_feather("ngrams_byparty")


#choices <- readRDS(here::here("data","ngrams_choices.RDS"))


# Creating visualization functions ------
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


#plot_ngrams(terms = c("election","people"),facet_ngram = TRUE, facet_party = TRUE)
