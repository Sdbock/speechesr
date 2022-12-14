% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch_ngram_viewer.R
\name{launch_ngram_viewer}
\alias{launch_ngram_viewer}
\title{launch_ngram_viewer}
\usage{
launch_ngram_viewer()
}
\description{
\code{launch_ngram_viewer()} opens a Shiny application called "The U.S. Presidential Speech Ngram Viewer". It displays frequencies of ngrams (unigrams, bigrams, and trigrams are possible) from U.S. Presidential Campaign speeches given by major party candidates between 1952 and 2020. Start typing an ngram in the search box, and possible selections will appear. The data have been limited to ngrams that are not made up entirely of stopwords (i.e., common words such as 'the', 'in', 'but', etc.). The frequencies display a given ngram's usage as a percent of all corresponding ngrams in a given year, either overall or by a given candidate. For example, an overall frequency of .05\% in the year 2016 for the unigram 'country' indicates that .05\% of all unigrams used in 2016 campaign speeches were 'country'. As another example: if the frequency of the bigram 'the people' is .05\% for the Democrat in 2008 (i.e., Barack Obama), that would indicate that .05\% of all bigrams used by Barack Obama in 2008 were 'the people'. There are several options for viewing frequencies of ngrams. The 'Trends by party' option displays ngram trends disaggregated by party (unchecked displays overall frequencies in each election). If multiple ngrams are entered, the 'Separate by ngram' option will display each ngram in a separate panel. Likewise, if 'Trends by party' is selected, the 'Separate by party' will display the results of each party in a separate panel.
}
