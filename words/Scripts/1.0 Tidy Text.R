# Author
#       Brandon Dey 
#
# Date
#     10.1.18
#
# Purpose
#       Establish a baseline sentiment of R documentation.

#################
## ENVIRONMENT ##
#################
# load libraries
library(tidytext)
library(tidyverse)
library(magrittr)
library(pdftools)
library(sentimentr)
library(tm)
library(syuzhet)


# Make vector of file paths
dir <- "/Users/brandondey/Desktop/All R Package Docs"

# Create a vector of file path + package name
pdfs <- paste(dir, "/", list.files(dir, pattern = "*.pdf"), sep = "")

# Vector of just package names
pdf_names <- list.files(dir, pattern = "*.pdf")

pdf_sam <- sample(pdfs, 500)

# Suck out the text from each PDF using pdftools:pdf_text wrapped in purrr::map to iterate on each pdf.
pdfs_text <- purrr::map(pdf_sam, pdftools::pdf_text)

# create a data frame w/ 1 row for each package
my_data <- data_frame(package = pdf_sam, 
                      text = pdfs_text)

# tokenize into sentences or detect sentence boundaries (sentence boundary disambiguation)
my_data %>% 
  unnest %>%
  sentimentr::get_sentences() %>%
  sentimentr::sentiment() -> bounded_sentences   # get sentiment score for e/ sentence

# remove "sentences" with only 1 character. 
bounded_sentences %>%
  mutate(characters = nchar(stripWhitespace(text))) %>%
  filter(characters >1 ) -> bounded_sentences

# explore it
summary(bounded_sentences)

# transform sentiment values
dct_values <- get_dct_transform(
  bounded_sentences$sentiment, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)

plot(dct_values)

bounded_sentences %>% 
  mutate(normalized_sent = (sentiment-min(sentiment))/(max(sentiment)-min(sentiment))) -> bounded_sentences


# this removes 466/ nrow(bounded_sentences)
bounded_sentences %>% filter(between(sentiment,-1,1)) ->  bounded_sentences

dat <- with(density(bounded_sentences$sentiment), data.frame(x, y))

ggplot(dat, aes(x = x, y = y)) +
  geom_line() +
  geom_area(mapping = aes(x = ifelse(x >=0 & x<=1 , x, 0)), fill = "green") +
  geom_area(mapping = aes(x = ifelse(x <=0 & x>=-1 , x, 0)), fill = "red") +
  scale_y_continuous(limits = c(0,7.5)) +
  theme_minimal(base_size = 16) +
  labs(x = "Sentiment", 
       y = "", 
       title = "The Distribution of Sentiment Across R Package Help Docs") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.y=element_blank()) -> gg

ggsave(filename = "./density.jpeg", 
       plot = gg,
       dpi = 1000, 
       device = "jpeg", 
       height = 10, 
       width = 10)

# the valence shifters
lexicon::hash_valence_shifters -> sh


# Highlight text and print
text <- "However, in the second row, you can see that sentimentr catches this negation and forces sentiment negative accordingly, while the syuzhet package erroneously assigns it the same sentiment score as “I love apple pie”. (Jocker made a solid defense of his package here.) sentimentr even reckons a higher sentiment score for, “I really really love apple pie!!!” because of how the algorithm captures the nuance of those crafty amplifiers, really really, which are missed by the syuzhet approach. But sentiementr is not without shortcoming. It’s still a lexicon approach, which suffers from the reductively I vented about above, even if its default lexicon is a combined and augmented version of the syuzhet package (Jocker 2017) and Rinker's augmented Hu & Liu (2004) from the lexicon package. Still a lexicon.
In order to validate the classifier I just built, which isn’t technically a classifier because I never dichotomized the continuous sentiment score into positive, negative, or neutral groups, I’d need labeled training data against which to test. Failing that, I could turn to a more sophisticated unsupervised approach, which though appealing, sure, is well beyond the scope of this post. 
But hey, now that I have an entire corpus of some 12k+ help docs, I have data aplenty to cut my teeth on in a later post!"

sentimentr::sentiment_by(text) %>% sentimentr::highlight()


