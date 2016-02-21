
# initialize --------------------------------------------------------------

library(dplyr)
library(tidyr)
library(quanteda)
library(lubridate)
library(ggthemes)
library(magrittr)

speeches <- readRDS("bo-gun-violence-clean.rds")

# prepare corpus ----------------------------------------------------------

# get just the text
speech_txt <- lapply(speeches[1:6], function(s) s[[2]])

# remove audience and anderson cooper text
# also remove leading text indicating when BO is speaking
speech_txt <- lapply(speech_txt, function(item) {
  item <- item[!grepl("Audience:", item)]
  item <- item[!grepl("Anderson Cooper:", item)]
  item <- item[!grepl("Question:", item)]
  item <- sub("President Obama: ", "", item)
  item
})

# ensure each speech is one element
speech_txt <- lapply(speech_txt, function(speech) paste(speech, collapse = " "))

# get names
names(speech_txt) <- lapply(speeches[1:6], function(s) s[[1]]$title) %>%
  unlist %>% unname

speech_txt <- unlist(speech_txt)

speech_corpus <- corpus(speech_txt)

docvars(speech_corpus, "date") <- lapply(speeches[1:6], function(s) s[[1]]$date) %>% 
  unlist %>% unname %>% as.Date(., format = "%d %B %Y")

# prepare dfm -------------------------------------------------------------

speech_dfm <- dfm(
  speech_corpus, 
  ignoredFeatures = c("will", "just", "get", stopwords())
)

kwic_multiple <- function(term, corpus, docs = 1:6) {
  kwics <- lapply(docs, function(doc) kwic(corpus[doc], term))
  
  # badly defined, but we're only using 6 rn
  title(xlab = "Token Index", ylab = term, outer = TRUE, line = 2)
  par(mfrow = c(2, 3))
  lapply(kwics, plot, bty = "n")
}

vio_sim <- similarity(speech_dfm, c("people", "gun", "violence", "congress"), n = 20)

vio_sim <- do.call(cbind, vio_sim) %>% data.frame %>% add_rownames(var = "term")

# term frequency ----------------------------------------------------------

# small multiples plot 
# track frequency of keywords as they accumulate in each text

# create 'dictionary' of keywords
term_dict <- list(
  community  = c("people", "community", "country", "nation", "public", 
                 "families", "family"),
  gov        = c("government", "congress", "senate", "politicians",
                 "legislators", "officials", "representatives"),
  gun        = c("gun", "guns", "weapon"),
  violence   = c("violence", "bloodshed"),
  sorrow     = c("mourning", "grief", "sadness"),
  duty       = c("duty", "responsibility", "obligation", "trust"),
  background = c("background"),
  solution   = c("solution", "act", "action", "problem", "fix")
)

# exclude last doc, is much larger than other 5
# for each doc:
# take cumulative sum of terms as they are found in tokens
# melt frame & record which document it represents
step_frames <- lapply(1:5, function(i) {
  steps <- data_frame(
    tokens = tokenize(speech_corpus[i]) %>% unlist()
  ) %>% 
    mutate(
      tokens           = tolower(tokens),
      token_index      = 1:length(tokens),
      count_gun        = cumsum(tokens %in% term_dict$gun),
      count_violence   = cumsum(tokens %in% term_dict$violence),
      count_gov        = cumsum(tokens %in% term_dict$gov),
      count_people     = cumsum(tokens %in% term_dict$community),
      count_background = cumsum(tokens %in% term_dict$background),
      count_duty       = cumsum(tokens %in% term_dict$duty),
      count_sorrow     = cumsum(tokens %in% term_dict$sorrow),
      count_solution   = cumsum(tokens %in% term_dict$solution)
    )
  
  steps %>% 
    select(token_index, contains("count")) %>% 
    gather(key = token_index) %>% 
    set_names(c("index", "Term", "count")) %>% 
    mutate(doc = i)
})

# bind each doc together
step_frames <- bind_rows(step_frames) 

# plot & facet
step_frames %>% 
  mutate(
    Term = factor(Term, labels = c("gun/guns", "violence", "government", "community",
                                   "background (check)", "duty", "sorrow", "solutions")),
    doc  = factor(doc, labels = docnames(speech_corpus)[1:5])
  ) %>% 
  filter(Term != "gun/guns", Term != "sorrow", Term != "duty") %>% 
  ggplot(aes(x = index, y = count, color = Term)) + 
  geom_step(size = 1.1) + 
  facet_wrap(~ doc) + 
  theme_light() +
  theme(
    legend.position   = c(.8, .3),
    legend.key        = element_rect(color = NA),
    legend.background = element_rect(fill = "transparent"),
    legend.title      = element_blank(),
    axis.ticks        = element_blank(),
    panel.border      = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  labs(x = "Progress through text (words)", y = "Term frequency")
