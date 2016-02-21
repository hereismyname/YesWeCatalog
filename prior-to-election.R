
# citation ----------------------------------------------------------------

# text taken from http://www.obamaspeeches.com/
# doesn't appear to be an author attribution? 

# setup -------------------------------------------------------------------

library(dplyr)
library(rvest)

# testing -----------------------------------------------------------------

t_link <- "http://obamaspeeches.com/P-Obama-Inaugural-Speech-Inauguration.htm"
p_ele  <- "br+ table font+ font"

text <- t_link %>% 
  read_html %>% 
  html_nodes(p_ele) %>% 
  html_text

text <- gsub("\n            ", " ", text)


t_link <- "http://obamaspeeches.com/E11-Barack-Obama-Election-Night-Victory-Speech-Grant-Park-Illinois-November-4-2008.htm"
p_ele <- "p+ p font"



grab_text <- function(link, p_ele) {
  text <- link %>% 
    read_html %>% 
    html_nodes(p_ele) %>% 
    html_text
  
  text <- gsub("\n             ", " ", text)
  text <- gsub("   ", " ", text)
  
  text
}

grab_text("http://obamaspeeches.com/E-Barack-Obama-Speech-Manassas-Virgina-Last-Rally-2008-Election.htm", p_ele)

grab_text("http://obamaspeeches.com/E10-Barack-Obama-The-American-Promise-Acceptance-Speech-at-the-Democratic-Convention-Mile-High-Stadium--Denver-Colorado-August-28-2008.htm", p_ele)

# get links ---------------------------------------------------------------

base_url <- "http://obamaspeeches.com"

links <- base_url %>% 
  read_html %>% 
  html_nodes("a") %>% 
  html_attr("href")

# drop the last two entries which are not related to speeches
links <- links[c(-113, -114)]

# ensure there are no leading slashes
links <- lapply(links, function(link) gsub("/", "", link)) %>% unlist

# set up for calls
links <- paste0(base_url, "/", links)

# remove duplicates
links <- links[!duplicated(links)]

# define funs -------------------------------------------------------------

scrape_text <- function(link, text_ele) {
  # bring in all the text
  text <- link %>% 
    read_html %>% 
    html_nodes(text_ele) %>% 
    html_text
  
  if (length(text) == 0) text <- "please insert coin"
  
  text
}

# scrape ------------------------------------------------------------------

# think this is the css structures we want
# ele <- "p+ p font , font font strong"
ele <- "td tr+ tr td , h1 font"

# first 10 speeches seem to be written differently
speeches_raw <- lapply(links, function(l) scrape_text(l, ele))

# clean -------------------------------------------------------------------

# remove slashes, newlines, unnecessary spacing, some regex
speeches <- lapply(speeches_raw, function(text) {
  text <- trimws(text)
  text <- gsub("\n             ", " ", text)
  text <- gsub("\\\"", "", text)
  text <- gsub("\\n", "", text)
  text <- gsub("   ", " ", text)
  text <- gsub("  ",  " ", text)
  text <- gsub("  ",  " ", text)
  text <- gsub("Compete Text", "Complete Text", text)
  text <- gsub("Full Text", "Complete Text", text)
  text <- gsub("Complete Podcast Transcript", "Complete Text", text)
  text <- gsub("Complete Transcript", "Complete Text", text)
  
  text
})

# separate metadata from text
speeches <- lapply(speeches, function(s) str_split(s, " Complete Text "))

# get raw speech text
speech_txt <- lapply(speeches, function(s) {
  l    <- length(s)     # how many items in list?
  smax <- s[[l]]        # get the last item in list
  lmax <- length(smax)  # how many elements in vector?
  out  <- smax[lmax]    # get the last element in vector
  out                   # return
})

#### TWO ENTRIES STILL HAVE METADATA: 87 & 95
speech_txt <- unlist(speech_txt) %>% trimws

speech_dat <- lapply(speeches, function(speech) {
  l <- length(speech)
  
  head <- speech[[1]][1]
  dat  <- speech[[l]][1]
  
  data.frame(head, dat, stringsAsFactors = FALSE)
})

# place all metadata into a frame
speech_dat <- do.call(rbind, speech_dat)

# some speech_dat$dat rowshave the whole speech text included because they did 
# not show a break between the header and text
# trim whitespace, clean up spacing, split to get only metadata
speech_dat$dat <- trimws(speech_dat$dat)
speech_dat$dat <- sub("  ", " ", speech_dat$dat)
speech_dat$dat <- speech_dat$dat %>% 
  str_split(., "  ") %>% 
  lapply(., function(item) item[1]) %>%
  unlist()

# find & extract dates if possible

months <- paste(month.name, collapse = "|")
dat <- speech_dat$dat
dat[!grepl(months, dat)] <- "January"
speech_dat$date <- gsub(
  paste0(".*(", months, ")"), "\\1", dat[grep(months, dat, TRUE)], TRUE
)

speech_dat$date <- substr(speech_dat$date, 1, 20)
speech_dat$date <- str_split(speech_dat$date, " ") %>% 
  lapply(., function(l) if (length(l) >= 3) l[1:3] else l[1]) %>% 
  lapply(., paste, collapse= " ") %>% 
  unlist %>% 
  sub("th", "", .) %>% 
  sub("st", "", .) %>% 
  sub(",", "", .)

speech_dat$date <- as.Date(speech_dat$date, format = "%B %d %Y")

# combine & save ----------------------------------------------------------

out <- cbind(speech_dat, speech_txt)

saveRDS(out, "~/Documents/YesWeCatalog/prior-to-election.rds")
