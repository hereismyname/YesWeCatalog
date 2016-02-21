
# citation ----------------------------------------------------------------

# www.americanrhetoric.com
# owner@americanrhetoric.com
# http://www.americanrhetoric.com/copyrightinformation.htm

# initialize --------------------------------------------------------------

library(dplyr)
library(rvest)
library(stringr)

# metadata & links --------------------------------------------------------

base_url <- "http://www.americanrhetoric.com/barackobamaspeeches.htm"
ele      <- "td+ td a , tr+ tr td:nth-child(1) font"

# get html, extract elements, return text
titles     <- title_link %>% 
  read_html %>% 
  html_nodes(title_ele) %>% 
  html_text

# remove newlines & tabs, blank lines, etc.
titles <- titles[!grepl("PDF", titles)]
titles <- titles[!grepl("mp3", titles)]
titles <- titles[nchar(titles) > 0]
titles <- gsub("\r", "", titles)
titles <- gsub("\n", "", titles)
titles <- gsub("\t", "", titles)
titles <- titles[-1]

# correct issue of one cell spreading over 3 lines instead of just 2
titles[664] <- paste0(titles[664], titles[665])
titles <- titles[-665]

# pair titles & dates as cols
titles <- data_frame(raw = titles) %>% add_rownames(var = "index")
titles <- mutate(titles, index = as.numeric(index))

dates  <- filter(titles, (index %% 2) == 1) %>% 
  rename(date = raw) %>% select(-index)
speech <- filter(titles, (index %% 2) == 0) %>% 
  rename(title = raw) %>% select(-index)

# make a data-frame
titles <- bind_cols(speech, dates)

# get list of urls to each speech
urls <- read_html(title_link) %>% 
  html_nodes("td:nth-child(2) a , tr:nth-child(3) td:nth-child(2)") %>% 
  html_attr("href")

# remove NA val
urls <- na.omit(urls)

# drop duplicated francois speech
urls <- urls[-333]

# bind with titles frame
titles$url <- paste0("http://www.americanrhetoric.com/", urls)

# gun control -------------------------------------------------------------

# helper function to read/parse html
#
# args:
#   link: link to webpage
#   element: which elements to read
#   
# returns:
#   (vector) text from html element
retrieve <- function(link, element) {
  text <- read_html(link) %>% 
    html_nodes(element)   %>%
    html_text
  
  text <- gsub("\r\n", "", text)
  text <- gsub("\t", "", text)
  
  text
}

text_ele <- "p"

# gather text from each speech
speech_raw <- titles %>% 
  filter(grepl("Gun", title)) %>% 
  select(url) %>% unlist() %>% unname() %>% 
  lapply(., function(url) {
  Sys.sleep(sample(1:5, 1)) # throttle the loop to be polite
  
  retrieve(url, text_ele) # look up the speech, pull it down
})

# clean -------------------------------------------------------------------

# drop elements that are just a space
speech_clean <- lapply(speech_raw, function(item) {
  # keep lines that have more than 10 characters on them
  # remove lines that start with a newline char
  # remove lines starting with brackets and html commented lines
  l    <- nchar(item)
  item <- item[l > 10]
  item <- item[!substr(item, 1, 1) == "\n"]
  item <- item[!substr(item, 1, 1) == "["]
  item <- item[!substr(item, 1, 2) == "<!"]
  
  # site specific issues
  item <- item[!grepl("Audio AR-XE", item)]
  item <- item[!grepl("click for", item)]
  item <- item[!grepl("Book/CDs", item)]
  item <- item[!grepl("Text & Audio Source:", item)]
  item <- item[!grepl("Online Speech Bank", item)]
  item <- item[!grepl("Movie Speeches", item)]
  item <- item[!grepl("American Rhetoric", item, fixed = TRUE)]
  item <- item[!grepl("Top 100 American Speeches", item, fixed = TRUE)]
  item <- item[-(1:3)]
  
  # take care of inline munging
  item <- gsub("\\n", "", item)
  item <- gsub("\\s+", " ", item)
  item <- gsub("\\\"", "", item)
  item <- trimws(item)
  
  # some text contains comments from audience
  item <- gsub("Audience Member:", "Audience:", item)
  item <- gsub("Audience Members:", "Audience:", item)
  
  item
})

# attach metadata ---------------------------------------------------------

out <- list()

for (i in 1:length(speech_clean)) {
  speech <- list(
    metadata =  filter(titles, grepl("Gun", title))[i, ],
    text     = speech_clean[[i]]
  )
  
  out <- append(out, list(speech))
}

# save --------------------------------------------------------------------

# 'out' object is prepared from previous section
# but also preserve the raw data
raw <- speech_raw

# save download date
raw$date_downloaded <- Sys.Date()
out$date_downloaded <- Sys.Date()

saveRDS(raw, "~/Documents/YesWeCatalog/bo-gun-violence-raw.rds")
saveRDS(out, "~/Documents/YesWeCatalog/bo-gun-violence-clean.rds")
