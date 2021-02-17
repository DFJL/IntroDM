
# Function to scrape elements from Amazon reviews
scrape_amazon <- function(url, throttle = 0){
  
  # Install / Load relevant packages
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr)
  
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  # obtain HTML of URL
  doc <- read_html(url)
  
  # # Parse relevant elements from HTML
  # title <- doc %>%
  #   html_nodes(".a-color-base") %>%
  #   html_text() 
  # title<- title[10:length(title)]
  # 
  author <- doc %>%
    html_nodes(".a-profile-name") %>%
    html_text()
  
  date <- doc %>%
    html_nodes(".review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  review_format <- doc %>% 
    html_nodes(".review-format-strip") %>% 
    html_text() 
  
  stars <- doc %>%
    html_nodes(".review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric() 
  
  comments <- doc %>%
    html_nodes(".review-text") %>%
    html_text() 
  
  # Combine attributes into a single data frame
  df <- data.frame(author, date, review_format, stars, comments, stringsAsFactors = F)
  
  return(df)
}


# load DT packege
pacman::p_load(DT)

prod_code <- "B01MYZ4OUW"

url <- paste0("https://www.amazon.com/dp/", prod_code,"/?pageNumber=1")

reviews <- scrape_amazon(url)

# display data
str(reviews)


# Set # of pages to scrape. Note: each page contains 8 reviews.
pages <- 5

# create empty object to write data into
reviews_all <- NULL

# loop over pages
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.com/dp/",prod_code,"/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}

