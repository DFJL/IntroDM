
#Script para realizar scrapping de reviews de una lista de libros

library(RCurl) 
library(XML)
library(stringr) 
library(rvest)
library(purrr)
library(learnr)
library(knitr)
library(lubridate)
library(robotstxt)
library(polite)
library(tidyverse)


# Function to scrape elements from Amazon reviews
scrape_amazon <- function(url, throttle = 5){
  
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  session <- bow(url, force = TRUE)
  # obtain HTML of URL
  doc <- scrape(session)
  
  # # Parse relevant elements from HTML
  # title <- doc %>%
  #   html_nodes(".a-color-base") %>%
  #   html_text() 
  # title<- title[10:length(title)]
  # 
  author <- doc %>%
    html_nodes(".a-profile-name") %>%
    html_text()
  
  author <- author[-1:-2]
  
  date <- doc %>%
    html_nodes(".review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  date <- date[-1:-2]
  
  review_format <- doc %>% 
    html_nodes(".review-format-strip") %>% 
    html_text() 
  
  stars <- doc %>%
    html_nodes(".review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric() 
  
  stars <- stars[-1:-2]
  
  comments <- doc %>%
    html_nodes(".review-text") %>%
    html_text() 
  
  # Combine attributes into a single data frame
  df <- data.frame(author, date, review_format, stars, comments, stringsAsFactors = F)
  
  return(df)
}

# Poner el número de páginas deseadas
pages <- 2

# Objeto vacio para guardar los datos
reviews_all <- NULL

# iterar sobre cada página

prod_code <- c("0062316117","B01MYZ4OUW","B07DHSPZT2","B001LNK9C4","B07NJCG1XS")

# ciclo para realizar scrapping de características de cada producto en prod_code
for(i in 1:length(prod_code)){

  url <- paste0("https://www.amazon.com/product-reviews/",prod_code[i],
                "/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews")
  
  #obtener el nombre del producto y realizar una limpieza del mismo
  
  session <- bow(url, force = TRUE)
  prod <- scrape(session) %>%
    html_nodes( "a.a-link-normal") %>% 
    html_text() %>% 
    gsub("\n", "", .) %>% 
    trimws()
  
  prod<- prod[1]
  
  for(page_num in 1:pages){
    
    url <- paste0("https://www.amazon.com/product-reviews/",
                  prod_code[i],"/ref=cm_cr_arp_d_paging_btm_next_",
                  page_num,
                  "?ie=UTF8&reviewerType=all_reviews&pageNumber=",
                  page_num)
    
    
    
    reviews <- scrape_amazon(url, throttle = 3)
    reviews_all <- rbind(reviews_all, cbind(prod, reviews))
    
    #books_reviews[[i]]<- reviews_all
  }
  #str(reviews_all)
  
  

  }


# Write results to csv file


#write_delim(reviews_all,"AmzReviews.csv",delim = ";")

#test<- read_delim("AmzReviews.csv",delim = ";")

#test2<- read_delim("https://raw.githubusercontent.com/DFJL/Datasets/master/AmzReviews.csv",delim = ";")
