library(rvest)
library(stringr)
library(dplyr)
library(magrittr)
library(XML)

# define tgt url
url <- 'https://www.tripadvisor.in/Hotels-g303881-Munnar_Idukki_District_Kerala-Hotels.html'    

#Reading the HTML code from the website
# alternately, using pipes operator %>%
url_content <- url %>% read_html()  

##hotel name
hotel_data_html <- html_nodes(url_content,'.prominent')
#Converting the hotel name data to text
hotel_data <- html_text(hotel_data_html)

length(hotel_data)

##url of hotels
hotel_link1 <- hotel_data_html %>% html_attr('href')
hotel_link <- paste0("https://www.tripadvisor.in",hotel_link1)
length(hotel_link)


### Extracting count of reviews
hotel_review_count_html <- html_nodes(url_content,'.review_count')
xmlTreeParse(hotel_review_count_html[[1]])
hotel_review_count_data <- html_text(hotel_review_count_html)
hotel_review_count_data<-gsub(" reviews","",hotel_review_count_data)
hotel_review_count_data <- as.numeric(hotel_review_count_data)
length(hotel_review_count_data)

####amenities of hotels
hotel_amenities_html <- html_nodes(url_content,'.hotel_icon')
xmlTreeParse(hotel_amenities_html[[1]])
hotel_amenities_data <- html_text(hotel_amenities_html)

###ratings
hotel_ratings <- html_nodes(url_content,'.ui_bubble_rating')
pos1 = regexpr('bubble_', hotel_ratings) ##position
ratings = as.numeric(substr(hotel_ratings, pos1+21,pos1+22))/10 ##extract ratings

length(ratings)
length(hotel_ratings)

##price of each room
price= html_text(html_nodes(url_content,'.price_night'))
hotel_price <- substr(price,regexpr(",",price)-2,regexpr("$",price))
length(hotel_price)

##dataframe
df = data.frame(hotel_data,hotel_link,hotel_price,hotel_review_count_data)
head(df)

###### FUNCTION LOOP  ######
func <- function(hotel_link){
  
  url1 <- html_session(hotel_link)
  url_content1 <- url1 %>% read_html()
  hotel_Address_1 <- html_nodes(url_content1,'.hotelActionsColumn')
  c <- html_text(hotel_Address_1)
  pos = regexpr('India', c) 
  c <- substr(c,1,pos+4)
  c<-gsub("Save","",c)
  return(c)
}

hotel_addr <- sapply(hotel_link,func(hotel_link))

hotel_savings = NULL
ratings = NULL
amenity = NULL
addr = NULL

#### For loop #####

for(i in df$hotel_link){
  url1 <- read_html(i)
  hotel_Address_1 <- html_nodes(url1,'.hotelActions')
  c <- html_text(hotel_Address_1)
  pos = regexpr('India', c) 
  c <- substr(c,1,pos+4)
  addr1<-gsub("Save","",c)
  
  hotel_ratings <- html_nodes(url1,'ui_bubble_rating')
  pos1 = regexpr('bubble_', hotel_ratings) ##position
  ratings1 = as.numeric(substr(hotel_ratings, pos1+21,pos1+22))/10 ##extract ratings
  
  hotel_res_price <- html_text(html_nodes(url_content,'.hasStrikeThrough'))
  hotel_total_price <- html_nodes(url_content,'.xthrough')
  hotel_saving <- hotel_total_price - hotel_res_price
  
  amenity0 = paste(html_text(html_nodes(url1,'.highlightedAmenity')), collapse= ", ")
  ratings = c(ratings,ratings1)
  amenity = c(amenity,amenity0)
  addr = c(addr,addr1)
  hotel_savings = c(hotel_savings,hotel_saving1)
  }

df$ratings = ratings
df$amenity = amenity
df$addr = addr
df$hotel_savings = hotel_savings
head(df)

###savings

#hotel_name

url_content <- url %>% read_html()  
url<- 'https://www.tripadvisor.in/Hotel_Review-g303881-d3581670-Reviews-Green_Shades-Munnar_Idukki_District_Kerala.html'

hotel.name <- url_content %>% html_nodes("#HEADING") %>% html_text()
hotel.name

##hotel address
address <- url_content %>% html_nodes(".is-hidden-mobile.blEntry.address") %>%
  html_nodes(".detail") %>% html_text() 
address


##price

price= html_text(html_nodes(url_content,'.price'))
x <- url_content %>% html_nodes(".hasStrikeThrough .price") %>% html_text()


hotel_res_price <- html_nodes(url_content,'')
c<- html_text(hotel_res_price)
hotel_total_price <- html_nodes(url_content,'.xthrough')
savings


### check for session - gireesh problem 

##saving file in system
write.csv(df,'df.csv')

###################################


for(i in df$hotel_link){
  
url_content1 <- i %>% read_html()  
hotel_res_price <- html_text(html_nodes(url_content1,'.hasStrikeThrough'))

d <- gsub('\u20b9','',hotel_res_price)
d <- gsub("\\s","",d)
c2 <- as.numeric(substr(d,1,nchar(d)/2))
c3 <- as.numeric(substr(d,nchar(d)/2,nchar(d)))
hotel_saving1 <- c2 - c3
hotel_savings = c(hotel_savings,hotel_saving1)

}
df$hotel_savings = hotel_savings

################################################

