#written by Dan Gunn 2023
#attempting to compile a list of all accredited art schools in the US. 
#info from rayobyte on polite package

library(tidyverse)
library(rvest)
library(polite)
library(stringr)
library(curl)
library(broom)

#reads in the names of the schools into session nasad_url
nasad_url <- bow("https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/?institutionname=&city=&state=&country=&search=true", force = TRUE)
info <- scrape(nasad_url) %>% 
  html_nodes("div.wpb_wrapper a") %>% 
  html_text2() %>%
  trimws()

school_names <-info

#part of the url's to add onto IE "?id=I1190"
nasad_nodes <- scrape(nasad_url) %>%
  html_nodes("div.wpb_wrapper a") %>%
  html_attr("href")%>%
  trimws()

# https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/ Add to URL
nasad_URL_clean <- str_replace(nasad_nodes,"[?]","https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/?")
#list of search terms
str(nasad_URL_clean)

nasad_URL_tbl <- as_tibble(nasad_URL_clean)

#combined first scrape of list of URLs and school names + export to CSV
nasad_web_full <- tibble(school_names, nasad_URL_clean)
write.csv(nasad_web_full, "nasadwebfull.csv", row.names=FALSE)

nasad_tbl_shrt <- nasad_URL_tbl[1:4,]


#trying to perfect the scrape
# start_html <- bow("https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/?id=I1190")
# start_nodes <- scrape(start_html) %>% html_elements(".wpb_wrapper")
# class(start_nodes)

#use 2nd instance of class= wpb_wrapper
# name <- start_nodes[2] %>% html_elements("h2") %>% html_text(.)
# address_url <- start_nodes[2] %>% html_elements("h2 + p") %>% html_text(.)
# school_type <- start_nodes[2] %>% html_elements("p:nth-child(5)") %>% html_text(.)
# school_info <- tibble(name=name, address=address_url, type=school_type)
# degrees <- start_nodes[2] %>% html_elements("p:nth-child(10)") %>% html_text(.)


#make into a function to do all of the urls from nasad_url_clean
# Bulk scrape function
nasad_f <- function(x) {
  url <- bow(x)
  node <- scrape(url) %>% html_elements(".wpb_wrapper")
  return(node[2]) 
}

#Scrape function works but produces a list of xml_nodesets
test <- apply(X= nasad_tbl_shrt, FUN = nasad_f, MARGIN = 1)
str(test)

#write a function to parse the xml_nodes
subset_f <- function(x){
  name <- html_text(html_elements(x, "h2")) 
  address_url <- html_text(html_elements(x, "h2 + p"))
  about <- html_text(html_elements(x, "p:nth-child(3)")) 
  school_type <- html_text(html_elements(x, "p:nth-child(5)"))
  deg <- html_text(html_elements(x, "h3 + p"))[2]
  school_info <-tibble(name=name, address=address_url, about= about, type=school_type, degree= deg)
}
#lapply and convert to table.
test_info <- lapply(test, subset_f) %>% 
  data.table::rbindlist()

#NOTE: degrees are tricky because both the 9th or 10th child of p is the degree listing.

#split out the degree types (4 year, associates, bachelors etc) on periods and colons.
#use them as columns and then separate the degrees subjects below them.

test_deg_longer <- test_info$degree %>% str_split_fixed(., pattern="\\.", n=Inf) %>% as.data.frame()
#expanded degrees (subjects still in tow) recombined with schools
test_info_combined <- cbind(test_info, test_deg_longer) %>% drop(name)
#now pivoting longer
test_info_pivot1 <- pivot_longer(test_info_combined, cols= starts_with("V"), values_to = "degree_type", names_repair = "minimal", values_drop_na=TRUE)
#this works, but use a mutate to keep it inside of the dataframe
test_set<- test_info_pivot1$degree_type %>% str_split_fixed(.,pattern=":|;", n=Inf)
test_info_pivot1 %>% group_by(name) 



#test_set <- lapply(test_info_pivot1, function(x) {str_split_fixed(x,pattern=":|;", n=Inf)}) %>% as.data.frame()
 
#now a data frame. rows are from the school. V1, V2, V3 are the degrees. 
class(test_deg_longer_split)
pivot_longer(test_deg_longer_split, everything(), cols_vary = "slowest")
#how to recombining with test_info?
test_info_expanded <- cbind(test_info, test_deg_longer_split)

# #test code
# degrees_longer <- as.data.frame(unlist(str_split(degrees, "\\.")))
# degrees_longer <- as.data.frame(str_split_fixed(degrees_longer[,1], pattern=":", n=2))
# degrees_longer <- degrees_longer %>% rename('degrees'='V1', 'subject'='V2')
# head(degrees_longer)
# 
# #expand the subjects
# degrees_longest <- degrees_longer$subject %>% str_split_fixed( pattern =";", n=30) %>% bind_cols(degrees_longer$degrees,.)
# head(degrees_longest)
# degrees_longest <- degrees_longest %>% rename('degrees'='...1', 'subject_1'='...2')


