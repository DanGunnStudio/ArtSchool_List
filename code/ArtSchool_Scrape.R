#written by Dan Gunn 2023
#attempting to compile a list of all accredited art schools in the US. 
#info from rayobyte on polite package

library(tidyverse)
library(rvest)
library(polite)
library(curl)
library(broom)

#reads in the names of the schools into session nasad_url
nasad_url <- bow("https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/?institutionname=&city=&state=&country=&search=true", force = TRUE)
info <- scrape(nasad_url) %>% 
  html_nodes("div.wpb_wrapper a") %>% 
  html_text2() %>%
  trimws()

#table of school names
school_names <-as_tibble(info)

#part of the url's to add onto IE "?id=I1190"
nasad_nodes <- scrape(nasad_url) %>%
  html_nodes("div.wpb_wrapper a") %>%
  html_attr("href")%>%
  trimws()

# https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/ Add to URL
nasad_URL_clean <- str_replace(nasad_nodes,"[?]","https://nasad.arts-accredit.org/directory-lists/accredited-institutions/search/?")

#list of search terms & inspecting
nasad_URL_tbl <- as_tibble(nasad_URL_clean)
str(nasad_URL_tbl)
head(nasad_URL_tbl)

#combined first scrape of list of URLs and school names + export to CSV
nasad_web_full <- tibble(school_names, nasad_URL_clean)
write.csv(nasad_web_full, "data/nasadwebfull.csv", row.names=FALSE)

#creating a smaller test set
nasad_tbl_shrt <- nasad_URL_tbl[1:4,]

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
head(test)

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
head(test_info)

#NOTE: degrees are tricky because both the 9th or 10th child of p is the degree listing.
#split out the degree types (4 year, associates, bachelors etc) on periods and colons.
#use them as columns and then separate the degrees subjects below them.
test_deg_longer <- test_info$degree %>% str_split_fixed(., pattern="\\.", n=Inf) %>% as.data.frame()
#expanded degrees (subjects still in tow) recombined with schools
test_info_combined <- cbind(test_info, test_deg_longer)
#drop redundant degree column
test_info_combined <- subset(test_info_combined, select=-degree)

#now pivoting longer
test_info_pivot1 <- pivot_longer(test_info_combined, 
                                 cols= starts_with("V"),
                                 names_to = NULL,
                                 values_to = "degree_type", 
                                 names_repair = "minimal", 
                                 values_drop_na=TRUE)
#now delete rows with NA or empty 'degree-type's
test_info_pivot1 <- test_info_pivot1[!(is.na(test_info_pivot1$degree_type) | test_info_pivot1$degree_type==""), ]

#expand degrees to break out subjects into columns_degree.type.2
test_set <- lapply(test_info_pivot1, function(x) {str_split_fixed(x,pattern=":|;", n=Inf)}) %>% as.data.frame()
#change column name to degree to avoid the pivot
colnames(test_set)[which(names(test_set) == 'degree_type.1')] <- 'degree'
#pivoting to make tidy
test_set2 <- pivot_longer(test_set, 
             cols= starts_with("degree_type."), 
             names_to= NULL,
             values_to = "subject", 
             names_repair = "minimal", 
             values_drop_na=TRUE)
#removing empty rows
test_set2 <- test_set2[!(is.na(test_set2$subject) | test_set2$subject ==""), ]


#rename address 1 &2 
#delete "Web.site" from address.1"
test_set2$address.1 <- str_replace(test_set2$address.1, pattern =".Web Site", "")
colnames(test_set2)[which(names(test_set2) == 'address.2')] <- 'url'
#parse address into street address, city, state, zip
#zip
test_set2$zip <- str_extract(test_set2$address.1, "[0-9]{5}")
#state
test_set2$state <- str_extract(test_set2$address.1, "[A-Z]{2}")
#delete zip and state
test_set2$address.1 <- str_replace(test_set2$address.1, pattern =", [A-Z]{2} [0-9]{5}", "")
#city (use first instance of comma from the end of the string)
tmp <- as_tibble(str_split_fixed(test_set2$address.1, "[,](?=[^,]*$)" , n=2))
colnames(tmp)[which(names(tmp) == 'V1')] <- 'streetaddress'
colnames(tmp)[which(names(tmp) == 'V2')] <- 'city'

#add back in temp address and delete 'address.1'
test_set2 <- bind_cols(test_set2, tmp)
test_set2 <- select(test_set2, -address.1)
test_set2 <- test_set2 %>% relocate(any_of(c("streetaddress","city","state","zip")), .after=name)


#left to do
#parse type into helpful categories ?

#test complete! Now to apply to the whole list.
full_list <- apply(X= nasad_URL_tbl, FUN = nasad_f, MARGIN = 1)
str(full_list)
head(full_list)

#selectors aren't specific enough as html class tags differ on different pages.
subset_full_f <- function(x){
  school_name <- html_text(html_elements(x, "h2:nth-child(1)")) 
  text <- html_text(html_elements(x, "h3 , p , h2"))
  school_info <-data.frame(school_name, text)
  return(school_info)
}
#broward college as example why the class selectors are failing.
nasad_web_full[27,]
html_text(html_elements(full_list[[27]],"p:nth-child(5)"))

full_info <- lapply(full_list, subset_full_f)  %>% 
  data.table::rbindlist()
head(full_info)
str(full_info)
