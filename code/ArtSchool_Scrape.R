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
full_nodes <- apply(X= nasad_URL_tbl, FUN = nasad_f, MARGIN = 1)
str(full_nodes)
head(full_nodes)

#selectors aren't specific enough as html class tags differ on different pages.
subset_full_f <- function(x){
  school_name <- html_text(html_elements(x, "h2:nth-child(1)")) 
  text <- html_text(html_elements(x, "h3 , p , h2"))
  school_info <-data.frame(school_name, text)
  return(school_info)
}
#broward college as example why the class selectors are failing.
nasad_web_full[27,]


full_info <- lapply(full_nodes, subset_full_f)  %>% 
  data.table::rbindlist()
head(full_info)
str(full_info)

#returns a full table of each heading and paragraph by school. Need to tidy.
full_about <- full_info %>% 
  group_by(school_name) %>%
  filter(grepl('^a\\s|^an\\s|research university|state-supported|land-grant|college of arts and sciences|coeducational|Guildhall', text, ignore.case = TRUE)) %>%
  rename(.,about = text)

full_address_url <- full_info %>% 
  group_by(school_name) %>%
  filter(grepl('Web Site|^\\d|[A-Z]{2}\\s[0-9]{5}', text)) %>%
  rename(.,address_url = text)

full_accred <- full_info %>%
  group_by(school_name) %>%
  filter(grepl('Date of Initial Accreditation', text)) %>%
  rename(.,address_url = text)

full_school_type <- full_info %>% 
  group_by(school_name) %>%
  filter(grepl('Public|Private', text)) %>%
  rename(.,school_type = text)

full_degree_type <- full_info %>% 
  group_by(school_name) %>%
  filter(grepl('Bachelor|Certificate|Associate of|Associate in|Master|Transfer|Advanced Fine Art Program-4 years.', text)) %>%
  rename(.,degree_type = text) 

#ending up with some strange lengths because schools have multiple campuses with different degree offerings. There are 333 schools, but more campuses

duplicated_campuses <- full_address_url$school_name[duplicated(full_address_url$school_name)] #30
matching_values <- full_address_url %>% filter(school_name %in% duplicated_campuses)
#showing 54 schools with multiple locations. We should have 54 schools with multiple degree types.

duplicated_degrees <- full_degree_type$school_name[duplicated(full_degree_type$school_name)]
length(duplicated_degrees) #now 32
matching_deg_values <- full_degree_type %>% filter(school_name %in% duplicated_degrees)

anti_join(full_address_url, full_degree_type)
#who has multiple campuses but is missing from the degree listing? 

full_degree_type %>% filter(school_name == "Rochester Institute of Technology")
#no web site for university of south florida st. petersburg campus. Need to recycle adding or statement to address pull.
#lengths now match.

#combining address and degree_type
full_df <- bind_cols(full_address_url, full_degree_type$degree_type)
full_df_2 <- distinct(full_df)
full_df_2 <- right_join(full_df, full_about)
full_df_3 <- distinct(full_df_2) %>% rename("degree_type" = ...3)

#still have some duplicates / addresses should be unique
fails <-full_df_3[which(duplicated(full_df_3$address_url)),]

#expanding address and url and breaking out degrees and subjects.

#str_split_fixed(full_df_3$address_url, pattern =".Web Site:", n=2) %>% as.data.frame()
full_df_3 <- full_df_3 %>% 
  separate(., address_url, 
           into = c("street", "url"), 
           sep =".Web Site:" )

colnames(full_df_3)

#extract address into street address, state, zip
#zip
full_df_3$zip <- str_extract(full_df_3$street, pattern="[0-9]{5}")
#state
full_df_3$state <- str_extract(full_df_3$street, "[A-Z]{2}")


#separate out the degrees& subjects
full_df_4 <- full_df_3 %>% 
  mutate(degree= str_split_fixed(degree_type, pattern="\\.", n=Inf))
#expand the data frame in the degree column
full_df_5 <- do.call(data.frame, full_df_4)
#pivot
full_df_5 <- pivot_longer(full_df_5,
                          cols=starts_with("degree."),
                          names_to = NULL,
                          values_to = "degree", 
                          names_repair = "minimal", 
                          values_drop_na=TRUE)

#now delete rows with NA or empty 'degree-type's
full_df_5 <- full_df_5[!(is.na(full_df_5$degree) | full_df_5$degree ==""), ]

#expand degrees to break out subjects into columns
full_df_6 <- full_df_5%>%
  mutate(subject = str_split_fixed(degree,pattern=":|;|\\.", n=Inf))

full_df_6 <- do.call(data.frame, full_df_6) %>% rename("tmp"= "subject.1")

full_df_7 <- pivot_longer(full_df_6, 
                          cols= starts_with("subject."), 
                          names_to= NULL,
                          values_to = "subject", 
                          names_repair = "minimal", 
                          values_drop_na=TRUE)
full_df_7 <- full_df_7[!(is.na(full_df_7$subject) | full_df_7$subject ==""), ]

#drop redundant degree column and change tmp
full_df_7 <- full_df_7%>% select(-degree) %>% rename("degree" = "tmp")


#need to remember the fails and investigate.
#exporting to csv
write.csv(full_df_7, file="data/cleaned_artschool_df.csv")
