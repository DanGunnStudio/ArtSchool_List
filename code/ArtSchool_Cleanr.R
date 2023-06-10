#written by Dan Gunn 2023
#script cleaning a scraped list of art schools in the US.

library(tidyverse)

#read in CSV and inspect
full_df <- read_csv("data/cleaned_artschool_df.csv")


#STATES
states <- unique(full_df$state)%>% sort()
length(states)

index <- states %in% state.abb
states[!index]
#  "DC" "FN" "HA" "PR" "SE"
#leaving PR and DC alone, fixing the rest. 
#Also in Tableau we have "NA" states

pattern_to_match <- "SE"
updated_df <- full_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, state), "GA", state))

pattern_to_match <- "FN"
updated_df <- updated_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, state), "AR", state))

pattern_to_match <- "HA"
updated_df <- updated_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, state), "PA", state))

#MISSING
#NA state names are all international programs. Eliminate?
full_df %>% filter(., is.na(state))  %>% print(n=60)

#NA zips are all international programs except for University of North Florida
full_df %>% filter(., is.na(zip))  %>% print(n=60)

pattern_to_match <- "Kennesaw State University"
updated_df <- updated_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, school_name), "GA", state))

pattern_to_match <- "University of Texas at San Antonio"
updated_df <- updated_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, school_name), "TX", state))

pattern_to_match <- "Kent State University"
updated_df <- updated_df %>%
  mutate(state = ifelse(grepl(pattern_to_match, school_name), "OH", state))
updated_df %>% filter(str_detect(school_name, "Kent")) %>% print(n=40)

#not working
pattern_to_match <- "Arizona State University (SPPA)"
updated_df <- updated_df %>%
  mutate(url = ifelse(grepl(pattern_to_match, school_name), "design.asu.edu", url))

#brute force
updated_df$url[224:228] <- "design.asu.edu"

#fixing ZIPS
pattern_to_match <- "University of North Florida"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "32246", zip))

pattern_to_match <- "Chabot College"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "94545", zip))

pattern_to_match <- "Los Angeles Academy of Figurative Art"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "91406", zip))

pattern_to_match <- "University of Arizona"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "85721", zip))

pattern_to_match <- "Morehead State University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "40351", zip))

pattern_to_match <- "Cleveland Institute of Art"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "44106", zip))

pattern_to_match <- "Stephen F. Austin State University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "75962", zip))

pattern_to_match <- "Biola University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "90639", zip))

pattern_to_match <- "Mississippi Valley State University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "38941", zip))

pattern_to_match <- "West Valley College"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "95070", zip))

pattern_to_match <- "California State University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "91330", zip))

pattern_to_match <- "Lawrence Technological University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "48075", zip))

pattern_to_match <- "Jacksonville State University"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "36265", zip))

pattern_to_match <- "Cranbrook Academy of Art"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "48303", zip))

pattern_to_match <- "East Central College"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "63084", zip))

pattern_to_match <- "University of Nebraska Omaha"
updated_df <- updated_df %>%
  mutate(zip = ifelse(grepl(pattern_to_match, school_name), "68182", zip))

updated_df %>% filter(str_detect(updated_df$street, "26265"))

#NASAD doesn't list any schools in "DE" "HI" "NH" "NM". Confirmed via web search as well.
index <- state.abb %in% states 
state.abb[!index]

#removing international programs
updated_df <- updated_df %>% filter(!is.na(state))

updated_df %>% filter(zip =="14000")

#DEGREES
full_df %>% filter(., is.na(degree))
#no missing

#inspecting
unique(full_df$degree)
#need split degree into degree and years in order to condense degrees (using the dash)
updated_df <- updated_df %>% 
  mutate(degree= str_split_fixed(degree, pattern="[-]|[–]", n=Inf)) %>% 
  do.call(data.frame, .) %>%
  unite(years, degree.2, degree.3, degree.4, sep="") 

updated_df <- rename(updated_df, id=...1)
colnames(updated_df)

full_df %>%distinct(degree) %>% nrow() #213 options
updated_df %>% distinct(degree.1) %>% nrow() #now 98

#still have some digits in the degrees.
updated_df %>% distinct(degree.1)

updated_df %>% distinct(years)

#cleaned unseparated years
index <- updated_df %>% filter(grepl("\\d", degree.1)) %>% select(id) %>% as.vector()
str(index)

new_rows <- updated_df %>% 
  filter(grepl("\\d", degree.1)) %>%
  mutate(degree= str_split_fixed(degree.1, pattern="(?<=\\D)(?=\\d)", n=Inf)) %>% 
  do.call(data.frame, .) %>%  
  unite(years, degree.2, degree.3, degree.4, years, sep= "") %>%
  select(-degree.1) %>%
  rename(degree.1 = degree.1.1) %>%
  relocate(degree.1, .after = state)

#need to overwrite problematic rows.
updated_df[unlist(index), ] <- new_rows
#checking
updated_df %>% filter(grepl("\\d", degree.1))
#worked

#inspecting degree names

#removing whitespace
updated_df$degree.1 <- trimws(updated_df$degree.1)
#ASSOCIATES
updated_df %>% group_by(degree.1) %>% 
  filter(grepl("Associate", degree.1)) %>% 
  count()%>% arrange(desc(n)) %>%
  print(n=25)

#combine in / of
newer_rows <- updated_df %>% 
  filter(grepl("Associate", degree.1))%>%
  mutate(degree.1 = str_replace(degree.1, pattern = " in ", replacement = " of ")) 

#update rows
index <- newer_rows$id
updated_df[unlist(index), ] <- newer_rows
#maybe combine Arts and Fine Arts? No, because these are actually different

#BACHELORS
updated_df %>% group_by(degree.1) %>% 
  filter(grepl("Bach", degree.1)) %>% 
  count()%>% arrange(desc(n)) %>%
  print(n=25)

#MASTERS
updated_df %>% group_by(degree.1) %>% 
  filter(grepl("Mast", degree.1)) %>% 
  count()%>% 
  arrange(desc(n)) %>%
  print(n=50)

#CERTIFICATE
updated_df %>% group_by(degree.1) %>% 
  filter(grepl("Cert", degree.1)) %>% 
  count()%>% arrange(desc(n)) %>%
  print(n=50)

#changing Art to Arts
updated_df <- updated_df %>% 
  mutate(degree.1 = str_replace(degree.1, pattern=" Art ", replacement =" Arts "))

updated_df %>% group_by(degree.1) %>% 
  count()%>% arrange(desc(n)) %>%
  print(n=100)

#checking years
updated_df %>% group_by(years) %>% count()%>% arrange(desc(n)) %>%
  print(n=100)
#trimws
updated_df$years <- trimws(updated_df$years)


#SUBJECT
#visual art = visual arts
#fine arts = art?
#studio art = studio arts
updated_df %>% group_by(subject) %>% count()%>% arrange(desc(n)) %>%
  print(n=500)

updated_df <- updated_df %>% 
  mutate(subject = str_replace(subject, pattern="Arts", replacement ="Art"))

#i could condense more by throwing out what's in parentheses
#but it would limit the information available to the searcher? Maybe it'd be best to limit
updated_df %>% 
  mutate(subject = trimws(gsub("\\([^()]*\\)", "", subject))) %>% group_by(subject) %>% count()%>% arrange(desc(n)) %>%
  print(n=500)

updated_df <- updated_df %>% 
  mutate(subject = str_replace(subject, pattern="Art Studio", replacement ="Studio Art"))

updated_df <- updated_df %>% 
  mutate(subject = str_replace(subject, pattern="Painting/Drawing ", replacement ="Painting and Drawing"))

updated_df <- updated_df %>% 
  mutate(subject = str_replace(subject, pattern="Drawing and Painting", replacement ="Painting and Drawing"))


write_csv(updated_df, "data/supercleaned_artschool_df.csv")
