require(tidyverse)
require(tm)
require(tokenizers)
require(slam)
require(topicmodels)
library(rlang)




mydir <- "/Users/juanberasateguigallego/Desktop/NHH Høst 2022/Applied Textual Analysis Fin/G2"

#Delete all files that are defined as "As written" i the folder
delfiles <- dir(path=mydir ,pattern="written")

file.remove(file.path(mydir, delfiles))

#Import files from 2016 defined as spoken speeches and pull out filenames 
#which include Upper cases, which indicates states. 
file_list <- list.files("TrumpSpeeches/", 
                        pattern = ("2016.+Speech.+\\b[A-Z]+\\b"),
                        full.names = TRUE)

#Make a function to delete unnessesary data in the speeches
delete_word<-function(x, y) {
  gsub(x," ", y)
}

#Read in a csv with statenames and codes
states <-read_csv("csvData.csv")

#Create characters out of the files  
trump_speeches <- sapply(file_list, 
                         FUN = function(x)readChar(x, file.info(x)$size))



#Clean speeches no crowd speaking, no applauses etc
for (x in 1:241) {
  trump_speeches[x]<-delete_word("UNIDENTIFIED\\s*(.*?)\\s*TRUMP:",trump_speeches[x])
  trump_speeches[x]<-delete_word("\n\n",trump_speeches[x])
  trump_speeches[x]<-delete_word("[[:digit:]]",trump_speeches[x])
  trump_speeches[x]<-delete_word("[[:punct:]]",trump_speeches[x])
  trump_speeches[x]<-delete_word("\\b[[:alpha:]]{1,5}\\b",trump_speeches[x])
  trump_speeches[x]<-delete_word("\\b[[:alpha:]]{21,}\\b",trump_speeches[x])
  trump_speeches[x]<-delete_word("\\b[A-Z]+\\b",trump_speeches[x])
  trump_speeches[x] <- tolower(trump_speeches[x])
  
}

#Removing stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
trump_speeches = str_replace_all(trump_speeches, stopwords_regex, '')


#Creates list of states codes which match with each file, extract
#the two letters on index -6 and -5 to pull out the state id

state_list <- str_sub(file_list, -6,-5) 

#Creates a tibble out of the speeches and mutate the state list to 
#to identify which state the speeches are from. 
data <- as.tibble(trump_speeches)%>%
  mutate(state = state_list)


#Make corpus
corpus <- VCorpus(VectorSource(trump_speeches))

#Make a dtm
dtm<-DocumentTermMatrix(corpus)


dtm <- dtm[row_sums(dtm) > 4,]
dim(dtm)


#Create the topic model
topic <- LDA(dtm,  # document term matrix
             k = 20, # specifify number of topics
             method = "Gibbs",
             control = list(
               seed = 1234, # eases replication
               burnin = 100,  # how often sampled before estimation recorded
               iter = 300,  # number of iterations
               keep = 1,    # saves additional data per iteration (such as logLik)
               save = F,     # saves logLiklihood of all iterations
               verbose = 10  # report progress
             ))

#Log of probability reported
beta <- exp(topic@beta) 
dim(beta)


#Chech top 20 terms in the topics
head(topic@terms[order(beta[12,], decreasing = T)], 20)

#Estimating the gamma for each topic in each speech
gamma <- topic@gamma
dim(gamma)

#Creating a tibble with the gamma values and mutate the state list
data<- as.tibble(gamma) %>%
  mutate(state=state_list)

#Checking unique states in the state list 
state_list %>% 
  unique() -> unique_state_test


#Counts how many speeches within the unique states
data %>%
  select("state")%>%
  count(state)->speech_count


#Adding speechcount to the data with gamma values.
data %>%
  left_join(speech_count, by="state") %>%
  mutate() -> test


#Calculating the mean for all the topics in texas
data%>%
  filter(state=="TX")%>%
  select(-last_col())%>%
  col_means()%>%
  t()%>%
  as.tibble()%>%
  mutate(Code="TX") -> tibb


#Calculating the mean for alle the topics in each state
for (x in speech_count$state){
  print(x)
  data%>%
    filter(state==x)%>%
    select(-last_col())%>%
    col_means()%>%
    t()%>%
    as.tibble()%>%
    mutate(Code=x)->dibb
  tibb%>%
    add_row(dibb)%>%
    unique()->tibb
  
}

#Adding the the mean values to data "States"
states%>%
  left_join(tibb)->states


#Chech top 20 terms in the topics
head(topic@terms[order(beta[13,], decreasing = T)], 20)




#Description to each topic - Erlend 
Description <- c("States","Abreviation","Code","Terrorism", "Undefined1", "Undefined3", "Nationalism1",
                        "Economy", "Immigration", "Energy", "Election1", "Undefined2",
                        "Mining industry", "Religion", "Manufacturing", "Election3",
                        "Nationalism2", "Military1", "Drugs","Military2","Undefined",
                        "Veterans", "Election2")%>%
  as.tibble()

colnames(states) <- Description

#Delete duplicated variables
states %>%
  mutate(Nationalism = Nationalism1/2 + Nationalism2/2,
         Election = Election1/3 + Election2/3 + Election3/3,
         Military = Military1/2 + Military2/2)%>%
  select(-Nationalism1,-Nationalism2, -Nationalism1, -Nationalism2,-Military1,
         -Military2,-Undefined1,-Undefined,-Undefined2,-Undefined3,
         -Election1,-Election2,-Election3)->states


#Function to make the map generation more user friendly
get_map_topic <-function(topic) {
  #Creating dataa to the map
  assign("y",topic)
  displayMap <- tibble(state = states$States,
                       state.abb = states$Abreviation,
                       value = unlist(states %>%
                                        select(y)))
  
  #Plotting the map
  plot_usmap(data = displayMap,
             values = "value",
             color = "blue",
  ) +
    scale_fill_continuous(low = "white", high = "blue",
                          name = "Topic ", label = scales::comma) +
    theme(legend.position = "right")+
    labs(title=topic)+
    theme(plot.title = element_text(hjust = 0.5))
  
}

get_map_topic("Immigration")
