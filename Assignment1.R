require(tidyverse)
require(tokenizers)
require(stopwords)
require(janeaustenr)
require(tm)
require(slam)
require(quanteda)
require(qlcMatrix)
require(Metrics)

setwd("/Users/juanberasateguigallego/Desktop/NHH Høst 2022/Applied Textual Analysis Fin/Lecture K means")
load("firm_dataset.Rdata")

#Clean digits, punctuation, and small and large words
section.1.business<-tolower(section.1.business)
section.1.business <- gsub('[[:digit:]]', ' ', section.1.business)
section.1.business <- gsub('[[:punct:]]',' ', section.1.business)
section.1.business <- gsub("\\b[[:alpha:]]{1,3}\\b", " ", section.1.business)
section.1.business <- gsub("\\b[[:alpha:]]{21,}\\b", " ", section.1.business)

#remove stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
section.1.business = str_replace_all(section.1.business, stopwords_regex, '')

#tokenize and make bigrams
section.1.business<-tokenize_ngrams(section.1.business, n=2,ngram_delim = "_")

#make corpus
corpus <- VCorpus(VectorSource(section.1.business))

#make document term matrix
dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            bounds = list( global = c(.01, .20)*length(section.1.business))))


#get the target bigrams which are in more than 5 but less than 100 docs
DF <- tidy(dtm)
DF%>%
  filter(duplicated(term) == FALSE)%>%
  select(term)->DF
# make binary
#dtm$v <- rep(1, length(dtm$v))

#make dataset with text in bigram format and corresponding cik
section.1.business
data<-tibble(bigrams = section.1.business,
             cik = raw.data$cik)

#Get cik for oil companies
raw.data%>%
  filter(industry.fama.french.49=="30 Oil")%>%
  select(cik)->oil.companies
#Get cik for non-oil companies
raw.data%>%
  filter(industry.fama.french.49 != "30 Oil")%>%
  select(cik)->not.oil.companies

#make tibbles of (non)oil bigrams texts
data%>%
  filter(cik %in% oil.companies$cik)->oil.cos.dtm.material
data%>%
  filter(cik %in% not.oil.companies$cik)->not.oil.cos.dtm.material

#make corpus of (non)oil bigram texts
corpus.oil <-Corpus(VectorSource(oil.cos.dtm.material))
corpus.not.oil <-Corpus(VectorSource(not.oil.cos.dtm.material))

#make DTM and then turn into tibble
targetDTM <- DocumentTermMatrix(corpus.oil)
comparisonDTM<- DocumentTermMatrix(corpus.not.oil)
DF.oil <- tidy(targetDTM)
DF.not.oil <- tidy(comparisonDTM)

#Clean the way the bigrams look
DF.not.oil$term<-gsub(",","",as.character(DF.not.oil$term))
DF.not.oil$term<-gsub('"',"",as.character(DF.not.oil$term))
DF.oil$term<-gsub(",","",as.character(DF.oil$term))
DF.oil$term<-gsub('"',"",as.character(DF.oil$term))


LL.list = list()

#Make a list of the bigrams from DF, that is the bigrams that appear (5 to 100 docs)
bigrams = as.list(DF$term)

#LOOP to get all LL for each bigram in the bigrams list
for (x in 1:41198) {
  word <- bigrams[[x]]
  
  DF.oil %>%
    filter(term ==word)%>%
    pull(count)->a
  ifelse(is_empty(a), 0,a)->a
  
  a=a/78551
  
  DF.not.oil %>%
    filter(term==word)%>%
    pull(count)->b
  ifelse(is_empty(b), 0,b)->b
  b=b/1798101
  c=78551
  d=1798101
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  LL.list[[x]]<-ll
}

#Get the 500 bigrams with highest LL
LL.list2 = unlist(LL.list)
DF %>%
  mutate(LL = LL.list2)%>%
  arrange(desc(LL))%>%
  head(500)->high.ll


###############################################


corpus <- Corpus(VectorSource(section.1.business))
dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            bounds = list( global = c(.01, .20)*length(section.1.business))))
# make binary
dtm$v <- rep(1, length(dtm$v))
#scale to unit length
dtm$v <- dtm$v / sqrt(row_sums(dtm^2))[dtm$i]
# check scaling
sum(dtm[1,]^2)

#List cik for (non)oil companies
list.oil.companies <-as.list(oil.companies$cik)
list.not.oil.companies <- as.list(not.oil.companies$cik)

#Dummy variable to iterate over
n =1
#Target list
not.oil.portfolio =list()

#Cosine similarity formula
CosineSimilarity <- function(A, B) {  
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) }

#Loop to get the cosine similarity of each oil firm with each non oil firm co
for (x in 1:18){
  for (y in 1:482){
    score <-CosineSimilarity(A = dtm[raw.data$cik == as.character(list.oil.companies[[x]]),], 
                     B = dtm[raw.data$cik == as.character(list.not.oil.companies[[y]]),])
    
    not.oil.portfolio[[n]]<-score
    n=n+1
  }
  
}

#Unlist and create matrix of cosine score
not.oil.portfolio = unlist(not.oil.portfolio)
cossimtibble<-as.tibble(matrix(not.oil.portfolio, nrow=482))

#Add cik to the data
cossimtibble %>%
  mutate(cik = not.oil.companies$cik)->cossimtibble

#Create target list and dummy variable to iterate over
portfolios<- list()
n=1

#Loops which gets the 5 peer firms per oil company
for (x in 1:18) {
  line<-cossimtibble[[x]]
  variable <-paste0("V",x)
  cossimtibble %>%
    select(variable, "cik")%>%
    mutate(line2=line)%>%
    arrange(desc(line2))%>%
    head(5)%>%
    select("cik")%>%
    pull()->portfolios[[x]]
  
}

#Target List to vector and then tibble
unlist(portfolios)
not.oil.portfolios<-as.tibble(unlist(portfolios))

#Get the average portfolio monthly return for (non) oil portfolio
not.oil.portfolios %>%
  rename(cik =value)%>%
  left_join(raw.data, by= "cik")%>%
  select("return.monthly.NY.m01","return.monthly.NY.m02" ,"return.monthly.NY.m03" ,
           "return.monthly.NY.m04" ,"return.monthly.NY.m05","return.monthly.NY.m06",
           "return.monthly.NY.m07" ,"return.monthly.NY.m08","return.monthly.NY.m09",
           "return.monthly.NY.m10" ,"return.monthly.NY.m11" ,"return.monthly.NY.m12" 
           )%>%
  summarise(across(everything(), list(mean)))->not.oil.companies.returns
not.oil.companies.returns<-unlist(list(not.oil.companies.returns))

oil.companies %>%
  left_join(raw.data, by= "cik")%>%
  select("return.monthly.NY.m01","return.monthly.NY.m02" ,"return.monthly.NY.m03" ,
         "return.monthly.NY.m04" ,"return.monthly.NY.m05","return.monthly.NY.m06",
         "return.monthly.NY.m07" ,"return.monthly.NY.m08","return.monthly.NY.m09",
         "return.monthly.NY.m10" ,"return.monthly.NY.m11" ,"return.monthly.NY.m12" 
  )%>%
  summarise(across(everything(), list(mean)))->oil.companies.returns
oil.companies.returns<-unlist(list(oil.companies.returns))  
  
 
#See the yearly return per portfolio
sum(not.oil.companies.returns)
sum(oil.companies.returns)

#Calculate the root mean squared error
rmse(oil.companies.returns, not.oil.companies.returns)




#####################SAME BUT WITH UNIGRAMS##################

load("firm_dataset.Rdata")

#Clean digits, punctuation, and small and large words
section.1.business<-tolower(section.1.business)
section.1.business <- gsub('[[:digit:]]', ' ', section.1.business)
section.1.business <- gsub('[[:punct:]]',' ', section.1.business)
section.1.business <- gsub("\\b[[:alpha:]]{1,3}\\b", " ", section.1.business)
section.1.business <- gsub("\\b[[:alpha:]]{21,}\\b", " ", section.1.business)

#remove stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
section.1.business = str_replace_all(section.1.business, stopwords_regex, '')

#tokenize and make bigrams
section.1.business<-tokenize_ngrams(section.1.business, n=1)

#make corpus
corpus <- VCorpus(VectorSource(section.1.business))

#make document term matrix
dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            bounds = list( global = c(.01, .20)*length(section.1.business))))


#get the target bigrams which are in more than 5 but less than 100 docs
DF <- tidy(dtm)
DF%>%
  filter(duplicated(term) == FALSE)%>%
  select(term)->DF
# make binary
#dtm$v <- rep(1, length(dtm$v))

#make dataset with text in bigram format and corresponding cik
section.1.business
data<-tibble(bigrams = section.1.business,
             cik = raw.data$cik)

#Get cik for oil companies
raw.data%>%
  filter(industry.fama.french.49=="30 Oil")%>%
  select(cik)->oil.companies
#Get cik for non-oil companies
raw.data%>%
  filter(industry.fama.french.49 != "30 Oil")%>%
  select(cik)->not.oil.companies

#make tibbles of (non)oil bigrams texts
data%>%
  filter(cik %in% oil.companies$cik)->oil.cos.dtm.material
data%>%
  filter(cik %in% not.oil.companies$cik)->not.oil.cos.dtm.material

#make corpus of (non)oil bigram texts
corpus.oil <-Corpus(VectorSource(oil.cos.dtm.material))
corpus.not.oil <-Corpus(VectorSource(not.oil.cos.dtm.material))

#make DTM and then turn into tibble
targetDTM <- DocumentTermMatrix(corpus.oil)
comparisonDTM<- DocumentTermMatrix(corpus.not.oil)
DF.oil <- tidy(targetDTM)
DF.not.oil <- tidy(comparisonDTM)

#Clean the way the bigrams look
DF.not.oil$term<-gsub(",","",as.character(DF.not.oil$term))
DF.not.oil$term<-gsub('"',"",as.character(DF.not.oil$term))
DF.oil$term<-gsub(",","",as.character(DF.oil$term))
DF.oil$term<-gsub('"',"",as.character(DF.oil$term))


LL.list = list()

#Make a list of the bigrams from DF, that is the bigrams that appear (5 to 100 docs)
bigrams = as.list(DF$term)

#LOOP to get all LL for each bigram in the bigrams list
for (x in 1:8547) {
  word <- bigrams[[x]]
  
  DF.oil %>%
    filter(term ==word)%>%
    pull(count)->a
  ifelse(is_empty(a), 0,a)->a
  
  a=a/6086
  
  DF.not.oil %>%
    filter(term==word)%>%
    pull(count)->b
  ifelse(is_empty(b), 0,b)->b
  b=b/35742
  c=6086
  d=35742
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  LL.list[[x]]<-ll
}

#Get the 500 bigrams with highest LL
LL.list2 = unlist(LL.list)
DF %>%
  mutate(LL = LL.list2)%>%
  arrange(desc(LL))%>%
  head(500)->high.ll


###############################################


corpus <- Corpus(VectorSource(section.1.business))
dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            bounds = list( global = c(.01, .20)*length(section.1.business))))
# make binary
dtm$v <- rep(1, length(dtm$v))
#scale to unit length
dtm$v <- dtm$v / sqrt(row_sums(dtm^2))[dtm$i]
# check scaling
sum(dtm[1,]^2)

#List cik for (non)oil companies
list.oil.companies <-as.list(oil.companies$cik)
list.not.oil.companies <- as.list(not.oil.companies$cik)

#Dummy variable to iterate over
n =1
#Target list
not.oil.portfolio =list()

#Cosine similarity formula
CosineSimilarity <- function(A, B) {  
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) }

#Loop to get the cosine similarity of each oil firm with each non oil firm co
for (x in 1:18){
  for (y in 1:482){
    score <-CosineSimilarity(A = dtm[raw.data$cik == as.character(list.oil.companies[[x]]),], 
                             B = dtm[raw.data$cik == as.character(list.not.oil.companies[[y]]),])
    
    not.oil.portfolio[[n]]<-score
    n=n+1
  }
  
}

#Unlist and create matrix of cosine score
not.oil.portfolio = unlist(not.oil.portfolio)
cossimtibble<-as.tibble(matrix(not.oil.portfolio, nrow=482))

#Add cik to the data
cossimtibble %>%
  mutate(cik = not.oil.companies$cik)->cossimtibble

#Create target list and dummy variable to iterate over
portfolios<- list()
n=1

#Loops which gets the 5 peer firms per oil company
for (x in 1:18) {
  line<-cossimtibble[[x]]
  variable <-paste0("V",x)
  cossimtibble %>%
    select(variable, "cik")%>%
    mutate(line2=line)%>%
    arrange(desc(line2))%>%
    head(5)%>%
    select("cik")%>%
    pull()->portfolios[[x]]
  
}

#Target List to vector and then tibble
unlist(portfolios)
not.oil.portfolios<-as.tibble(unlist(portfolios))

#Get the average portfolio monthly return for (non) oil portfolio
not.oil.portfolios %>%
  rename(cik =value)%>%
  left_join(raw.data, by= "cik")%>%
  select("return.monthly.NY.m01","return.monthly.NY.m02" ,"return.monthly.NY.m03" ,
         "return.monthly.NY.m04" ,"return.monthly.NY.m05","return.monthly.NY.m06",
         "return.monthly.NY.m07" ,"return.monthly.NY.m08","return.monthly.NY.m09",
         "return.monthly.NY.m10" ,"return.monthly.NY.m11" ,"return.monthly.NY.m12" 
  )%>%
  summarise(across(everything(), list(mean)))->not.oil.companies.returns
not.oil.companies.returns<-unlist(list(not.oil.companies.returns))

oil.companies %>%
  left_join(raw.data, by= "cik")%>%
  select("return.monthly.NY.m01","return.monthly.NY.m02" ,"return.monthly.NY.m03" ,
         "return.monthly.NY.m04" ,"return.monthly.NY.m05","return.monthly.NY.m06",
         "return.monthly.NY.m07" ,"return.monthly.NY.m08","return.monthly.NY.m09",
         "return.monthly.NY.m10" ,"return.monthly.NY.m11" ,"return.monthly.NY.m12" 
  )%>%
  summarise(across(everything(), list(mean)))->oil.companies.returns
oil.companies.returns<-unlist(list(oil.companies.returns))  


#See the yearly return per portfolio
sum(not.oil.companies.returns)
sum(oil.companies.returns)

#Calculate the root mean squared error
rmse(oil.companies.returns, not.oil.companies.returns)


