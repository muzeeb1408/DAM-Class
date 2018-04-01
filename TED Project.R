rm(list=ls())
#install.packages('anytime')
#install.packages('stringr')
install.packages("stargazer")
install.packages("MASS")
install.packages("corrplot")
library(corrplot)
library(MASS)
library(stargazer)
library(tidytext)
require(tm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(ggplot2)


ted_main        <- read.csv("D:\\Google Drive Backup\\Data Analytics Course-Sudhir Voleti\\TED Project\\ted-talks\\ted_main.csv")
#ted_transcripts <- read.csv("D:\\Google Drive Backup\\Data Analytics Course-Sudhir Voleti\\TED Project\\ted-talks\\transcripts.csv")
data <-ted_main  # merge(ted_main ,ted_transcripts,by="url")

### Converting the Unix time stamos into dates
library(anytime)
data$film_date = anydate(data$film_date)
data$published_date = anydate(data$published_date)
data$year_published <- year(data$published_date)
data$years <- 2018-data$year_published 

#Creating Various Variables
data$num_tags <- str_count(data$tags, ",")
data$num_tags <- data$num_tags+1
data$ln_views <- log(data$views)
data$duration <- (data$duration/60)
data$num_speakers<- data$num_speaker

#Function to Clean Text
text.clean = function(d){                    # x = text data
  x        <-   data$description
  require("tm")
  x = gsub("http\\w+", "", x)
  x = gsub("#\\w+", "", x)
  x = gsub("&amp", "",x)
  x = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  x = gsub("@\\w+", "", x)
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("[[:digit:]]", " ", x)
  x = gsub("http\\w+", "", x)
  x = gsub("#\\w+", "", x)
  x = gsub("[ \t]{2,}", " ", x)
  x = gsub("^\\s+|\\s+$", "", x) 
  
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  #  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  
  # Read Stopwords list
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  stopwords = unique(gsub("'"," ", stpw2))  # final stop word lsit after removing punctuation
  
  x  =  removeWords(x,stopwords)            # removing stopwords created above
  x  =  stripWhitespace(x)                  # removing white space
  #  x  =  stemDocument(x)                   # can stem doc if needed.
  
  
  clean_text_df        <-   data_frame(title = d$title, text = x)
  return(clean_text_df )
  
}

#Cleaning the Text Data
clean_text<-data %>% text.clean()

Clean_data<-merge(data ,clean_text,by="title")

#Fuction to Sentiment Analysis using BING lexicon
sentiment_analysis_bing<- function(data_frame ){
  textdf<- data_frame
  
  bing <- textdf %>% ungroup() %>%
                              unnest_tokens(word, text) %>%
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing")%>%
                                count(method, title= title, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(polarity = positive - negative)
    return( bing )
}

sentiment_analysis_bing<- function(data_frame ){
  textdf<- data_frame
  
  bing <- textdf %>% ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing")%>%
    count(method, title= title, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(polarity = positive - negative)
  return( bing )
}

#Performing Sentiment Analysis Using BING
Bing_Sent_Ann <-Clean_data%>%  sentiment_analysis_bing()

#Fuction to Sentiment Analysis using NRC lexicon

sentiment_analysis_nrc<- function(data_frame ){
  textdf<- data_frame
  

nrc<-textdf %>% ungroup() %>%
  unnest_tokens(word, text)%>% 
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive", 
                                       "negative"))) %>%
  mutate(method = "NRC") %>%
  count(method, title= title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative)

return( nrc )
}
#Performing Sentiment Analysis Using NRC
NRC_Sent_Ann <-Clean_data%>%  sentiment_analysis_nrc()

data_final_bing <- inner_join(data,Bing_Sent_Ann)
data_final_bing<- data_final_bing[,c('views','ln_views','positive','negative','num_tags','num_speakers','polarity','years', 'duration')]
data_final_nrc <- inner_join(data,NRC_Sent_Ann)
data_final_nrc<- data_final_nrc[,c('views','ln_views','positive','negative','num_tags','num_speakers','polarity','years','duration')]

## Data Summary
stargazer(data_final_bing,type="text",title="Summary for BING Data")
stargazer(data_final_nrc,type="text",title="Summary for NRC Data")


###Correlation matrix
correlation.matrix_bing <- cor(data_final_bing[,c('views','ln_views','positive','negative','num_tags','num_speakers','polarity','years', 'duration')])
stargazer(correlation.matrix_bing,type="text", title="Correlation Matrix for BING Data")

correlation.matrix_nrc <- cor(data_final_nrc[,c('views','ln_views','positive','negative','num_tags','num_speakers','polarity','years', 'duration')])
stargazer(correlation.matrix_nrc,type="text", title="Correlation Matrix for NRC Data")


###OLS Regression
OLS1<- lm(formula = ln_views ~  positive+negative+ num_tags + years + duration + num_speakers, data = data_final_bing)
OLS2 <- lm(formula = ln_views ~ positive+negative+ num_tags + years + duration + num_speakers, data = data_final_nrc)


##Plotting the regression tables

stargazer(OLS1,OLS2, type="text",title="OLS Regression Results",##
          align=TRUE, dep.var.labels=c("Log of No. of Views"),##
          covariate.labels=c("Positive Sentiment","Negative Sentiment",##
                             "No. of tags on the talk","No. of Years since published",
                             "Duration of the talk","No. of speakers"),column.labels=c("Bing","NRC"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)







