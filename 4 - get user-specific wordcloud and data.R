#Section 4 allows you to look into a specific indvidual's timeline and trends.

#Enter the individual's username here
user = "NYTimeskrugman"

#Specificy the period you're interested in exploring. Use the same date format and replace the dates between " " below. 
##Note that, to get valuable insights about an individual, it's better to include as wide of a date-range as possible (becase the sample size is not as large as with multiple users' data). 
date1 = as.Date("2014-06-30","%Y-%m-%d")
date2 = as.Date("2015-06-30","%Y-%m-%d")


#These variables should be identical to those in parts 1,2 and3; only changed them if you chanegd the location or name of the file produced in part 1. 
##The program will look for files in the directory specificed below basd on the name of the "Topic" and will not work if there is a mis-match. 
###If you change anything from sections 1,2 or 3 (like the file name or path), enter them below and make sure to use forward slash (/) and not backslah (\). 

location = "H:/Projects/R/"
topic = "Economics"

#Please select "run-all"

library(network)
library(httr) 
library(devtools)
library(twitteR)
library(xlsx)
library(plyr)
library(ggplot2)
library(zoo)
library(stringr)
library(wordcloud)

api_key <- "bxcaT2LFsolsnmJAxJNLCKjmO"
api_secret <- "4mJnhfDND84onydHi29nRi87kSyoDRasPge14IpnPvU3pwLZTm"
access_token <- "610740494-h1UMQNJ1GKN1BWT6jMhhtKux3KT9JfKVIzzh1Wnt"
access_token_secret <- "VdJgH23KU4TLvZyYiy5gG7Rgn1h5RrFIkbKDwXeCKSrG4"
setup_twitter_oauth(api_key,api_secret)


all_tweets = matrix(nrow=0,ncol=16)
colnames(all_tweets) = c("text","favorited","favoriteCount","replyToSN","created","truncated","replyToSID","id","replyToUID","statusSource","screenName","retweetCount","isRetweet","retweeted","longitude","latitude")   
raw_tweets = userTimeline(user,n=3200,includeRts=TRUE)
raw_tweets_df = twListToDF(raw_tweets)
all_tweets_final = rbind(all_tweets,raw_tweets_df)
all_tweets_final

##wordcloud


##most retweeted 

a=matrix(nrow=0,ncol=16)
for (i in 1:nrow(all_tweets_final))
{
  if (all_tweets_final[i,"isRetweet"] == TRUE) 
  {
    a=c(a,i)
  }
}
retweeted = all_tweets_final[a,]


d = matrix(nrow=nrow(retweeted),ncol=1)
vector = matrix(nrow=0,ncol=1)
for(i in 1:nrow(retweeted))
{
  text=unlist(strsplit(retweeted[i,"text"], ' '))
  retweeted_person=grep(pattern = "^@", text, value = TRUE, fixed=FALSE)
  vector = as.matrix(c(vector,retweeted_person))
}

colnames(vector) = ("Retweeted Account")
retweeted_freq = sort(table(vector),decreasing=T)
retweeted_freq


##most shared links

e = matrix(nrow=nrow(all_tweets_final),ncol=1)
vector = matrix(nrow=0,ncol=1)
for(i in 1:nrow(all_tweets_final))
{
  text_web=unlist(strsplit(all_tweets_final[i,"text"], ' '))
  links=grep(pattern = "^http", text_web, value = TRUE, fixed=FALSE)
  vector = as.matrix(c(vector,links))
}

write.xlsx2(vector,paste(location,"1.xls"))

colnames(vector) = ("Website Tweeted")
links_freq = sort(table(vector),decreasing=T)
links_freq
links









####### not in use:

###full_retweets = cbind(retweeted,vector)
#nrow(retweeted)

  if(length(retweeted_people)>0)
  {
    temp=matrix(nrow=length(hashtags),ncol=4)
    for(j in 1:length(hashtags)){
    temp[j,1]=tweets[i,"screenName"]
    temp[j,2]=as.character(as.Date(all_tweets[i,"created"]))
    temp[j,3]=all_tweets_df[i,"text"]
    temp[j,4]=tolower(retweeted[j])
    all_retweeted=rbind(retweeted,temp)
  }
}
}
all_hashtags_omitted = na.omit(all_hashtags)

