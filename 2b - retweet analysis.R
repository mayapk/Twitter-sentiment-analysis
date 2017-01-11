######parse out retweets

location= "C:/Users/MPER/Desktop/R/Test/"
topic = "Economics"

#Please specificy the period you're interested in exploring. Use the same date format and replace the dates between " " below.
date1 = as.Date("2014-06-30","%Y-%m-%d")
date2 = as.Date("2015-06-30","%Y-%m-%d")

#Please specificy a number of top people retweeted to see in the graph.
##If you are running this part for the first time, leave n as-is, adjust after you get the initial results and then re-run
n = "10"

load(paste(location,topic,".RData"))

all_tweets_final = all_tweets_df


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
  vector = (c(vector,retweeted_person))
}

colnames(vector) = ("Retweeted Account")

retweeted_freq = sort(table(vector),decreasing=T)
write.xlsx2(retweeted_freq,paste(location,topic,"-Top Retweeted.xls"))

if (n != "")
{
  top_retweeted = head(retweeted_freq,n=n)
}

class(top_retweeted)
col_1 = c(rownames(top_retweeted))
col_2 = top_retweeted[,1]
as.data.frame(top_retweeted)
max(top_retweeted)

barplot(top_retweeted, axes=F)
