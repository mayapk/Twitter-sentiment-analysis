#Section 2a separates out the hashtags from the statuses in Section 2 and creates a heatmap and a wordcloud based on the tweets from the obtained list of 100 individuals and a date range specified by the user. 
#It will also generate an excel file with the hashtag data for further independent analysis. 
#It should not take long to run. 

#Please specificy the period you're interested in exploring. 
#Use the same date format and replace the dates between " " below.
#NOTE: hashtags are best analyzed in short periods of up to 8 weeks, because of their short-lived nature. 
date1 = as.Date("2014-06-30","%Y-%m-%d")
date2 = as.Date("2015-06-30","%Y-%m-%d")

#These variables should be identical to those in part 1; only changed them if you chanegd the location or name of the file produced in part 1. If you do change the below, make sure to use forward slash (/) and not backslah (\)
location = "C:/Users/MPER/Desktop/R/Test/15-07-17/"
topic = "Innovation"

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

load(paste(location,topic,"Part2.Rdata")) 

########################################################################

#parse out hashtags 
all_hashtags=matrix(nrow=0,ncol=5)
for(i in 1:nrow(all_tweets_df))
{
  text=unlist(strsplit(all_tweets_df[i,"text"], ' '))
  hashtags=grep(pattern = "^#", text, value = TRUE, fixed=FALSE)
  if(length(hashtags)>0)
  {
    temp=matrix(nrow=length(hashtags),ncol=5)
    for(j in 1:length(hashtags))
    {
      
      #temp[j,1]=all_tweets_df[i,"screenName"]
      #temp[j,2]=as.character(as.Date(all_tweets[i,"created"]))
      #temp[j,3]=tolower(hashtags[j])
      #temp[j,4]=all_tweets_df[i,"text"]
      temp[j,5]=i
      all_hashtags=rbind(all_hashtags,temp)
    }
  }
}
all_hashtags_omitted = na.omit(all_hashtags)

#remove punctuation differences 
symbol1 = "."
symbol2 = ";"
symbol3 = ":"
symbol4 = ","
symbol5 = "'s"
symbols_to_remove = c(symbol1,symbol2,symbol3,symbol4,symbol5)

for (i in 1:length(symbols_to_remove))
{
  gsub(as.character(symbols_to_remove[i]),replacement="",as.character(all_hashtags_omitted[,3]),fixed=T)
}
 
for (i in 1:length(symbols_to_remove))
  {for (j in 1:length(test))
    {
      gsub(as.character(symbols_to_remove[i]),"",as.character(test[j]))
    }
}

#hashtags is now a matrix with all hashtags in column 3, save excel with all hashtags 
write.xlsx2(all_hashtags_omitted,paste(location,"Part 3 - All Hashtags",topic,".xls"))

#####filter by dates 

#first need to create data as df and add 5th column for data_formatted
ht_data_df = as.data.frame(all_hashtags_omitted)
colnames(ht_data_df) = c("User","Date","Hashtag","Text")
col_5 = as.Date(ht_data_df$Date)
ht_data_df_with5col = cbind(ht_data_df,col_5)
colnames(ht_data_df_with5col) = c("User","Date","Hashtag","Text","Date_F")

#now filter between dates 
data_filtered_bydate = ht_data_df_with5col[ht_data_df_with5col$Date_F >= date1 & ht_data_df_with5col$Date_F <= date2,]

#and write to excel
write.xlsx2(data_filtered_bydate,paste("C:/Users/MPER/Desktop/R/",topic,date1," - ",date2,".xls"))


##get top 10 hashtags
index=rep(0,0)
data_ordered = data_filtered_bydate[order(data_filtered_bydate$Hashtag, decreasing=T),]
data_counted = count(data_ordered, "Hashtag")
data_ranked = data_counted[order(data_counted$freq, decreasing=T),]
#top_hashtags = head(data_ranked,n=10)
top_hashtags = sort(table(data_ordered[,3]),decreasing = TRUE)[1:10]
as.data.frame(top_hashtags)

a=rep(0,0)
for(i in 1:nrow(data_filtered_bydate))
{if(data_filtered_bydate[i,3] %in% rownames(top_hashtags) == TRUE)
{a=c(a,i)}}
data_final=data_filtered_bydate[a,]


#create date bins
#### I AM HERE

x_range=range(date2-date1)
y_range=range(top_hashtags)

p=round((date2-date1)/30)
bins=seq(date1,date2,p)

plot(x_range, y_range, type="n", xlab="Period", ylab="Frequency") 
colors = rainbow(length(top_hashtags)) 
linetype = c(1:length(top_hashtags)) 
plotchar = seq(18,18+length(top_hashtags),1)

for(i in 1:length(top_hashtags)) 
{ 
    lines(data_final$Date, data_final$Hashtag, type="b", lwd=1.5, lty=linetype[i], col=colors[i]) 
} 

help(lines)

x=count(data_final[,"Hashtag"])
x[order(x$freq,decreasing=T),]

bar = ggplot((data_final), aes(x=Date, Y=count(data_final[,2]))) + geom_line()


#function to get top 10 results of all_tweets
#data_df = as.data.frame(all_tweets)
#data_df_count = data_df[order(data_df$Hashtag, decreasing=T),]
#library(plyr)
#count(data_df, "Hashtag")
#count_df = count(data_df, "Hashtag")
#count_df_ordered = count_df[order(count_df$freq, decreasing=T),]
#count_df_ordered = count_df[order(-freq),]
#mostused = sort(table(data_ordered[,3]),decreasing = TRUE)[1:20]


###experiment - create data as data.frame, count frequency and plot timeline###
#FIRST: need data to be all statuses for xyz period, not n=200
#HERE: need to get all tweets from lfinal, separate hashtags, count hashtags
#then plot along timeline 

#library(ggplot2)

#data_df = as.data.frame(data)
#data_df_count = data_df[order(data_df$Hashtag, decreasing=T),]
#library(plyr)
#count(data_df, "Hashtag")
#count_df = count(data_df, "Hashtag")
#count_df_ordered = count_df[order(count_df$freq, decreasing=T),]
#count_df_ordered = count_df[order(-freq),]
#head(count_df_ordered,n=10)

#names(data_df_count) = c("Hashtag","Freq")
#head(data_df_count)


days = 180
ggplot(data=data_df, aes(x=created)) + 
  geom_bar(aes(fill=..count..), binwidth=30*days) + 
  scale_x_datetime("Date") + 
  scale_y_continuous("Frequency") 
  #theme(title="Frequency", legend.position='none')
ggsave(file=paste(location,"FREQ.png", width=7, height=7, dpi=100)


#####WORDCLOUD

top_hashtags = head(data_ranked,n=100)
test = wordcloud(top_hashtags[,1],top_hashtags[,2])


### end experiment


###########################end of my experiment

#plot heatmap

mostused=sort(table(data_filtered_bydate[,3]),decreasing = TRUE)[1:30]
a=rep(0,0)
for(i in 1:length(data_filtered_bydate[,1])){if(data_filtered_bydate[i,3] %in% rownames(mostused) == TRUE){a=c(a,i)}}
data_mostused=data_filtered_bydate[a,]

data_mostused_bydate=data_mostused[order(data_mostused[,5]),]
data_table = as.matrix(table(data_mostused[,"Hashtag"],data_mostused_bydate[,5]))

data_filtered_bydate = as.matrix(data_filtered_bydate)
ord=sort(table(data_filtered_bydate[,"Hashtag"]),decreasing = TRUE)[1:30]
final_table=data_table[rownames(ord),]

blue_scale=c("#f5f1fc","#e1d4f7","#ccb8f2","#b79bed","#a37ee8","#8e61e2","#7a45dd","#6528d8","#5722bc","#4a1d9f","#3d1882","#2f1266","#220d49","#15082c","#07030f")

final_table_log=log(final_table+1)

library(RColorBrewer)
heatmap = heatmap(final_table_log,Rowv=NA,Colv=NA,col=blue_scale,margins=c(5,10),scale="none")
png(paste(topic,location,".png"))
plot(heatmap)
dev.off ()
################

