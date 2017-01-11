#This is the PDT Social Media Analytics Tool – Part 1
#Part 1 allows you to obtain a list of 100 most influential Twitter accounts on a specific topic based on an initial list of 5 people. Please input the Twitter usernames of the 5 people you choose to base your analysis on in the appropriate place before. Note that you can also include the account of an institution here, not just a person, if you believe it to be appropriate. 

#Please input below the Twitter usernames of 5 key actors in the space you'd like to explore.
##Note that you must use Twitter usernames as they appear online, not full names. 
person1 = "Lagarde"
person2 = "Wolf_Schauble"
person3 = "KeyuJin"
person4 = "gabriel_zucman"
person5 = "LSEpoliticsblog"

#Now, please name the topic that you are researching. For instance – "Macroeconomics" or "Climate Change". 
topic = "European Economists" 

#This is the Twitter authentication part. Highlight the 3 rows below and click CTL+R or Edit-->Run line or selection. 
api_key = "bxcaT2LFsolsnmJAxJNLCKjmO"
api_secret = "4mJnhfDND84onydHi29nRi87kSyoDRasPge14IpnPvU3pwLZTm"
access_token = "3325249193-U9QGCkMVDXPHA3s9Q4xF4Wsu2oyig3RMhTlIxhz"
access_token_secret = "TfRrpArJSi2K3ohYzECLWFVr1EKsUKyv00e8hrGDQhJMN"
setup_twitter_oauth(api_key,api_secret)


#Now, highlight the entire text (CTRL+A) and then run the code by using CTRL+R. Then sit back and wait! The code will take approximately 3 hours to run.

#After you’ve had a chance to review the list of accounts received in S:/PDT:/Portfolios/Social Media Analytics, looks at the users and see if there are ones you’d like to eliminate. For instance – media accounts, politician accounts or institutional accounts. This depends on whether you’d like to obtain a “pure” list of individuals only. For hashtag analysis purposes, you can leave all accounts. 

#If there are accounts you’d like to remove, please write them here instead of the…:
remove_account1 = "nberpubs"
remove_account2 = "BBCWorld"
remove_account3 = "BillGates"
remove_account4 = "BrookingsEcon"
remove_account5 = "business"
remove_account6 = "WSJecon"
remove_account7 = "nytimes"
remove_account8 = "BarackObama"
remove_account9 = "CNN"
remove_account10 = "dealbook"
remove_account11 = "EconBizFin"
remove_account12 = ""
remove_account13 = ""
remove_account14 = ""
remove_account15 = ""

for_removal = c(remove_account1, remove_account2, remove_account3, remove_account4, remove_account5, remove_account6, remove_account7, remove_account8, remove_account9, remove_account10, remove_account11, remove_account12, remove_account13, remove_account14, remove_account15)

#This is the code portion of the tool. Do not change anything below. 

location = "C:/Users/MPER/Desktop/R/Test/15-07-17/"

require(XML)

library(network)
library(httr) 
library(devtools)
library(twitteR)
library(xlsx)
	
#Initial set up – iteration 0 

original_top_users = as.matrix(c(person1, person2, person3 , person4, person5))

#collection of the people followed for the starting names 
#reformatting and juxtaposing of the follower to have a matrix follower-followee

r=matrix(0,ncol=3,nrow=0)
colnames(r) = c("Follower","Followee","Iteration")

linfo0 = matrix(0,ncol=6,length(original_top_users))
colnames(linfo0) = c("Account","Name","Description","# of followers","# of friends","Iteration")

safety=0

#for every name in original list from 1-5
#get_friends = get users of i name in original_top_users 
#if friends>0, create 3-column matrix with #friends=#rows 


for (i in 1:length(original_top_users))
{
  get_friends=getUser(original_top_users[i,])$getFriends()
  if (length(get_friends)>0)
  {
    m=matrix(0,ncol=3,nrow=length(get_friends))
    safety=safety+getUser(original_top_users[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }
    
#for j between 1 and the number of friends of name i (for every friend)
#place in matrix m in column1: name of the original user the friend came from, column 2: friend's username, column3: "0"

for (j in 1:length(get_friends)) 
{
  m[j,1]=original_top_users[i,]
  m[j,2]=get_friends[[j]]$screenName
  m[j,3]="0"
}
    
#place in matrix linfo0 in column1:original users, column2:user description, column3:follower count, column4:friends count, column5: "0"
    linfo0[i,1]=original_top_users[i,]
    linfo0[i,2]= getUser(original_top_users[i,])$name
    linfo0[i,3]=getUser(original_top_users[i,])$description
    linfo0[i,4]=getUser(original_top_users[i,])$followersCount
    linfo0[i,5]=getUser(original_top_users[i,])$friendsCount
    linfo0[i,6]="0"

#place in r the results of m (follower:original user, followee:friend's username, iteration:0)

  r=rbind(r,m)
  Sys.sleep(60*0.5)
  }
}

r0=r

Sys.sleep(60*15)

#in order to not select in the following 19 those that were in the previous list, we exclude them 
#we keep the matrix warm because we’ll use it later with its links 
#store the result of r in r0 

test_excl=r0
a=rep(0,0)

#test if any row in r (list of followees of each original user) is itself in origial user list 
#create vector with all i's that are duplicates 

for (i in 1:nrow(test_excl))
{
  if(test_excl[i,2] %in% original_top_users == TRUE)
  {
    a=c(a,i)
  }
}

#delete duplicate rows/followees (moves everything up)  
list=test_excl[-a,]

#delete for_removals 
d=rep(0,0)
for (i in 1:nrow(list)) 
{
  if (list[i,2] %in% for_removal == TRUE)
  {
    d=c(d,i)
  }
}
list=list[-d,]

#we filter to see the accounts most followed by the initial people and we create our list of 25

#sort list of followees by count of number of times he/she is followed
#make top-40 followees as row names of new matrix list_0

users=sort(table(list[,2]),decreasing = TRUE)
list_0=as.matrix(row.names(users)[1:40])

b=rep(0,0)

#for every followee i in list_0, if user has 8000+ friends, add them to vector b
for (i in 1:length(list_0[,1]))
{
  if(getUser(list_0[i,])$friendsCount > 8000)
    {b=c(b,i)
  }
}

#create new column called excluded with "1" for all users in b that have 8000+ friends
excluded=cbind(list_0[b,],"1")

#if there are users with 8000+ friends,eliminate them from list_0 
#if list_0 has more than 18 users remaining take only top 19 

if(length(b)>0)
{
  list_0=as.matrix(list_0[-b,])
}

if(length(list_0)>18)
{
  list_0=as.matrix(list_0[1:19,])
} 

# iteration 1 - same as former iteration - obtain followees of each of 19 users, count and rank by most followed by previous list then obtain their friends/followees, remove duplicates (users already appearing in former iteration) and add to original users list

### Q FOR BENJAMIN: why 1-9 and 1-12?   

r=matrix(0,ncol=3,nrow=0)
colnames(r)<-c("Follower","Followee","Iteration")

linfo1=matrix(0,ncol=6,length(list_0[,1]))
colnames(linfo1)<-c("Account","Name","Description","# of followers","# of friends","Iteration")

safety=0

for (i in 1:9)
{
  get_friends=getUser(list_0[i,])$getFriends()
  if (length(get_friends)>0)
  {
    m=matrix(0,ncol=3,nrow=length(get_friends))
    safety=safety+getUser(list_0[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }
for (j in 1:length(get_friends))
{
  m[j,1]=list_0[i,]
  m[j,2]=get_friends[[j]]$screenName
  m[j,3]="1"
}

linfo1[i,1]=list_0[i,]
linfo1[i,2]=getUser(list_0[i,])$name
linfo1[i,3]=getUser(list_0[i,])$description
linfo1[i,4]=getUser(list_0[i,])$followersCount
linfo1[i,5]=getUser(list_0[i,])$friendsCount
linfo1[i,6]="1"

r=rbind(r,m)
  }
}

###BENJAMIN: WHY TWICE HERE? (1-9 then 10 through length

Sys.sleep(60*15)

safety=0

for (i in 10:length(list_0[,1]))
{
  f=getUser(list_0[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(list_0[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }

for (j in 1:length(f)) 
{
  m[j,1]=list_0[i,]
  m[j,2]=f[[j]]$screenName
  m[j,3]="1"
}

linfo1[i,1]=list_0[i,]
linfo1[i,2]=getUser(list_0[i,])$name
linfo1[i,3]=getUser(list_0[i,])$description
linfo1[i,4]=getUser(list_0[i,])$followersCount
linfo1[i,5]=getUser(list_0[i,])$friendsCount
linfo1[i,6]="1"

r=rbind(r,m)
  }
}

Sys.sleep(60*15)

#results include the follower-followee couples for utilization in the graph so before we remove any (couples) we redefine the list 

r1=r

test_excl=rbind(r0,r1)
a=rep(0,0)

lstep=as.matrix(rbind(original_top_users,list_0))

for (i in 1:nrow(test_excl))
{
  if(test_excl[i,2] %in% lstep == TRUE){a=c(a,i)
}
}

list=test_excl[-a,]

#delete for_removals 
d=rep(0,0)
for (i in 1:nrow(list)) 
{
  if (list[i,2] %in% for_removal == TRUE)
  {
    d=c(d,i)
  }
}
list=list[-d,]


#we filter to see the most followed accounts by the initial people and we create our list of 45 

users=sort(table(list[,2]),decreasing = TRUE)
l2=as.matrix(row.names(users)[1:40])

#we remove from the list the accounts with an extreme number of followers 

b=rep(0,0)

for (i in 1:length(l2[,1])){
if(getUser(l2[i,])$friendsCount > 8000){b=c(b,i)}}

excl=cbind(l2[b,],"2")
excluded=rbind(excluded,excl)

if(length(b)>0)
{
  l2=as.matrix(l2[-b,])
}
if(length(l2)>18)
{
  l2=as.matrix(l2[1:19,])
}

#iteration 2

r=matrix(0,ncol=3,nrow=0)
colnames(r)<-c("Follower","Followee","iteration")

linfo2=matrix(0,ncol=6,length(l2[,1]))
colnames(linfo2)<-c("account","name","desc","nbfollowers","nbfriends","iteration")

safety=0

for (i in 1:9)
{
  f=getUser(l2[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l2[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }

for (j in 1:length(f)) 
{
m[j,1]=l2[i,]
m[j,2]=f[[j]]$screenName
m[j,3]="2"
}

linfo2[i,1]=l2[i,]
linfo2[i,2]=getUser(l2[i,])$name
linfo2[i,3]=getUser(l2[i,])$description
linfo2[i,4]=getUser(l2[i,])$followersCount
linfo2[i,5]=getUser(l2[i,])$friendsCount
linfo2[i,6]="2"

r=rbind(r,m) 
  } 
}

Sys.sleep(60*15)

safety=0

for (i in 10:length(l2[,1]))
{
  f=getUser(l2[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l2[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }

for (j in 1:length(f)) 
{
  m[j,1]=l2[i,]
  m[j,2]=f[[j]]$screenName
  m[j,3]="2"
}

linfo2[i,1]=l2[i,]
linfo2[i,2]=getUser(l2[i,])$name
linfo2[i,3]=getUser(l2[i,])$description
linfo2[i,4]=getUser(l2[i,])$followersCount
linfo2[i,5]=getUser(l2[i,])$friendsCount
linfo2[i,6]="2"
r=rbind(r,m)
  }
}

Sys.sleep(60*15)

#results include the follower-followee couples for utilization in the graph so before we remove any (couples) we redefine the list 

r2=r

test_excl=rbind(r0,r1,r2)
a=rep(0,0)

lstep=as.matrix(rbind(original_top_users,list_0,l2))

for (i in 1:nrow(test_excl)){
if(test_excl[i,2] %in% lstep == TRUE){a=c(a,i)}}

list=test_excl[-a,]


#delete for_removals 
d=rep(0,0)
for (i in 1:nrow(list)) 
{
  if (list[i,2] %in% for_removal == TRUE)
  {
    d=c(d,i)
  }
}
list=list[-d,]


#we filter the see the most followed accounts by the initial people and we create our list of 45 

users=sort(table(list[,2]),decreasing = TRUE)
l3=as.matrix(row.names(users)[1:40])

#we remove from the list accounts with extreme number of followers

b=rep(0,0)

for (i in 1:length(l3[,1]))
{
  if(getUser(l3[i,])$friendsCount > 8000)
  {
    b=c(b,i)
  }
}

excl=cbind(l3[b,],"3")
excluded=rbind(excluded,excl)

if(length(b)>0)
{
  l3=as.matrix(l3[-b,])
}
if(length(l3)>18)
{
  l3=as.matrix(l3[1:19,])
}

#iteration 3

r = matrix(0,ncol=3,nrow=0)
colnames(r) = c("Follower","Followee","Iteration")

linfo3 = matrix(0,ncol = 6,length(l3[,1]))
colnames(linfo3) = c("Account","Name","Description","# of followers","# of friends","Iteration")

safety=0

for (i in 1:9)
{
  f=getUser(l3[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l3[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }

  for (j in 1:length(f))
  {
    m[j,1]=l3[i,]
    m[j,2]=f[[j]]$screenName
    m[j,3]="3"
  }

linfo3[i,1]=l3[i,]
linfo3[i,2]=getUser(l3[i,])$name
linfo3[i,3]=getUser(l3[i,])$description
linfo3[i,4]=getUser(l3[i,])$followersCount
linfo3[i,5]=getUser(l3[i,])$friendsCount
linfo3[i,6]="3"

r=rbind(r,m)
  }
}

Sys.sleep(60*15)

safety=0

for (i in 10:length(l3[,1]))
{
  f=getUser(l3[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l3[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }
  for (j in 1:length(f)) 
  {
    m[j,1]=l3[i,]
    m[j,2]=f[[j]]$screenName
    m[j,3]="3"
  }
 
linfo3[i,1]=l3[i,]
linfo3[i,2]=getUser(l3[i,])$name
linfo3[i,3]=getUser(l3[i,])$description
linfo3[i,4]=getUser(l3[i,])$followersCount
linfo3[i,5]=getUser(l3[i,])$friendsCount
linfo3[i,6]="3"

r=rbind(r,m)
  }
}

Sys.sleep(60*15)

#results include the follower-followee couples for utilization in the graph so before we remove any (couples) we redefine the list

r3=r

test_excl=rbind(r0,r1,r2,r3)
a=rep(0,0)

lstep=as.matrix(rbind(original_top_users,list_0,l2,l3))

for (i in 1:nrow(test_excl))
{
  if(test_excl[i,2] %in% lstep == TRUE){a=c(a,i)
}
}

list=test_excl[-a,]

#delete for_removals 
d=rep(0,0)
for (i in 1:nrow(list)) 
{
  if (list[i,2] %in% for_removal == TRUE)
  {
    d=c(d,i)
  }
}
list=list[-d,]


#we filter to see the most followed accounts by the initial people and we create our list of 45 

users=sort(table(list[,2]),decreasing = TRUE)
l4=as.matrix(row.names(users)[1:40])

#we remove from the list the accounts with an extreme number of followers 

b=rep(0,0)

for (i in 1:length(l4[,1]))
{
  if(getUser(l4[i,])$friendsCount > 8000){b=c(b,i)
}
}

excl=cbind(l4[b,],"4")
excluded=rbind(excluded,excl)

if(length(b)>0)
{
  l4=as.matrix(l4[-b,])
}
if(length(l4)>18)
{
  l4=as.matrix(l4[1:19,])
}

### iteration 4

r=matrix(0,ncol=3,nrow=0)
colnames(r)<-c("Follower","Followee","iteration")

linfo4=matrix(0,ncol=6,length(l4[,1]))
colnames(linfo4)<-c("account","Name","desc","nbfollowers","nbfriends","iteration")

safety=0

for (i in 1:9)
{
  f=getUser(l4[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l4[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }
for (j in 1:length(f)) 
{
m[j,1]=l4[i,]
m[j,2]=f[[j]]$screenName
m[j,3]="4"
}

linfo4[i,1]=l4[i,]
linfo4[i,2]=getUser(l4[i,])$name
linfo4[i,3]=getUser(l4[i,])$description
linfo4[i,4]=getUser(l4[i,])$followersCount
linfo4[i,5]=getUser(l4[i,])$friendsCount
linfo4[i,6]="4"
r=rbind(r,m)
  }
}

Sys.sleep(60*15)

safety=0

for (i in 10:length(l4[,1]))
{
  f=getUser(l4[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l4[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
    }

for (j in 1:length(f)) 
{
m[j,1]=l4[i,]
m[j,2]=f[[j]]$screenName
m[j,3]="4"
}

linfo4[i,1]=l4[i,]
linfo4[i,2]=getUser(l4[i,])$name
linfo4[i,3]=getUser(l4[i,])$description
linfo4[i,4]=getUser(l4[i,])$followersCount
linfo4[i,5]=getUser(l4[i,])$friendsCount
linfo4[i,6]="4"
r=rbind(r,m)
  }
}

Sys.sleep(60*15)

#results include the follower-followee couples for utilization in the graph so before we remove any (couples) we redefine the list 


r4=r

test_excl=rbind(r0,r1,r2,r3,r4)
a=rep(0,0)

lstep=as.matrix(rbind(original_top_users,list_0,l2,l3,l4))

for (i in 1:nrow(test_excl))
{
  if(test_excl[i,2] %in% lstep == TRUE){a=c(a,i)}
}

list=test_excl[-a,]

#delete for_removals 
d=rep(0,0)
for (i in 1:nrow(list)) 
{
  if (list[i,2] %in% for_removal == TRUE)
  {
    d=c(d,i)
  }
}
list=list[-d,]



#we filter the see the most followed accounts by the initial people and we create our list of 45 

users=sort(table(list[,2]),decreasing = TRUE)
l5=as.matrix(row.names(users)[1:40])

#we remove from the list the accounts with an extreme number of followers

b=rep(0,0)

for (i in 1:length(l5[,1])){
if(getUser(l5[i,])$friendsCount > 8000){b=c(b,i)}}

excl=cbind(l5[b,],"5")
excluded=rbind(excluded,excl)

if(length(b)>0){l5=as.matrix(l5[-b,])}
if(length(l5)>18){l5=as.matrix(l5[1:19,])}


#collection of most recent friends

r=matrix(0,ncol=3,nrow=0)
colnames(r)<-c("Follower","Followee","iteration")

linfo5=matrix(0,ncol=6,length(l5[,1]))
colnames(linfo5)<-c("account","Name","desc","nbfollowers","nbfriends","iteration")

safety=0

for (i in 1:9)
{
  f=getUser(l5[i,])$getFriends()
  if (length(f)>0)
  {
    m=matrix(0,ncol=3,nrow=length(f))
    safety=safety+getUser(l5[i,])$friendsCount
    if(safety>14000)
    {
      Sys.sleep(60*15)
      safety=0
     }

for (j in 1:length(f)) 
{
m[j,1]=l5[i,]
m[j,2]=f[[j]]$screenName
m[j,3]="5"
}

linfo5[i,1]=l5[i,]
linfo5[i,2]=getUser(l5[i,])$name
linfo5[i,3]=getUser(l5[i,])$description
linfo5[i,4]=getUser(l5[i,])$followersCount
linfo5[i,5]=getUser(l5[i,])$friendsCount
linfo5[i,6]="5"
r=rbind(r,m)
  }
}

Sys.sleep(60*15)

safety=0

for (i in 10:length(l5[,1]))
{
  f=getUser(l5[i,])$getFriends()
  if (length(f)>0)
  {
     m=matrix(0,ncol=3,nrow=length(f))
     safety=safety+getUser(l5[i,])$friendsCount
     if(safety>14000)
     {
Sys.sleep(60*15)
safety=0}

for (j in 1:length(f))
{
m[j,1]=l5[i,]
m[j,2]=f[[j]]$screenName
m[j,3]="5"
}

linfo5[i,1]=l5[i,]
linfo5[i,2]=getUser(l5[i,])$name
linfo5[i,3]=getUser(l5[i,])$description
linfo5[i,4]=getUser(l5[i,])$followersCount
linfo5[i,5]=getUser(l5[i,])$friendsCount
linfo5[i,6]="5"
r=rbind(r,m)
  }
}

r5=r

rfinal=rbind(r0,r1,r2,r3,r4,r5)
lfinal=rbind(linfo0,linfo1,linfo2,linfo3,linfo4,linfo5)
colnames(lfinal) = c("Account","Name","Description","Number of followers","Number of friends","Iteration")

write.xlsx2(excluded,paste(location,"excluded_",topic,".xls"))
write.xlsx2(rfinal,paste(location,"rfinal_",topic,".xls"))
write.xlsx2(lfinal,paste(location,"List of 100 Individuals-",topic,".xls"))

save.image(paste(location,topic,"Part1.RData"))



