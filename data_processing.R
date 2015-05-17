d<-read.csv("data/houses.csv")
names(d)
d<-d[,c(1,2,grep(".*ALL.*",names(d)))]
names(d)
names(d)<-gsub("_MED_PRICE_ALL_TYPES","",names(d))
names(d)
library(reshape)
d<-melt(d,id=1:2)
str(d)
d$year<-as.numeric(gsub("X","",d$variable))
str(d)
names(d)<-c("id","Constituency","variable","value","year")
d<-d[,c(1,2,4,5)]
names(d)
head(d)
library(rgdal)
map<-readOGR("data","constituencies")
map<-data.frame(map@data,coordinates(map))
dd<-merge(map,d)
names(dd)[c(3,4)]<-c("x","y")
head(dd)
write.csv(dd,"data/house_prices.csv",row.names=FALSE)

library(reshape)
d<-read.csv("data/age_profile.csv")
d<-d[,-3]
str(d)
names(d)[1:2]<-c("id","name")
d<-melt(d,id=1:2)
str(d)
d$year<-as.numeric(gsub("X","",d$variable))
d<-na.omit(d)
head(d)
d$cat<-cut(d$year,c(0,18,40,100))
aa<-cast(d,id~cat,sum)
dd<-data.frame(aa$id,percent=aa[,3]/aa[,4])

library(rgdal)
d<-read.csv("data/uk-election-results-2015.csv")
dd<-read.csv("data/constituency_names.csv")[,c(2,5)]

#write.csv(d,"data/election-results.csv",row.names=F)
d$Party.all[d$Candidate == "John Bercow"] <- "Conservative"
d$Party[d$Candidate == "John Bercow"] <- "Conservative"
d<-d[d$Party.all%in%c("Conservative","Labour","Liberal Democrat","Green Party","UKIP","Plaid Cymru","Scottish National Party"),]
d<-d[d$Region!="Northern Ireland",]
d<-droplevels(d)
d<-d[order(d$Constituency),]
d$rank<-unlist(tapply(-d$Votes,d$Constituency,order))

win<-subset(d, rank==1)
win<-win[,2:3]
names(win)<-c("Constituency","Winner")
second<-subset(d, rank==2)
second<-second[,2:3]
names(second)<-c("Constituency","Second")
ds<-melt(d,id=c(2,3,4),m=6)
d1<-cast(ds,Constituency~Party,sum)
d1<-merge(d1,win,by.x="Constituency",by.y="Constituency")
d1<-merge(d1,second,by.x="Constituency",by.y="Constituency")
d1<-merge(dd,d1,by.x="Constituency",by.y="Constituency",sort=TRUE)

names(d1)
names(d1)<-c("name","id","Con","Green","Lab","LibDem","Plaid","SNP","UKIP","Win","Second")
write.csv(d1,"data/election_results_prop.csv",row.names=FALSE)






