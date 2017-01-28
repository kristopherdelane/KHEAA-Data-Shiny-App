library(dplyr)
library(reshape2)
library(zipcode)
library(ggplot2)
library(ggmap)
library(maptools)

df<-read.csv(file = "C:/Temp/Mel Data/data/Zipcode RAW.csv", stringsAsFactors = FALSE)

#Remove NA's
df<-df[!is.na(df$Zip.Code),]
df$HS.Grad.Year<-substring(df$HS.Grad.Year,7)
# 
# ### Create Total Percentages for all variables
# columns<- c(4,5,7,9,11,12,14,15,16,18,20,22,23)
# for(i in 1:length(columns)){
#   df[,(length(df)+1)]<- as.numeric(df[,columns[[i]]])/as.numeric(df[,3])
# }
# test<-c(paste(substring(colnames(df[columns]),0,4), "Percent of ", substring(colnames(df[columns]),5), sep = ""))
# colnames(df)[25:37]<- test
# 
# ### Create Minority Percentages with in Population
# columns<- c(15,16,18,20,22,23)
# for(i in 1:length(columns)){
#   df[,(length(df)+1)]<- as.numeric(df[,columns[[i]]])/as.numeric(df[,14])
# }
# test<-c(paste(substring(colnames(df[columns]),0,4), "Percent of Minority Only ", substring(colnames(df[columns]),5), sep = ""))
# colnames(df)[38:43]<- test
# 
# # 
# df[,(length(df)+1)]<- as.numeric(df[,12])/as.numeric(df[,11])
# df[,(length(df)+1)]<- as.numeric(df[,23])/as.numeric(df[,22])
# test<-c("ALL.Percent Pell Eligible Filing FASFA", "MIN.Percent Pell Eligible Filing FASFA")
# colnames(df)[44:45]<- test

#Convert to Long
df<-melt(df, id=c("HS.Grad.Year", "Zip.Code"), direction = "long")

#Remove category from variable
df$Category<-substring(df$variable,0 ,3)

#shorten the string 
df$variable<-substring(df$variable,5 ,length(df$variable))

#clean up punctuation and extra spaces
df$variable<- gsub("[//.]"," ", df$variable)
df$variable<-gsub("    0"," >0",df$variable)
df$variable<-gsub("Percent","Pct",df$variable)

df$variable<-gsub("F T","FT",df$variable)
df$variable<-gsub("Minority Only ","Minority ",df$variable)
df$variable<-gsub(" SAT","",df$variable)
df$value<-gsub("[//-]","",df$value)
df$value<-gsub("[//,]","",df$value)
df$value<-gsub("[//$]","",df$value)

#create NA in blank spades
df$value<-as.numeric(df$value)/1

# combine minority and all values and clean up environment
df1<-df[df$Category=="ALL",]
df2<-df[df$Category!="ALL",]

df<-right_join(df1, df2, by = c("HS.Grad.Year", "Zip.Code", "variable"))
rm(df1)
rm(df2)

#rebuild data to be more manageable along with column names
df<-df[,c(1:4,6)]
colnames(df)<- c("Year", "Zip Code", "Variable","Total","Minority")

df$Percentage<-round(df$Minority/df$Total*100,0)
dfy<-df[,c(1:3,6)]
dfy$`Zip Code`<-as.character(dfy$`Zip Code`)
df<-df[,1:5]

df$`Zip Code`<-as.character(df$`Zip Code`)
df<-melt(df, id=c("Year", "Zip Code", "Variable"), direction = "long")
colnames(df)<-c("Year", "Zip Code", "Classification", "Demographic","Value")

save(df, file = "C:/Temp/Mel Data/R Code/Zipcode App/data/EdData.Rdata")


##########################################################################################

dfx<-read.csv("c:/temp/Mel Data/data/Mel.csv", stringsAsFactors = FALSE)

dfx[c(1:33),1:14]-> dfx

dfx<-melt(dfx, id=c("GRADYEAR", "Race.Ethnicity"), direction = "long")

dfx$variable<- gsub("[//.]"," ", dfx$variable)
dfx$variable<-gsub("                0"," >0",dfx$variable)
dfx$variable<-gsub("F T","FT",dfx$variable)
dfx$variable<-gsub(" SAT","",dfx$variable)
dfx$value<-gsub("[//-]","",dfx$value)
dfx$value<-gsub("[//,]","",dfx$value)
dfx$value<-gsub("[//$]","",dfx$value)
dfx$variable<-gsub("X Mean ACT  equivalent ","Mean ACT  equivalent ",dfx$variable)

#create NA in blank spades
dfx$value<-as.numeric(dfx$value)/1

dfx$GRADYEAR<- substring(dfx$GRADYEAR,7)

dfx<-dfx[dfx$variable!="Ethnicity Code",]
dfx$value <- round(dfx$value, digits = 0)

colnames(dfx)<- c("Year", "Demographic", "Classification", "Value")

save(dfx, file = "C:/Temp/Mel Data/R Code/Zipcode App/data/EdxData.Rdata")

#########################################################################################33

NationalZips<-readShapePoly("c:/Temp/zip/cb_2015_us_zcta510_500k")
NationalZips<-fortify(NationalZips,region="ZCTA5CE10")

# retain zips in Louisville from Shape File
JCKYZips<-NationalZips[NationalZips$id %in% unique(dfy$`Zip Code`),]
dfy1<-left_join(JCKYZips, dfy, by = c("id" = "Zip Code"))

colnames(dfy1)<-c("long","lat","order","hole","piece","Zip Code","group","Year","Variable","Percentage")

dfy1<-dfy1[dfy1$`Zip Code`!= "40047",]

save(dfy1, file = "C:/Temp/Mel Data/R Code/Zipcode App/data/EdyData.Rdata")

