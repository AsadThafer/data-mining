#Asad Asad 201810523  
#Kholoud Thabet 201810602
#Rakan Al Badarneh 201810890


read.csv("udemy_output_All_Finance__Accounting_p1_p626.csv")
install.packages("tidyr") # tidyr is to help you create tidy data
library(tidyr)
install.packages("readr") #is to provide a fast and friendly way to read csv
library(readr)

std <- read_delim("udemy_output_All_Finance__Accounting_p1_p626.csv") #: Read a delimited file
std #display dataset content

colnames(std) #Display the column names

str(std) #check the data type of each column

which(is.na(std)) #identify the location of NA

unique(std['discount_price__currency']) #discount_price__currency unique values (NA,INR)

unique(std['price_detail__currency']) #price_detail__currency unique values 

cols <- std[,c('discount_price__amount','discount_price__price_string', 'price_detail__amount', 'price_detail__price_string')]

head(cols,20) # display cols,we can find that these cols almost matches

droppedcols <- std[,c('discount_price__currency','discount_price__price_string','price_detail__currency', 'price_detail__price_string')]

#we will drop those useless columns : 'discount_price__currency','discount_price__price_string'
# 'price_detail__currency', 'price_detail__price_string'



std[,c('discount_price__currency','discount_price__price_string','price_detail__currency', 'price_detail__price_string')]<- list(NULL)
#dropped the columns

colnames(std) #Display the column names now 16 cols we have

head(std,20)
std[,c('url','avg_rating','avg_rating_recent', 'rating')]

# we can find that avg_rating_recent = rating , so we will drop one of them
# also the url column is useless for our data mining

std[,c('url','rating')]<- list(NULL)
#dropped the columns

head(std,3) #display 3 values to check our latest version of dataset

which(is.na(std)) #identify the location of NA

sum(is.na(std))  #sum the number of NA Values in our Dataset

#discount_price__amount Cleaning

which(is.na(std$'discount_price__amount')) #identify the location of NA 1403 entries

which(std$'discount_price__amount' == 0) #identify the location of entries with 0 value we got 0 entries



std$discount_price__amount[is.na(std[['discount_price__amount']])] <- 0 # we filled all Na discount with a Value of 0

which(is.na(std$'discount_price__amount')) #identify the location of NA ,now it's 0 entries

#price_detail__amount Cleaning

which(is.na(std$'price_detail__amount')) #identify the location of NA 497 entries


std$'price_detail__amount'[is.na(std[['price_detail__amount']])] <- 0 # we filled all Na price with a Value of 0


# we need to remove the columns which matches the following condition : it's paid and also at 0 price

paidwithprice <- which(std$'is_paid'==TRUE & std$'price_detail__amount'== 0) # we found the specific entry number

std = std[-c(13608),] # we dropped it by index (1 entry)

# check that every not paid course doesn't have a price (price =0)
checkfreecourses <- which(std$'is_paid'==FALSE & std$'price_detail__amount'!= 0) 


#identify the location of NA:
which(is.na(std))

#also remove is_wishlisted column (no course is wishlisted)
which(std$'is_wishlisted'==TRUE)   # we got 0 entries so it's useless for our mining

std[,c('is_wishlisted')]<- list(NULL)

#now there is NO NULL Values (We are Done Dealing with Missing Values)
#...........
# Dealing with Date & Time
install.packages("lubridate")
library(lubridate)
OlsonNames() #time zones list
Sys.setenv(TZ='Etc/GMT-3') #set as our time zone
timenow <- now()
std$created <- ymd_hms(std$created)      # to make sure all on same format
std$published_time <- ymd_hms(std$published_time)   # to make sure all on same format


#...................... Done Dealing with Date & Time ......................#



#...................... Association Rules ..............................#

install.packages('arules')
library('arules')

summary(std) #very useful command that gives us informations about our dataset


#...........
install.packages("RColorBrewer")   # for plotting
install.packages("tidyverse") #collection of R packages 
library(tibble) 
library(RColorBrewer)
tibble(std)
library("knitr") #give users full control of the output without heavy coding work.

# install association rules Visualization package
install.packages("arulesViz")
library('arulesViz')  # for plotting

std2 <- std[,c(2:13)] # remove the id from the dataset

cor(std$avg_rating, std2$avg_rating_recent) 
#we found correlation for avg_rating and avg_rating_recent = 98.9%
#we will remove one of them (avg_rating_recent)
std2[,c('avg_rating_recent')]<- list(NULL)

#L1
itemsets<-apriori(std2,parameter=list(minlen=1,maxlen=1,support=0.2,target="frequent itemsets"))


summary(itemsets) 
inspect(head(sort(itemsets,by="support"),30))

# Second, get itemsets of length 2 #L2
itemsets<-apriori(std2,parameter=list(minlen=2,maxlen=2,support=0.2,target="frequent itemsets"))

summary(itemsets)                     
inspect(head(sort(itemsets,by="support"),50))

# Third, get itemsets of length 3 #L3
itemsets<-apriori(std2,parameter=list(minlen=3,maxlen=3,support=0.2,target="frequent itemsets"))
summary(itemsets)                              
inspect(head(sort(itemsets,by="support"),55))

# Fourth, get itemsets of length 4 #L4
itemsets<-apriori(std2,parameter=list(minlen=4,maxlen=4,support=0.2,target="frequent itemsets"))
summary(itemsets)                              
inspect(head(sort(itemsets,by="support"),20))

# get all rules according to supp. and conf.
rules <- apriori(std2,parameter=list(support=0.2,confidence=0.6,target="rules"))

summary(rules)  

plot(rules,jitter = 0)               # displays scatterplot

slope<-sort(round(rules@quality$lift/rules@quality$confidence,2))

inspect(head(sort(rules,by="lift"),10))

AssociationRules<-rules[quality(rules)$confidence>0.6] 
AssociationRules
inspect(AssociationRules[1:50])

summary(AssociationRules)  
plot(AssociationRules,method="matrix",measure=c("lift","confidence"),control=list(reorder="none"))

highLiftRules<-head(sort(rules,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))
plot(rules, method="paracoord")

inspect(rules[1:10])

#{} => {discount_price__amount=[455,3.2e+03]}  will be created. These rules mean that no matter what other items are involved the item 
#in the RHS will appear with the probability given by the rule's confidence (which equals the support)
#...........

write.csv(std2,"lastedit.csv", row.names = FALSE)   # final csv
write(rules, file="RulesList.txt")                 #all rules as a txt file



#...................... correlation matrix ......................#
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library(corrplot)
#remove all non-numeric values
stdcor <- std[,c(2:13)]
stdcor[,c('id','title','is_paid','created','published_time')]<- list(NULL)
res <- cor(stdcor)
round(res, 2)
corrplot(res)


#To extract the values from this object into a useable data structure 
#correlation can be between -1 and 1
stdcor.rcorr = rcorr(as.matrix(stdcor))
stdcor.rcorr
stdcor.coeff = stdcor.rcorr$r #contains 1
stdcor.p = stdcor.rcorr$P #contains NA

#..................................................................#