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

unique(std['price_detail__currency']) #price_detail__currency unique values  (NA,INR)

cols <- std[,c('discount_price__amount','discount_price__price_string', 'price_detail__amount', 'price_detail__price_string')]

head(cols,20) # display cols,we can find that these cols almost matches

droppedcols <- std[,c('discount_price__currency','discount_price__price_string','price_detail__currency', 'price_detail__price_string')]

#we will drop those useless columns : 
#'discount_price__currency','discount_price__price_string' 'price_detail__currency', 'price_detail__price_string'



std[,c('discount_price__currency','discount_price__price_string','price_detail__currency', 'price_detail__price_string')]<- list(NULL)
#dropped the columns

colnames(std) #Display the column names now 16 cols we have

head(std,20)
std[,c('url','avg_rating','avg_rating_recent', 'rating')]

# we can find that avg_rating_recent = rating , so we will drop one of them
# also the url column is useless for our data mining

std[,c('url','avg_rating_recent')]<- list(NULL)
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

freebutwithprice <- which(std$'is_paid'==TRUE & std$'price_detail__amount'== 0) # we found the specific entry number

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

stdcor <-std

#...................... Done Dealing with Date & Time ......................#
install.packages("e1071")
install.packages("caret")
library("e1071")
library("caret")
library('dplyr')
stdt <- std
stdt[,c('id','title','created','published_time')]<- list(NULL)

#avg_rating is copy of rating after categorizing

#rating into 4 categories
stdt$rating[stdt$rating<=2] <- 0
stdt$rating[stdt$rating<=3 & stdt$rating>2] <- 1
stdt$rating[stdt$rating>3 & stdt$rating<4] <- 2
stdt$rating[stdt$rating>=4] <- 3

stdt$rating[stdt$rating==0] <- 'Unacceptable'
stdt$rating[stdt$rating==1] <- 'Weak'
stdt$rating[stdt$rating==2] <- 'Good'
stdt$rating[stdt$rating==3] <- 'Excellent'

#

#avg_rating into 4 categories
stdt$avg_rating[stdt$avg_rating<=2] <- 0
stdt$avg_rating[stdt$avg_rating<=3 & stdt$avg_rating>2] <- 1
stdt$avg_rating[stdt$avg_rating>3 & stdt$avg_rating<4] <- 2
stdt$avg_rating[stdt$avg_rating>=4] <- 3

stdt$avg_rating[stdt$avg_rating==0] <- 'Unacceptable'
stdt$avg_rating[stdt$avg_rating==1] <- 'Weak'
stdt$avg_rating[stdt$avg_rating==2] <- 'Good'
stdt$avg_rating[stdt$avg_rating==3] <- 'Excellent'

#


#num_published_lectures into 3 categories
quantile(std$num_published_lectures)

stdt$num_published_lectures[stdt$num_published_lectures<=12] <- 0
stdt$num_published_lectures[stdt$num_published_lectures<37 & stdt$num_published_lectures>12] <- 1
stdt$num_published_lectures[stdt$num_published_lectures>=37] <- 2

stdt$num_published_lectures[stdt$num_published_lectures==0] <- 'low'
stdt$num_published_lectures[stdt$num_published_lectures==1] <- 'medium'
stdt$num_published_lectures[stdt$num_published_lectures==2] <- 'high'
#

#num_published_practice_tests into 2 categories
quantile(std$num_published_practice_tests)

stdt$num_published_practice_tests[stdt$num_published_practice_tests==0] <- 0
stdt$num_published_practice_tests[stdt$num_published_practice_tests>1] <- 1


stdt$num_published_practice_tests[stdt$num_published_practice_tests==0] <- 'None'
stdt$num_published_practice_tests[stdt$num_published_practice_tests==1] <- 'Has'
#

#discount_price__amount into 3 categories
quantile(std$discount_price__amount)

stdt$discount_price__amount[stdt$discount_price__amount==0] <- 0
stdt$discount_price__amount[stdt$discount_price__amount<=450 & stdt$discount_price__amount>0] <- 1
stdt$discount_price__amount[stdt$discount_price__amount>450] <- 2


stdt$discount_price__amount[stdt$discount_price__amount==0] <- 'None'
stdt$discount_price__amount[stdt$discount_price__amount==1] <- 'Small'
stdt$discount_price__amount[stdt$discount_price__amount==2] <- 'High'
#

#price_detail__amount into 3 categories
quantile(std$price_detail__amount)

stdt$price_detail__amount[stdt$price_detail__amount<=1280] <- 0
stdt$price_detail__amount[stdt$price_detail__amount<=8640 & stdt$price_detail__amount>1280] <- 1
stdt$price_detail__amount[stdt$price_detail__amount>8640] <- 2


stdt$price_detail__amount[stdt$price_detail__amount==0] <- 'Cheap'
stdt$price_detail__amount[stdt$price_detail__amount==1] <- 'Acceptable'
stdt$price_detail__amount[stdt$price_detail__amount==2] <- 'Expensive'
#

#num_reviews into 3 categories
quantile(std$num_reviews)

stdt$num_reviews[stdt$num_reviews<=7] <- 0
stdt$num_reviews[stdt$num_reviews<2000 & stdt$num_reviews>7] <- 1
stdt$num_reviews[stdt$num_reviews>=2000] <- 2

stdt$num_reviews[stdt$num_reviews==0] <- 'low'
stdt$num_reviews[stdt$num_reviews==1] <- 'medium'
stdt$num_reviews[stdt$num_reviews==2] <- 'high'

#

#num_subscribers into 3 categories 
quantile(std$num_subscribers) 

stdt$num_subscribers[stdt$num_subscribers<=62] <- 0
stdt$num_subscribers[stdt$num_subscribers >62 & stdt$num_subscribers<2280] <- 1
stdt$num_subscribers[stdt$num_subscribers>=2280 ] <- 2

stdt$num_subscribers[stdt$num_subscribers==0] <- 'low'
stdt$num_subscribers[stdt$num_subscribers==1] <- 'medium'
stdt$num_subscribers[stdt$num_subscribers==2] <- 'high'
#




stdt

stdt$is_paid <-as.factor(stdt$is_paid)


TrainIndex <- createDataPartition(stdt$is_paid,p=.6,list=FALSE)
TrainData <-stdt[TrainIndex,]
TestData <-stdt[-TrainIndex,]

m <-naiveBayes(is_paid~.,data=TrainData,laplace=1)
predict(m,TrainData)
table(predict(m,TrainData),TrainData$is_paid)
table(predict(m,TestData),TestData$is_paid)



#...................... correlation matrix ......................#
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library(corrplot)

stdcor        #got the latest version of std after cleaning


#remove all non-numeric values
stdcor[,c('id','title','is_paid','created','published_time')]<- list(NULL)

corr_result <- cor(stdcor)
corr_result
corrplot(corr_result)

#correlation can be between -1 and 1



#..................................................................#

write.csv(stdt,"lasteversion.csv", row.names = FALSE)   # final csv written

