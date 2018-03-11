########################## Part 1: Setting the Dataset, Importing and exploring#########

getwd()
setwd('C:\\Users\\chait\\Desktop\\Important US Stuff\\Purdue\\Fall\\R for Analytics\\R Project')

### Importing the dataset

imdb_database<-read.csv('https://www.dropbox.com/s/mv8mgwy3ianrotq/movie_metadata.csv?dl=1',header=T,sep=",")


#### Importing the inflation dataset to account for inflation in prices  #########

inflation<-read.csv('https://www.dropbox.com/s/3w2wit7zplf8hx0/inflation.csv?dl=1',header=T,sep=",")

##### Merging with the previous dataset #######

imdb_database<-merge(x=imdb_database,y=inflation,by.x='title_year',by.y='YEAR',all.x=TRUE)

imdb_database$gross_net<-with(imdb_database,gross*Multiplying.Factor)

imdb_database$budget_net<-with(imdb_database,budget*Multiplying.Factor)

imdb_database<-subset(imdb_database,select=-c(gross,budget,Multiplying.Factor))

#### Analyzing the column classes in the dataset

library(ggplot2)

#Plotting the IMDB ratings to the get a sense of where the scores lie

ggplot(imdb_database, aes(x=imdb_score)) + geom_histogram()


################# Part 2 : Cleaning the data ######################################

#### Checking the classes of all the columns

str(imdb_database)

#### Reducing our dataset to only the movies made in USA to resolve issues related to currency in different countries,etc

imdb_usa<-subset(imdb_database[imdb_database$country=="USA",])

#### Converting Genres into categorical variables ############

imdb_usa$genres<-toupper(imdb_usa$genres) ###Converting to upper case

##### The Genre column is in the form of Action|Thriller|Drama, etc so we are searching
##### for say 'Action' and assigning it a dummy variable

imdb_usa$action<-ifelse(grepl("ACTION",imdb_usa$genres)==TRUE,1,0)
imdb_usa$animation<-ifelse(grepl("ANIMATION",imdb_usa$genres)==TRUE,1,0)
imdb_usa$comedy<-ifelse(grepl("COMEDY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$documentary<-ifelse(grepl("DOCUMENTARY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$family<-ifelse(grepl("FAMILY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$horror<-ifelse(grepl("HORROR",imdb_usa$genres)==TRUE,1,0)
imdb_usa$musical<-ifelse(grepl("MUSICAL",imdb_usa$genres)==TRUE,1,0)
imdb_usa$romance<-ifelse(grepl("ROMANCE",imdb_usa$genres)==TRUE,1,0)
imdb_usa$sport<-ifelse(grepl("SPORT",imdb_usa$genres)==TRUE,1,0)
imdb_usa$war<-ifelse(grepl("WAR",imdb_usa$genres)==TRUE,1,0)
imdb_usa$adventure<-ifelse(grepl("ADVENTURE",imdb_usa$genres)==TRUE,1,0)
imdb_usa$biography<-ifelse(grepl("BIOGRAPHY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$crime<-ifelse(grepl("CRIME",imdb_usa$genres)==TRUE,1,0)
imdb_usa$drama<-ifelse(grepl("DRAMA",imdb_usa$genres)==TRUE,1,0)
imdb_usa$fantasy<-ifelse(grepl("FANTASY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$history<-ifelse(grepl("HISTORY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$music<-ifelse(grepl("MUSIC",imdb_usa$genres)==TRUE,1,0)
imdb_usa$mystery<-ifelse(grepl("MYSTERY",imdb_usa$genres)==TRUE,1,0)
imdb_usa$sci_fi<-ifelse(grepl("SCI",imdb_usa$genres)==TRUE,1,0)
imdb_usa$thriller<-ifelse(grepl("THRILLER",imdb_usa$genres)==TRUE,1,0)
imdb_usa$western<-ifelse(grepl("WESTERN",imdb_usa$genres)==TRUE,1,0)




##### Selecting only the numeric variables

nums<-sapply(imdb_usa,is.numeric)

imdb_usa_numeric<-subset(imdb_usa[,nums])


###### Removing the NA values

imdb_usa_final<-na.omit(imdb_usa_numeric)

############################ Part 3: Modelling ########################################

##Comparing with the most obvious variable, number of voted users

ggplot(imdb_usa_final, aes(x=num_voted_users, y=imdb_score)) + geom_point(colour="blue")+
  stat_smooth(method=lm, se=FALSE, colour="black")

### Comparing with the duration

ggplot(imdb_usa_final,aes(x=duration,y=imdb_score))+geom_point(colour="red")+stat_smooth(method=lm,se=FALSE,colour="black")

#### Comparing with Number of poster faces

ggplot(imdb_usa_final,aes(x=facenumber_in_poster,y=imdb_score))+geom_point(colour="yellow")+stat_smooth(method=lm,se=FALSE,colour="black")

###### Comparing with director fb likes, actor and cast fb likes

ggplot(imdb_usa_final,aes(x=director_facebook_likes,y=imdb_score))+geom_point(colour="purple")+stat_smooth(method=lm,se=FALSE,colour="black")


ggplot(imdb_usa_final,aes(x=actor_1_facebook_likes,y=imdb_score))+geom_point(colour="orange")+stat_smooth(method=lm,se=FALSE,colour="black")


ggplot(imdb_usa_final,aes(x=cast_total_facebook_likes,y=imdb_score))+geom_point(colour="orange")+stat_smooth(method=lm,se=FALSE,colour="black")

#### Comparing with the Title year #######

ggplot(imdb_usa_final,aes(x=title_year,y=imdb_score))+geom_point(colour="purple")+stat_smooth(method=lm,se=FALSE,colour="black")

##### Movie FB Likes ####

ggplot(imdb_usa_final,aes(x=movie_facebook_likes,y=imdb_score))+geom_point(colour="purple")+stat_smooth(method=lm,se=FALSE,colour="black")


##### Gross #####


ggplot(imdb_usa_final,aes(x=gross_net,y=imdb_score))+geom_point(colour="purple")+stat_smooth(method=lm,se=FALSE,colour="black")


##### Checking the significance with each variable

#### Making IMDB the first column of the dataset to make it easier for comparison

imdb_usa_final<-imdb_usa_final[c(12,1,2,3,4,5,6,7,8,9,10,11,13,14,15,16:37)]

X<-imdb_usa_final[1]

Y<-imdb_usa_final[2:37]

cor(X,Y)


#### Preparing the correlation matrix

M <- cor(imdb_usa_final) # get correlations


library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix

######## Preparing the linear model for imdb rating prediction

set.seed(516) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data 

sample <- sample.int(n = nrow(imdb_usa_final), size = floor(.75*nrow(imdb_usa_final)), replace = F)

train <- imdb_usa_final[sample, ]

test  <- imdb_usa_final[-sample, ]


#### Applying regression on all variables

model<-lm(imdb_score ~ ., data = train)

summary(model)



pred <- predict(model,test)

sample<-cbind(pred,test)



mean((sample$imdb_score-pred)^2)

#*****Another try ******#
  
  
lmfit2<-lm(imdb_score~duration+facenumber_in_poster+movie_facebook_likes+budget_net+action+animation+comedy+documentary+family+horror+musical+romance+sport+war+adventure+biography+crime+drama+fantasy+history+music+mystery+sci_fi+thriller+western,data=train)

summary(lmfit2)


pred2 <- predict(lmfit,test)

sample<-cbind(pred2,test)



mean((sample$imdb_score-pred2)^2)



###### Keeping only the best variables ######
imdb_shiny<-subset(imdb_usa_final[,c(1,3,4,8,10,11,14,16,17:37)])


####### Removing the Movies with zero FB likes as that seems to be a data error ####
imdb_shiny_final<-subset(imdb_shiny[imdb_shiny$movie_facebook_likes!=0,]) ## Treating movies with zero fb likes as an error and removing them from the dataset


sample <- sample.int(n = nrow(imdb_shiny_final), size = floor(.75*nrow(imdb_shiny_final)), replace = F)

train <- imdb_shiny_final[sample, ]

test  <- imdb_shiny_final[-sample, ]

lm_final<-lm(imdb_score~.,data=train)

summary(lm_final)


predfinal <- predict(lm_final,test)

sample<-cbind(predfinal,test)


mean((sample$imdb_score-predfinal)^2)

################### Decision Trees ####################3

library(rpart)

set.seed(516)

m.rpart <- rpart(imdb_score~.,data=train)
m.rpart

install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(m.rpart,digits = 3)

p.rpart <- predict(m.rpart,test)

mean((p.rpart-test$imdb_score)^2)
#### The RMSE is almost the same, so we would go for Simple Regression as we have a deeper understanding of that model


##### Finally we are getting an Adjusted R2 of around 53%


### Checking for heterosceodascity
install.packages('faraway')
library('faraway')
reviewDiag <- function(lmfit) {
  # Diagnostic plots
  par(mfcol=c(2,3), fg="black", bg="white",col.lab="black")
  # cooks distance - check for influential points
  cook<-cooks.distance(lmfit) 
  library(faraway) # library needed for half-normalplot
  halfnorm(cook,3,ylab="Cooks distance", main="Influences", col="blue") 
  boxplot(cook, col="blue", ylab="Cooks distance"
          , main="Boxplot Cooks Distances")
  # constant variance
  plot(fitted(lmfit),residuals(lmfit),xlab="Fitted",ylab="Residuals", col="blue"
       , pch=19, type='p', main="Resid vs. Fitted") 
  abline(h=0) 
  plot(fitted(lmfit),abs(residuals(lmfit)),xlab="Fitted",ylab="Abs(Residuals)"
       , main="Abs(Resid) vs. Fitted", col="blue", pch=19)
  # normality
  qqnorm(residuals(lmfit),ylab="Residuals", pch=19, col="blue") 
  qqline(residuals(lmfit)) 
  hist(residuals(lmfit), col="blue", main="Historgram of Residuals")
}
# review assumptions
reviewDiag(lm_final)


####Breush Pagan Test checking for heteroscedasticity

##Breush Pagan Test
install.packages('lmtest')
library('lmtest')

lmtest::bptest(lm_final)  # Breusch-Pagan test

res<-data.frame(residuals(lm_final))

###### There is a problem of Heteroscedasticity in the model #######

##### Exporting the final dataset

write.csv(imdb_shiny_final, "imdb_shiny_final.csv")

