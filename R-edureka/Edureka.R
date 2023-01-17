# Case study 2
setwd("~/Desktop")
job <- read.csv("jobs.csv")
head(job)
str(job)
# Create an Alberta and BC data frame
library(dplyr)
job1 <- job%>%select(BC,Alberta)


#Find the month with the highest total employment across the states

tapply(job$Atlantic,job$Date,max)
tapply(job$BC,job$Date,max)
tapply(job$Alberta,job$Date,max)
tapply(job$Ontario,job$Date,max)
tapply(job$Atlantic,job$Date,max)


#Find the months in which employment figures in Atlantic went below 950.
job$Date <- as.Date(job$Date,origin="1990-01-01")
below_950 <-job%>%filter(Atlantic < 950)%>%select(Atlantic,Date)%>%group_by(Date)
below_950

# Sort the figures for Quebec in ascending order
job3 <- job[order(job$Quebec),]


temperature <- c(35,26,39,50,42,4,9)
city <- c("Mumbai","Nice","Dubai","Jacobabad","Riyadh","Madrid","Milan")
city_temp <- matrix(c(temperature,city),nrow = 2,ncol = 7,byrow = T,dimnames = list(rownam,colnam))
rownam <- c('temperature','city')
colnam <- c("India","France","UAE","Pakistan","Qatar","Spain","Italy")
city_temp

# chelsea dataframe
chelsea <- data.frame(players = c("Ruben","Mason","Kurt","Cesar","Marcos","Tammy"),
                      goals = c(0,5,2,2,6,20),
                      assists = c(0,4,1,3,2,7),
                      country = c("England","England","France","Spain","Spain","England"),stringsAsFactors = F)
class(chelsea)
str(chelsea)
chelsea[1:2,]
chelsea[2,3]
chelsea[,2:3]
library(ggplot2)
ggplot(data = chelsea,aes(x = players,y = assists,col = players)) +
  geom_bar(stat = "identity")

#sequence
rand_num <- seq(0,100,5)
rand_num[6]
rand_num[2:5]
rand_num[c(-2,-5)]
set.seed(1)
Random <- c(1,2,3,4,5,6,7,8,9,0)
Random[1:3] <- rnorm(3,mean = 5,sd = 1)
Random[4:6] <- rnorm(3,mean = 3,sd = 3)
Random[7:10] <- rnorm(4,mean = 1,sd = 4)
Random <- round(Random,2)
Random
rand_log <- c(1,2,3,4,5,6,7,8,9,0)
rand_log[1:5] <- log(rand_log[1:5])
rand_log[6:10] <- dexp(rand_log[6:10],rate = 1,log = F)
rand_log

negative_thousand <- seq(-1000,0)
negative_thousand
thousand <- seq(0,1000)
thousand

final_thousand <- c(negative_thousand,thousand)
final_thousand[500]
first_hundred <- final_thousand[1:100]
first_hundred
last_hundred <- final_thousand[1902:2002]
last_hundred
Weird_Set <- final_thousand[321:764]
Weird_Set

book_details <- list(Book_name =c("Business Analytics","R for data science","Python crash course","Introduction to statistical learning","Machine learning for dummies"),
                     Author_name = c("Dinesh Kumar","Hadley Wickham","Eric Matthes","Gareth James","John Paul Mueller"),
                     Book_cost = c(700,1200,1800,2500,500))
book_details
book_details$Book_name
book_details$Author_name
book_details$Book_cost
book_details$Book_name[4]
book_details[[2]][2]
book_details[[3]][5]

data("women")
head(women)
women[6,2]
dim(women)
women[12:15,]
alternate <- women[c(1,3,5,7,9,11,13,15),]
alternate

student <- data.frame(Name = c("Kepa","Marcos","Kurt","Toni","Cesar","Mason","Mateo","Tammy","Michy","Oliver"),
                      Department = c("Mechanical","Electrical","Civil","Environment","DataScience","Computer Science","Telecommunication","Textile","Chemical","IT"),
                      CGPA = c(8,7,9,7,5,9,8,3,8,6),
                      Placement = c(T,F,F,F,T,T,F,T,F,F))
student


#loops and functions
# relational operators
v1 = c(1,2,4,5)
v2 = c(1,6,3,1)
v1==v2
v1>v2
v1<v2
v1<=v2
v1!=v2
#logical operators
# and -> &
# or -> |

#conditional statements
# if else
a = 5
b = 9
c = 4
pass_marks = 15
total_marks = a + b + c
ifelse(total_marks>pass_marks,"Pass","Fail")
ifelse(total_marks>19,"Excellent",ifelse(total_marks>16,"Great",ifelse(total_marks>15,"Good","bad")))

#while loop


#for loop
fruits <- c("apples","bananas","grapes","oranges","pineapple")
for(fruit in fruits)
{
  print(fruit)
}
for(fruit in fruits)
{
  if(fruit=='apples')
    print("I like apples")
  if(fruit=='bananas')
    print("I like bananas")
  else
    print("I dont like this fruit")
}

#inbuilt functions
data("mtcars")
#frequency distribution
table(mtcars$cyl)
prop.table(table(mtcars$cyl))
#apply
apply(mtcars[1:11],2,mean)
# 1st argument - dataset,2nd-row(1)or col(2),3rd - function
#tapply
data("iris")
tapply(iris$Sepal.Length,iris$Species,mean)

#map_dbl function
library(purrr)
map_dbl(mtcars,mean)

#case study
first_hundred <- 1:100
first_hundred
for(number in first_hundred)
{
  if(number %% 2 == 0)
    print("even")
  else
    print("odd")
}

#2
sepal_6.5 <- ifelse(iris$Sepal.Length>6.5,"Less than 6.5","More than 6.5")
sepal_6.5 <- as.factor(sepal_6.5)
summary(sepal_6.5)

#3

View(CO2)
head(CO2)
mean_1 <- ifelse(CO2$Type == 'Mississippi' & CO2$Treatment == 'chilled',mean(CO2$uptake),'Na') 
mean_1
unique(mean_1)[2]
mean(CO2$uptake[64:84])
mean(CO2$uptake)


#4
tapply(CO2$uptake,CO2$Treatment,mean)
tapply(CO2$uptake,CO2$Treatment,median)
tapply(CO2$uptake,CO2$Treatment,min)
tapply(CO2$uptake,CO2$Treatment,max)

#5
library(purrr)
data("swiss")
View(swiss)
invoke_map(list(min_fertility = 'min',max_infant = 'max'),list(swiss$Fertility,swiss$Infant.Mortality))



#6

dice <- function() {
  sample(1:6)
}
dice()
dice()

#object oriented programming
library(dplyr)
fruit <- R6::R6Class("Fruit",list(name = apple,cost = 100))
class(fruit)
install.packages("R6", type = "binary")
library(R6)

#webscraping using R
library(rvest)
#rvest: helps in understanding HTML and CSS tags
# reading the Html from the website
url <- 'https://www.imdb.com/list/ls009997493/'

#read the HTML data
website <- read_html(url)
# using CSS selectors to scrape the ranking section
rank_data_html <- html_nodes(website,'.text-primary')
# convert ranking data to text
rank_data <- html_text(rank_data_html)
head(rank_data)
#convert to numeric
rank_data <- as.numeric(rank_data)

#repeat same process for others
#Movie name
Movie_html <- html_nodes(website,'.lister-item-header a')
Movie <- html_text(Movie_html)

#year
movie_year_html <- html_nodes(website,'.text-muted.unbold')
movie_year <- html_text(movie_year_html)
head(movie_year)
movie_year <- movie_year[4:103]


#genre
genre_html <- html_nodes(website,'.genre')
genre <- html_text(genre_html)

#rating 
rating_html <- html_nodes(website,'.ipl-rating-star.small .ipl-rating-star__rating')
rating <- html_text(rating_html)

# runtime
length_html <- html_nodes(website,'.runtime')
lengthh <- html_text(length_html)

#director
director_html <- html_nodes(website,'.text-small a:nth-child(1)')
director <- html_text(director_html)
director <- director[2:101]

#actor
actor_html <- html_nodes(website,'.ghost+ a')
actor <- html_text(actor_html)
actor <- as.factor(actor)

#votes
votes_html <- html_nodes(website,'.text-muted+ span:nth-child(2)')
votes <- html_text(votes_html)
votes <- votes[4:103]

#description
description_html <- html_nodes(website,'.ratings-metascore+ p , .ipl-rating-widget+ p')
description <- html_text(description_html)

Imdb_top100 <- cbind(rank_data,Movie,movie_year,genre,rating,lengthh,director,actor,votes,description)

Imdb_top100 <- as.data.frame(Imdb_top100)
class(Imdb_top100)
str(Imdb_top100)

write.csv(Imdb_top100,"Imdbtop100.csv")
setwd("~/Desktop")
Imdb_top100$rating <- as.numeric(as.character(Imdb_top100$rating))
plot(Imdb_top100$rating,Imdb_top100$Movie)
hist(Imdb_top100$rating)
class(Imdb_top100$rating)
plot(Imdb_top100$actor,Imdb_top100$rating)

library(dplyr)
library(ggplot2)
tapply(Imdb_top100$rating,Imdb_top100$actor,length)
which(Imdb_top100$actor == "Aamir Khan")
table(Imdb_top100$actor,Imdb_top100$rating)

Imdb_top100 <- read.csv('Imdbtop100.csv')
head(Imdb_top100)
str(Imdb_top100)
Imdb_test <- Imdb_top100
Imdb_test$movie_year <- Imdb_top100$movie_year
summary(Imdb_top100)
summary(Imdb_top100$actor)
amir_movies <- subset(Imdb_test,Imdb_test$actor == 'Aamir Khan' & Imdb_test$rating >8)
amir_movies$Movie
Imdb_test$movie_year <- gsub("[[:punct:]]", replacement = "",Imdb_test$movie_year)
Imdb_test$movie_year <- gsub(pattern ="I ",replacement = "",Imdb_test$movie_year)
#alternate approach
Imdb_test$movie_year <- substr(Imdb_test$movie_year,2,5)
Imdb_test$movie_year[84,] <- '2005'
class(Imdb_test$movie_year)
Imdb_test$movie_year <- as.numeric(Imdb_test$movie_year)
Imdb_test$movie_year[is.na(Imdb_test$movie_year)] <- 2005
#changing of columns
Imdb_test <- Imdb_test[,c(5,4,1:3,6:11)]
Imdb_test <- Imdb_test[,c(1:3,5,4,6:11)]


class(Imdb_test$movie_year)
Imdb_test$movie_year <- as.integer(Imdb_test$movie_year)
hist(Imdb_test$movie_year)

groupby_amir <- aggregate(Imdb_test$actor == 'Amir Khan',by = list(Imdb_test$movie_year),FUN = length)
plot(groupby_amir)
ggplot2::ggplot(data = groupby_amir,aes(x = Group.1,y = x))+
  geom_count()
  

# downloading twitter data
install.packages("twitteR")
library(twitteR)
#Wait for confirmation

# data wrangling
movie <- read.csv('movie_metadata.csv')
head(movie)
str(movie)
summary(movie)

movie_test <- movie
movie_test$aspect_ratio <- as.integer(movie_test$aspect_ratio)
class(movie_test$aspect_ratio)
movie_test[1,6]
movie_test[4,"language"]

#replacing a particular entry in a dataset
levels(movie_test$color)
levels(movie_test$color)[levels(movie_test$color) == ''] <- 'Not mentioned'
movie_test$duration[which(movie_test$duration == 178.5)] <- 178.0
class(movie_test$duration)
mean(movie_test$duration)
mean(movie_test$duration,na.rm = TRUE)
which(is.na(movie_test$duration)==T)
head(movie_test$duration)

#imputing with mean, median , mode
install.packages("Hmisc")n
library(Hmisc)
hist(movie$duration)
movie_test$duration <- impute(movie_test$duration,what = "mean")
movie_test$duration[is.na(movie_test$duration)] <- mean(movie_test$duration,na.rm = TRUE)
movie_test$duration <-  round(movie_test$duration,2)
movie_without_na <- movie
movie_without_na <- na.omit(movie_without_na)
table(movie_test$color)
group_by_year <- aggregate(movie_test$movie_title, by = list(movie_test$title_year,movie_test$language),FUN = length)
plot(group_by_year)
group_by_year

#splitting of dataset and random sampling
training_row_index <- sample(1:nrow(movie),0.7*nrow(movie))
head(training_row_index)
training_data <- movie[training_row_index,]
test_data <- movie[-training_row_index,]

#module 3 assignment
m2 <- read.csv('Disease_data (Assignment Dataset).csv',stringsAsFactors = T)
head(m2$Age)
class(m2)
str(m2)
m2_test <- m2
m2_test$Age <- as.numeric(m2_test$Age)
head(m2_test$Age)
m2_test$Age <- m2$Age
m2_test$Age <- as.numeric(as.character(m2_test$Age))
m2_test$Age[is.na(m2_test$Age)] <- 7

2.
m2$timesupper <- 
m2_average <- m2_test
m2_average$timesupper[is.na(m2_average$timesupper)] <- mean(m2_average$timesupper,na.rm = TRUE)
m2_average$timesupper <- as.Date(m2_average$timesupper,origin = "1935-01-01")


m2_time$timesupper <- strptime(m2_time$timesupper,format = "%Y-%m-%d %H:%M:%S",tz = 'UTC')
m2_without_na$timesupper <-strptime(m2_without_na$timesupper,format = "%Y-%m-%d %H:%M:%S",tz = 'UTC')
summary(m2)
str(m2$Age)


#time
m2_average$onsettime <- m2$onsettime
m2_average$onsettime <- strptime(m2_average$onsettime,format = '%H:%M',tz = 'UTC')

summary(m2)
str(m2)
ggplot2::ggplot(data = m2_food_item,aes(x = m2_food_item))
m2$fruitsalad[46] <- 'yes'
m2$fruitsalad[46]


par(mfrow = c(4,4))
plot(m2$vanilla)
plot(m2$baked_hamburgur)
plot(m2$spinach)
plot(m2$mashed_potato)
plot(m2$cabbages)
plot(m2$jello)
plot(m2$rolls)
plot(m2$brown)
plot(m2$milk)
plot(m2$coffee)
plot(m2$water)
plot(m2$cakes)
plot(m2$vanilla)
plot(m2$chocolate)
plot(m2$fruitsalad)


boxplot(m2_test$Age,title = "Average age of ill people")
library(ggplot2)
ggplot(data = m2_test,aes(y = Age,x = ill))+
  geom_boxplot(fill = 'red',alpha = 0.5)

m2$sex <- factor(m2$sex,levels = c('Male','Female'))
plot(m2$sex,col = m2$sex)
text(m2$sex,labels = table(m2$sex))
table(m2$sex)

















# data cleaning using regex functions
country <- c("USA","US","America","Russia")
grep(pattern = 'US',country)
sub(pattern = 'US','America',country)
club <- c("Chelsea Football Club","Liverpool Football Club","Everton Football Club")
#gsub
gsub(pattern = 'Football Club',replacement = 'FC',club)
#substr
substr(club,1,7)
last_4 <- substr(club,nchar(club)-3,nchar(club))
#How to split a string
x <- "I love the courses provided by EDUREKA"
strsplit(x, " ")
x1 <- c(1,3,5,2,9,3,5,3)
sort(x1,decreasing = TRUE)
order(x1)
sort[order(x1)]
install.packages("XLConnect")

dat <- read.csv("Data import.csv")
head(dat)
mean_people <- mean(dat$Height>4.7)

  
library(dplyr)
dat1 <- dat%>%select(Names,Height,Weight)%>%filter(Height>4.7)
mean(dat1$Weight)

library(XML)
url_cric= "https://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;template=results;type=batting"
tables <- readHTMLTable('http://decisionstats.com/2013/04/25/using-r-for-cricket-analysis-rstats-ipl/')
tables <- readHTMLTable(url)

library(rvest)
tables <- read_html(url_cric)
tables_new <- html_text(url_cric)
class(url_cric)
tables_df <- as.data.frame(tables) 

#visualization
install.packages("gridExtra")
library(gridExtra)
hist(Imdb_test$movie_year)
library(ggplot2)
hist(log(Imdb_test$movie_year))
hist(sqrt(Imdb_test$movie_year))
a <- qplot(x = Imdb_test$movie_year,data = Imdb_test)
b <- qplot(log(x = Imdb_test$movie_year),data = Imdb_test)
c <- qplot(sqrt(x = Imdb_test$movie_year),data = Imdb_test)
grid.arrange(a,b,c)
d <- stripchart(Imdb_test$movie_year)
e <- stripchart(log(Imdb_test$movie_year))


#dplyr
install.packages("hflights")
library(hflights)
data("hflights")
hf <- hflights
hf1 <- hf%>%select(1,8,14,15)
hf2 <- hf%>%select(contains("Time"))
hf3 <- hf%>%select(starts_with("Day"),ends_with("time"))

#filter
data("mtcars")
mt <- mtcars
mt_cyl <- mt%>%filter(cyl>=6)
mt_hp <- mt%>%filter(hp>100 & am ==1)%>%summarise(max(mpg))
mt_arrange <- mt%>%arrange(desc(mpg))

#joining tables
Player_club <- data.frame(Name = c('Salah','Aguero','Hazard','Vardy','Pogba'), 
                             club = c('Liverpool','Mancity','Chelsea','Leicester city',))
Player_Nation <- data.frame(Name = c('Salah','Aguero','Hazard','Vardy','Pogba'),
                            country = c('Egypt','Argentina','Belgium','England','France'))
#left join
player_combine1 <- left_join(Player_club,Player_Nation,by = "Name")
player_combine1
#right join
player_combine2 <- right_join(Player_club,Player_Nation,by = "Name")
#full join
player_combine3 <- full_join(Player_club,Player_Nation,by = "Name")
#inner join
player_combine4 <- inner_join(Player_club,Player_Nation,by = "Name")

#data.table
install.packages("data.table")
library(data.table)
data(iris)
ir <- iris
class(ir)
irt<- as.data.table(ir) 
View(irt)
class(irt)
irt[2:6]
irt[c(1,3,5,6)]
irt[Species=='setosa']
irt[,3:5]
irt[,mean(Sepal.Width),by = Species]

#case study Data Manipulation
#1.
housing_data <- read.csv('housingdata_v2.0.csv')
library(dplyr)
#2.
head(housing_data)
nrow(housing_data)
ncol(housing_data)
dim(housing_data)
class(housing_data)
str(housing_data)
#3.
property_purchased <- Sys.Date()-housing_data$Loan_Period*12
head(property_purchased)

# visualization
hd <- read.csv('Housing_data.csv')
head(hd)
hist(hd$PropertyValue)
colorRampPalette(c("green","orange","red"))->colfun
hist(hd$PropertyValue,col = colfun(10),xlab = 'Property value',ylab = 'No of houses',main = 'Property value',breaks = 10)

#scatter plot
plot(hd$PropertyValue,hd$Income,pch = 25,col = 'red') #pch gives shape(1-25)
colorRampPalette(c('blue','black','brown'))->colfun
plot(hd$PropertyValue,hd$Education  ,col = colfun(10))

#boxplot
boxplot(hd$PropertyValue~hd$Gender,col = c('red','blue'))
data(iris)
plot(iris$Sepal.Length,iris$Petal.Length,pch=c(4,7),col = iris$Species)
#piechart
table(hd$Gender)
pie(table(hd$Gender))
library(ggplot2)
ggplot(data = iris,aes(x = Sepal.Length,y = iris$Petal.Length,col = Species,shape = Species ))+geom_point()

ggplot(data = hd,aes(x = PropertyValue,fill = Gender))+
  geom_histogram(col = 'blue')
  
ggplot(data = hd,aes(x = PropertyValue,fill = Gender))+
  geom_histogram(position = 'identity')
ggplot(data = hd,aes(x = PropertyValue,fill = Gender))+
  geom_histogram(position = 'fill')
#bar chart
ggplot(data = hd,aes(Gender))+
  geom_bar(fill = c('Blue','red'))

#density plot
ggplot(data = hd, aes(PropertyValue,fill = Gender))+
  geom_density()
#transparency values
ggplot(data = hd, aes(PropertyValue,fill = Gender))+
  geom_density(alpha = 0.5)


#boston
data("boston",package = "mlbench")
install.packages("Hmisc")
library(Hmisc)
library(lattice)
library(dplyr)
install.packages(installr)
install.packages("RWeka")
install.packages(u)

install.packages("devtools")
devtools::install_github("AndreaCirilloAC/updateR")
updateR(admin_password = "s")
updateR::updateR(admin_password = "s")

install.packages("Hmisc")
library(Hmisc)

# data visualization assignment

#1
hd <- read.csv('Housing_data.csv',stringsAsFactors = T)
head(hd)
dim(hd)
class(hd)
str(hd)

#2.
library(ggplot2)
library(dplyr)
ggplot(data = hd,aes(x = Credit_Record,y = Income))+
  geom_point()
# there are only two values so the plot is different
hd1 <- hd%>%select(Credit_Record,Income)
hd1$Credit_Record <- as.factor(hd1$Credit_Record)

ggplot(data = hd1,aes(x = Credit_Record,y = Income))+
  geom_point()
# there are only two outcomes on the x-axis

#3.
ggplot(data = hd,aes(x = Income, y = PropertyValue,col = No_kids))+
  geom_point()+
  geom_smooth(se = FALSE)

#5
plot(x = hd$Income, y = hd$PropertyValue,col = hd$No_kids)
plot(x = hd$Income, y = hd$PropertyValue,col = as.factor(hd$No_kids))

ggplot(data = hd,aes(x = Income, y = PropertyValue,col = as.factor(No_kids)))+
  geom_point()

#6
ggplot(data = hd,aes(x = PropertyValue, y = Income,col = No_kids))+
geom_point(alpha=0.3)

ggplot(data = hd,aes(x = Income, y = PropertyValue,col = No_kids))+
  geom_point()+
  geom_jitter(width = 0.1)

#7.
ggplot(data = hd,aes(x = Income))+
  geom_histogram(binwidth = 100)+
  geom_density()


ggplot(data = hd,aes(x = Income,))+
  geom_histogram(fill = '377EB8',binwidth = 100)+
  geom_line(stat = "density")
  
#8
ggplot(data = hd,aes(x = Education, fill = Property_Purchased))+
  geom_bar()
ggplot(data = hd,aes(x = Education, stack = Property_Purchased))+
  geom_bar()
ggplot(data = hd,aes(x = Education, dodge = Property_Purchased))+
  geom_bar(fill = 'blue')
 

#9
posn_d <- function(x){
  position_dodge(0.7)
}
ggplot(data = hd,aes(x = Education, fill = Property_Purchased))+
  geom_bar(position = posn_d(0.7),alpha = 0.6)

#10
ggplot(data = hd,aes(x = Income,fill = HasCar,color = Income ))+
  geom_histogram(bins = 30,position = 'identity')
#12
ggplot(data = hd,aes(x = Income, y = PropertyValue,col = No_kids))+
  geom_point()+
  facet_wrap(~HasCar)
ggplot(data = hd,aes(x = Income, y = PropertyValue,col = No_kids))+
  geom_point()+
  facet_wrap(~No_kids)

ggplot(data = hd,aes(x = Income, y = PropertyValue,col = No_kids))+
  geom_point()+
  facet_wrap(~No_kids +HasCar)

# in-class project
#setting the working directory and reading the file 
setwd("~/Desktop/EDUREKA/R/Inclass dataset/553_m8_datasets_v1.0")
census <- read.csv('census.csv',stringsAsFactors = T)
# data preprocessing
head(census)
summary(census)
str(census)
# removing the first column
census <- census[,-1]
head(census)

# giving names to columns
colnames(census) <- c('Age','Working-Class','Citizen-Score','Education','Education_Number',
                      'Marital_status','Occupation','Relationship','Race','Sex',
                      'Capital_gain','Capital_loss','Hrs/week','Country','Avg_salary')
names(census)
#frequencydistribution
barplot(table(census$`Working-Class`))
# tapply function to calculate the mean age across working class
tapply(census$Age,census$`Working-Class`,mean)
tapply(census$Age,census$`Working-Class`,max)
tapply(census$Age,census$`Working-Class`,min)

# education variable
table(census$Education)
class(census$Education)
census$Education <- as.character(census$Education)
which(census$Education == ' 5th-6th')
census$Education[census$Education == " 1st-4th"] <- "Primary School"
census$Education[census$Education == " 5th-6th"] <- "Primary School"
census$Education[census$Education == " 7th-8th"] <- "Secondary School"
census$Education[census$Education == " 9th"]     <- "Secondary School"
census$Education[census$Education == " 10th"]    <- "Secondary School"
census$Education[census$Education == " 11th"]    <- "Secondary School"
census$Education[census$Education == " 12th"]    <- "Secondary School"

table(census$Education)

#marital status variable
levels(census$Marital_status)
table(census$Marital_status)
library(dplyr)
census$Marital_status <- ifelse(census$Marital_status == " Married-AF-spouse" | census$Marital_status == " Married-civ-spouse"
                                | census$Marital_status == " Married-spouse-absent","Married",census$Marital_status )

census$Marital_status[census$Marital_status == 'Divorced'] <- 'Single'
census$Marital_status[census$Marital_status == 'Never-married'] <- 'Single'
census$Marital_status[census$Marital_status == 'Separated'] <- 'Single'
census$Marital_status[census$Marital_status == 'Widowed'] <- 'Single'

table(census$Marital_status)

#occupation column
table(census$Occupation)
levels(census$Occupation)
class(census$Occupation)
census$Occupation <- as.character(census$Occupation)

census$Occupation[census$Occupation ==' ?'] <-  NA
census$Occupation[census$Occupation ==' Adm-clerical'] <-  'Executive'
census$Occupation[census$Occupation ==' Craft-repair'] <-  'Blue-Collar'
census$Occupation[census$Occupation ==' Exec-managerial'] <-  'Executive'
census$Occupation[census$Occupation ==' Farming-fishing'] <-  'Blue-Collar'
census$Occupation[census$Occupation ==' Handlers-cleaners'] <-  'Blue-Collar'
census$Occupation[census$Occupation ==' Machine-op-inspct'] <-  'Blue-Collar'
census$Occupation[census$Occupation ==' Other-service'] <-  'Service'
census$Occupation[census$Occupation ==' Priv-house-serv'] <-  'Service'
census$Occupation[census$Occupation ==' Prof-specialty'] <-  'Executive'
census$Occupation[census$Occupation ==' Protective-serv'] <-  'Service'
census$Occupation[census$Occupation ==' Tech-support'] <-  'Executive'
census$Occupation[census$Occupation ==' Transport-moving'] <-  'Blue-Collar'


# country variable
table(census$Country)
levels(census$Country)
census$Country <- as.character(census$Country)
census$Country[census$Country == '?'] <- NA
census$Country[census$Country == ' Cambodia'] <- 'Asia'
census$Country[census$Country == ' China'] <- 'Asia'
census$Country[census$Country == ' Columbia'] <- 'South-America'
census$Country[census$Country == ' Cuba'] <- 'Central-America'
census$Country[census$Country == ' Dominican-Republic'] <- 'Central-America'
census$Country[census$Country == ' Ecuador'] <- 'South-America'
census$Country[census$Country == ' El-Salvador'] <- 'Central-America'
census$Country[census$Country == ' England'] <- 'Europe'
census$Country[census$Country == ' France'] <- 'Europe'
census$Country[census$Country == ' Germany'] <- 'Europe'
census$Country[census$Country == ' Greece'] <- 'Europe'
census$Country[census$Country == ' Guatemala'] <- 'Central-America'
census$Country[census$Country == ' Haiti'] <- 'Central-America'
census$Country[census$Country == ' Holand-Netherlands'] <- 'Europe'
census$Country[census$Country == ' Honduras'] <- 'Central-America'
census$Country[census$Country == ' Hong'] <- 'Asia'
census$Country[census$Country == ' Hungary'] <- 'Europe'
census$Country[census$Country == ' India'] <- 'Asia'
census$Country[census$Country == ' Iran'] <- 'Asia'
census$Country[census$Country == ' Ireland'] <- 'Europe'
census$Country[census$Country == ' Italy'] <- 'Europe'
census$Country[census$Country == ' Jamaica'] <- 'Caribbean'
census$Country[census$Country == ' Japan'] <- 'Asia'
census$Country[census$Country == ' Laos'] <- 'Caribbean'
census$Country[census$Country == ' Mexico'] <- 'Central-America'
census$Country[census$Country == ' Nicaragua'] <- 'Central-America'
census$Country[census$Country == ' Peru'] <- 'South-America'
census$Country[census$Country == ' Philippines'] <- 'Asia'
census$Country[census$Country == ' Poland'] <- 'Europe'
census$Country[census$Country == ' Portugal'] <- 'Europe'
census$Country[census$Country == ' Puerto-Rico'] <- 'Central-America'
census$Country[census$Country == ' Scotland'] <- 'Europe'
census$Country[census$Country == ' Taiwan'] <- 'Asia'
census$Country[census$Country == ' Thailand'] <- 'Asia'
census$Country[census$Country == ' Trinadad&Tobago'] <- 'Caribbean'
census$Country[census$Country == ' Vietnam'] <- 'Asia'

table(census$Country)

# filtering records
female_50 <- census%>%filter(Age > 50 & Sex == ' Female')%>%select(Age,Sex)%>%summarise(Total_count = n())
head(female_50)

census <- census%>%mutate(birth_year = 2019 - Age)

library(ggplot2)
class(census$birth_year)
census$birth_year <- as.factor(census$birth_year)
plot(census$birth_year)
ggplot(data = census,aes(x = Sex,y = Age,col = Sex))+
  geom_boxplot()


# certification project
setwd("~/Desktop/EDUREKA/R/Certification project/Data science using R")
hrm <- read.csv("338_cert_proj_datasets_v3.0.csv")
head(hrm)
dim(hrm)
str(hrm)
hist(hrm$satisfaction_level)
hist(hrm$average_montly_hours)
hrm$salary <- as.factor(hrm$salary)
table(hrm$salary)
colSums(is.na(hrm))
plot(hrm$salary)
hist(hrm$time_spend_company)
hrm$department <- as.factor(hrm$department)
table(hrm$department)
plot(hrm$department)
plot(cor(hrm[,1:8]))
corrplot::corrplot(hrm[,1:8],method = "circle")
barplot(as.factor(hrm$left))
hrm$left_f <- as.factor(hrm$left)
hrm$left_f <- ifelse(hrm$left_f == 1,'Yes','No')
table(hrm$left_f)
library(dplyr)
hrm_dept <- hrm%>%group_by(department)%>%select(department,left)
hrm_dept_left <- hrm%>%filter(left == 1)%>%group_by(department)%>%select(department,left)
table(hrm_dept_left$department)
prop.table(table(hrm$left_f))
# decision tree model 
hrm_n <- hrm[-7]
hrm_n$left_f<- as.factor(hrm_n$left_f)
set.seed(4)
index <- sample(1:nrow(hrm_n),0.70*nrow(hrm_n),replace = T)
hrm_train <- hrm_n[index,]
hrm_test <- hrm_n[-index,]
hrm_model <- C5.0(hrm_train[-10],hrm_train$left_f)
hrm_model
summary(hrm_model)
#evaluating model performance
hrm_predict <- predict(hrm_model,hrm_test)
library(gmodels)
CrossTable(hrm_test$left_f,hrm_predict,
           prop.chisq = F,prop.c = F,prop.r = F,
           dnn = c('actual default','predicted default'))

# using rpart
hrm_model2 <- rpart::rpart(hrm_train$left_f~.,data = hrm_train)
hrm_predict2 <- predict(hrm_model2,newdata = hrm_test,type = "class")
summary(hrm_model2)
dt <-  caret::confusionMatrix(table(hrm_predict2,hrm_test$left_f))
dt$overall[1]

plot(hrm_model2,margin = 0.1)
text(hrm_model2,use.n = T,pretty = T,cex = 0.8)

# random forest
library(randomForest)
library(caret)

set.seed(4)
index <- sample(1:nrow(hrm_n),0.70*nrow(hrm_n),replace = T)
hrm_train <- hrm_n[index,]
hrm_test <- hrm_n[-index,]
hrm_model <- randomForest(hrm_train$left_f~.,data = hrm_train,ntree = 200)
hrm_model
plot(hrm_model)
legend("right",colnames(hrm_model$err.rate),col = 1:4,cex = 0.8,fill = 1:4)
hrm_predict_rf <- predict(hrm_model,newdata = hrm_test)
rf <- confusionMatrix(table(hrm_predict_rf,hrm_test$left_f))
rf$overall
# to see the how significant a parameter is
varImp(hrm_model)
varImpPlot(hrm_model,sort = T,type = 2)

a = c()
i = 5
for (i in 3:8) {
  hrm_model3 <- randomForest(hrm_train$left_f~.,data = hrm_train,ntree = 500,mtry = i , importance = T)
  hrm_predict3 <- predict(hrm_model3,newdata = hrm_test,type = "class")
  a[i-2] = mean(hrm_predict3 == hrm_test$left_f)
}
a
plot(3:8,a)

# Naïve Bayes
library('e1071')
hrm_bayes_model <- naiveBayes(hrm_train$left_f~.,data = hrm_train)
hrm_bayes_model
hrm_bayes_model_pred <- predict(hrm_bayes_model,newdata = hrm_train)
hrm_bayes_model_pred
n <- confusionMatrix(table(hrm_bayes_model_pred,hrm_train$left_f))
n$overall
# support vector machine
hrm_svm_model <- svm(hrm_train$left_f~.,data = hrm_train,kernel = "linear",cost = 0.1)
summary(hrm_svm_model)
plot(hrm_svm_model,(hrm_train),satisfaction_level~last_evaluation)
names(hrm_n)
pred_svm_hrm <- predict(hrm_svm_model,newdata = hrm_test,type = "class")
confusionMatrix(table(pred_svm_hrm,hrm_test$left_f))

# tune for the best cost function
set.seed(1)
obj <- tune(svm,left_f~.,data = hrm_train,
            ranges = list(gamma = 2^(-1:1),cost = c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"),kernel = "linear")
summary(obj)
best_model <- obj$best.model
summary(best_model)
pred_svm_hrm_best <- predict(best_model,hrm_test,type = "class")
pred_svm_hrm_best
confusionMatrix(table(pred_svm_hrm_best,hrm_test$left_f))
p <- confusionMatrix(table(pred_svm_hrm_best,hrm_test$left_f))
p$overall
# kernel  = polynomial
set.seed(1)
obj <- tune(svm,left_f~.,data = hrm_train,
            ranges = list(gamma = 2^(-1:1),cost = c(0.001,0.01,0.1,1,5,10,100)),
            tunecontrol = tune.control(sampling = "fix"),kernel = "polynomial")
summary(obj)
best_model <- obj$best.model
summary(best_model)
pred_svm_hrm_best <- predict(best_model,hrm_test,type = "class")
pred_svm_hrm_best
sv <- confusionMatrix(table(pred_svm_hrm_best,hrm_test$left_f))


# overall model 

model_accuracy <- data.frame(model = c("Decision Tree","Random Forest","Naïve Bayes","SVM"),
                             Accuracy  = c(dt$overall[1],rf$overall[1],n$overall[1],p$overall[1]))
model_accuracy


#digital vidya

matches <- read.csv('matches.csv')
deliveries <- read.csv('deliveries.csv')
match_del <- merge(matches,deliveries,by.x = 'id',by.y = 'match_id',all =T)
head(match_del)




