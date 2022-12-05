library(ggplot2)
library(dplyr)
library(stringr)
library(assertive)


netflix <- read.csv("Dataset/netflix_titles.csv", header = TRUE)

#netflix$date_added <- as.Date(netflix$date_added, format = "%B %d, %Y")
?apply()
unique_counts <- apply(netflix,2,FUN = function(x) length(x))
unique_counts <- data.frame(Columns = names(uniq))
class(netflix$show_id)
head(netflix)
?netflix
library(stringr)

id_without_string = str_remove(netflix$show_id, "s") 
id_without_string
library(dplyr)
library(assertive)
mean(as.numeric(id_without_string))
?glimpse
rating = c(1,2,2,2,2,3,3,5,2,1,5,7,0)
rating
breaks <- c(min(rating),0,5,max(rating))
breaks
library(ggplot2)
ggplot(rating,aes(rating)) + geom_histogram(breaks = breaks)
assert_all_are_in_closed_range(rating,lower = 0 , upper = 5)

data("ChickWeight")
View(ChickWeight)
count = table(ChickWeight$weight)
View(count)
barplot(count,main='weight')
avg = aggregate(ChickWeight[,1] , list(ChickWeight$Time), mean)

data("VADeaths")
View(VADeaths)
barplot(VADeaths , col = rainbow(5),legend = T)
barplot(VADeaths , col = rainbow(5),legend = T , beside = T)


data("mtcars")
View(mtcars)


x = rep(1:5,times =5 )
x
y = rep(1:5 , each = 5)
y
plot(x,y,pch=3,lwd=x,cex=y,xlim = c(0,5),ylim = c(0,5))
?replace
?assert_all_are_in_closed_range
?filter
today()

df = as.data.frame(c(1,1,2,2,3,4,5))

duplicated(df)
sum(duplicated(df))
filter(df,duplicated(df))
df1 = distinct(df)
df1
sum(duplicated(df1))

df %>% group_by(index) 

df = as.data.frame(c("4,5"))
aft = str_remove(df,",")
is_numeric(aft)
nju = as.numeric(aft)
nju
is_numeric(nju)
df %>% mutate(rev = as.numeric(aft))
View(df)
df1 = as.data.frame(c(1,2,3))
df = c(1,2)
df1 %>% anti_join(df1, by = df)

library(forcats)
other_cat = c("amphibian" , "fish" , "bug" , "invertebrate" , "reptile")
animals
animals %>%
  mutate(type_coll = fct_collapse(type_trimmed,other = other_cat))


df['m'] = as.data.frame(c("1-2" , "11" , "1-4-5"))
df
df %>%
  mutate(rem = str_replace_all(m , "-" , " "))
df$m

cle = df$m %>%
  str_remove_all("-") %>%
  str_remove_all(" ")
cle

df %>%
  mutate(clen = cle)

df %>%
  filter(str_length(cle) == 2)
df

?str_remove_all


df
str.remove_all(df[2],"-")
?left_join


library(lubridate)
date_deff = floor(as.numeric(as.Date("2015-12-05") %--% today() , "years"))
date_deff
sum(is.na(netflix))
library(visdat)
install.packages("visdat")
library(visdat)
vis_miss(netflix)
?summarise
x <- matrix(1:4, nrow = 2)
x
x[2,2] = 2000
x
calculate_bmi <- function(height, weight) {
  weight/(height^2) * 10000
}

calculate_bmi(173,63)
as.numeric('50') + 20
measures_of_center <- function(x){
  c(mean = mean(x), median = median(x))
}

vectors <- list(
  A = c(9, 1, 8, 4, 3),
  B = c(7,  3,  4,  4,  1,  1, 10,  8)
)
measures_of_center(vectors)
for(i <1 , i <=10 , i++){
  print(i)
}

x <- c(1, 2, 3)
x[2]= 5
x
ifelse()
str_length(c("summer", "sun", "volleyball", "flowers"))
as.x = ''

class(x) <- "Class of X"
install.packages("stringdist")
library(stringdist)
stringdist("baboon" , "typhoon" , method = "jaccard")
df["m"] = as.data.frame(c("cair" , "alex"))
install.packages("fuzzyjoin")
df
library(fuzzyjoin)
df1["m1"] = as.data.frame("cairo")
df1
stringdist_left_join(df1,m,by=df1["m1"],method ="dl" )

?stringdist
stringdist("puffin" , "muffins" , method = "dl")
