#libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(assertive)
library(visdat)


#importing the dataset
netflix <- read.csv("Dataset/netflix_titles.csv" , header = TRUE)

#different ways to see the structure of the dataset
head(netflix)
glimpse(netflix)
View(netflix)

summary(netflix)

#date_added is not defined well
netflix <- netflix %>%
  mutate(date_added = parse_date_time(date_added , orders = c("%B %d, %Y")))
  # OR mutate(date_added = mdy(date_added))
View(netflix)

summary(netflix)

#checking for duplicated
filter(netflix,duplicated(netflix)) # no duplicated values
sum(duplicated(netflix)) # equal Zero

#let's check there isn't any duplicated names


#check for NA
vis_miss(netflix) # we found there are na values :(

#checking for dates
assert_all_are_in_past(netflix$date_added) 

miz_date = netflix %>%
  filter(is.na(date_added))

netflix %>%
  filter(is.na(date_added)) %>%
  count(type)

netflix %>%
  count(type)

netflix %>%
  count(is.na(date_added))

View(miz_date)

netflix %>%
  count(country == "")

#there is many empty records in country columns
#all na values in TV Shows - and na count = 10 of 2676
#let's drop these values
netflix <- netflix %>%
  filter(!is.na(date_added))

sum(is.na(netflix)) # equal Zero

vis_miss(netflix) #Done

View(netflix)

#show the movies vs TV shows
ggplot(data = netflix , aes(x = type)) +
  geom_bar(fill = "dark blue")+
  labs(title = "Movies Vs TV Shows")+
  xlab("Type")+
  ylab("count")
#6131 movie and 2666 TV show

#make two datafram for each type
netflix_movies <- netflix %>%
  filter(type == "Movie")
View(netflix_movies)

netflix_TV_shows <- netflix %>%
  filter(type == "TV Show")
View(netflix_TV_shows)


movies_summerize <- netflix_movies%>%
  filter(country!="")%>%
  group_by(country)%>%
  summarize(number =n())%>%
  arrange(desc(number))%>%
  slice(1:10)



head(movies_summerize)

View(movies_summerize)

ggplot(data = movies_summerize , aes(x = reorder(country, -number),y = number))+
  geom_col() +
  labs(title = "Top 10 county movies")+
  xlab("country")+
  ylab("Number of movies")+
  theme(plot.title = element_text(40),axis.title.x = element_text(30)
        ,axis.title.y = element_text(30),panel.background = element_blank()
        ,axis.text.x = element_text(15, angle = 45))
  
