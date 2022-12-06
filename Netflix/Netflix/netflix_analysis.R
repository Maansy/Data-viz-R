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
#assert_all_are_in_past(netflix$date_added) 

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

rubbish_in_country = netflix %>%
  filter(type == "Movie") %>%
  count(country) %>%
  group_by(country)
View(rubbish_in_country)
#A real problem.
#how to deal with all this rubbish?

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

summerize_data <- function(DataFrame){
  return(
    DataFrame%>%
      filter(country!="")%>%
      group_by(country)%>%
      summarize(number =n())%>%
      arrange(desc(number))%>%
      slice(1:10)
  )
}

#Get top 10 country in movies
movies_summerize <- summerize_data(netflix_movies)

head(movies_summerize)

View(movies_summerize)

ggplot(data = movies_summerize , aes(x = reorder(country, -number),y = number))+
  geom_col() +
  labs(title = "Top 10 county movies")+
  xlab("country")+
  ylab("Number of movies")+
  theme(panel.background = element_blank()
        ,axis.text.x = element_text(15, angle = 45))
  
#Get top 10 country in TV Shows
TV_show_summrize <- summerize_data(netflix_TV_shows)

head(TV_show_summrize)

View(TV_show_summrize)

ggplot(data = TV_show_summrize , aes(x = reorder(country , -number) , y = number))+
  geom_col()+
  labs(title = "Top 10 TV Shows country")+
  xlab("County")+
  ylab("number of TV Shows")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(15, angle = 45))

#the directors turn
directors <- netflix %>%
  group_by(director) %>%
  filter(director != "") %>%
  summarise(number = n()) %>%
  arrange(desc(number)) %>%
  slice(1:20)

View(directors)

ggplot(data = directors , aes(x= reorder(director, -number) , y = number))+
  geom_col() +
  labs(title = "The Top 20 Directors")+
  xlab("Director")+
  ylab("Number of works")+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(15,angle = 90))

#which year has most release
netflix_years <- netflix %>%
  filter(release_year >= 2010) %>%
  group_by(release_year) %>%
  summarise(number= n()) %>%
  arrange()%>%
  select(number , release_year)
    
#group_by(release_year) %>%
  #arrange() %>%
  #summarize()

View(netflix_years)

ggplot(data=netflix_years, aes(x=reorder(release_year , -release_year) , y = number))+
  geom_col() +
  labs(title = "Which year had more Movies and TV Shows") +
  xlab("Release Year")+
  ylab("Count")+
  theme(panel.background = element_blank())


#rating
ggplot(data=netflix,aes(x=rating, fill=type))+geom_bar(position=position_dodge())+
  labs(title='Ratings - Movies vs TV Shows')+
  xlab("Rating")+
  ylab("Count")+
  theme(panel.background = element_blank())+
  scale_fill_manual(breaks =c("Movie","TV Show"),
                    values=c("navy blue", "light blue"))

#tv duration
duration_movies<-netflix%>%
  filter(type=='TV Show')%>%
  group_by(release_year)%>%
  summarise(across(c(duration)))


ggplot(duration_movies,aes(x=release_year, y = duration)) +
  geom_point(col = 'dark blue') +
  labs(title = 'Duration of TV Shows Over the Years') +
  xlab('Year')+
  ylab('Duration')+
  theme(panel.background =element_blank()
        ,axis.text.x = element_text(20,angle=90))

netflix%>%
  filter(release_year >= 2010)%>%
  filter(type=='TV Show')%>%
  group_by(release_year)%>%
  summarise(across(c(duration)))%>%
  ggplot(aes(x=release_year, y = duration)) +geom_col(fill = 'dark blue') +
  labs(title = 'Duration of TV Shows in Recent Years') +
  xlab('Year')+
  ylab('Duration')+
  theme(panel.background =element_blank()
        ,plot.title = element_text(40)
        ,axis.text.x = element_text(20,angle=90)
        ,axis.text.y = element_text(20)
        ,axis.title.x = element_text(30)
        ,axis.title.y = element_text(30)) + 
  facet_wrap(~release_year)
