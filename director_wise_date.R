keggle2 <- read.csv("./datasets/movie_metadata.csv", sep=",", header=TRUE, na.strings = c("", "NA"))
#keggle2 <- read.csv("movie_metadata.csv", sep=",", header=TRUE, na.strings = c("", "NA"))
temp3<-keggle2
attach(temp3)
#removing special characters from movie_title
temp3$movie_title1<- gsub("Â", "", temp3$movie_title)
#trim leading and trailing white spaces
trimws(temp3$movie_title1)
library(dplyr)
temp4 <- temp3 %>% group_by(director_name) %>% summarize(count=n()) %>% arrange(desc(count)) %>% subset(count >15)
library(ggplot2)
#maximum number of films, director wise.
ggplot(subset(temp4,!is.na(director_name)), aes(reorder(director_name, -count), count)) +
  xlab("Director") +
  ylab("Number of Films") +
geom_bar(stat="identity", width=0.5)

#get the highest grossers, director wise
temp5 <- temp3 %>% select(director_name, gross, imdb_score)
#using as.numeric since after summing up, integer values overflows
temp5 <- temp5 %>% group_by(director_name)
temp5 <- temp5 %>% summarize(total_gross=sum(as.numeric(gross, na.rm=TRUE)), avg_imdb_score =mean(imdb_score))
temp5 <- temp5 %>% arrange(desc(total_gross))
#get the top 10 highest grossing directors

ggplot(head(temp5, 10), aes(reorder(director_name, -total_gross), total_gross/1000000)) +
  xlab("Director") +
  ylab("Total Gross Amount") +
  geom_bar(stat="identity", width = 0.5, position="stack", colour = "red") +
  geom_smooth(method =lm) +
  #scale_x_continuous(breaks=c(5, 6, 7, 8, 9)) +
  scale_y_continuous(breaks=c(1000, 2000), labels = c("1 Billion", "2 Billion"))

#get the average imdb rating for top 10 highest grossing directors
ggplot(head(temp5, 10), aes(reorder(director_name, -total_gross), avg_imdb_score, group = 1)) +
  xlab("Director") +
  ylab("Avg IMDB Rating of 10 top directors") +
  geom_line(colour = "red")
