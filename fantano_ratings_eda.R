library(tidyverse)

#Reading in data
fantano <- read_csv("fantano.csv")

#Filtering out NA values in total_length
fantano_filter <- fantano %>%
  drop_na(total_length)

#Creating new column that only contains only numeric value of total_length and with no white space
fantano_filter <- fantano_filter %>%
  mutate(album_length = str_trim(str_to_lower(str_replace(fantano_filter$total_length, 'Total Length: ', ''))))

#Creating new column that only contains minute value of album_length and converting those values to doubles
fantano_filter <- fantano_filter %>%
  tidyr::extract(album_length, 'album_minutes_only', regex = '(\\d{1,} minute)', remove = F) %>%
  mutate(album_minutes_only = as.double(str_replace(album_minutes_only, 'minute', '')))

##Creating new column that only contains hour value of album_length and creating new column that converts it to minutes
fantano_filter<-fantano_filter%>%
  tidyr::extract(col=album_length,into = "hours", regex = "(\\d\\shour)", remove=F)%>%
  tidyr::extract(col=hours, into="hours", regex="(\\d)", remove=F)%>%
  mutate(hours_minutes=as.double(hours))%>%
  mutate(hours_minutes=hours_minutes*60)

#Creating new column that sums hours_minutes and album_minutes_only to find total length of each album in minutes
fantano_filter <- fantano_filter %>%
  rowwise() %>%
  mutate(total_length_minutes = sum(c(hours_minutes, album_minutes_only), na.rm = T))

#Finding the album that had the longest length in minutes
longest_album <- fantano_filter %>%
  arrange(desc(total_length_minutes)) %>%
  head(1) %>%
  pull(album)

#Creating a new column that is set to 1 if 'hip hop' is detected as a genre
fantano <- fantano %>%
  mutate(hip_hop = ifelse(str_detect(tolower(genres), "hip hop"), 1, 0))

#Creating a new column that finds the number of producers of each album)
fantano <- fantano %>%
  mutate(producer_number = stringr::str_count(producer, ",") + 1 )

#Finding the hip hop albums with the most amount of producers
most_producers <- fantano %>%
  arrange(desc(producer_number)) %>%
  head(1) %>%
  pull(album)

#Creating a new column that is the release_date column in YYYY-MM-DD format, filtering to only include hip hop albums released in 2022, selecting only certain columns
fantano <- fantano %>%
  mutate(date = lubridate::mdy(release_date)) %>%
  filter(str_detect(date, "2022")) %>%
  filter(hip_hop == 1)

fantano_graph <- fantano %>%
  select(artist, album, fantano_rating)

#Loading in glue library and creating new column that contains the artist and album name
library(glue)
fantano_graph <- fantano_graph %>%
  mutate(artist_album = glue('{fantano_graph$artist}: {fantano_graph$album}'))

#Finding the top 15 hip hop albums
fantano_graph_subset <- fantano_graph %>%
  arrange(desc(fantano_rating)) %>%
  head(15)

#Visualizing the top 15 hip hop albums of 2022
fantano_graph_final <- fantano_graph_subset %>%
  mutate(artist_album = fct_reorder(artist_album, fantano_rating)) %>% ## edit to put the graph in descending order
  ggplot(aes(x = fantano_rating, y = artist_album)) + ## edit
  geom_col() + ## do not edit
  coord_cartesian(xlim = c(60,90)) + ## do not edit
  labs(x = "Fantano Score",
       y = "",
       title = "Top 15 Hip Hop Albums of 2022",
       subtitle = "Anthony Fantano Ratings") + ## do not edit
  theme_minimal() ## do not edit

#Finding the average difference between fantano_rating and critic_score for the top 10 highest rated fantano albums
highest_fantano_ratings <- fantano_writeup %>%
  arrange(desc(fantano_rating)) %>%
  select(album, fantano_rating, critic_score) %>%
  mutate(difference = fantano_rating - critic_score) %>%
  head(10) %>%
  mutate(average_difference = mean(difference))

#Finding the average difference between fantano_rating and critic_score for the top 10 lowest rated fantano albums
lowest_fantano_ratings <- fantano_writeup %>%
  drop_na() %>%
  arrange(desc(fantano_rating)) %>%
  select(album, fantano_rating, critic_score) %>%
  mutate(difference = fantano_rating - critic_score) %>%
  tail(10) %>%
  mutate(average_difference = mean(difference))

#Visualizing the difference between the top 10 highest fantno ratings and their critic equivalent rating
top_ranking_difference <- highest_fantano_ratings %>%
  ggplot(aes(y = album)) +
  geom_col(aes(x = fantano_rating, fill = 'coral2')) +
  geom_col(aes(x= critic_score, fill = 'cornflowerblue'))+
  scale_fill_manual(
    name = 'Reviewer',
    values = c('coral2' = 'coral2', 'cornflowerblue' =
                 'cornflowerblue'),
    labels = c('Fantanto Score', 'Critic Score')
  ) +
  labs( x = 'Rating',
        y = 'Album',
        title = 'Top 10 Highest Fantano Ratings and Their Critic Equivalent')

#Visualizing the difference between the bottom 10 highest fantno ratings and their critic equivalent rating
lowest_ranking_difference <- lowest_fantano_ratings %>%
  ggplot(aes(y = album)) +
  geom_col(aes(x= critic_score, fill = 'cornflowerblue'))+
  geom_col(aes(x = fantano_rating, fill = 'coral2')) +
  scale_fill_manual(
    name = 'Reviewer',
    values = c('coral2' = 'coral2', 'cornflowerblue' =
                 'cornflowerblue'),
    labels = c('Fantanto Score', 'Critic Score')
  ) +
  labs( x = 'Rating',
        y = 'Album',
        title = 'Top 10 Lowest Fantano Ratings and Their Critic Equivalent')

#Finding average fantano, critic, and user scores and finding differences between them
average_fantano_rating <- mean(fantano_writeup$fantano_rating, na.rm = T)
average_critic_rating <- mean(fantano_writeup$critic_score, na.rm = T)
average_user_rating <- mean(as.double(fantano_writeup$user_score), na.rm = T)
average_critic_and_user <- (average_critic_rating + average_user_rating) /2
difference_in_critic_rating <- average_fantano_rating - average_critic_rating
difference_in_user_rating<- average_fantano_rating - average_user_rating

fantano_writeup <- read_csv('fantano.csv')
#Standardizing date values and dropping NA values in release_date
fantano_writeup <- fantano_writeup %>%
  mutate(release_date = lubridate::year(release_date)) %>%
  drop_na(release_date)

#Grouping the average fantano ratings by year
review_average_by_year <- fantano_writeup %>%
  group_by(release_date) %>%
  summarise(fantano_rating = mean(fantano_rating, na.rm =T)) %>%
  head(14)

#Visualizing the average fantano ratings by year
ggplot(review_average_by_year, aes(x = release_date, y = fantano_rating)) +
  geom_line()




