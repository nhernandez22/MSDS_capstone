
## prep 4 vis


head(route_quality)

## load ploty & extract coordinates


#devtools::install_github('ropensci/plotly')
library(plotly)

rqGeo <- route_quality %>%
  mutate(lon = as.numeric(str_extract(parent_loc, '(-|)\\d+.\\d+')),
         lat = as.numeric(str_extract(parent_loc, '\\s(-|)\\d+.\\d+')))  %>%
  filter(!is.na(lat), !is.na(lon))

rqGeo

## mapbox set up 


#install.packages("mapboxapi")
library(mapboxapi)

my_token <- 'pk.eyJ1Ijoibmhlcm5hbmRlejE5OTkiLCJhIjoiY2wzZGZjZDEwMDFyajNjbDVxMnJ2M2lwdSJ9.N9R9fzcgvK1ieQ_s5eVwQw'

#mb_access_token(my_token, install = TRUE, overwrite = TRUE)
#readRenviron("~/.Renviron")

Sys.setenv('MAPBOX_PUBLIC_TOKEN' = my_token)

Sys.getenv('MAPBOX_PUBLIC_TOKEN')

## plot all routes


fig <- rqGeo %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          text = rqGeo$parent_sector,
          hoverinfo = 'text') %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))

fig


## by type using ARQI_median as metric for 'route quality'

##Sport climbing: definition 

### sport 

metric <- "ARQI_median"
sport <- 'sport'

df_sport <- rqGeo %>%
  filter(type_string == sport) %>%
  group_by(sector_ID) %>%
  select(sector_ID, route_name, nopm_YDS, safety, metric)

name_sport <- rqGeo %>%
  select(parent_sector, sector_ID, lon, lat) %>%
  filter(!duplicated(sector_ID))

agg_sport <- inner_join(df_sport, name_sport) 

rqGeo_sport <- agg_sport %>%
  group_by(parent_sector) %>%
  mutate(n_routes = length(route_name)) %>%
  mutate(best_route_name = route_name[which.max(ARQI_median)])

rqGeo_sport




### trad 

trad <- 'trad'

df_trad <- rqGeo %>%
  filter(type_string == trad) %>%
  group_by(sector_ID) %>%
  select(sector_ID, route_name, nopm_YDS, safety, metric)

name_trad <- rqGeo %>%
  select(parent_sector, sector_ID, lon, lat) %>%
  filter(!duplicated(sector_ID))

agg_trad <- inner_join(df_trad, name_trad) 

rqGeo_trad <- agg_trad %>%
  group_by(parent_sector) %>%
  mutate(n_routes = length(route_name)) %>%
  mutate(best_route_name = route_name[which.max(ARQI_median)])

rqGeo_trad


## plot sports 


rqGeo_sport %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          text = rqGeo_sport$parent_sector,
          hoverinfo = 'text') %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))



## plot trad

##Trad climbing: definition 


rqGeo_trad %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          text = rqGeo_trad$parent_sector,
          hoverinfo = 'text') %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))



# Different measures to quantify route quality 

"""
According to https://openbeta.substack.com/p/estimating-route-quality-analysis?s=r (1): 
  
  - 0 star: A truly terrible route, extremely chossy, covered in plant life, a blight on humanity, etc.

- 1 star: Not particularly enjoyable, somewhat chossy, needs some cleaning, short, too near another line, etc.

- 3 star: Area classic, one the of best routes in the vicinity (note that this does not apply if all the routes in the vicinity are terrible, use your judgement). Excellent rock, clean, great movement.

- 4 Star: Classic, one of the best routes anywhere, pristine, beautiful, striking movement, various superlatives, guide-book-cover nonsense.
"""

head(route_quality) %>%
  select(10:16)

"""
Mountain Project uses mean_rating, but median_rating is usually a better metric for: 
  - ordinal data
- outliers
- skewed distributions """


all_ratings <- read_csv("all_ratings.csv")

head(all_ratings)


# what does the distribution look like for trad/sport across entire database?


all_ratings <- all_ratings %>%
  mutate(trad = ifelse(str_extract(type, "tr") == "tr", 1, 0)) %>%
  mutate(sport = ifelse(str_extract(type, "sp") == "sp", 1, 0)) %>%
  mutate(trad = ifelse(is.na(trad), 0, trad)) %>%
  mutate(sport = ifelse(is.na(sport), 0, sport))

all_ratings


## boxplot by type 


library(cowplot)

p1 <- all_ratings %>%
  filter(sport == 1) %>%
  ggplot(aes(x = sport, y = ratings)) +
  labs(title = "sport") + 
  geom_boxplot() +
  ylim(0,5)

p2 <- all_ratings %>%
  filter(trad == 1) %>%
  ggplot(aes(x = trad, y = ratings)) +
  labs(title = "trad") + 
  geom_boxplot() +
  ylim(0,5)

plot_grid(p1, p2)

## count 


p1 <- all_ratings %>%
  filter(sport == 1) %>%
  ggplot(aes(x = ratings)) +
  labs(title = "sport") + 
  geom_bar() 

p2 <- all_ratings %>%
  filter(trad == 1) %>%
  ggplot(aes(x = ratings)) +
  labs(title = "trad") + 
  geom_bar() +
  ylim(0,4.1e05)

plot_grid(p1, p2)

## mosaic for trad against sport & bouldering


trad_counts <- table(trad = all_ratings$trad, rating = all_ratings$ratings)
trad_counts



plot(trad_counts, xlab = "Not Trad/Trad", ylab = "Rating", main = "Non Trad versus Trad Ratings")


## mosaic for sport against trad & bouldering


sport_counts <- table(sport = all_ratings$sport, rating = all_ratings$ratings)
sport_counts



plot(sport_counts, xlab = "Not Sport/Sport", ylab = "Rating", main = "Non Sport versus Sport Ratings")

#It is clear that we're working with skewed distributions and we need better metrics to measure route quality that is robust against outliers. What is the best metric to use?


table(route_quality$type_string)



all_ratings %>%
  filter(sport == 1 | trad == 1) %>%
  group_by(ratings) %>%
  summarise(count = n())



all_ratings %>%
  group_by(ratings, trad, sport) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head()


## what is the most common grade for sport and trad routes only? 

"""using YDS (Yosemite Decimal System) for counts, it seems like average to lower grades seem to be the most popular routes in this dataset. 
"""

## sport 
route_quality %>%
  group_by(nopm_YDS) %>%
  filter(type_string == "sport") %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(nopm_YDS,(-count)), y = count)) + 
  geom_bar(stat = "identity") +
  labs(title = "sport") + 
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = 0.5))



## trad
route_quality %>%
  group_by(nopm_YDS) %>%
  filter(type_string == "trad") %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(nopm_YDS,(-count)), y = count)) + 
  geom_bar(stat = "identity") +
  labs(title = "trad") + 
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = 0.5)) 

## Deciphering "classic" from opinion 

"""RQI = S(1-1/N) where S is the average stars (or median) and N is the number of votes. As N -> infinity, (1-1/N) -> 1, and RQI -> S. 

According to (1), 
- Classic -> RQI >= 3.5
- Area Classic -> 2.5 <= RQI < 3.5 
- Good -> 1.5 <= RQI < 2.5 
- Bad -> 0.5 >= RQI < 1.5
- Bomb -> RQI < 0.5"""

## using RQI_median 

library(RColorBrewer)
route_quality <- route_quality %>% 
  mutate(RQI_class = case_when(
    RQI_median >= 3.5 ~ "classic",
    RQI_median >= 2.5 & RQI_median < 3.5 ~ "area classic",
    RQI_median >= 1.5 & RQI_median < 2.5 ~ "good",
    RQI_median >= 0.5 & RQI_median < 1.5 ~ "bad",
    RQI_median < 0.5 ~ "bomb"
  )) 

route_quality$RQI_class <- factor(route_quality$RQI_class, 
                                  levels = c("classic", "area classic", "good", "bad", "bomb"))

route_quality %>%
  select(RQI_class, RQI_median, num_votes) %>%
  ggplot(aes(num_votes, RQI_median, group = RQI_class)) +
  geom_point(aes(color = RQI_class), position = position_jitterdodge(), alpha = 0.3) +
  scale_color_brewer(palette = "Dark2") 

## vs median 
"""
- note that some routes with a lower RQI_median may have a higher median rating. The RQI takes the number of votes into consideration, allowing for a more accurate and fair route designation. 
- we don't want just *any* route falling into a classic """

route_quality %>%
  ggplot(aes(num_votes, median_rating, group = RQI_class)) +
  geom_point(aes(color = RQI_class), position = position_jitterdodge(), alpha = 0.3) +
  scale_color_brewer(palette = "Dark2") 

"According to (1), an issue w/ RQI is that harder routes get fewer ascents and therefore less votes, making it difficult for hard routes to make it into the "classic" class. 
- 8 votes are minimum for a route to be a classic
- To get an RQI of 3.5 --> 4(1 - 1/8) = 3.5
- It is rare for a 5.14a to get more than 8 votes 
- Easier routes will tend to have higher RQIs (arguments to both sides (i.e. classic is a popularity contest vs a hard route should be adjusted for its difficulty))

ARQI = S(1 - 1/Nw)
- Nw is the number of adjusted votes (weighted more for harder routes)
- weights calculated from the votes per route for each grade and type from the all_ratings dataset
- votes weighted relative to the maximum vpr for either sport or trad "


ARQI_sport <- all_ratings %>%
  filter(sport == 1 ) %>%
  filter(grade %in% c(levels(as.factor(route_quality$nopm_YDS)))) %>%
  group_by(route_id) %>%
  mutate(votes = n()) %>%
  ungroup() %>%
  group_by(grade) %>%
  mutate(sum_votes = sum(votes)) %>%
  mutate(total_routes = n()) %>%
  mutate(vpr = sum_votes/total_routes) %>%
  ungroup() %>%
  mutate(max_vpr = max(vpr)) %>%
  group_by(grade) %>%
  mutate(weight = max_vpr/vpr) %>%
  group_by(route_id) %>%
  mutate(Nw = votes*weight) %>%
  mutate(S = median(ratings)) %>%
  mutate(ARQI = S*(1-1/Nw)) %>%
  ungroup()

ARQI_sport


my_levels = c(str_sort(ARQI_sport$grade) %>% unique())

ARQI_sport$grade <- as_factor(ARQI_sport$grade)

levels(ARQI_sport$grade) = c(my_levels[1:2], my_levels[39:47], my_levels[3:38])

ARQI_sport$grade <-reorder(ARQI_sport$grade, -(ARQI_sport$vpr))

ARQI_sport %>%
  select(grade, vpr) %>%
  unique()


## plot 

colors <- c("Adjustment Factor" = "red", "Adjusted Votes per Route" = "blue")
ARQI_sport %>%
  select(grade, vpr, weight) %>%
  unique() %>%
  ggplot(aes(x= grade, y = vpr)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = 0.5)) + 
  geom_point(aes(x = grade, y = weight, color = "Adjustment Factor")) +
  geom_point(aes(x = grade, y = weight*vpr, color = "Adjusted Votes per Route")) +
  scale_color_manual(values = colors)


## RQI vs ARQI 
"""According to 1), 
- RQI is best used when determining whether a route is a classic or not 
- ARQI corrects for the bias of RQI towards easier routes, especially for harder than 12a
- Either RQI or ARQI can be used when ranking routes in 5.2-5.12a range 
- ARQI better for ranking routes harder than 5.12a """

df1 <- route_quality %>%
  group_by(nopm_YDS) %>%
  mutate(avg_RQI = mean(RQI_median)) %>%
  select(nopm_YDS, avg_RQI) %>%
  distinct()

df2 <- route_quality %>%
  group_by(nopm_YDS) %>%
  mutate(avg_ARQI = mean(ARQI_median)) %>%
  select(nopm_YDS, avg_ARQI) %>%
  distinct()

colors <- c("avg_RQI" = "red", "avg_ARQI" = "blue")

ggplot(data = df1, mapping = aes(x = nopm_YDS, y = avg_RQI, fill = "avg_RQI")) +
  geom_col(alpha = 0.3) +
  geom_col(data = df2, mapping = aes(x = nopm_YDS, y = avg_ARQI, fill = "avg_ARQI"), alpha = 0.3)+
  labs(y = "rating", x = "grade", fill = "Legend") + 
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = 0.5)) +
  scale_fill_manual(values = colors)



## Outline 
""" 1. Pick a difficulty range 
2. Pick a type 
3. Pick a location 
- Map to take you to that location
- Route quality maps of that location
- Provides recommendation """



route_quality %>%
  head()



all_routes <- inner_join(all_ratings, route_quality)
all_routes

# look at one state 


all_routes %>% 
  filter(state == "Nevada") %>%
  group_by(route_id, route_name) %>%
  mutate(count = n()) %>%
  select(route_id, route_name, count) %>%
  arrange(desc(count)) %>%
  distinct()


## CA Ratings Data 

For now, we will only observe routes in California. 


ca_ratings <- read_csv("~/Downloads/ca-ratings.csv")
ca_ratings


## Adjust type 

We will only look at sport and trad. 


ca_ratings <- ca_ratings %>%
  mutate(trad = ifelse(str_extract(type, "tr") == "tr", 1, 0)) %>%
  mutate(sport = ifelse(str_extract(type, "sp") == "sp", 1, 0)) %>%
  mutate(trad = ifelse(is.na(trad), 0, trad)) %>%
  mutate(sport = ifelse(is.na(sport), 0, sport)) 



## Basic EDA

""" 75% of climbers have given 14 ratings or less. """


ratings_sum <- ca_ratings %>%
  group_by(name) %>%
  count()

summary(ratings_sum$n)



## Collaborative filtering 

"""
Find like-minded climbers given their ratings or preferences they have given to other climbing routes.
"""
## Cosine distance to calculate similarity


library(lsa)

other_ratings <- c(3, 4, 2, 1)
your_ratings <- c(2, 3, 4, 3)

cosine(other_ratings, your_ratings)


""" We will use KNN algorithm to build a prediction model using Cosine Similarity calculation to rank climbs by similarity scores. 
""" 

## First for sport 


ca_sport <- ca_ratings %>%
  filter(sport == 1) %>%
  select(ratings, route_id, users)

ca_sport %>% 
  head()



routes_climbed <- ca_sport %>%
  group_by(users) %>%
  count()

summary(routes_climbed$n)



dim(ca_sport)




library(recommenderlab)
trows <- sample(nrow(ca_sport), 10000)
sport <- ca_sport[trows, ]




summary(ca_sport$ratings)



wide <- ca_sport %>%
  group_by(route_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = route_id, values_from = ratings) %>%
  select(-row)



wide <- as.matrix(wide[, -1])



library(reccomenderlab)
wide <- as(wide, "realRatingMatrix")


