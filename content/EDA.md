---
date: "2016-05-05T21:48:51-07:00"
title: EDA
---

      The population of climbers is exploding and we need more and better access to data to make the sport more accessible.  I am using data provided from OpenBeta, a nonprofit built and run by climbers that enables “open access and innovative uses of climbing data” (1).  I plan to build a Shiny app that will include a map of sport and trad climbing routes on the West Coast ranked by route quality and a recommendation engine tailored to the user of any skill level.
      Following the sport of climbing’s Olympic debut in Tokyo 2021 and the success of films like *Free Solo* featuring Alex Honnold (2018) and *The Dawn Wall* featuring Tommy Caldwell (2018), the industry is seeing historic growth and opportunities for new, profitable markets. According to *Forbes*, Google searches that included the term “climbing” reached an all time high in the first week of August 2021; the same time frame that men’s and women’s combined events were held (2). Not only is the sport gaining a bigger audience, but it is also attracting regular people like you and me to take a crack at the crag. Following the pandemic, nearly 100 climbing gyms have opened in North America and profits of El Cap, one of America’s largest operators of climbing facilities, saw a 100% increase in online interactions (2). 
      One concern with the sport’s booming popularity is the barrier to entry and as a result, there has been a push to make the sport more accessible. In addition, there are only a few databases with outdoor climbing routes that are in access to the public like The Mountain Project or 8A. Without these platforms, climbers who are looking to hit their local crag or boulder may not be able to find routes or know about the quality of them if they do not already have community or word of mouth. While these websites have provided helpful tools to climbers of all experience and skill levels, they are still heavily lacking data and scrapings of these platforms have resulted in DMCA takedowns or lawsuits (3). At the bare minimum, we need better and easier access to climbing data so that data scientists like myself can work to advance the sport for others. As the sport grows so will the influx of data, and with any field that is expanding and rapidly changing, data science can add value to it by making better-informed decisions for multiple stakeholders, generating new insights about its players and audience, and increasing the overall experience for users.  
      OpenBeta is a non-profit built and run by climbers that enables “open access and innovative uses of climbing data” (1). Though they have also faced several challenges with their attempt to use onX’s data from the Mountain Project with copyright infringement and blocked repositories, according to *Outside Learn* (3). At the moment their data is public, and Github recently reversed the DMCA takedown thanks to legal efforts from the owner, Viet Nguyen, who is "empowering the community with open license climbing betas and source tools" (1). His goal for OpenBeta is to make climbing data more like an open source project, which in turn would help platforms like Mountain Project to increase their recommendation systems, geolocation data, and the accuracy of submissions (3). In addition to pushing for accessible data, the OpenBeta also posts articles that fit the needs of any climber in STEM: tutorials, current events, and project inspirations like recommendation systems and route quality maps. The community that OpenBeta is fostering aligns heavily with the forward mentality of climbing currently which is: don’t be a gatekeeper, spread the beta, and anyone is capable enough.  
      As a young climber and data scientist, I found myself incredibly inspired by OpenBeta’s work and wanted to support the nonprofit by using their data and some of their resources for my capstone project.  I plan to leverage climbing data to influence decision making for climbers of all skill sets and as a result, contribute to the overarching goal of OpenBeta which is to make the sport of climbing safer, more knowledgeable, and more accessible. Recommendation systems are extremely powerful and if done well, can be a great tool for young climbers when exploring outdoor routes. To get a better understanding of the data and to ensure the viability of this goal, I am performing an exploratory data analysis of the OpenBeta data. 
      
# Setup 
      Here I am pulling all ratings from the Open Beta which I will then take a subset of to getWest Coast ratings. I wanted to work with West Coast recommendations for a couple reasons. For one, the Sierra Nevada of California and the Cascade Range of the Pacific Northwest are prime western U.S. rock climbing locales. In addition the West Coast is scattered with popular climbing spots (i.e. those in Yosemite National Park) but there is a common misconception that these areas only have expert graded routes. In reality the opposite is true, and there are actually more beginner to moderate routes than expert ones. Therefore with an application like this one, anyone can have access to local classics even in lower grades. This dataset contains all route ratings in US along with route ID, grade, name, and type (trad, sport, ice, bouldering, etc.).
```{r setup, include=FALSE}
library(tidyverse)
all_ratings <- read_csv("all_ratings.csv")
all_ratings
```

## Fix type string, only select trad/sport

        I'm going to look at only trad and sport routes on the West Coast, which are the most two popular types of outdoor climbing. A limit of this method is that we lose routes that could be both sport and trad.
```{r}
all_ratings <- all_ratings %>%
  mutate(trad = ifelse(str_extract(type, "tr") == "tr", 1, 0)) %>%
  mutate(sport = ifelse(str_extract(type, "sp") == "sp", 1, 0)) %>%
  mutate(trad = ifelse(is.na(trad), 0, trad)) %>%
  mutate(sport = ifelse(is.na(sport), 0, sport)) %>%
  filter(trad != sport) %>%
  mutate(type = ifelse(trad == 1, "trad", "sport")) %>%
  select(-trad, -sport)

```

## Pull ratings/geolocation data 

       Next, I'm pulling some aggregate rating data from OpenBeta along with the location of parent walls to use for plotting. The features we will use from this dataset are the parent wall ID, name, and location along with the state and ARQI rating (we will explain this metric later).
```{r}
route_quality <-
  read_csv("route_quality_data.csv")

route_quality
```

## Format parent wall location data 
       Now I'm pulling the latitude and longitude from the parent_loc variable into two separate columns for plotly. 
```{r}
rqGeo <- route_quality %>%
   mutate(lon = as.numeric(str_extract(parent_loc, '(-|)\\d+.\\d+')),
         lat = as.numeric(str_extract(parent_loc, '\\s(-|)\\d+.\\d+')))  %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(route_ID = as.character(route_ID))
rqGeo
```
# EDA: West Coast Ratings & geoLocation
      As part of the EDA process, we first take a subset of our datasets to only include West Coast information. We used the features from route_quality dataset to join onto our ratings dataset: wc_ratings. 
```{r}
wcGeo <- rqGeo %>%
  filter(state == "Oregon" | state == "Washington" | state == "California")

wc_ratings <- all_ratings %>%
  mutate(route_id = as.character(route_id)) %>%
  inner_join(wcGeo, by = c("route_id" = "route_ID"))

wcGeo
wc_ratings
```

## Distribution of Trad/Sport across different states
      We can see that our dataset is dominated by trad routes and observations that come from California. This raises a bias concern with an Item Based Collaborative Filtering recommendation system. It may be worth keeping the California data regardless, as the data is the most complete and probably more accurate. 
```{r}
library(RColorBrewer)
library(ggthemes)
wc_ratings %>%
  ggplot(aes(x = state, fill = type)) +
  geom_histogram(stat = "count") +
  scale_fill_brewer(palette = "Set2") + 
  theme_tufte() + 
  labs(title = "Sport Route and Trad Route Counts by State\n", 
       x = "\nState\n", y = "\nCount\n") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10)) 
```

## Sort routes, take those only in YDS 
      We will only take routes with grades that are in the Yosemite Decimal System, which is the traditional difficulty rating for routes in the US. 
```{r}
my_levels = c(str_sort(wc_ratings$grade) %>% unique())

wc_ratings <- wc_ratings %>%
  filter(grade %in% c(my_levels[1:2], my_levels[51:63], my_levels[3:50]))

wc_ratings$grade <- as_factor(wc_ratings$grade)

levels(wc_ratings$grade) =  c(my_levels[1:2], my_levels[51:63], my_levels[3:50]) # easy to expert

levels(wc_ratings$grade)
```
## Proportion of Routes per Grade & Type 

      There are actually more beginner to moderate routes than expert ones! According to the Yosemite Decimal System, a 5.0 to 5.7 is considered easy, 5.8 to 5.10 is considered intermediate, 5.11 to 5.12 is hard, and 5.13 to 5.15 is reserved for a very elite few. This means that the app will be of use to any climber, as some local classics come even in lower grades.

```{r}
wc_ratings <- wc_ratings %>%
  mutate(level = case_when(
    grade %in% c(my_levels[1:2], my_levels[51:57]) ~ "easy",
    grade %in% c(my_levels[58:63], my_levels[3:12]) ~ "intermediate",
    grade %in% c(my_levels[13:32]) ~ "hard",
    grade %in% c(my_levels[33:50]) ~ "elite"))

wc_ratings$level <- factor(wc_ratings$level, 
                                  levels = c("easy", "intermediate", "hard", "elite"))
wc_ratings %>%
  group_by(grade, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(perc = count/sum(count)) %>%
  ggplot(aes(x = reorder(grade, -perc), y = perc)) +
  geom_col(aes(fill = level)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "\nGrade (YDS)", y = "Percent\n", title = "Proportion of Routes in the West Coast By Grade") + 
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, size = 5, vjust = 0.5)) 
```
## By state 
We see that across all three states, a majority of the top proportion of routes per grade are in the easy to intermediate range with a handful from Oregon and Washington being hard. 
```{r}
wc_ratings %>%
  group_by(state, grade, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(perc = count/sum(count)) %>%
  ungroup() %>%
  group_by(state) %>%
  slice_max(order_by = perc, n = 5) %>%
  ggplot(aes(x = reorder(grade, -perc), y = perc)) +
  geom_col(aes(fill = level)) +
  facet_wrap(~state, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "\nGrade (YDS)", y = "Percent\n", title = "Top Proportion of Routes per Grade by State") + 
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) 
```

## Breakdown of classic routes 

      As a metric for route quality, we can look at the aggregate metric RQI or ARQI precalculated in the data. The RQI is equal to S(1-1/N) where S is the average stars (or median) and N is the number of votes. As N approaches infinity, (1-1/N) approaches 1 and RQI approaches S. One issue with this metric is that harder routes get fewer ascents and therefore less votes, making it difficult for hard routes to make it into the "classic" class. We will use the Adjusted RQI (ARQI), which corrects for bias of RQI towards easier routes by adjusting the number of votes and therefore doesn't make route quality a "popularity metric." The ARQI is equal to S(1-1/Nw) where Nw is the number of weighted or adjusted votes and  is determined by the votes-per-route for each grade. 
       
According to OpenBeta, 
- Classic: ARQI >= 3.5
- Area Classic:  2.5 <= ARQI < 3.5 
- Good: 1.5 <= ARQI < 2.5 
- Bad: 0.5 >= ARQI < 1.5
- Bomb: ARQI < 0.5 

```{r}
wc_ratings <- wc_ratings %>% 
  distinct() %>%
  mutate(class = case_when(
    ARQI_median >= 3.5 ~ "classic",
    ARQI_median >= 2.5 & ARQI_median < 3.5 ~ "area classic",
    ARQI_median >= 1.5 & ARQI_median < 2.5 ~ "good",
    ARQI_median >= 0.5 & ARQI_median < 1.5 ~ "bad",
    ARQI_median < 0.5 ~ "bomb")) %>%
  group_by(parent_sector) %>%
  mutate(best_route = route_name[which.max(ARQI_median)]) %>%
  ungroup()
    
wc_ratings
```

## Class Distribution

Note that some routes with a lower ARQI may have a higher median rating. The ARQI takes the number of votes into consideration, allowing for a more accurate and fair route designation (we don't want just *any* route falling into a classic). 

```{r}
wc_ratings$class <- factor(wc_ratings$class, 
                                  levels = c("classic", "area classic", "good", "bad", "bomb"))
wc_ratings %>% 
  filter(num_votes < 400) %>% #filter outliers
  select(num_votes, median_rating, class) %>%
  distinct() %>%
  ggplot(aes(num_votes, median_rating, group = class)) +
  geom_point(aes(color = class), alpha = 0.4, position = position_jitterdodge(jitter.width = .9, jitter.height = 0.1)) +
  scale_color_brewer(palette = "YlOrRd") +
  theme_tufte() +
  labs(x = "\nNumber of Votes\n", y = "\nMedian Rating\n", title = "Median Ratings vs Number of Votes by ARQI Class\n")
```

## Class Distribution Across State
We see that there are a majority of routes in the Area Classic range (between 2.5 and 3.5) across all three states with outliers in the Bad class. But what about by grade? 
```{r}
wc_ratings %>%
  group_by(state) %>%
  ggplot(aes(x = state, y = ARQI_median, fill = state)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") + 
  theme_tufte() +
  labs(x = "\nState\n", y = "ARQI\n", title = "Distribution of ARQI Scores by State\n")
```

## Class Distribution Across Grade
      We find that a majority of easy and intermediate routes are both classic and area classics, supporting my claim that anyone can climb a classic not only at their local crag but additionally famous big walls climbed by the legends. 

```{r}
wc_ratings %>%
  group_by(level, class) %>%
  ggplot(aes(x = level)) +
  geom_bar(aes(fill = class), position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  theme_tufte() + 
  labs(x = "\nLevel\n", y = "Number of Routes\n", title = "Distribution of Route Classes by Grade Level")
  
```

## Plot routes

      I plan to combine a recommendation and route ranking system in a map format using geolocation and ratings data. Specifically, I will use Plotly for interactivity, Mapbox for geocoding, and Shiny for construction of the web application. Here I am accessing my public token from Mapbox in order to do some basic plotting and interactivity with Plotly. 

```{r}
library(mapboxapi)

my_token <- 'pk.eyJ1Ijoibmhlcm5hbmRlejE5OTkiLCJhIjoiY2wzZGZjZDEwMDFyajNjbDVxMnJ2M2lwdSJ9.N9R9fzcgvK1ieQ_s5eVwQw'

#mb_access_token(my_token, install = TRUE, overwrite = TRUE)
#readRenviron("~/.Renviron")

Sys.setenv('MAPBOX_PUBLIC_TOKEN' = my_token)

Sys.getenv('MAPBOX_PUBLIC_TOKEN')

```

## Plot all West Coast walls 
With some basic plotly commands, we can plot all the West Coast routes. Ideally the user will be able to filter grade range, type, location, and rating on the Shiny app. 
```{r}
library(plotly)

fig <- wc_ratings %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          color = wc_ratings$class,
          hoverinfo = 'text',
          text = paste("Parent wall: ", wc_ratings$parent_sector,
                       "<br>",
                       "Best Route: ", wc_ratings$best_route,
                       "<br>",
                       "Type: ", wc_ratings$type_string, 
                       "<br>", 
                       "Class: ", wc_ratings$class,
                       "<br>",
                       "ARQI: ", wc_ratings$ARQI_median,
                       "<br>",
                       "Grade: ", wc_ratings$grade)
                       ) %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))

fig
```
## Sport

Now we plot by type using ARQI_median as metric for 'route quality.' Here I'm doing some basic data wrangling to find the route with the best ARQI for parent walls with sport routes. 

```{r}
metric <- "ARQI_median"
sport <- 'sport'

df_sport <- wcGeo %>%
  filter(type_string == sport) %>%
  group_by(sector_ID) %>%
  select(sector_ID, route_name, nopm_YDS, safety, metric)

name_sport <- wcGeo %>%
  select(parent_sector, sector_ID, lon, lat) %>%
  filter(!duplicated(sector_ID))

agg_sport <- inner_join(df_sport, name_sport) 

wcGeo_sport <- agg_sport %>%
  group_by(parent_sector) %>%
  mutate(n_routes = length(route_name)) %>%
  mutate(best_route_name = route_name[which.max(ARQI_median)])

wcGeo_sport
```

## Plot 

After setting various filters on the app, a map colored and sized by route quality would be provided to help the climber find the best possible routes in their ideal range. 

```{r message = FALSE}
wcGeo_sport %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          text = wcGeo_sport$parent_sector,
          hoverinfo = 'text',
          color = ~ wcGeo_sport$ARQI_median,
          size = ~ wcGeo_sport$ARQI_median) %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))

```
## Trad
I did a similar process for trad routes.
```{r}
trad <- 'trad'

df_trad <- wcGeo %>%
  filter(type_string == trad) %>%
  group_by(sector_ID) %>%
  select(sector_ID, route_name, nopm_YDS, safety, metric)

name_trad <- wcGeo %>%
  select(parent_sector, sector_ID, lon, lat) %>%
  filter(!duplicated(sector_ID))

agg_trad <- inner_join(df_trad, name_trad) 

wcGeo_trad <- agg_trad %>%
  group_by(parent_sector) %>%
  mutate(n_routes = length(route_name)) %>%
  mutate(best_route_name = route_name[which.max(ARQI_median)])

wcGeo_trad

```

## Plot 

```{r}
wcGeo_trad %>%
  plot_ly(lat = ~lat, 
          lon = ~lon, 
          mode = 'markers',
          type = 'scattermapbox',
          text = wcGeo_trad$parent_sector,
          hoverinfo = 'text', 
          color = ~ wcGeo_trad$ARQI_median,
          size = ~ wcGeo_trad$ARQI_median) %>%
  layout(
    mapbox = list(
      style = 'open-street-map', # or 'light'
      zoom = 1.5,
      center = list(lon = -98, lat = 38)
    )
  ) %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))

```

# Recommendations

For the recommendation system, I will create an item based collaborative-filtering recommender which asks the question “for users who climbed route x, which routes did they also climb?” and can predict routes based on past preferences of other users (1). From what I've seen on famous route finders like theCrag or MP, these platforms do not implement recommendation systems for their routes (4, 5). They usually order the routes by popularity (average ratings or number of votes) but any data analyst using mean, median, or count as a metric for popularity should know to always consider outliers, skewed data, and relative proportions. In addition, I think having a simple recommendation system would be ideal for new climbers looking to find their first projects. I believe that a recommendation system combined with a map of route quality by the AQRI score also benefits the experienced climber too. For example, if they disagree with the location, rating or quality assessment of a certain route and as a result, a failed recommendation to the climber, the user can enter more data into the Mountain Project (where OpenBeta gets its data) which they believe is more accurate. When the data funneling into the model becomes more accurate, you get a better recommendation, a better user experience, increased retention, and so on.  

## Item based recommendation

My main reference for creating a simple item based reccomendation comes from (6). Here I am taking the complete cases of my entire ratings dataset. Since recommendation systems are so computationally heavy, I had to get rid of any observations with nulls to decrease the load. This is definitely a limitation to the accuracy of the recommendation. 

```{r}
wc_ratings <- wc_ratings[complete.cases(wc_ratings),]
```

## Find a route we care about 
We are going to choose a route to get a reccomendation for in the California area. We will choose the "most popular route" by the sum of votes. 
```{r}
wc_ratings %>%
  filter(state == "California") %>%
  group_by(route_id) %>%
  summarise(sum = sum(num_votes)) %>%
  arrange(desc(sum)) 
```
## Overview 

Snake Dike: California easy 5.4 classic sport route at the Southwest Face (Half Dome in Yosemite)

IBCF answers the question "climbers who climbed Snake Dike also climbed...?"
```{r}
wc_ratings %>% 
  filter(route_id == "105836362")  %>%
  select(route_id, route_name, grade, type, parent_sector, class) %>%
  slice(1)
```
## Create user-product matrix 

First we spread out our users, route ID, and ratings across a pivot table that we convert to a simple matrix, or the user-product matrix, for calculating similarity scores. 

```{r}
wc_wide <- wc_ratings %>%
  select(users, route_id, ratings) %>%
  distinct() %>%
  pivot_wider(names_from = route_id, values_from = ratings)

row.names(wc_wide) <- wc_wide$users
wc_wide$users <- NULL
```
```{r}
wc_mat <- (as.matrix(wc_wide))
wc_mat[1:3, 1:3]
```

## Calculate degree of sparsity 

99% of cells lack data... another limitation to this method. But we may be able to tackle this issue with cosine similarity. 

```{r}
sum(is.na(wc_mat))/(ncol(wc_mat) * nrow(wc_mat))
```

## Use cosine similarity to measure distance 

We use cosine similarity to measure distances versus the Euclidean distance because cosine looks at directional similarity rather than magnitudal differences. Here I am hoping to capture beyond the numbers and get the content that the numbers tell. For example, a route that gets a rating of 3.0 four times and 4.0 eight times will have a 100% similarity score to a route that has one 3.0 ratings and two 4.0 ratings. If we were computing euclidean distance, this would give us a similarity of only 13%. I'm hoping that this method can be viable in tackling the data sparsity issue. 

```{r}
library(lsa)

route_x <- c(4, 8)
route_y <- c(1, 2)

cosine(route_x, route_y) # cosine
1/(1 + sqrt((1-4)^2 + (2-8)^2)) # euclidean
```

## Compute across a matrix
We then can use a function to compute the simlarity for various routes in our matrix. 

```{r}
cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T))*sqrt(sum(B^2, na.rm = T)) 
  result = num/den

  return(result)
}
```

## Apply this function to obtain Product-Product matrix 

To prevent memory overload, we create a function to calculate the similarity only on the route we choose. 

```{r}
route_recommendation = function(route_id, rating_matrix = wc_mat, n_recommendations = 5){

  route_index = which(colnames(rating_matrix) == route_id)

  similarity = apply(rating_matrix, 2, FUN = function(y) 
                      cos_similarity(rating_matrix[,route_index], y))

  recommendations = tibble(ID = names(similarity), 
                               similarity = similarity) %>%
    filter(ID!= route_id) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 

  return(recommendations)

}
```

## Get reccomendations for some route 

Our function returns the top 5 similar routes to Snake Dike. 

```{r}
my_route <- "105836362" 
recommendations = route_recommendation(my_route)
recommendations
```

## Join with ratings data 

Next, we can join back to our original data to get information about the recommended routes. To build upon this method, I could implement machine learning frameworks like caret or tidymodels in R such as the K-Nearest Neighbors algorithm to compute the cosine similarity more accurately with cross validation and hyperparameter tuning. My biggest concern is that using this method will overload the Shiny app. 

```{r}
wc_sub <- wc_ratings %>%
  mutate(route_id = as.character(route_id))

rec_tbl <- recommendations %>%
  mutate(ID = as.character(ID)) %>%
  left_join(wc_sub, by = c("ID" = "route_id")) %>%
  select(ID, name, similarity, grade, type, state, sector_ID, parent_sector, lon, lat, grade, ARQI_median, class, level) %>%
  distinct() 

rec_tbl
```

# Conclusion 

Another limitation of my project is the restriction placed on the data I’m using. Following a legal battle with onX regarding a copyright infringement, which OpenBeta won (as noncommercial and educational factual data cannot be copyrighted), OpenBeta is working to release their data under a public domain, permissive license (1,3). For this reason, all of OpenBeta’s current datasets only include user ratings up to 2020, and it doesn’t seem like they will be able to update them by August. Therefore, there is a missed opportunity for generating better recommendations without input following the increase of climbers after Tokyo. In the future,  climbing data from the Mountain Project will be streamed directly into the model prior to rollouts and will allow for optimal and more accurate results or recommendations.  
      In summary, I plan to provide a recommendation engine and route quality mapping system to climbers to streamline the route searching process for climbers of varying skill sets. I truly believe that any stakeholder (like athletes, sponsors, spectators, media, businesses, brands, participants, etc.) could benefit from this project. Guiding the recent explosion of climbers properly could help make the sport extremely profitable and the climbing community greater and diverse. I hope to help dissolve barriers to entry by providing a tool to climbers that can be utilized as a spot to keep them satisfied, safe, and yearning for more. For the data community, I also want to promote the open source movement in software and data as I strongly believe that it is essential in encouraging innovation, attracting diverse talent, and broadening perspectives within tech. 

1. https://OpenBeta.io/
2. https://www.forbes.com/sites/michellebruton/2021/11/24/interest-in-climbing-and-gym-memberships-have-spiked-following-sports-tokyo-olympics-debut/?sh=3daaf24326a8
3. https://www.climbing.com/news/mountain-project-OpenBeta-and-the-fight-over-climbing-data-access/
4. https://www.thecrag.com/
5. https://www.mountainproject.com/
6. https://anderfernandez.com/en/blog/how-to-code-a-recommendation-system-in-r/
