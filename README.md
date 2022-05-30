# Route Quality Map and Recommendation System in RShiny using OpenBeta Climbing Data
## By: Nina Hernandez

      The population of climbers is exploding and we need more and better access to data to make the sport more accessible.  I am using data provided from OpenBeta, a nonprofit built and run by climbers that enables “open access and innovative uses of climbing data” (1).  I plan to build a Shiny app that will include a map of sport and trad climbing routes in the US ranked by route quality and a recommendation engine tailored to the user of any skill level.  
      Following the sport of climbing’s Olympic debut in Tokyo 2021 and the success of films like *Free Solo* featuring Alex Honnold (2018) and *The Dawn Wall* featuring Tommy Caldwell (2018), the industry is seeing historic growth and opportunities for new, profitable markets. According to *Forbes*, Google searches that included the term “climbing” reached an all time high in the first week of August 2021; the same time frame that men’s and women’s combined events were held (2). Not only is the sport gaining a bigger audience, but it is also attracting regular people like you and me to take a crack at the crag. Following the pandemic, nearly 100 climbing gyms have opened in North America and profits of El Cap, one of America’s largest operators of climbing facilities, saw a 100% increase in online interactions (2).  
      One concern with the sport’s booming popularity is the barrier to entry and as a result, there has been a push to make the sport more accessible. In addition, there are only a few databases with outdoor climbing routes that are in access to the public like The Mountain Project or 8A. Without these platforms, climbers who are looking to hit their local crag or boulder may not be able to find routes or know about the quality of them if they do not already have community or word of mouth. While these websites have provided helpful tools to climbers of all experience and skill levels, they are still heavily lacking data and scrapings of these platforms have resulted in DMCA takedowns or lawsuits (3). At the bare minimum, we need better and easier access to climbing data so that data scientists like myself can work to advance the sport for others. As the sport grows so will the influx of data, and with any field that is expanding and rapidly changing, data science can add value to it by making better-informed decisions for multiple stakeholders, generating new insights about its players and audience, and increasing the overall experience for users.  
      OpenBeta is a non-profit built and run by climbers that enables “open access and innovative uses of climbing data” (1). Though they have also faced several challenges with their attempt to use onX’s data from the Mountain Project with copyright infringement and blocked repositories, according to *Outside Learn* (3). At the moment however, their data is public and the owner Viet Nguyen is making great strides in the climbing and software community. His goal for OpenBeta is to make climbing data more like an open source project, which in turn would help platforms like Mountain Project to increase their recommendation systems, geolocation data, and the accuracy of submissions (3). In addition to pushing for accessible data, the OpenBeta also posts articles that fit the needs of any climber in STEM: tutorials, current events, and project inspirations like recommendation systems and route quality maps. The community that OpenBeta is fostering aligns heavily with the forward mentality of climbing currently which is: don’t be a gatekeeper, spread the beta, and anyone is capable enough.  
      As a young climber and data scientist, I found myself incredibly inspired by OpenBeta’s work and wanted to support the nonprofit by using their data and some of their resources for my capstone project.  I plan to leverage climbing data to influence decision making for climbers of all skill sets and as a result, contribute to the overarching goal of OpenBeta which is to make the sport of climbing safer, more knowledgeable, and more accessible. Recommendation systems are extremely powerful and if done well, can be a great tool for young climbers when exploring outdoor routes.  
      I plan to combine a recommendation and route ranking system in a map format using geolocation and ratings from OpenBeta using R. Specifically, I will use Plotly for interactivity, Mapbox for geocoding, and Shiny for construction of the web application. For the recommendation system, I will create a collaborative-filtering recommender using machine learning frameworks in R (caret or tidymodels) which asks the question “for users who climbed route x, which routes did they also climb?” and can predict choices based on past behaviors or preferences of other users (1). While the concept of this project isn’t totally inventive, the current work that I’ve seen could benefit from improvements like formatting and simplicity for new users, like theCrag or MP (4, 5). I think having a simple recommendation system tailored to the user for specific routes with varying quality scores would be ideal for new climbers looking to find their first projects. Don’t get me wrong: it is good to be thorough and these platforms have undoubtedly revolutionized the sport of climbing. But I think that a new climber could benefit from having access to both. I believe that a recommendation system combined with a map also benefits the experienced climber too. For example, if they disagree with a location, rating or quality assessment of a certain route and as a result, a failed recommendation to the climber, the user can enter more data into the Mountain Project (where OpenBeta gets its data) which they believe is more accurate. When the data funneling into the model becomes more accurate, you get a better recommendation, a better user experience, increased retention, and so on.  
      One limitation of my project is the restriction placed on the data I’m using: OpenBeta is currently in a legal battle with onX to make their data completely free and accessible (3). For this reason, all of *OpenBeta’s* current datasets only include user ratings up to 2020 (and it doesn’t seem like they will be able to update them any time soon). Therefore, there is a missed opportunity for generating better recommendations without input following the increase of climbers after Tokyo. In an ideal world, climbing data from the Mountain Project could be streamed directly into the model prior to rollouts and would allow for optimal and more accurate results or recommendations.  
      In summary, I plan to provide a recommendation engine and route quality mapping system to climbers to streamline the route searching process for climbers of varying skill sets. I truly believe that any stakeholder (like athletes, sponsors, spectators, media, businesses, brands, participants, etc.) could benefit from this project. Guiding the recent explosion of climbers properly could help make the sport extremely profitable and the climbing community greater and diverse. I hope to help dissolve barriers to entry by providing a tool to climbers that can be utilized as a spot to keep them satisfied, safe, and yearning for more. For the data community, I also want to promote the open source movement in software and data as I strongly believe that it is essential in encouraging innovation, attracting diverse talent, and broadening perspectives within tech. 


1. https://OpenBeta.io/
2. https://www.forbes.com/sites/michellebruton/2021/11/24/interest-in-climbing-and-gym-memberships-have-spiked-following-sports-tokyo-olympics-debut/?sh=3daaf24326a8
3. https://www.climbing.com/news/mountain-project-OpenBeta-and-the-fight-over-climbing-data-access/
4. https://www.thecrag.com/
5. https://www.mountainproject.com/
