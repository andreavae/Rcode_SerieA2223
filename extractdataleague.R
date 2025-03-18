install.packages("worldfootballR")
library(worldfootballR)

# to get the serie a results:
seriea_results <- understat_league_match_results(league = "Serie A", season_start_year = 2020)
ds=dplyr::glimpse(seriea_results)


