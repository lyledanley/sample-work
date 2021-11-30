library(tidyverse)
library(readr)
library(formattable)
library(Rcurl)

x <- getURL("https://raw.githubusercontent.com/lyledanley/sample-work/main/datasets/shots_data.csv")
shots <- read.csv(text = x)

#deriving shot_type based on dimensions given and plotting points on a circle
shots <- shots |>
  mutate(shot_type = 
           ifelse(abs(x) < 22 & y < 7.8, "2PT", 
                  ifelse(abs(x) > 22 & y <= 7.8, "C3",
                         ifelse(x < 22 & y > 7.8 & sqrt(x^2+y^2) >= 23.75, 
                                "NC3", "2PT")
                         )
                  )
         )
#deriving shot percentages based on made/attempted for each shot type

shots |>
  group_by(team, shot_type) |>
  summarise(attempted = n()) |>
  ungroup()|>
  group_by(team) |>
  mutate(distribution = round(attempted/sum(attempted),3)) |>
  select(team, shot_type, distribution) -> shot_distribution


#deriving eFG based on formula given. Since 2PT shots have no weight to the (0.5*3PM), using a conditional formula based on shot type.
eFG_distribution <- shots |>
  group_by(team, shot_type, fgmade) |>
  summarise(made = n()) |>
  mutate(total = sum(made)) |>
  filter(fgmade == 1) |> 
  mutate(eFG = case_when(shot_type == "2PT" ~ made/total, 
                      TRUE ~ (made + 0.5*made)/total)) |>
  mutate(eFG = round(eFG, 3)) |>
  select(team, shot_type, eFG)

#joining the two derived columns together for one output df
answer_output <- shot_distribution |> left_join(eFG_distribution, by = c("team", "shot_type"))

formattable(answer_output)
