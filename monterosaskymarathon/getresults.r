# get the results from the Monte Rosa Skymarathon
# just read the lines, because document is malformed and is not read (enough) by htmlTreeParse

library(dplyr)
library(stringr)

get_results <- function()
{
  url <- "https://www.wedosport.net/vedi_classifica_dettaglio.cfm?idc="
  team.name <- c()
  team.country <- c()
  team.cp1.time <- c()
  team.cp2.time <- c()
  team.cp3.time <- c()
  team.cp4.time <- c()
  team.cp1.rank <- c()
  team.cp2.rank <- c()
  team.cp3.rank <- c()
  team.cp4.rank <- c()
  for(i in seq(986229, 986364))
  {
    page.content <- readLines(paste0(url, i), encoding = "UTF-8")
    team.name <- c(team.name, trimws(page.content[214]))
    team.country <- c(team.country, trimws(page.content[230]))
    team.cp1.time <- c(team.cp1.time, trimws(page.content[301]))
    team.cp2.time <- c(team.cp2.time, trimws(page.content[305]))
    team.cp3.time <- c(team.cp3.time, trimws(page.content[309]))
    team.cp4.time <- c(team.cp4.time, trimws(page.content[313]))
    team.cp1.rank <- c(team.cp1.rank, trimws(page.content[320]))
    team.cp2.rank <- c(team.cp2.rank, trimws(page.content[322]))
    team.cp3.rank <- c(team.cp3.rank, trimws(page.content[324]))
    team.cp4.rank <- c(team.cp4.rank, trimws(page.content[326]))
  }
  return(
    data.frame(
      name = team.name,
      country = team.country,
      cp1.time = team.cp1.time,
      cp2.time = team.cp2.time,
      cp3.time = team.cp3.time,
      cp4.time = team.cp4.time,
      cp1.rank = team.cp1.rank,
      cp2.rank = team.cp2.rank,
      cp3.rank = team.cp3.rank,
      cp4.rank = team.cp4.rank,
      stringsAsFactors = FALSE
    )
  )
}
monterosa.results <- get_results()
#last row is filled with NA
monterosa.results <- monterosa.results[1:135, ]
# clean up name
monterosa.results <- monterosa.results %>% mutate(name.team = substr(name, str_locate(name, "\\)") + 2, 65))
monterosa.results <- monterosa.results %>% mutate(name.runners = trimws(substr(name, 2, str_locate(name, "\\)") - 1)))
monterosa.results <-  monterosa.results %>% mutate(name.runner1 = substr(name.runners, 1, str_locate(name, " - ") - 1))
monterosa.results <- monterosa.results %>% mutate(name.runner2 = substr(name.runners, str_locate(name, " - ") + 2, 65))
# clean up country
monterosa.results <- monterosa.results %>% 
  mutate(country = gsub("<img src='../../images/bandiere_NEW/.{3}.gif' class=\"img-responsive\" title='.{3}' data-toggle='tooltip' ><b>", '', country)) %>% 
  mutate(country = gsub('</b>', '', country))
monterosa.results[3, "country"] <- "ESP" # country for Jornet-Forsberg is not set

