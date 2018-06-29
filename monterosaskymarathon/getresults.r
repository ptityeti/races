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
# manually set times and rankings for Jornet-Forsberg
# the missing country has messed up my extraction
monterosa.results[3, "cp1.time"] <- "1:56:33"
monterosa.results[3, "cp2.time"] <- "3:36:48"
monterosa.results[3, "cp3.time"] <- "4:13:25"
monterosa.results[3, "cp4.time"] <- "5:03:56"
# clean up ranks
monterosa.results <- monterosa.results %>% mutate(cp1.rank = as.numeric(gsub("([^0-9])",'', cp1.rank)))
monterosa.results <- monterosa.results %>% mutate(cp2.rank = as.numeric(gsub("([^0-9])",'', cp2.rank)))
monterosa.results <- monterosa.results %>% mutate(cp3.rank = as.numeric(gsub("([^0-9])",'', cp3.rank)))
monterosa.results <- monterosa.results %>% mutate(cp4.rank = as.numeric(gsub("([^0-9])",'', cp4.rank)))
# add numeric times (in hours)
monterosa.results <- monterosa.results %>%
  mutate(
    cp1.time.numeric = as.numeric(substr(cp1.time, 1,1)) + as.numeric(substr(cp1.time, 3,4)) / 60 + as.numeric(substr(cp1.time, 6,7)) / 3600,
    cp2.time.numeric = as.numeric(substr(cp2.time, 1,1)) + as.numeric(substr(cp2.time, 3,4)) / 60 + as.numeric(substr(cp2.time, 6,7)) / 3600,
    cp3.time.numeric = as.numeric(substr(cp3.time, 1,1)) + as.numeric(substr(cp3.time, 3,4)) / 60 + as.numeric(substr(cp3.time, 6,7)) / 3600,
    cp4.time.numeric = as.numeric(substr(cp4.time, 1,1)) + as.numeric(substr(cp4.time, 3,4)) / 60 + as.numeric(substr(cp4.time, 6,7)) / 3600
  )
# for teams that don't have an intermediate time this will give then NA in subsequent checkpoints
# doesn't matter to me because those teams probably didn't go the whole way.

# write out the work for easy reference
write.table(monterosa.results, file = 'results_monterosa_2018.txt', sep = '\t', quote = FALSE, row.names = FALSE)
