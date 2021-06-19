proj 1
================
Lucy Yin
6/8/2021

-   [Install and load required
    packages](#install-and-load-required-packages)
-   [NHL records API](#nhl-records-api)
-   [NHL stats API](#nhl-stats-api)
-   [Wrapper Function](#wrapper-function)
-   [Grabbing data and analyzing](#grabbing-data-and-analyzing)
-   [Create new variables](#create-new-variables)
-   [Read data from 2 different API’s and
    combine](#read-data-from-2-different-apis-and-combine)
-   [Read data from 2 different API’s](#read-data-from-2-different-apis)
-   [Contingency Tables](#contingency-tables)
-   [Plots](#plots)

## Install and load required packages

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
library(haven)
library(qwraps2)
library(rmarkdown)
library(data.table)
library(kableExtra)
library(xml2)
```

## NHL records API

``` r
# map id, team names and most recent id
franchise.url <- GET("https://records.nhl.com/site/api/franchise")
franchise.txt <- content(franchise.url, "text", encoding = "UTF-8")
franchise.json <- fromJSON(franchise.txt, flatten=TRUE)
franchise.list <- franchise.json$data %>% as.data.frame()
franchise.tbl <- tibble(franchise.list$id, franchise.list$teamCommonName, franchise.list$fullName, franchise.list$mostRecentTeamId)
franchise.tbl
```

    ## # A tibble: 39 x 4
    ##    `franchise.list$… `franchise.list$tea… `franchise.list$f… `franchise.list$mo…
    ##                <int> <chr>                <chr>                            <int>
    ##  1                 1 Canadiens            Montréal Canadiens                   8
    ##  2                 2 Wanderers            Montreal Wanderers                  41
    ##  3                 3 Eagles               St. Louis Eagles                    45
    ##  4                 4 Tigers               Hamilton Tigers                     37
    ##  5                 5 Maple Leafs          Toronto Maple Lea…                  10
    ##  6                 6 Bruins               Boston Bruins                        6
    ##  7                 7 Maroons              Montreal Maroons                    43
    ##  8                 8 Americans            Brooklyn Americans                  51
    ##  9                 9 Quakers              Philadelphia Quak…                  39
    ## 10                10 Rangers              New York Rangers                     3
    ## # … with 29 more rows

``` r
name.id.list <- franchise.tbl$`franchise.list$teamCommonName`
id.most.recent.id.list <- franchise.tbl$`franchise.list$mostRecentTeamId`
```

``` r
get.records <- function(table.name, id=NULL, team.common.name=NULL, ...) {
  base.url <- "https://records.nhl.com/site/api"
  

  if (!is.null(table.name)) {  ## has a table name
    
    ## check if user input the team.common.name, if yes then convert it to the corresponding id
    if (!is.null(team.common.name)) {
      for (i in 1:length(name.id.list)) {
        if (team.common.name == name.id.list[i]) {
          id <- (i)
        }
      }
    }
    
    ## change user inputted id to most.recent.id to be used for the franchise-details table
    if (is.numeric(id)) {
      for (i in 1:length(id.most.recent.id.list)) {
        if (i == id) {
          most.recent.id <- id.most.recent.id.list[i]
        }
      }
    } 

    
    if (table.name %in% ("franchise") & (is.numeric(id))) {  ## table name is this one and given id
      return ("The table selected cannot be returned with the specified ID or team common name input.")
    }  
    
    else if (is.null(id)) {  ## has table name but no id
      full.url <- paste0(base.url, "/", table.name)
      
      nfl.records <- GET(full.url)
      nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
      return (nfl.records.json$data %>% as_tibble())
    }
    
    else if (table.name %in% c("franchise-team-totals",
                               "franchise-season-records",
                               "franchise-goalie-records",
                               "franchise-skater-records") & 
             (is.numeric(id))) {  ## table name is one of the 4 and given id
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=franchiseId=", id)
      nfl.records <- GET(full.url)
      nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
      return (nfl.records.json$data %>% as_tibble())
    }
    
    ## table name is this, id is one of the 8 (because these results don't return html content)
    else if (table.name %in% ("franchise-detail") & 
             (id %in% c(2:4, 7:9, 13, 39))) {
      nfl.records.details <- GET(paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id))
      nfl.records.details.txt <- content(nfl.records.details, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.details.json <- fromJSON(nfl.records.details.txt, flatten=TRUE)  ## convert to list  
      return (nfl.records.details.json$data %>% as_tibble())
    }
    
    ## table name is this, id is one of the 31 (these tibbles contain html content, further parse out html)
    else if (table.name %in% ("franchise-detail") & 
             (is.numeric(id) %in% c(1, 5 ,6, 10:12, 14:38))) {  ## table name is this one and given id which converts to most.recent.id
      nfl.records.details <- GET(paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id))
      nfl.records.details.txt <- content(nfl.records.details, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.details.json <- fromJSON(nfl.records.details.txt, flatten=TRUE)  ## convert to list
      fran.details <- nfl.records.details.json$data
      
      ## these columns contain html content, save as separate vectors
      txt.string1 <- fran.details$captainHistory
      txt.string2 <- fran.details$coachingHistory
      txt.string3 <- fran.details$generalManagerHistory
      txt.string4 <- fran.details$retiredNumbersSummary
      
      ## delete the original columns of html content, then add parsed html content back to table
      fran.details <- fran.details %>% select(-c(3,4,8,11))
      fran.details$captainHistory <- read_html(txt.string1) %>% xml_text()
      fran.details$coachingHistory <- read_html(txt.string2) %>% xml_text()
      fran.details$generalManagerHistory <- read_html(txt.string3) %>% xml_text()
      fran.details$retiredNumbersSummary <- read_html(txt.string4) %>% xml_text()
      return (fran.details)
    }
  }
  
  else {  ## if no table name
    return ("Must provide a table name to get results.")
  }
}
```

test different scenarios

``` r
#get.records("franchise-goalie-records")
#get.records("franchise-goalie-records", team.common.name = "Eagles")
#get.records("franchise-detail", team.common.name = "Hurricanes")
#get.records("franchise-detail", id=39)
#get.records("franchise-team-totals", 26)
#get.records("franchise-detail", id=23)
#get.records("franchise-detail")
```

## NHL stats API

``` r
get.stat2 <- function(modifier, id=NULL, season=NULL, id2=NULL, id3=NULL, id4=NULL, ...) {
  base.url3 <- "https://statsapi.web.nhl.com/api/v1/teams"
  
  ## change the id inputs to most.recent.id to be used in this stats api section
  if (is.numeric(id)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id) {
        most.recent.id <- id.most.recent.id.list[i]
      }
    }
  }
  
  if (is.numeric(id2)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id2) {
        most.recent.id2 <- id.most.recent.id.list[i]
      }
    }
  }  
  
  if (is.numeric(id3)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id3) {
        most.recent.id3 <- id.most.recent.id.list[i]
      }
    }
  }   

  if (is.numeric(id4)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id4) {
        most.recent.id4 <- id.most.recent.id.list[i]
      }
    }
  }      
  
  if (modifier %in% c("team.roster",
                      "person.names",
                      "team.schedule.next", 
                      "team.schedule.previous",  
                      "team.stats") & 
      is.null(id)) {  ## modifier is 1 of the 5 and no most recent id known (from converting from id)
    full.url2 <- paste0(base.url3, "?expand=", modifier)
  }

  else if (modifier %in% c("team.roster",
                           "person.names",
                           "team.schedule.next",
                           "team.schedule.previous",
                           "team.stats") & 
           is.numeric(most.recent.id)) {  ## modifier is 1 of the 5 and yes most recent id
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier)
  }
  
  else if (modifier %in% ("team.roster&season") & 
           is.null(id) & 
           is.numeric(season)) {  ## modifier is this and no most recent id and yes season
    full.url2 <- paste0(base.url3, "?expand=", modifier, "=", season)
  }
  
  else if (modifier %in% ("team.roster&season") & 
           is.numeric(most.recent.id) & 
           is.numeric(season)) {  ## modifier is this and yes most recent id and yes season
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier, "=", season)
  }  
  
  else if (modifier %in% ("team.roster&season") & is.null(season)) {  ## modifier is this and no season
    return ("Must provide a season input to get the team roster & season stats.")
  }    

  else if (modifier %in% ("teamId") & 
           is.numeric(c(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4))) {  ## modifier is this and multiple most recent team id
    full.url2 <- paste0(base.url3, "?", modifier, "=",
                        paste(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4, sep = ","))
  }
  
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.null(id)) {  ## modifier is this and no most recent id
    full.url2 <- paste0(base.url3, "?stats=", modifier)
  }
  
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.numeric(most.recent.id)) {  ## modifier is this and yes most recent id
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?stats=", modifier)
  }
  
  else {
    return ("Invalid input, please try again.")  
  }
      
  nfl.stats3 <- GET(full.url2)
  nfl.stats3.txt <- content(nfl.stats3, "text", encoding = "UTF-8")
  nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
  return (nfl.stats3.json$teams %>% unnest(everything()) %>% unnest(everything()) %>% as_tibble())

}
```

try out function

``` r
#get.stat2("team.stats", 26)
#get.stat2("team.roster&season", NULL, 20142015)
#get.stat2("person.names")
#get.stat2("teamId", most.recent.id = 12, most.recent.id2 = 21)
```

## Wrapper Function

``` r
get.nhl.data <- function (table.name=NULL, 
                          modifier=NULL, 
                          id=NULL, 
                          team.common.name=NULL, 
                          season=NULL, 
                          id2=NULL, id3=NULL, id4=NULL, ...) {

  # yes table name, yes modifier
  if (!is.null(table.name) & !is.null(modifier)) {
    stop ("Cannot use both table name and modifier to get back information.")
  }
  
  # yes table name
  else if (!is.null(table.name) & is.null(team.common.name) & is.null(id) & is.null(modifier)) {
    output <- get.records(table.name)
  }  
  
  # yes table name, yes id or team.common.name, no modifier
  else if (!is.null(table.name) & (is.numeric(id) | !is.null(team.common.name)) & is.null(modifier)) {
    output <- get.records(table.name, id, team.common.name)
  }
  
  # yes modifier
  else if (!is.null(modifier) & is.null(table.name) & is.null(id) & is.null(team.common.name)) {
    output <- get.stat2(modifier)
  }
  
  # yes modifier, yes id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id)) {
    output <- get.stat2(modifier, id)
  }
  
  # yes modifier, yes season
  else if (!is.null(modifier) & is.null(table.name) & !is.null(season)) {
    output <- get.stat2(modifier, season)
  }
  
  # yes modifier, yes multiple id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id) & is.numeric(id2)) {
    output <- get.stat2(modifier, id, id2, id3, id4)
  }
  
  else {
    return ("Invalid combination of inputs, please try again.")
  }
  
  return(output)
}
```

``` r
get.nhl.data(table.name = "franchise-detail")
```

    ## # A tibble: 39 x 13
    ##       id active captainHistory     coachingHistory     dateAwarded directoryUrl 
    ##    <int> <lgl>  <chr>              <chr>               <chr>       <chr>        
    ##  1     1 TRUE   "<ul class=\"stri… "<ul class=\"strip… 1917-11-26… https://www.…
    ##  2     2 FALSE   <NA>               <NA>               1917-11-26… <NA>         
    ##  3     3 FALSE   <NA>               <NA>               1917-11-26… <NA>         
    ##  4     4 FALSE   <NA>               <NA>               1917-11-26… <NA>         
    ##  5     5 TRUE   "<ul class=\"stri… "<ul class=\"strip… 1917-11-26… https://www.…
    ##  6     6 TRUE   "<ul class=\"stri… "<ul class=\"strip… 1924-11-01… https://www.…
    ##  7     7 FALSE   <NA>               <NA>               1924-11-01… <NA>         
    ##  8     8 FALSE   <NA>               <NA>               1925-09-22… <NA>         
    ##  9     9 FALSE   <NA>               <NA>               1925-11-07… <NA>         
    ## 10    10 TRUE   "<ul class=\"stri… "<ul class=\"strip… 1926-05-15… https://www.…
    ## # … with 29 more rows, and 7 more variables: firstSeasonId <int>,
    ## #   generalManagerHistory <chr>, heroImageUrl <chr>, mostRecentTeamId <int>,
    ## #   retiredNumbersSummary <chr>, teamAbbrev <chr>, teamFullName <chr>

``` r
#get.nhl.data(table.name = "franchise-team-totals")
#get.nhl.data(modifier = "person.names", id = 26, season = 20142015)
#get.nhl.data(modifier = "team.stats", id = 26)
```

## Grabbing data and analyzing

``` r
# get all information from "franchise-team-totals"
team.total.raw <- get.nhl.data(table.name = "franchise-team-totals") 

# filter to get only the 31 active teams
team.total <- team.total.raw %>% rename(abbreviation = triCode) %>% select(activeFranchise, gameTypeId, lastSeasonId, everything()) %>% filter(activeFranchise==1 & gameTypeId==2 & is.na(lastSeasonId))

# get division data for different teams from "team.stats"
division <- get.nhl.data(modifier = "team.stats") %>% select(franchiseId, name, abbreviation, teamName, venue.name, venue.city, division.id, division.name, conference.id, conference.name)

# find common columns between the 2 data frames
common.names.dv <- intersect(names(team.total), names(division))
```

``` r
# combine the 2 tables together using "franchiseId" as index
new.data <- left_join(team.total, division, by="franchiseId")
new.data
```

    ## # A tibble: 62 x 39
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId
    ##              <int>      <int>        <int> <int>         <int>       <int>
    ##  1               1          2           NA     1      19821983          23
    ##  2               1          2           NA     1      19821983          23
    ##  3               1          2           NA     3      19721973          22
    ##  4               1          2           NA     3      19721973          22
    ##  5               1          2           NA     5      19261927          10
    ##  6               1          2           NA     5      19261927          10
    ##  7               1          2           NA     8      19671968          16
    ##  8               1          2           NA     8      19671968          16
    ##  9               1          2           NA     9      19671968          17
    ## 10               1          2           NA     9      19671968          17
    ## # … with 52 more rows, and 33 more variables: gamesPlayed <int>,
    ## #   goalsAgainst <int>, goalsFor <int>, homeLosses <int>,
    ## #   homeOvertimeLosses <int>, homeTies <int>, homeWins <int>, losses <int>,
    ## #   overtimeLosses <int>, penaltyMinutes <int>, pointPctg <dbl>, points <int>,
    ## #   roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName.x <chr>, ties <int>, abbreviation.x <chr>, wins <int>, name <chr>,
    ## #   abbreviation.y <chr>, teamName.y <chr>, venue.name <chr>, venue.city <chr>,
    ## #   division.id <int>, division.name <chr>, conference.id <int>,
    ## #   conference.name <chr>

## Create new variables

``` r
# % of wins from all games played
new.data$perc.total.games.win <- (new.data$wins / new.data$gamesPlayed * 100) %>% round(2)

# % of losses from all games played
new.data$perc.total.games.loss <- (new.data$losses / new.data$gamesPlayed * 100) %>% round(2)

# % of wins from home games
new.data$perc.home.win <- (new.data$homeWins / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

# % of losses from home games
new.data$perc.home.loss <- (new.data$homeLosses / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

# % of wins from road games
new.data$perc.road.win <- (new.data$roadWins / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)

# % of losses from road games
new.data$perc.road.loss <- (new.data$roadLosses / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)

new.data
```

    ## # A tibble: 62 x 45
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId
    ##              <int>      <int>        <int> <int>         <int>       <int>
    ##  1               1          2           NA     1      19821983          23
    ##  2               1          2           NA     1      19821983          23
    ##  3               1          2           NA     3      19721973          22
    ##  4               1          2           NA     3      19721973          22
    ##  5               1          2           NA     5      19261927          10
    ##  6               1          2           NA     5      19261927          10
    ##  7               1          2           NA     8      19671968          16
    ##  8               1          2           NA     8      19671968          16
    ##  9               1          2           NA     9      19671968          17
    ## 10               1          2           NA     9      19671968          17
    ## # … with 52 more rows, and 39 more variables: gamesPlayed <int>,
    ## #   goalsAgainst <int>, goalsFor <int>, homeLosses <int>,
    ## #   homeOvertimeLosses <int>, homeTies <int>, homeWins <int>, losses <int>,
    ## #   overtimeLosses <int>, penaltyMinutes <int>, pointPctg <dbl>, points <int>,
    ## #   roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName.x <chr>, ties <int>, abbreviation.x <chr>, wins <int>, name <chr>,
    ## #   abbreviation.y <chr>, teamName.y <chr>, venue.name <chr>, venue.city <chr>,
    ## #   division.id <int>, division.name <chr>, conference.id <int>,
    ## #   conference.name <chr>, perc.total.games.win <dbl>,
    ## #   perc.total.games.loss <dbl>, perc.home.win <dbl>, perc.home.loss <dbl>,
    ## #   perc.road.win <dbl>, perc.road.loss <dbl>

``` r
#write_csv(new.data, "new.data.csv")
```

## Read data from 2 different API’s and combine

``` r
# read from goalie records endpoint for hurricanes
goalie.records <- get.nhl.data(table.name = "franchise-goalie-records", id = 26)
goalie.records
```

    ## # A tibble: 38 x 29
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   336 FALSE        Tom                26 Carolina Hur…          2          34
    ##  2   363 FALSE        Richard            26 Carolina Hur…          2           6
    ##  3   369 FALSE        Sean               26 Carolina Hur…          2         256
    ##  4   411 FALSE        Mark               26 Carolina Hur…          2           3
    ##  5   425 FALSE        John               26 Carolina Hur…          2         122
    ##  6   430 FALSE        Mario              26 Carolina Hur…          2          23
    ##  7   470 FALSE        Pat                26 Carolina Hur…          2           5
    ##  8   490 FALSE        Mike               26 Carolina Hur…          2         252
    ##  9   508 FALSE        Kirk               26 Carolina Hur…          2           8
    ## 10   525 FALSE        Greg               26 Carolina Hur…          2         219
    ## # … with 28 more rows, and 22 more variables: lastName <chr>, losses <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
# read from skater records endpoint for hurricanes
skater.records <- get.nhl.data(table.name = "franchise-skater-records", id = 26)
skater.records
```

    ## # A tibble: 487 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName     gameTypeId
    ##    <int> <lgl>          <int> <chr>           <int> <chr>                  <int>
    ##  1 17239 FALSE              0 Jim                26 Carolina Hurrica…          2
    ##  2 17418 FALSE              1 Mike               26 Carolina Hurrica…          2
    ##  3 17543 FALSE              0 Fred               26 Carolina Hurrica…          2
    ##  4 17703 FALSE              2 Jergus             26 Carolina Hurrica…          2
    ##  5 17728 FALSE              0 Reid               26 Carolina Hurrica…          2
    ##  6 18169 FALSE              0 Bob                26 Carolina Hurrica…          2
    ##  7 18233 FALSE              0 Charlie            26 Carolina Hurrica…          2
    ##  8 18288 FALSE              0 Greg               26 Carolina Hurrica…          2
    ##  9 18328 FALSE              1 Jeff               26 Carolina Hurrica…          2
    ## 10 18799 FALSE              1 Shane              26 Carolina Hurrica…          2
    ## # … with 477 more rows, and 24 more variables: gamesPlayed <int>, goals <int>,
    ## #   lastName <chr>, mostAssistsGameDates <chr>, mostAssistsOneGame <int>,
    ## #   mostAssistsOneSeason <int>, mostAssistsSeasonIds <chr>,
    ## #   mostGoalsGameDates <chr>, mostGoalsOneGame <int>, mostGoalsOneSeason <int>,
    ## #   mostGoalsSeasonIds <chr>, mostPenaltyMinutesOneSeason <int>,
    ## #   mostPenaltyMinutesSeasonIds <chr>, mostPointsGameDates <chr>,
    ## #   mostPointsOneGame <int>, mostPointsOneSeason <int>,
    ## #   mostPointsSeasonIds <chr>, penaltyMinutes <int>, playerId <int>,
    ## #   points <int>, positionCode <chr>, rookieGamesPlayed <int>,
    ## #   rookiePoints <int>, seasons <int>

``` r
# find common columns between the two data frames
common.names <- intersect(names(goalie.records), names(skater.records))

# only keep common columns in each data frame
goalie.records <- goalie.records %>% select(all_of(common.names))
skater.records <- skater.records %>% select(all_of(common.names))

# combine the 2 data frames together
hurricanes.players.record <- rbind(goalie.records, skater.records)
hurricanes.players.record
```

    ## # A tibble: 525 x 12
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   336 FALSE        Tom                26 Carolina Hur…          2          34
    ##  2   363 FALSE        Richard            26 Carolina Hur…          2           6
    ##  3   369 FALSE        Sean               26 Carolina Hur…          2         256
    ##  4   411 FALSE        Mark               26 Carolina Hur…          2           3
    ##  5   425 FALSE        John               26 Carolina Hur…          2         122
    ##  6   430 FALSE        Mario              26 Carolina Hur…          2          23
    ##  7   470 FALSE        Pat                26 Carolina Hur…          2           5
    ##  8   490 FALSE        Mike               26 Carolina Hur…          2         252
    ##  9   508 FALSE        Kirk               26 Carolina Hur…          2           8
    ## 10   525 FALSE        Greg               26 Carolina Hur…          2         219
    ## # … with 515 more rows, and 5 more variables: lastName <chr>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, seasons <int>

``` r
# read from goalie records endpoint for hurricanes
goalie.records2 <- get.nhl.data(table.name = "franchise-goalie-records")
goalie.records2
```

    ## # A tibble: 1,078 x 29
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   235 FALSE        Don                15 Dallas Stars           2         315
    ##  2   236 FALSE        Bob                28 Arizona Coyo…          2         281
    ##  3   237 FALSE        Tony               11 Chicago Blac…          2         873
    ##  4   238 FALSE        Grant              25 Edmonton Oil…          2         423
    ##  5   239 FALSE        Ron                16 Philadelphia…          2         489
    ##  6   240 FALSE        Curtis             18 St. Louis Bl…          2         280
    ##  7   241 FALSE        Olie               24 Washington C…          2         711
    ##  8   242 FALSE        Mike               18 St. Louis Bl…          2         347
    ##  9   243 FALSE        Kirk               20 Vancouver Ca…          2         516
    ## 10   244 FALSE        Gilles             13 Cleveland Ba…          2         250
    ## # … with 1,068 more rows, and 22 more variables: lastName <chr>, losses <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
# read from skater records endpoint for hurricanes
skater.records2 <- get.nhl.data(table.name = "franchise-skater-records")
skater.records2
```

    ## # A tibble: 17,209 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName     gameTypeId
    ##    <int> <lgl>          <int> <chr>           <int> <chr>                  <int>
    ##  1 16888 FALSE            417 George              5 Toronto Maple Le…          2
    ##  2 16889 FALSE              0 Billy               2 Montreal Wandere…          2
    ##  3 16890 FALSE            794 Johnny              6 Boston Bruins              2
    ##  4 16891 FALSE            712 Jean                1 Montréal Canadie…          2
    ##  5 16892 FALSE           1111 Ray                 6 Boston Bruins              2
    ##  6 16893 FALSE             33 Harold              9 Philadelphia Qua…          2
    ##  7 16894 FALSE             13 Herb                9 Philadelphia Qua…          2
    ##  8 16895 FALSE            852 Bobby              16 Philadelphia Fly…          2
    ##  9 16896 FALSE            142 Ken                23 New Jersey Devils          2
    ## 10 16897 FALSE              0 Gerry               2 Montreal Wandere…          2
    ## # … with 17,199 more rows, and 24 more variables: gamesPlayed <int>,
    ## #   goals <int>, lastName <chr>, mostAssistsGameDates <chr>,
    ## #   mostAssistsOneGame <int>, mostAssistsOneSeason <int>,
    ## #   mostAssistsSeasonIds <chr>, mostGoalsGameDates <chr>,
    ## #   mostGoalsOneGame <int>, mostGoalsOneSeason <int>, mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>,
    ## #   mostPointsGameDates <chr>, mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>, mostPointsSeasonIds <chr>, penaltyMinutes <int>,
    ## #   playerId <int>, points <int>, positionCode <chr>, rookieGamesPlayed <int>,
    ## #   rookiePoints <int>, seasons <int>

``` r
# find common columns between the two data frames
common.names <- intersect(names(goalie.records), names(skater.records))

# only keep common columns in each data frame
goalie.records2 <- goalie.records2 %>% select(all_of(common.names))
skater.records2 <- skater.records2 %>% select(all_of(common.names))

# combine the 2 data frames together
players.record.all <- rbind(goalie.records2, skater.records2)
players.record.all
```

    ## # A tibble: 18,287 x 12
    ##       id activePlayer firstName franchiseId franchiseName gameTypeId gamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>              <int>       <int>
    ##  1   235 FALSE        Don                15 Dallas Stars           2         315
    ##  2   236 FALSE        Bob                28 Arizona Coyo…          2         281
    ##  3   237 FALSE        Tony               11 Chicago Blac…          2         873
    ##  4   238 FALSE        Grant              25 Edmonton Oil…          2         423
    ##  5   239 FALSE        Ron                16 Philadelphia…          2         489
    ##  6   240 FALSE        Curtis             18 St. Louis Bl…          2         280
    ##  7   241 FALSE        Olie               24 Washington C…          2         711
    ##  8   242 FALSE        Mike               18 St. Louis Bl…          2         347
    ##  9   243 FALSE        Kirk               20 Vancouver Ca…          2         516
    ## 10   244 FALSE        Gilles             13 Cleveland Ba…          2         250
    ## # … with 18,277 more rows, and 5 more variables: lastName <chr>,
    ## #   playerId <int>, positionCode <chr>, rookieGamesPlayed <int>, seasons <int>

``` r
#write_csv(players.record.all, "players.record.all.csv")
```

## Read data from 2 different API’s

``` r
franchise.data <- get.nhl.data(table.name = "franchise")
franchise.data <- franchise.data %>% select(id, mostRecentTeamId, teamCommonName, teamAbbrev, teamPlaceName)
franchise.data
```

    ## # A tibble: 39 x 5
    ##       id mostRecentTeamId teamCommonName teamAbbrev teamPlaceName
    ##    <int>            <int> <chr>          <chr>      <chr>        
    ##  1     1                8 Canadiens      MTL        Montréal     
    ##  2     2               41 Wanderers      MWN        Montreal     
    ##  3     3               45 Eagles         SLE        St. Louis    
    ##  4     4               37 Tigers         HAM        Hamilton     
    ##  5     5               10 Maple Leafs    TOR        Toronto      
    ##  6     6                6 Bruins         BOS        Boston       
    ##  7     7               43 Maroons        MMR        Montreal     
    ##  8     8               51 Americans      BRK        Brooklyn     
    ##  9     9               39 Quakers        QUA        Philadelphia 
    ## 10    10                3 Rangers        NYR        New York     
    ## # … with 29 more rows

``` r
goalie.data.hurricanes <- get.nhl.data(table.name = "franchise-goalie-records", id=26) %>% select(activePlayer, firstName, lastName, gamesPlayed, wins, ties, losses, seasons, shutouts, mostShotsAgainstOneGame, mostGoalsAgainstOneGame, mostSavesOneGame, mostShutoutsOneSeason, mostWinsOneSeason)
```

## Contingency Tables

``` r
table(players.record.all$franchiseName)
```

    ## 
    ##         Anaheim Ducks       Arizona Coyotes         Boston Bruins 
    ##                   402                   590                   969 
    ##    Brooklyn Americans        Buffalo Sabres        Calgary Flames 
    ##                   152                   508                   602 
    ##   Carolina Hurricanes    Chicago Blackhawks      Cleveland Barons 
    ##                   525                   928                   145 
    ##    Colorado Avalanche Columbus Blue Jackets          Dallas Stars 
    ##                   540                   289                   656 
    ##     Detroit Red Wings       Edmonton Oilers      Florida Panthers 
    ##                   912                   559                   410 
    ##       Hamilton Tigers     Los Angeles Kings        Minnesota Wild 
    ##                    39                   651                   268 
    ##    Montréal Canadiens      Montreal Maroons    Montreal Wanderers 
    ##                   838                    84                    12 
    ##   Nashville Predators     New Jersey Devils    New York Islanders 
    ##                   298                   559                   544 
    ##      New York Rangers       Ottawa Senators   Philadelphia Flyers 
    ##                  1038                   393                   617 
    ##  Philadelphia Quakers   Pittsburgh Penguins       San Jose Sharks 
    ##                    42                   711                   354 
    ##       St. Louis Blues      St. Louis Eagles   Tampa Bay Lightning 
    ##                   653                    94                   408 
    ##   Toronto Maple Leafs     Vancouver Canucks  Vegas Golden Knights 
    ##                   971                   615                    61 
    ##   Washington Capitals         Winnipeg Jets 
    ##                   552                   298

``` r
table(players.record.all$activePlayer)
```

    ## 
    ## FALSE  TRUE 
    ## 16171  2116

``` r
# find the count of active/inactive players by franchise
contingency.table.players <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>% kable(caption = "Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by Franchise")
contingency.table.players
```

<table>
<caption>
Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by
Franchise
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Anaheim Ducks
</td>
<td style="text-align:right;">
323
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
520
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
894
</td>
<td style="text-align:right;">
75
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
152
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Buffalo Sabres
</td>
<td style="text-align:right;">
425
</td>
<td style="text-align:right;">
83
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
538
</td>
<td style="text-align:right;">
64
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
459
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
848
</td>
<td style="text-align:right;">
80
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Avalanche
</td>
<td style="text-align:right;">
463
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
215
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
594
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
854
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
496
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
75
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Los Angeles Kings
</td>
<td style="text-align:right;">
586
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
208
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
768
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Wanderers
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville Predators
</td>
<td style="text-align:right;">
230
</td>
<td style="text-align:right;">
68
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
492
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
486
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
966
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
309
</td>
<td style="text-align:right;">
84
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
556
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Penguins
</td>
<td style="text-align:right;">
628
</td>
<td style="text-align:right;">
83
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
292
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
595
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
351
</td>
<td style="text-align:right;">
57
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
892
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
543
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
53
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
490
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
239
</td>
<td style="text-align:right;">
59
</td>
</tr>
</tbody>
</table>

``` r
# find the proportion by margin=1 (rows)
contingency.table.players.prop <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>% prop.table(margin=2)*100
contingency.table.players.prop %>% kable(caption = "Proportion of Active (TRUE) / Inactive (FALSE) Players by Franchise") 
```

<table>
<caption>
Proportion of Active (TRUE) / Inactive (FALSE) Players by Franchise
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Anaheim Ducks
</td>
<td style="text-align:right;">
1.9974028
</td>
<td style="text-align:right;">
3.733459
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
3.2156329
</td>
<td style="text-align:right;">
3.308129
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
5.5284151
</td>
<td style="text-align:right;">
3.544423
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
0.9399542
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Buffalo Sabres
</td>
<td style="text-align:right;">
2.6281615
</td>
<td style="text-align:right;">
3.922495
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
3.3269433
</td>
<td style="text-align:right;">
3.024575
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
2.8384144
</td>
<td style="text-align:right;">
3.119093
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
5.2439552
</td>
<td style="text-align:right;">
3.780718
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
0.8966669
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Avalanche
</td>
<td style="text-align:right;">
2.8631501
</td>
<td style="text-align:right;">
3.638941
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
1.3295405
</td>
<td style="text-align:right;">
3.497164
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
3.6732422
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
5.2810587
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
3.0672191
</td>
<td style="text-align:right;">
2.977316
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
2.0716097
</td>
<td style="text-align:right;">
3.544423
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
0.2411725
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Los Angeles Kings
</td>
<td style="text-align:right;">
3.6237709
</td>
<td style="text-align:right;">
3.071834
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
1.2862532
</td>
<td style="text-align:right;">
2.835539
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
4.7492425
</td>
<td style="text-align:right;">
3.308129
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
0.5194484
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Wanderers
</td>
<td style="text-align:right;">
0.0742069
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville Predators
</td>
<td style="text-align:right;">
1.4222992
</td>
<td style="text-align:right;">
3.213611
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
3.0424835
</td>
<td style="text-align:right;">
3.166352
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
3.0053800
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
5.9736565
</td>
<td style="text-align:right;">
3.402646
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
1.9108280
</td>
<td style="text-align:right;">
3.969754
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
3.4382537
</td>
<td style="text-align:right;">
2.882798
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
0.2597242
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Penguins
</td>
<td style="text-align:right;">
3.8834951
</td>
<td style="text-align:right;">
3.922495
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
1.8057016
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
3.6794261
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
0.5812875
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
2.1705522
</td>
<td style="text-align:right;">
2.693762
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
5.5160472
</td>
<td style="text-align:right;">
3.733459
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
3.3578628
</td>
<td style="text-align:right;">
3.402646
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
0.0494713
</td>
<td style="text-align:right;">
2.504726
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
3.0301156
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
1.4779544
</td>
<td style="text-align:right;">
2.788280
</td>
</tr>
</tbody>
</table>

``` r
table(new.data$division.name)
```

    ## 
    ## Discover Central       Honda West  MassMutual East     Scotia North 
    ##               16               16               16               14

``` r
table(new.data$wins)
```

    ## 
    ##  173  214  382  678  759  827  852  889  971  985  990 1007 1070 1084 1394 1469 
    ##    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2 
    ## 1497 1649 1688 1700 1754 1805 1903 1929 2079 2812 2873 2883 2891 3241 3473 
    ##    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2

``` r
# find the count of franchises w/i a division that had over 2000 wins
contingency.table2 <- table(new.data$division.name, new.data$wins>2000) %>% kable(caption = "Contingency Table of Number of Franchises in a Division that had Over 200 Wins"); contingency.table2
```

<table>
<caption>
Contingency Table of Number of Franchises in a Division that had Over
200 Wins
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Discover Central
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
4
</td>
</tr>
</tbody>
</table>

``` r
# find the proportion by margin=2 (columns)
contingency.table2.prop <- table(new.data$division.name, new.data$wins>2000) %>% prop.table(margin=2)*100
contingency.table2.prop %>% kable(caption = "Proportion of Over 200 Win Teams By Division")
```

<table>
<caption>
Proportion of Over 200 Win Teams By Division
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Discover Central
</td>
<td style="text-align:right;">
25.00000
</td>
<td style="text-align:right;">
28.57143
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
33.33333
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
20.83333
</td>
<td style="text-align:right;">
42.85714
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
20.83333
</td>
<td style="text-align:right;">
28.57143
</td>
</tr>
</tbody>
</table>

## Plots

### Bar plot

``` r
#new.data.no.na <- new.data %>% drop_na(division.name)
ggplot(data = new.data, aes(x = division.name)) + 
  geom_bar(aes(fill = as.factor(conference.name)), position = "dodge") + 
  labs(x = "Division", title = "Bar Plot of Number of Franchise in Each Division") + 
  scale_fill_discrete(name = "Conference Name", labels = c("Eastern Conference", "Western Conference"))
```

![](Project1_files/figure-gfm/plot%20-%20bar-1.png)<!-- -->

### Histogram

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  labs(x = "Games Played", title = "Number of Games Played by all Franchise's Players")
```

![](Project1_files/figure-gfm/plot%20-%20histogram-1.png)<!-- -->

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  facet_grid(~ positionCode, 
             labeller = as_labeller(c(C = "Center", D = "Defense", G = "Goalie", L = "Left Wing", R = "Right Wing"))) +
  labs(x = "Games Played", title = "Number of Games Played by all Franchise's Players by Position Played")
```

![](Project1_files/figure-gfm/plot%20-%20histogram%20facet-1.png)<!-- -->

### Boxplot

``` r
ggplot(data = new.data, aes(x = division.name, y = gamesPlayed)) +
  geom_boxplot(fill = "maroon") +
  stat_summary(fun = mean, geom = "line", aes(group = conference.name, col = conference.name)) +
  labs(x = "Division Name", y = "# of Games Played", title = "Number of Games Played for Each Division")
```

![](Project1_files/figure-gfm/plot%20-%20box%20plot-1.png)<!-- -->

### Scatter Plot

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Each Franchise")
```

![](Project1_files/figure-gfm/plot%20-%20scatter-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  facet_grid(~ conference.name, labeller = as_labeller(c(Eastern = "Eastern Conference", 
                                      Western = "Western Conference"))) +
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Franchies in Each Division")
```

![](Project1_files/figure-gfm/plot%20-%20scatter%20facet-1.png)<!-- -->

### Scatter Plot using Group

``` r
ggplot(data = new.data, aes(x = homeWins, y = homeLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "Home Games Won", y = "Home Games Lost", title = "Number of Home Games Won vs. Lost for Each Franchise by Conference")
```

![](Project1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20home%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = roadWins, y = roadLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "Road Games Won", y = "Road Games Lost", title = "Number of Road Games Won vs. Lost for Each Franchise by Conference")
```

![](Project1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20road%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.win, y = perc.road.win)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Won", y = "% Road Games Won", title = "% of Home Games Won vs. Road Games Won for Each Franchise by Conference")
```

![](Project1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20won%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.loss, y = perc.road.loss)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Loss", y = "% Road Games Loss", title = "% of Home Games Loss vs. Road Games Loss for Each Franchise by Conference")
```

![](Project1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20lost%20games-1.png)<!-- -->
