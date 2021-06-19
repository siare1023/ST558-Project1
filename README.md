proj 1
================
Lucy Yin
6/8/2021

## Install and load required packages

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
library(haven)
#install.packages("qwraps2")
library(qwraps2)
library(rmarkdown)
library(data.table)
library(kableExtra)
```

## NHL records API

``` r
# map id and team names
franchise.url <- GET("https://records.nhl.com/site/api/franchise")
franchise.txt <- content(franchise.url, "text", encoding = "UTF-8")
franchise.json <- fromJSON(franchise.txt, flatten=TRUE)
franchise.list <- franchise.json$data %>% as.data.frame()
franchise.tbl <- tibble(franchise.list$id, franchise.list$teamCommonName, franchise.list$fullName, franchise.list$mostRecentTeamId)
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
    }
    
    else if (table.name %in% c("franchise-team-totals",
                               "franchise-season-records",
                               "franchise-goalie-records",
                               "franchise-skater-records") & 
             (is.numeric(id))) {  ## table name is one of the 4 and given id
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=franchiseId=", id)
    }

    else if (table.name %in% ("franchise-detail") & 
             (is.numeric(id))) {  ## table name is this one and given id which converts to most.recent.id
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id)
    }
    
  
    nfl.records <- GET(full.url)
    nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
    nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
    
    return (nfl.records.json$data)
    
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
#get.records("franchise-detail", team.common.name = "Eagles")
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
    
  return (nfl.stats3.json$teams %>% unnest(everything()) %>% unnest(everything()))

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

head(new.data)
```

    ##   activeFranchise gameTypeId lastSeasonId id firstSeasonId franchiseId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses
    ## 1               1          2           NA  1      19821983          23        2993         8902     8792        525                 85
    ## 2               1          2           NA  1      19821983          23        2993         8902     8792        525                 85
    ## 3               1          2           NA  3      19721973          22        3788        11907    12045        678                 84
    ## 4               1          2           NA  3      19721973          22        3788        11907    12045        678                 84
    ## 5               1          2           NA  5      19261927          10        6560        20020    20041       1143                 76
    ## 6               1          2           NA  5      19261927          10        6560        20020    20041       1143                 76
    ##   homeTies homeWins losses overtimeLosses penaltyMinutes pointPctg points roadLosses roadOvertimeLosses roadTies roadWins
    ## 1       96      790   1211            169          44773    0.5306   3176        686                 84      123      604
    ## 2       96      790   1211            169          44773    0.5306   3176        686                 84      123      604
    ## 3      170      963   1587            166          57792    0.5133   3889        909                 82      177      725
    ## 4      170      963   1587            166          57792    0.5133   3889        909                 82      177      725
    ## 5      448     1614   2716            153          86129    0.5127   6727       1573                 77      360     1269
    ## 6      448     1614   2716            153          86129    0.5127   6727       1573                 77      360     1269
    ##   shootoutLosses shootoutWins shutouts teamId         teamName.x ties abbreviation.x wins               name abbreviation.y teamName.y
    ## 1             84           78      196      1  New Jersey Devils  219            NJD 1394  New Jersey Devils            NJD     Devils
    ## 2             84           78      196      1  New Jersey Devils  219            NJD 1394  New Jersey Devils            NJD     Devils
    ## 3             70           86      177      2 New York Islanders  347            NYI 1688 New York Islanders            NYI  Islanders
    ## 4             70           86      177      2 New York Islanders  347            NYI 1688 New York Islanders            NYI  Islanders
    ## 5             68           79      408      3   New York Rangers  808            NYR 2883   New York Rangers            NYR    Rangers
    ## 6             68           79      408      3   New York Rangers  808            NYR 2883   New York Rangers            NYR    Rangers
    ##                          venue.name venue.city division.id   division.name conference.id conference.name
    ## 1                 Prudential Center     Newark          25 MassMutual East             6         Eastern
    ## 2                 Prudential Center     Newark          25 MassMutual East             6         Eastern
    ## 3 Nassau Veterans Memorial Coliseum  Uniondale          25 MassMutual East             6         Eastern
    ## 4 Nassau Veterans Memorial Coliseum  Uniondale          25 MassMutual East             6         Eastern
    ## 5             Madison Square Garden   New York          25 MassMutual East             6         Eastern
    ## 6             Madison Square Garden   New York          25 MassMutual East             6         Eastern

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

head(new.data)
```

    ##   activeFranchise gameTypeId lastSeasonId id firstSeasonId franchiseId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses
    ## 1               1          2           NA  1      19821983          23        2993         8902     8792        525                 85
    ## 2               1          2           NA  1      19821983          23        2993         8902     8792        525                 85
    ## 3               1          2           NA  3      19721973          22        3788        11907    12045        678                 84
    ## 4               1          2           NA  3      19721973          22        3788        11907    12045        678                 84
    ## 5               1          2           NA  5      19261927          10        6560        20020    20041       1143                 76
    ## 6               1          2           NA  5      19261927          10        6560        20020    20041       1143                 76
    ##   homeTies homeWins losses overtimeLosses penaltyMinutes pointPctg points roadLosses roadOvertimeLosses roadTies roadWins
    ## 1       96      790   1211            169          44773    0.5306   3176        686                 84      123      604
    ## 2       96      790   1211            169          44773    0.5306   3176        686                 84      123      604
    ## 3      170      963   1587            166          57792    0.5133   3889        909                 82      177      725
    ## 4      170      963   1587            166          57792    0.5133   3889        909                 82      177      725
    ## 5      448     1614   2716            153          86129    0.5127   6727       1573                 77      360     1269
    ## 6      448     1614   2716            153          86129    0.5127   6727       1573                 77      360     1269
    ##   shootoutLosses shootoutWins shutouts teamId         teamName.x ties abbreviation.x wins               name abbreviation.y teamName.y
    ## 1             84           78      196      1  New Jersey Devils  219            NJD 1394  New Jersey Devils            NJD     Devils
    ## 2             84           78      196      1  New Jersey Devils  219            NJD 1394  New Jersey Devils            NJD     Devils
    ## 3             70           86      177      2 New York Islanders  347            NYI 1688 New York Islanders            NYI  Islanders
    ## 4             70           86      177      2 New York Islanders  347            NYI 1688 New York Islanders            NYI  Islanders
    ## 5             68           79      408      3   New York Rangers  808            NYR 2883   New York Rangers            NYR    Rangers
    ## 6             68           79      408      3   New York Rangers  808            NYR 2883   New York Rangers            NYR    Rangers
    ##                          venue.name venue.city division.id   division.name conference.id conference.name perc.total.games.win
    ## 1                 Prudential Center     Newark          25 MassMutual East             6         Eastern                46.58
    ## 2                 Prudential Center     Newark          25 MassMutual East             6         Eastern                46.58
    ## 3 Nassau Veterans Memorial Coliseum  Uniondale          25 MassMutual East             6         Eastern                44.56
    ## 4 Nassau Veterans Memorial Coliseum  Uniondale          25 MassMutual East             6         Eastern                44.56
    ## 5             Madison Square Garden   New York          25 MassMutual East             6         Eastern                43.95
    ## 6             Madison Square Garden   New York          25 MassMutual East             6         Eastern                43.95
    ##   perc.total.games.loss perc.home.win perc.home.loss perc.road.win perc.road.loss
    ## 1                 40.46         52.81          35.09         40.35          45.82
    ## 2                 40.46         52.81          35.09         40.35          45.82
    ## 3                 41.90         50.82          35.78         38.30          48.02
    ## 4                 41.90         50.82          35.78         38.30          48.02
    ## 5                 41.40         49.19          34.84         38.70          47.97
    ## 6                 41.40         49.19          34.84         38.70          47.97

``` r
#write_csv(new.data, "new.data.csv")
```

## Read data from 2 different API’s and combine

``` r
# read from goalie records endpoint for hurricanes
goalie.records <- get.nhl.data(table.name = "franchise-goalie-records", id = 26)
head(goalie.records)
```

    ##    id activePlayer firstName franchiseId       franchiseName gameTypeId gamesPlayed    lastName losses
    ## 1 336        FALSE       Tom          26 Carolina Hurricanes          2          34    Barrasso     12
    ## 2 363        FALSE   Richard          26 Carolina Hurricanes          2           6     Brodeur      2
    ## 3 369        FALSE      Sean          26 Carolina Hurricanes          2         256       Burke    120
    ## 4 411        FALSE      Mark          26 Carolina Hurricanes          2           3 Fitzpatrick      2
    ## 5 425        FALSE      John          26 Carolina Hurricanes          2         122     Garrett     57
    ## 6 430        FALSE     Mario          26 Carolina Hurricanes          2          23    Gosselin     13
    ##                            mostGoalsAgainstDates mostGoalsAgainstOneGame mostSavesDates mostSavesOneGame  mostShotsAgainstDates
    ## 1             2001-12-30, 2001-12-18, 2001-11-29                       5     2001-12-10               40             2001-12-10
    ## 2                                     1988-03-09                       4     1988-04-03               31             1988-04-03
    ## 3                                     1992-12-11                       9     1994-01-01               51 1996-01-27, 1994-01-01
    ## 4                                     2000-02-15                       5     2000-02-15               40             2000-02-15
    ## 5 1982-01-02, 1980-10-11, 1980-03-08, 1980-02-26                       9     1981-02-14               50             1981-02-14
    ## 6                         1993-04-10, 1993-03-24                       6     1993-03-22               45             1993-03-22
    ##   mostShotsAgainstOneGame mostShutoutsOneSeason        mostShutoutsSeasonIds mostWinsOneSeason mostWinsSeasonIds overtimeLosses
    ## 1                      43                     2                     20012002                13          20012002             NA
    ## 2                      34                     0                     19871988                 4          19871988             NA
    ## 3                      54                     4           19951996, 19961997                28          19951996             NA
    ## 4                      45                     0                     19992000                 0          19992000             NA
    ## 5                      57                     0 19791980, 19801981, 19811982                16          19791980             NA
    ## 6                      50                     0           19921993, 19931994                 5          19921993             NA
    ##   playerId positionCode rookieGamesPlayed rookieShutouts rookieWins seasons shutouts ties wins
    ## 1  8445275            G                NA             NA         NA       1        2    5   13
    ## 2  8445694            G                NA             NA         NA       1        0    0    4
    ## 3  8445769            G                NA             NA         NA       6       10   24  100
    ## 4  8446829            G                NA             NA         NA       1        0    0    0
    ## 5  8447066            G                NA             NA         NA       3        0   27   36
    ## 6  8447303            G                NA             NA         NA       2        0    1    5

``` r
# read from skater records endpoint for hurricanes
skater.records <- get.nhl.data(table.name = "franchise-skater-records", id = 26)
head(skater.records)
```

    ##      id activePlayer assists firstName franchiseId       franchiseName gameTypeId gamesPlayed goals   lastName
    ## 1 17239        FALSE       0       Jim          26 Carolina Hurricanes          2          16     0      Agnew
    ## 2 17418        FALSE       1      Mike          26 Carolina Hurricanes          2           5     0 Antonovich
    ## 3 17543        FALSE       0      Fred          26 Carolina Hurricanes          2           3     0     Arthur
    ## 4 17703        FALSE       2    Jergus          26 Carolina Hurricanes          2          10     0       Baca
    ## 5 17728        FALSE       0      Reid          26 Carolina Hurricanes          2          12     0     Bailey
    ## 6 18169        FALSE       0       Bob          26 Carolina Hurricanes          2           1     0      Bodak
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             mostAssistsGameDates
    ## 1 1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1979-10-13
    ## 3                                                 1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1990-10-27, 1991-02-24
    ## 5                                                 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ##   mostAssistsOneGame mostAssistsOneSeason mostAssistsSeasonIds
    ## 1                  0                    0             19921993
    ## 2                  1                    1             19791980
    ## 3                  0                    0             19801981
    ## 4                  1                    2             19901991
    ## 5                  0                    0             19831984
    ## 6                  0                    0             19891990
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   mostGoalsGameDates
    ## 1                                                                                                                                     1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                     1979-10-11, 1979-10-13, 1979-10-14, 1979-10-17, 1979-10-19, 1979-10-20, 1979-10-24, 1979-10-26, 1979-10-28, 1979-10-31, 1979-11-02, 1979-11-06, 1979-11-09, 1979-11-11, 1979-11-14, 1979-11-17, 1979-11-18, 1979-11-21, 1979-11-24, 1979-11-25, 1979-11-27, 1979-11-30, 1979-12-01, 1979-12-04, 1979-12-07, 1979-12-09, 1979-12-11, 1979-12-12, 1979-12-15, 1979-12-19, 1979-12-22, 1979-12-23, 1979-12-26, 1979-12-29, 1980-01-02, 1980-01-04, 1980-01-06, 1980-01-07, 1980-01-09, 1980-01-12, 1980-01-17, 1980-01-19, 1980-01-21, 1980-01-24, 1980-01-26, 1980-01-28, 1980-01-30, 1980-02-02, 1980-02-03, 1980-02-06, 1980-02-08, 1980-02-10, 1980-02-12, 1980-02-15, 1980-02-16, 1980-02-18, 1980-02-19, 1980-02-23, 1980-02-26, 1980-02-27, 1980-02-29, 1980-03-01, 1980-03-06, 1980-03-08, 1980-03-09, 1980-03-12, 1980-03-13, 1980-03-15, 1980-03-16, 1980-03-19, 1980-03-21, 1980-03-22, 1980-03-24, 1980-03-26, 1980-03-28, 1980-03-29, 1980-04-01, 1980-04-02, 1980-04-04, 1980-04-06
    ## 3                                                                                                                                                                                     1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4 1990-10-17, 1990-10-19, 1990-10-24, 1990-10-27, 1990-10-28, 1990-10-31, 1990-11-03, 1990-11-06, 1990-11-09, 1990-11-10, 1990-11-14, 1990-11-15, 1990-11-17, 1990-11-21, 1990-11-23, 1990-11-24, 1990-11-28, 1990-11-29, 1990-12-01, 1990-12-03, 1990-12-05, 1990-12-07, 1990-12-08, 1990-12-12, 1990-12-13, 1990-12-15, 1990-12-18, 1990-12-20, 1990-12-22, 1990-12-23, 1990-12-26, 1990-12-29, 1990-12-30, 1991-01-02, 1991-01-05, 1991-01-08, 1991-01-10, 1991-01-12, 1991-01-13, 1991-01-16, 1991-01-23, 1991-01-24, 1991-01-26, 1991-01-29, 1991-01-31, 1991-02-02, 1991-02-03, 1991-02-06, 1991-02-09, 1991-02-10, 1991-02-13, 1991-02-15, 1991-02-16, 1991-02-20, 1991-02-23, 1991-02-24, 1991-02-26, 1991-02-28, 1991-03-02, 1991-03-03, 1991-03-05, 1991-03-09, 1991-03-10, 1991-03-12, 1991-03-14, 1991-03-16, 1991-03-17, 1991-03-19, 1991-03-23, 1991-03-25, 1991-03-27, 1991-03-30, 1991-03-31, 1992-02-25, 1992-02-27, 1992-02-29, 1992-03-01, 1992-03-03, 1992-03-05, 1992-03-07, 1992-03-09, 1992-03-11, 1992-03-13, 1992-03-14, 1992-03-16, 1992-03-18, 1992-03-21, 1992-03-22, 1992-03-24, 1992-03-26, 1992-03-28, 1992-03-29, 1992-04-12, 1992-04-13, 1992-04-15
    ## 5                                                                                                                                                                                     1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ##   mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds mostPenaltyMinutesOneSeason mostPenaltyMinutesSeasonIds
    ## 1                0                  0           19921993                          68                    19921993
    ## 2                0                  0           19791980                           2                    19791980
    ## 3                0                  0           19801981                           0                    19801981
    ## 4                0                  0 19901991, 19911992                          14                    19901991
    ## 5                0                  0           19831984                          25                    19831984
    ## 6                0                  0           19891990                           7                    19891990
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              mostPointsGameDates
    ## 1 1992-10-06, 1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-17, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-28, 1992-10-31, 1992-11-03, 1992-11-06, 1992-11-07, 1992-11-11, 1992-11-13, 1992-11-14, 1992-11-18, 1992-11-19, 1992-11-21, 1992-11-25, 1992-11-27, 1992-11-28, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-09, 1992-12-11, 1992-12-12, 1992-12-16, 1992-12-18, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-03, 1993-01-06, 1993-01-09, 1993-01-10, 1993-01-13, 1993-01-15, 1993-01-16, 1993-01-18, 1993-01-21, 1993-01-23, 1993-01-24, 1993-01-27, 1993-01-28, 1993-01-30, 1993-02-03, 1993-02-08, 1993-02-12, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-21, 1993-02-24, 1993-02-27, 1993-02-28, 1993-03-03, 1993-03-05, 1993-03-06, 1993-03-08, 1993-03-10, 1993-03-13, 1993-03-16, 1993-03-19, 1993-03-22, 1993-03-24, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-05, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14, 1993-04-16
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1979-10-13
    ## 3                                                 1980-10-09, 1980-10-11, 1980-10-12, 1980-10-15, 1980-10-18, 1980-10-19, 1980-10-22, 1980-10-25, 1980-10-26, 1980-10-29, 1980-10-30, 1980-11-01, 1980-11-05, 1980-11-06, 1980-11-08, 1980-11-12, 1980-11-15, 1980-11-16, 1980-11-20, 1980-11-22, 1980-11-23, 1980-11-26, 1980-11-28, 1980-11-29, 1980-12-02, 1980-12-03, 1980-12-06, 1980-12-07, 1980-12-10, 1980-12-13, 1980-12-17, 1980-12-20, 1980-12-21, 1980-12-23, 1980-12-26, 1980-12-27, 1981-01-02, 1981-01-03, 1981-01-07, 1981-01-09, 1981-01-10, 1981-01-12, 1981-01-14, 1981-01-17, 1981-01-18, 1981-01-21, 1981-01-23, 1981-01-24, 1981-01-28, 1981-01-30, 1981-01-31, 1981-02-02, 1981-02-04, 1981-02-07, 1981-02-08, 1981-02-12, 1981-02-14, 1981-02-15, 1981-02-18, 1981-02-19, 1981-02-22, 1981-02-25, 1981-02-27, 1981-03-01, 1981-03-03, 1981-03-06, 1981-03-08, 1981-03-10, 1981-03-11, 1981-03-14, 1981-03-15, 1981-03-18, 1981-03-21, 1981-03-22, 1981-03-25, 1981-03-27, 1981-03-29, 1981-04-01, 1981-04-03, 1981-04-05
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1990-10-27, 1991-02-24
    ## 5                                                 1983-10-05, 1983-10-08, 1983-10-09, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-22, 1983-10-25, 1983-10-28, 1983-10-30, 1983-11-01, 1983-11-02, 1983-11-05, 1983-11-06, 1983-11-08, 1983-11-12, 1983-11-15, 1983-11-17, 1983-11-19, 1983-11-23, 1983-11-26, 1983-11-30, 1983-12-03, 1983-12-04, 1983-12-06, 1983-12-08, 1983-12-10, 1983-12-13, 1983-12-15, 1983-12-17, 1983-12-20, 1983-12-21, 1983-12-23, 1983-12-26, 1983-12-27, 1983-12-30, 1984-01-03, 1984-01-05, 1984-01-07, 1984-01-08, 1984-01-10, 1984-01-13, 1984-01-15, 1984-01-17, 1984-01-19, 1984-01-21, 1984-01-24, 1984-01-26, 1984-01-28, 1984-01-29, 1984-02-01, 1984-02-04, 1984-02-05, 1984-02-07, 1984-02-11, 1984-02-12, 1984-02-14, 1984-02-16, 1984-02-18, 1984-02-19, 1984-02-23, 1984-02-25, 1984-02-26, 1984-03-03, 1984-03-04, 1984-03-07, 1984-03-08, 1984-03-11, 1984-03-13, 1984-03-15, 1984-03-17, 1984-03-18, 1984-03-20, 1984-03-21, 1984-03-24, 1984-03-25, 1984-03-27, 1984-03-29, 1984-03-31, 1984-04-01
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                 1989-12-14, 1989-12-16, 1989-12-19, 1989-12-20, 1989-12-23, 1989-12-26, 1989-12-30, 1990-01-03, 1990-01-05, 1990-01-06, 1990-01-10, 1990-01-13, 1990-01-15, 1990-01-17, 1990-01-19, 1990-01-23, 1990-01-25, 1990-01-27, 1990-01-30, 1990-02-01, 1990-02-03, 1990-02-04, 1990-02-07, 1990-02-09, 1990-02-10, 1990-02-14, 1990-02-17, 1990-02-18, 1990-02-21, 1990-02-23, 1990-02-24, 1990-02-28, 1990-03-02, 1990-03-03, 1990-03-06, 1990-03-08, 1990-03-10, 1990-03-11, 1990-03-13, 1990-03-17, 1990-03-18, 1990-03-21, 1990-03-24, 1990-03-25, 1990-03-27, 1990-03-29, 1990-03-31, 1990-04-01
    ##   mostPointsOneGame mostPointsOneSeason mostPointsSeasonIds penaltyMinutes playerId points positionCode rookieGamesPlayed rookiePoints
    ## 1                 0                   0            19921993             68  8444893      0            D                NA           NA
    ## 2                 1                   1            19791980              2  8445015      1            C                NA           NA
    ## 3                 0                   0            19801981              0  8445090      0            D                 3            0
    ## 4                 1                   2            19901991             14  8445195      2            D                 9            2
    ## 5                 0                   0            19831984             25  8445211      0            D                NA           NA
    ## 6                 0                   0            19891990              7  8445567      0            L                 1            0
    ##   seasons
    ## 1       1
    ## 2       1
    ## 3       1
    ## 4       2
    ## 5       1
    ## 6       1

``` r
# find common columns between the two data frames
common.names <- intersect(names(goalie.records), names(skater.records))

# only keep common columns in each data frame
goalie.records <- goalie.records %>% select(all_of(common.names))
skater.records <- skater.records %>% select(all_of(common.names))

# combine the 2 data frames together
hurricanes.players.record <- rbind(goalie.records, skater.records)
head(hurricanes.players.record)
```

    ##    id activePlayer firstName franchiseId       franchiseName gameTypeId gamesPlayed    lastName playerId positionCode
    ## 1 336        FALSE       Tom          26 Carolina Hurricanes          2          34    Barrasso  8445275            G
    ## 2 363        FALSE   Richard          26 Carolina Hurricanes          2           6     Brodeur  8445694            G
    ## 3 369        FALSE      Sean          26 Carolina Hurricanes          2         256       Burke  8445769            G
    ## 4 411        FALSE      Mark          26 Carolina Hurricanes          2           3 Fitzpatrick  8446829            G
    ## 5 425        FALSE      John          26 Carolina Hurricanes          2         122     Garrett  8447066            G
    ## 6 430        FALSE     Mario          26 Carolina Hurricanes          2          23    Gosselin  8447303            G
    ##   rookieGamesPlayed seasons
    ## 1                NA       1
    ## 2                NA       1
    ## 3                NA       6
    ## 4                NA       1
    ## 5                NA       3
    ## 6                NA       2

``` r
# read from goalie records endpoint for hurricanes
goalie.records2 <- get.nhl.data(table.name = "franchise-goalie-records")
head(goalie.records2)
```

    ##    id activePlayer firstName franchiseId       franchiseName gameTypeId gamesPlayed lastName losses  mostGoalsAgainstDates
    ## 1 235        FALSE       Don          15        Dallas Stars          2         315  Beaupre    125             1983-10-07
    ## 2 236        FALSE       Bob          28     Arizona Coyotes          2         281  Essensa    114 1992-12-11, 1992-10-12
    ## 3 237        FALSE      Tony          11  Chicago Blackhawks          2         873 Esposito    302 1983-10-15, 1980-11-26
    ## 4 238        FALSE     Grant          25     Edmonton Oilers          2         423     Fuhr    117 1984-02-05, 1982-10-12
    ## 5 239        FALSE       Ron          16 Philadelphia Flyers          2         489  Hextall    172             1987-04-05
    ## 6 240        FALSE    Curtis          18     St. Louis Blues          2         280   Joseph     96 1992-11-25, 1990-02-20
    ##   mostGoalsAgainstOneGame mostSavesDates mostSavesOneGame mostShotsAgainstDates mostShotsAgainstOneGame mostShutoutsOneSeason
    ## 1                      10     1987-03-15               52            1986-03-21                      55                     1
    ## 2                       8     1989-12-29               49            1989-12-29                      50                     5
    ## 3                      10     1977-02-26               50            1976-12-12                      53                    15
    ## 4                       9     1986-03-12               49            1986-03-12                      54                     4
    ## 5                       9     1990-12-23               45            1988-10-13                      50                     5
    ## 6                       8     1992-03-02               51            1992-03-02                      54                     2
    ##          mostShutoutsSeasonIds mostWinsOneSeason mostWinsSeasonIds overtimeLosses playerId positionCode rookieGamesPlayed
    ## 1 19841985, 19851986, 19861987                25          19851986             NA  8445381            G                44
    ## 2                     19911992                33          19921993             NA  8446719            G                36
    ## 3                     19691970                38          19691970             NA  8446720            G                63
    ## 4                     19871988                40          19871988             NA  8446991            G                48
    ## 5                     19961997                37          19861987             NA  8447775            G                66
    ## 6                     19911992                36          19931994             NA  8448382            G                30
    ##   rookieShutouts rookieWins seasons shutouts ties wins
    ## 1              0         18       9        3   45  126
    ## 2              1         18       7       14   32  116
    ## 3             15         38      15       74  148  418
    ## 4              0         28      10        9   54  226
    ## 5              1         37      11       18   58  240
    ## 6              0         16       6        5   34  137

``` r
# read from skater records endpoint for hurricanes
skater.records2 <- get.nhl.data(table.name = "franchise-skater-records")
head(skater.records2)
```

    ##      id activePlayer assists firstName franchiseId        franchiseName gameTypeId gamesPlayed goals  lastName
    ## 1 16888        FALSE     417    George           5  Toronto Maple Leafs          2        1188   296 Armstrong
    ## 2 16889        FALSE       0     Billy           2   Montreal Wanderers          2           2     1      Bell
    ## 3 16890        FALSE     794    Johnny           6        Boston Bruins          2        1436   545     Bucyk
    ## 4 16891        FALSE     712      Jean           1   Montréal Canadiens          2        1125   507  Beliveau
    ## 5 16892        FALSE    1111       Ray           6        Boston Bruins          2        1518   395   Bourque
    ## 6 16893        FALSE      33    Harold           9 Philadelphia Quakers          2         216    60   Darragh
    ##                                                                                         mostAssistsGameDates mostAssistsOneGame
    ## 1 1956-01-07, 1957-03-16, 1957-11-24, 1961-01-15, 1961-12-02, 1962-02-25, 1964-02-23, 1965-12-18, 1969-01-31                  3
    ## 2                                                                                     1917-12-19, 1917-12-29                  0
    ## 3                                                                                                 1971-01-01                  5
    ## 4                                                 1955-02-19, 1956-12-01, 1962-11-24, 1965-11-20, 1967-12-28                  4
    ## 5                                                                                     1990-02-18, 1994-01-02                  5
    ## 6                                                 1926-01-19, 1929-11-19, 1929-11-23, 1929-12-10, 1930-01-18                  2
    ##   mostAssistsOneSeason mostAssistsSeasonIds                                                                 mostGoalsGameDates
    ## 1                   35             19651966                                                             1959-03-15, 1961-12-16
    ## 2                    0             19171918                                                                         1917-12-19
    ## 3                   65             19701971                                                             1973-01-18, 1974-01-05
    ## 4                   58             19601961                                                 1955-11-05, 1959-03-07, 1969-02-11
    ## 5                   73             19901991                                                                         1983-03-08
    ## 6                   17             19291930 1927-03-20, 1928-03-12, 1928-03-17, 1929-11-16, 1930-01-18, 1930-02-01, 1930-02-22
    ##   mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds mostPenaltyMinutesOneSeason mostPenaltyMinutesSeasonIds
    ## 1                3                 23           19591960                          97                    19551956
    ## 2                1                  1           19171918                           0                    19171918
    ## 3                4                 51           19701971                          57                    19571958
    ## 4                4                 47           19551956                         143                    19551956
    ## 5                3                 31           19831984                          96                    19801981
    ## 6                2                 15           19291930                           8          19271928, 19291930
    ##                                          mostPointsGameDates mostPointsOneGame mostPointsOneSeason mostPointsSeasonIds penaltyMinutes
    ## 1 1957-03-16, 1962-02-25, 1964-12-12, 1965-03-21, 1967-11-02                 4                  53            19611962            726
    ## 2                                                 1917-12-19                 1                   1            19171918              0
    ## 3                                     1970-12-10, 1971-02-25                 6                 116            19701971            436
    ## 4                                                 1959-03-07                 7                  91            19581959           1033
    ## 5                                                 1990-02-18                 6                  96            19831984           1087
    ## 6                                                 1930-01-18                 4                  32            19291930             32
    ##   playerId points positionCode rookieGamesPlayed rookiePoints seasons
    ## 1  8444971    713            R                52           25      21
    ## 2  8445044      1            C                 2            1       1
    ## 3  8445240   1339            L                NA           NA      21
    ## 4  8445408   1219            C                44           34      20
    ## 5  8445621   1506            D                80           65      21
    ## 6  8445843     93            L                35           17       6

``` r
# find common columns between the two data frames
common.names <- intersect(names(goalie.records), names(skater.records))

# only keep common columns in each data frame
goalie.records2 <- goalie.records2 %>% select(all_of(common.names))
skater.records2 <- skater.records2 %>% select(all_of(common.names))

# combine the 2 data frames together
players.record.all <- rbind(goalie.records2, skater.records2)
head(players.record.all)
```

    ##    id activePlayer firstName franchiseId       franchiseName gameTypeId gamesPlayed lastName playerId positionCode rookieGamesPlayed
    ## 1 235        FALSE       Don          15        Dallas Stars          2         315  Beaupre  8445381            G                44
    ## 2 236        FALSE       Bob          28     Arizona Coyotes          2         281  Essensa  8446719            G                36
    ## 3 237        FALSE      Tony          11  Chicago Blackhawks          2         873 Esposito  8446720            G                63
    ## 4 238        FALSE     Grant          25     Edmonton Oilers          2         423     Fuhr  8446991            G                48
    ## 5 239        FALSE       Ron          16 Philadelphia Flyers          2         489  Hextall  8447775            G                66
    ## 6 240        FALSE    Curtis          18     St. Louis Blues          2         280   Joseph  8448382            G                30
    ##   seasons
    ## 1       9
    ## 2       7
    ## 3      15
    ## 4      10
    ## 5      11
    ## 6       6

``` r
#write_csv(players.record.all, "players.record.all.csv")
```

## Read data from 2 different API’s

``` r
franchise.data <- get.nhl.data(table.name = "franchise")
franchise.data <- franchise.data %>% select(id, mostRecentTeamId, teamCommonName, teamAbbrev, teamPlaceName)
head(franchise.data)
```

    ##   id mostRecentTeamId teamCommonName teamAbbrev teamPlaceName
    ## 1  1                8      Canadiens        MTL      Montréal
    ## 2  2               41      Wanderers        MWN      Montreal
    ## 3  3               45         Eagles        SLE     St. Louis
    ## 4  4               37         Tigers        HAM      Hamilton
    ## 5  5               10    Maple Leafs        TOR       Toronto
    ## 6  6                6         Bruins        BOS        Boston

``` r
goalie.data.hurricanes <- get.nhl.data(table.name = "franchise-goalie-records", id=26) %>% select(activePlayer, firstName, lastName, gamesPlayed, wins, ties, losses, seasons, shutouts, mostShotsAgainstOneGame, mostGoalsAgainstOneGame, mostSavesOneGame, mostShutoutsOneSeason, mostWinsOneSeason)
```

## Contingency Tables

``` r
table(players.record.all$franchiseName)
```

    ## 
    ##         Anaheim Ducks       Arizona Coyotes         Boston Bruins    Brooklyn Americans        Buffalo Sabres        Calgary Flames 
    ##                   402                   590                   969                   152                   508                   602 
    ##   Carolina Hurricanes    Chicago Blackhawks      Cleveland Barons    Colorado Avalanche Columbus Blue Jackets          Dallas Stars 
    ##                   525                   928                   145                   540                   289                   656 
    ##     Detroit Red Wings       Edmonton Oilers      Florida Panthers       Hamilton Tigers     Los Angeles Kings        Minnesota Wild 
    ##                   912                   559                   410                    39                   651                   268 
    ##    Montréal Canadiens      Montreal Maroons    Montreal Wanderers   Nashville Predators     New Jersey Devils    New York Islanders 
    ##                   838                    84                    12                   298                   559                   544 
    ##      New York Rangers       Ottawa Senators   Philadelphia Flyers  Philadelphia Quakers   Pittsburgh Penguins       San Jose Sharks 
    ##                  1038                   393                   617                    42                   711                   354 
    ##       St. Louis Blues      St. Louis Eagles   Tampa Bay Lightning   Toronto Maple Leafs     Vancouver Canucks  Vegas Golden Knights 
    ##                   653                    94                   408                   971                   615                    61 
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
    ##  173  214  382  678  759  827  852  889  971  985  990 1007 1070 1084 1394 1469 1497 1649 1688 1700 1754 1805 1903 1929 2079 2812 2873 
    ##    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2 
    ## 2883 2891 3241 3473 
    ##    2    2    2    2

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

![](README_files/figure-gfm/plot%20-%20bar-1.png)<!-- -->

### Histogram

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  labs(x = "Games Played", title = "Number of Games Played by all Franchise's Players")
```

![](README_files/figure-gfm/plot%20-%20histogram-1.png)<!-- -->

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  facet_grid(~ positionCode, 
             labeller = as_labeller(c(C = "Center", D = "Defense", G = "Goalie", L = "Left Wing", R = "Right Wing"))) +
  labs(x = "Games Played", title = "Number of Games Played by all Franchise's Players by Position Played")
```

![](README_files/figure-gfm/plot%20-%20histogram%20facet-1.png)<!-- -->

### Boxplot

``` r
ggplot(data = new.data, aes(x = division.name, y = gamesPlayed)) +
  geom_boxplot(fill = "maroon") +
  stat_summary(fun = mean, geom = "line", aes(group = conference.name, col = conference.name)) +
  labs(x = "Division Name", y = "# of Games Played", title = "Number of Games Played for Each Division")
```

![](README_files/figure-gfm/plot%20-%20box%20plot-1.png)<!-- -->

### Scatter Plot

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Each Franchise")
```

![](README_files/figure-gfm/plot%20-%20scatter-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  facet_grid(~ conference.name, labeller = as_labeller(c(Eastern = "Eastern Conference", 
                                      Western = "Western Conference"))) +
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Franchies in Each Division")
```

![](README_files/figure-gfm/plot%20-%20scatter%20facet-1.png)<!-- -->

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

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20home%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = roadWins, y = roadLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "Road Games Won", y = "Road Games Lost", title = "Number of Road Games Won vs. Lost for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20road%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.win, y = perc.road.win)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Won", y = "% Road Games Won", title = "% of Home Games Won vs. Road Games Won for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20won%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.loss, y = perc.road.loss)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Loss", y = "% Road Games Loss", title = "% of Home Games Loss vs. Road Games Loss for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20lost%20games-1.png)<!-- -->
