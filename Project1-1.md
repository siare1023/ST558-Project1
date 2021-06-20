proj 1
================
Lucy Yin
6/8/2021

-   [Install and load required
    packages](#install-and-load-required-packages)
-   [Write functions to contact the NHL records and stats
    API’s](#write-functions-to-contact-the-nhl-records-and-stats-apis)
    -   [NHL records API](#nhl-records-api)
        -   [Try out NHL records API](#try-out-nhl-records-api)
    -   [NHL stats API](#nhl-stats-api)
        -   [Try out NHL stats API](#try-out-nhl-stats-api)
    -   [Wrapper Function](#wrapper-function)
-   [Exporatory Data Analysis](#exporatory-data-analysis)
    -   [Grabbing data and combine](#grabbing-data-and-combine)
    -   [Create new variables](#create-new-variables)
    -   [Read data from 2 different endpoints and
        combine](#read-data-from-2-different-endpoints-and-combine)
    -   [Contingency Tables](#contingency-tables)
    -   [Numerical Summaries for Categorical
        Variables](#numerical-summaries-for-categorical-variables)
    -   [Plots](#plots)
        -   [Bar plot](#bar-plot)
        -   [Histogram](#histogram)
        -   [Boxplot](#boxplot)
        -   [Scatter Plot](#scatter-plot)
        -   [Scatter Plot using Group](#scatter-plot-using-group)

# Install and load required packages

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

# Write functions to contact the NHL records and stats API’s

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
# pull out the common name vector (index can serve as the id)
name.id.list <- franchise.tbl$`franchise.list$teamCommonName`
# pull out the most recent id vector (index can serve as the id)
id.most.recent.id.list <- franchise.tbl$`franchise.list$mostRecentTeamId`
```

``` r
get.records <- function(table.name, id=NULL, team.common.name=NULL, ...) {
  base.url <- "https://records.nhl.com/site/api"
  
  ## table name is given
  if (!is.null(table.name)) {
    
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

    ## table name is this one and yes id
    if (table.name %in% ("franchise") & (is.numeric(id))) {
      return ("The table selected cannot be returned with the specified ID or team common name input.")
    }  
    
    ## yes table name, no id
    else if (is.null(id)) {
      full.url <- paste0(base.url, "/", table.name)
      
      nfl.records <- GET(full.url)
      nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
      return (nfl.records.json$data %>% as_tibble())
    }
    
    ## table name is one of the 4, yes id
    else if (table.name %in% c("franchise-team-totals",
                               "franchise-season-records",
                               "franchise-goalie-records",
                               "franchise-skater-records") & 
             (is.numeric(id))) {
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
             (is.numeric(id) %in% c(1, 5 ,6, 10:12, 14:38))) {
      nfl.records.details <- GET(paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id))
      nfl.records.details.txt <- content(nfl.records.details, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.details.json <- fromJSON(nfl.records.details.txt, flatten=TRUE)  ## convert to list
      fran.details <- nfl.records.details.json$data
      
      ## these columns contain html content, save as separate vectors
      txt.string1 <- fran.details$captainHistory
      txt.string2 <- fran.details$coachingHistory
      txt.string3 <- fran.details$generalManagerHistory
      txt.string4 <- fran.details$retiredNumbersSummary
      
      ## delete the original columns of html content, then add parsed html content back into table
      fran.details <- fran.details %>% select(-c(3,4,8,11))
      fran.details$captainHistory <- read_html(txt.string1) %>% xml_text()
      fran.details$coachingHistory <- read_html(txt.string2) %>% xml_text()
      fran.details$generalManagerHistory <- read_html(txt.string3) %>% xml_text()
      fran.details$retiredNumbersSummary <- read_html(txt.string4) %>% xml_text()
      return (fran.details)
    }
  }
  
  ## table name not given
  else {
    return ("Must provide a table name to get results.")
  }
}
```

### Try out NHL records API

``` r
get.records("franchise-goalie-records")
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
get.records("franchise-goalie-records", team.common.name = "Eagles")
```

    ## # A tibble: 4 x 29
    ##      id activePlayer firstName franchiseId franchiseName  gameTypeId gamesPlayed
    ##   <int> <lgl>        <chr>           <int> <chr>               <int>       <int>
    ## 1  1231 FALSE        Clint               3 St. Louis Eag…          2         158
    ## 2   538 FALSE        Bill                3 St. Louis Eag…          2          90
    ## 3   248 FALSE        Alec                3 St. Louis Eag…          2         294
    ## 4   600 FALSE        Sammy               3 St. Louis Eag…          2           2
    ## # … with 22 more variables: lastName <chr>, losses <int>,
    ## #   mostGoalsAgainstDates <chr>, mostGoalsAgainstOneGame <int>,
    ## #   mostSavesDates <lgl>, mostSavesOneGame <lgl>, mostShotsAgainstDates <lgl>,
    ## #   mostShotsAgainstOneGame <lgl>, mostShutoutsOneSeason <int>,
    ## #   mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <lgl>, playerId <int>,
    ## #   positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
get.records("franchise-detail")
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
get.records("franchise-detail", team.common.name = "Hurricanes")
```

    ##   id active         dateAwarded                              directoryUrl
    ## 1 26   TRUE 1979-06-22T00:00:00 https://www.nhl.com/hurricanes/team/staff
    ##   firstSeasonId
    ## 1      19791980
    ##                                                            heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Aho.jpg
    ##   mostRecentTeamId teamAbbrev        teamFullName
    ## 1               12        CAR Carolina Hurricanes
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   captainHistory
    ## 1 Jordan Staal: 2019-20 – Present\r\n\tJustin Williams: 2018-19\r\n\tJustin Faulk and Jordan Staal: 2017-18\r\n\t(No Captain): 2016-17\r\n\tEric Staal: 2010-11 – 2015-16\r\n\tRod Brind’Amour and Eric Staal: 2009-10\r\n\tRod Brind’Amour: 2005-06 – 2008-09\r\n\tRon Francis: 2000-01 – 2003-04\r\n\tKeith Primeau and Ron Francis: 1999-00\r\n\tKeith Primeau: 1998-99\r\n\tKevin Dineen: 1996-97 – 1997-98\r\n\tBrendan Shanahan: 1995-96\r\n\tPat Verbeek: 1992-93 – 1994-95\r\n\tRandy Ladouceur: 1991-92\r\n\tRon Francis: 1985-86 – 1990-91\r\n\tMark Johnson and Ron Francis: 1984-85\r\n\tMark Johnson: 1983-84\r\n\tRuss Anderson: 1982-83\r\n\tDave Keon: 1981-82\r\n\tRick Ley and Mike Rogers: 1980-81\r\n\tRick Ley: 1979-80\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     coachingHistory
    ## 1 Rod Brind’Amour: Oct. 4, 2018 – Present\r\n\tBill Peters: Oct. 10, 2014 – April 7, 2018\r\n\tKirk Muller: Nov. 29, 2011 – April 13, 2014\r\n\tPaul Maurice: Dec. 4, 2008 – Nov. 27, 2011\r\n\tPeter Laviolette: Dec. 18, 2003 – Nov. 30, 2008\r\n\tPaul Maurice: Nov. 7, 1995 – Dec. 14, 2003\r\n\tPaul Holmgren: Jan. 21 – Nov. 5, 1995\r\n\tPierre McGuire: Nov. 17, 1993 – April 14, 1994\r\n\tPaul Holmgren: Oct. 6, 1992 – Nov. 13, 1993\r\n\tJim Roberts:  Oct. 5, 1991 – May 1, 1992\r\n\tRick Ley: Oct. 5, 1989 – April 13, 1991\r\n\tLarry Pleau: Feb. 7, 1988 – April 9, 1989\r\n\tJack Evans: Oct. 5, 1983 – Feb. 6, 1988\r\n\tJohn Cunniff: March 8 – April 3, 1983\r\n\tLarry Pleau: Jan. 27 – March 6, 1983\r\n\tLarry Kish: Oct. 6, 1982 – Jan. 23, 1983\r\n\tLarry Pleau: Feb. 22, 1981 – April 4, 1982\r\n\tDon Blackburn: Oct. 11, 1979 – Feb. 19, 1981\r\n\t* Date range indicates first and last games coached during tenure (regular season or playoffs)\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                generalManagerHistory
    ## 1 Don Waddell: May 8, 2018 – Present\r\n\tRon Francis: April 28, 2014 – March 7, 2018\r\n\tJim Rutherford: June 28, 1994 – April 28, 2014\r\n\tPaul Holmgren: Sept. 8, 1993 – June 28, 1994\r\n\tBrian Burke: May 26, 1992 – Sept. 1, 1993\r\n\tEddie Johnston: May 11, 1989 – May 12, 1992\r\n\tEmile Francis: May 2, 1983 – May 11, 1989\r\n\tLarry Pleau: April 2, 1981 – May 2, 1983\r\n\tJack Kelley: May 6, 1977 – April 2, 1981\r\n\t* Date range indicates first and last days of tenure\r\n
    ##                                                                                          retiredNumbersSummary
    ## 1 2 – Glen Wesley (1994-08)\r\n\t10 – Ron Francis (1981-91, 1998-04)\r\n\t17 – Rod Brind’Amour (2000-10)  \r\n

``` r
get.records("franchise-detail", id=39)
```

    ## # A tibble: 1 x 13
    ##      id active captainHistory coachingHistory dateAwarded directoryUrl
    ##   <int> <lgl>  <lgl>          <lgl>           <lgl>       <chr>       
    ## 1    39 TRUE   NA             NA              NA          ""          
    ## # … with 7 more variables: firstSeasonId <int>, generalManagerHistory <lgl>,
    ## #   heroImageUrl <lgl>, mostRecentTeamId <int>, retiredNumbersSummary <lgl>,
    ## #   teamAbbrev <chr>, teamFullName <chr>

``` r
get.records("franchise-team-totals", 26)
```

    ## # A tibble: 4 x 30
    ##      id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ##   <int>           <int>         <int>       <int>      <int>       <int>
    ## 1    24               1      19971998          26          2        1812
    ## 2    67               1      19791980          26          2        1420
    ## 3    23               1      19971998          26          3         112
    ## 4    68               1      19791980          26          3          49
    ## # … with 24 more variables: goalsAgainst <int>, goalsFor <int>,
    ## #   homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>, homeWins <int>,
    ## #   lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
get.records("franchise", id=23)
```

    ## [1] "The table selected cannot be returned with the specified ID or team common name input."

``` r
get.records("franchise")
```

    ## # A tibble: 39 x 8
    ##       id firstSeasonId fullName         lastSeasonId mostRecentTeamId teamAbbrev
    ##    <int>         <int> <chr>                   <int>            <int> <chr>     
    ##  1     1      19171918 Montréal Canadi…           NA                8 MTL       
    ##  2     2      19171918 Montreal Wander…     19171918               41 MWN       
    ##  3     3      19171918 St. Louis Eagles     19341935               45 SLE       
    ##  4     4      19191920 Hamilton Tigers      19241925               37 HAM       
    ##  5     5      19171918 Toronto Maple L…           NA               10 TOR       
    ##  6     6      19241925 Boston Bruins              NA                6 BOS       
    ##  7     7      19241925 Montreal Maroons     19371938               43 MMR       
    ##  8     8      19251926 Brooklyn Americ…     19411942               51 BRK       
    ##  9     9      19251926 Philadelphia Qu…     19301931               39 QUA       
    ## 10    10      19261927 New York Rangers           NA                3 NYR       
    ## # … with 29 more rows, and 2 more variables: teamCommonName <chr>,
    ## #   teamPlaceName <chr>

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
  
  ## if modifier is 1 of the 5, no id (most.recent.id)
  if (modifier %in% c("team.roster",
                      "person.names",
                      "team.schedule.next", 
                      "team.schedule.previous",  
                      "team.stats") & 
      is.null(id)) {  ## modifier is 1 of the 5 and no most recent id known (from converting from id)
    full.url2 <- paste0(base.url3, "?expand=", modifier)
  }
  ## if modifier is 1 of the 5, yes id (most.recent.id)
  else if (modifier %in% c("team.roster",
                           "person.names",
                           "team.schedule.next",
                           "team.schedule.previous",
                           "team.stats") & 
           is.numeric(most.recent.id)) {
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier)
  }
  
  ## if modifier is this, no id (most.recent.id), yes season
  else if (modifier %in% ("team.roster&season") & 
           is.null(id) & 
           is.numeric(season)) {
    full.url2 <- paste0(base.url3, "?expand=", modifier, "=", season)
  }
  
  ## if modifier is this, yes id (most.recent.id), yes season
  else if (modifier %in% ("team.roster&season") & 
           is.numeric(most.recent.id) & 
           is.numeric(season)) {
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier, "=", season)
  }  
  
  ## if modifier is this, no season
  else if (modifier %in% ("team.roster&season") & is.null(season)) {  ## modifier is this and no season
    return ("Must provide a season input to get the team roster & season stats.")
  }    

  ## if modifier is this, multiple id's (most recent id's) given
  else if (modifier %in% ("teamId") & 
           is.numeric(c(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4))) {
    full.url2 <- paste0(base.url3, "?", modifier, "=",
                        paste(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4, sep = ","))
  }
  ## if modifier is this, no id (most recent id)
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.null(id)) {
    full.url2 <- paste0(base.url3, "?stats=", modifier)
  }
  
  ## if modifier is this, yes id (most.recent.id)
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.numeric(most.recent.id)) {  ## modifier is this and yes most recent id
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?stats=", modifier)
  }
  
  ## all other combinations
  else {
    return ("Invalid input, please try again.")  
  }
      
  nfl.stats3 <- GET(full.url2)
  nfl.stats3.txt <- content(nfl.stats3, "text", encoding = "UTF-8")
  nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
  
  ## because return tibble has data frames within, need to unnest twice to parse out everything  
  return (nfl.stats3.json$teams %>% unnest(everything()) %>% unnest(everything()) %>% as_tibble())

}
```

### Try out NHL stats API

``` r
get.stat2("team.stats", 26)
```

    ## # A tibble: 2 x 65
    ##      id name        link      abbreviation teamName locationName firstYearOfPlay
    ##   <int> <chr>       <chr>     <chr>        <chr>    <chr>        <chr>          
    ## 1    12 Carolina H… /api/v1/… CAR          Hurrica… Carolina     1979           
    ## 2    12 Carolina H… /api/v1/… CAR          Hurrica… Carolina     1979           
    ## # … with 58 more variables: stat.gamesPlayed <int>, stat.wins <chr>,
    ## #   stat.losses <chr>, stat.ot <chr>, stat.pts <chr>, stat.ptPctg <chr>,
    ## #   stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>,
    ## #   stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>,
    ## #   stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>,
    ## #   stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>,
    ## #   team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>,
    ## #   type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.id <int>, venue.name <chr>,
    ## #   venue.link <chr>, venue.city <chr>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>,
    ## #   division.name <chr>, division.link <chr>, conference.id <int>,
    ## #   conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

``` r
get.stats.data <- get.stat2("team.stats")
row.odd <- seq_len(nrow(get.stats.data)) %% 2  ## Create row indicator
row.odd ## Print row indicator
```

    ##  [1] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
    ## [39] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1

``` r
# subset odd rows where numbers are given in raw
data.row.odd <- get.stats.data[row.odd == 1, ]
data.row.odd
```

    ## # A tibble: 32 x 65
    ##       id name       link      abbreviation teamName locationName firstYearOfPlay
    ##    <int> <chr>      <chr>     <chr>        <chr>    <chr>        <chr>          
    ##  1     1 New Jerse… /api/v1/… NJD          Devils   New Jersey   1982           
    ##  2     2 New York … /api/v1/… NYI          Islande… New York     1972           
    ##  3     3 New York … /api/v1/… NYR          Rangers  New York     1926           
    ##  4     4 Philadelp… /api/v1/… PHI          Flyers   Philadelphia 1967           
    ##  5     5 Pittsburg… /api/v1/… PIT          Penguins Pittsburgh   1967           
    ##  6     6 Boston Br… /api/v1/… BOS          Bruins   Boston       1924           
    ##  7     7 Buffalo S… /api/v1/… BUF          Sabres   Buffalo      1970           
    ##  8     8 Montréal … /api/v1/… MTL          Canadie… Montréal     1909           
    ##  9     9 Ottawa Se… /api/v1/… OTT          Senators Ottawa       1990           
    ## 10    10 Toronto M… /api/v1/… TOR          Maple L… Toronto      1917           
    ## # … with 22 more rows, and 58 more variables: stat.gamesPlayed <int>,
    ## #   stat.wins <chr>, stat.losses <chr>, stat.ot <chr>, stat.pts <chr>,
    ## #   stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>,
    ## #   stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>,
    ## #   stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>,
    ## #   stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>,
    ## #   team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>,
    ## #   type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.name <chr>, venue.link <chr>,
    ## #   venue.city <chr>, venue.id <int>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>,
    ## #   division.name <chr>, division.link <chr>, conference.id <int>,
    ## #   conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

``` r
# subset even rows where ranks are given instead of raw number
data.row.even.ranks <- get.stats.data[row.odd == 0, ]
data.row.even.ranks
```

    ## # A tibble: 31 x 65
    ##       id name       link      abbreviation teamName locationName firstYearOfPlay
    ##    <int> <chr>      <chr>     <chr>        <chr>    <chr>        <chr>          
    ##  1     1 New Jerse… /api/v1/… NJD          Devils   New Jersey   1982           
    ##  2     2 New York … /api/v1/… NYI          Islande… New York     1972           
    ##  3     3 New York … /api/v1/… NYR          Rangers  New York     1926           
    ##  4     4 Philadelp… /api/v1/… PHI          Flyers   Philadelphia 1967           
    ##  5     5 Pittsburg… /api/v1/… PIT          Penguins Pittsburgh   1967           
    ##  6     6 Boston Br… /api/v1/… BOS          Bruins   Boston       1924           
    ##  7     7 Buffalo S… /api/v1/… BUF          Sabres   Buffalo      1970           
    ##  8     8 Montréal … /api/v1/… MTL          Canadie… Montréal     1909           
    ##  9     9 Ottawa Se… /api/v1/… OTT          Senators Ottawa       1990           
    ## 10    10 Toronto M… /api/v1/… TOR          Maple L… Toronto      1917           
    ## # … with 21 more rows, and 58 more variables: stat.gamesPlayed <int>,
    ## #   stat.wins <chr>, stat.losses <chr>, stat.ot <chr>, stat.pts <chr>,
    ## #   stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>,
    ## #   stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>,
    ## #   stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>,
    ## #   stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>,
    ## #   team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>,
    ## #   type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.name <chr>, venue.link <chr>,
    ## #   venue.city <chr>, venue.id <int>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>,
    ## #   division.name <chr>, division.link <chr>, conference.id <int>,
    ## #   conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

## Wrapper Function

``` r
get.nhl.data <- function (table.name=NULL, 
                          modifier=NULL, 
                          id=NULL, 
                          team.common.name=NULL, 
                          season=NULL, 
                          id2=NULL, id3=NULL, id4=NULL, ...) {

  ## yes table name, yes modifier
  if (!is.null(table.name) & !is.null(modifier)) {
    stop ("Cannot use both table name and modifier to get back information.")
  }
  
  ## yes table name
  else if (!is.null(table.name) & is.null(team.common.name) & is.null(id) & is.null(modifier)) {
    output <- get.records(table.name)
  }  
  
  ## yes table name, yes id or team.common.name, no modifier
  else if (!is.null(table.name) & (is.numeric(id) | !is.null(team.common.name)) & is.null(modifier)) {
    output <- get.records(table.name, id, team.common.name)
  }
  
  ## yes modifier
  else if (!is.null(modifier) & is.null(table.name) & is.null(id) & is.null(team.common.name)) {
    output <- get.stat2(modifier)
  }
  
  ## yes modifier, yes id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id)) {
    output <- get.stat2(modifier, id)
  }
  
  ## yes modifier, yes season
  else if (!is.null(modifier) & is.null(table.name) & !is.null(season)) {
    output <- get.stat2(modifier, season)
  }
  
  ## yes modifier, yes multiple id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id) & is.numeric(id2)) {
    output <- get.stat2(modifier, id, id2, id3, id4)
  }
  
  ## all other combinations
  else {
    return ("Invalid combination of inputs, please try again.")
  }
  
  return(output)
}
```

``` r
get.nhl.data(table.name = "franchise-detail", id=12)
```

    ##   id active         dateAwarded
    ## 1 12   TRUE 1926-09-25T00:00:00
    ##                                            directoryUrl firstSeasonId
    ## 1 https://www.nhl.com/redwings/team/business-operations      19261927
    ##                                                                     heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/DET/WingsWin.jpg
    ##   mostRecentTeamId teamAbbrev      teamFullName
    ## 1               17        DET Detroit Red Wings
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     captainHistory
    ## 1 Dylan Larkin: 2020-21 – Present\r\n\t(No Captain): 2018-19 – 2019-20\r\n\tHenrik Zetterberg: 2012-13 – 2017-18\r\n\tNicklas Lidstrom: 2006-07 – 2011-12\r\n\tSteve Yzerman: 1986-87 – 2005-06\r\n\tDanny Gare: 1982-83 – 1985-86\r\n\tReed Larson: 1981-82\r\n\tErrol Thompson and Reed Larson: 1980-81\r\n\tDale McCourt: 1979-80\r\n\tDennis Hextall, Nick Libett and Paul Woods: 1978-79\r\n\tDan Maloney and Dennis Hextall: 1977-78\r\n\tDanny Grant and Dennis Polonich: 1976-77\r\n\tDanny Grant and Terry Harper: 1975-76\r\n\tMarcel Dionne: 1974-75\r\n\tAlex Delvecchio, Nick Libett, Red Berenson, Gary Bergman, Ted Harris, Mickey Redmond and Larry Johnston: 1973-74\r\n\tAlex Delvecchio: 1962-63 – 1972-73\r\n\tGordie Howe: 1958-59 – 1961-62\r\n\tRed Kelly: 1956-57 – 1957-58\r\n\tTed Lindsay: 1952-53 – 1955-56\r\n\tSid Abel: 1946-47 – 1951-52\r\n\tFlash Hollett and Sid Abel: 1945-46\r\n\tFlash Hollett: 1944-45\r\n\tMud Bruneteau and Flash Hollett: 1943-44\r\n\tSid Abel: 1942-43\r\n\tEbbie Goodfellow and Syd Howe: 1941-42\r\n\tEbbie Goodfellow: 1938-39 – 1940-41\r\n\tDoug Young: 1935-36 – 1937-38\r\n\tEbbie Goodfellow: 1934-35\r\n\tHerbie Lewis: 1933-34\r\n\tLarry Aurie: 1932-33\r\n\tCarson Cooper: 1931-32\r\n\tGeorge Hay: 1930-31\r\n\tReg Noble: 1927-28 – 1929-30\r\n\tArt Duncan: 1926-27\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              coachingHistory
    ## 1 Jeff Blashill: Oct. 9, 2015 – Present\r\n\tMike Babcock: Oct. 5, 2005 – April 29, 2015\r\n\tDave Lewis: Oct. 10, 2002 – May 3, 2004\r\n\tScotty Bowman: Oct. 23, 1998 – June 13, 2002\r\n\tDave Lewis and Barry Smith (Co-Coaches): Oct. 10-21, 1998\r\n\tScotty Bowman: Oct. 5, 1993 – June 16, 1998\r\n\tBryan Murray: Oct. 4, 1990 – May 1, 1993\r\n\tJacques Demers: Oct. 9, 1986 – April 1, 1990\r\n\tBrad Park: Dec. 31, 1985 – April 6, 1986\r\n\tHarry Neale: Oct 10 – Dec. 29, 1985\r\n\tNick Polano: Oct. 6, 1982 – April 13, 1985\r\n\tBilly Dea: March 11 – April 4, 1982\r\n\tWayne Maxner: Nov. 26, 1980 – March 8, 1982\r\n\tTed Lindsay: March 21 – Nov. 22, 1980\r\n\tBobby Kromm: Oct. 13, 1977 – March 19, 1980\r\n\tLarry Wilson: Jan. 20 – April 3, 1977\r\n\tAlex Delvecchio: Dec. 5, 1975 – Jan. 15, 1977\r\n\tDoug Barkley: Oct. 8 – Dec. 3, 1975\r\n\tAlex Delvecchio: Nov. 7, 1973 – April 6, 1975\r\n\tTed Garvin: Oct. 10 – Nov. 4, 1973\r\n\tJohnny Wilson: Nov. 1, 1971 – April 1, 1973\r\n\tDoug Barkley: Jan. 9 – Oct. 31, 1971\r\n\tNed Harkness: Oct. 10, 1970 – Jan. 7, 1971\r\n\tSid Abel: Oct. 16, 1969 – April 12, 1970\r\n\tBill Gadsby: Oct. 11, 1968 – Oct. 15, 1969\r\n\tSid Abel: Jan. 4, 1958 – March 31, 1968\r\n\tJimmy Skinner: Oct. 7, 1954 – Jan. 1, 1958\r\n\tTommy Ivan: Oct. 15, 1947 – April 16, 1954\r\n\tJack Adams: Nov. 15, 1927 – April 5, 1947\r\n\tDuke Keats: Feb. 24 – March 26, 1927\r\n\tArt Duncan: Nov. 18, 1926 – Feb. 22, 1927\r\n\t* Date range indicates first and last games coached during tenure (regular season or playoffs)\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   generalManagerHistory
    ## 1 Steve Yzerman: April 19, 2019 – Present\r\n\tKen Holland: July 18, 1997 – April 19, 2019\r\n\tJim Devellano: June 3, 1994 – July 18, 1997\r\n\tBryan Murray: July 13, 1990 – June 3, 1994\r\n\tJim Devellano: July 12, 1982 – July 11, 1990\r\n\tJimmy Skinner: April 11, 1980 – July 12, 1982\r\n\tTed Lindsay: March 16, 1977 – April 11, 1980\r\n\tAlex Delvecchio: May 21, 1974 – March 16, 1977\r\n\tJimmy Skinner: Feb. 6 – May 21, 1974\r\n\tNed Harkness: Jan. 8, 1971 – Feb. 6, 1974\r\n\tSid Abel: April 26, 1962 – Jan. 6, 1971\r\n\tJack Adams: May 14, 1927 – April 26, 1962\r\n\tArt Duncan: Oct. 18, 1926 – May 14, 1927\r\n\t* Date range indicates first and last days of tenure\r\n
    ##                                                                                                                                                                                                                                                                                   retiredNumbersSummary
    ## 1 1 – Terry Sawchuk (1949-55, 1957-64, 1968-69)\r\n\t4 – Red Kelly (1947-60)\r\n\t5 – Nicklas Lidstrom (1991-12)\r\n\t7 – Ted Lindsay (1947-57, 1964-65)\r\n\t9 – Gordie Howe (1946-71)\r\n\t10 – Alex Delvecchio (1951-73)\r\n\t12 – Sid Abel (1938-43, 1945-52)\r\n\t19 – Steve Yzerman (1983-06)\r\n

``` r
get.nhl.data(table.name = "franchise-team-totals")
```

    ## # A tibble: 105 x 30
    ##       id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed
    ##    <int>           <int>         <int>       <int>      <int>       <int>
    ##  1     1               1      19821983          23          2        2993
    ##  2     2               1      19821983          23          3         257
    ##  3     3               1      19721973          22          2        3788
    ##  4     4               1      19721973          22          3         309
    ##  5     5               1      19261927          10          2        6560
    ##  6     6               1      19261927          10          3         518
    ##  7     7               1      19671968          16          3         449
    ##  8     8               1      19671968          16          2        4171
    ##  9     9               1      19671968          17          2        4171
    ## 10    10               1      19671968          17          3         391
    ## # … with 95 more rows, and 24 more variables: goalsAgainst <int>,
    ## #   goalsFor <int>, homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>,
    ## #   homeWins <int>, lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>,
    ## #   roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>,
    ## #   teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
get.nhl.data(modifier = "person.names", id = 26, season = 20142015)
```

    ## # A tibble: 1 x 27
    ##      id name  link  abbreviation teamName locationName firstYearOfPlay shortName
    ##   <int> <chr> <chr> <chr>        <chr>    <chr>        <chr>           <chr>    
    ## 1    12 Caro… /api… CAR          Hurrica… Carolina     1979            Carolina 
    ## # … with 19 more variables: officialSiteUrl <chr>, franchiseId <int>,
    ## #   active <lgl>, venue.id <int>, venue.name <chr>, venue.link <chr>,
    ## #   venue.city <chr>, venue.timeZone.id <chr>, venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>, division.id <int>, division.name <chr>,
    ## #   division.link <chr>, conference.id <int>, conference.name <chr>,
    ## #   conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

``` r
get.nhl.data(modifier = "team.stats", id = 26)
```

    ## # A tibble: 2 x 65
    ##      id name        link      abbreviation teamName locationName firstYearOfPlay
    ##   <int> <chr>       <chr>     <chr>        <chr>    <chr>        <chr>          
    ## 1    12 Carolina H… /api/v1/… CAR          Hurrica… Carolina     1979           
    ## 2    12 Carolina H… /api/v1/… CAR          Hurrica… Carolina     1979           
    ## # … with 58 more variables: stat.gamesPlayed <int>, stat.wins <chr>,
    ## #   stat.losses <chr>, stat.ot <chr>, stat.pts <chr>, stat.ptPctg <chr>,
    ## #   stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>,
    ## #   stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>,
    ## #   stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>,
    ## #   stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>,
    ## #   team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>,
    ## #   type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.id <int>, venue.name <chr>,
    ## #   venue.link <chr>, venue.city <chr>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>,
    ## #   division.name <chr>, division.link <chr>, conference.id <int>,
    ## #   conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>,
    ## #   franchise.teamName <chr>, franchise.link <chr>

# Exporatory Data Analysis

## Grabbing data and combine

``` r
# get all information from "franchise-team-totals"
team.total.raw <- get.nhl.data(table.name = "franchise-team-totals") 

# filter to get only the 31 active teams
team.total <- team.total.raw %>% rename(abbreviation = triCode) %>% 
  select(activeFranchise, gameTypeId, lastSeasonId, everything()) %>%
  filter(activeFranchise==1 & gameTypeId==2 & is.na(lastSeasonId))

# get division data for different teams from "team.stats"
division.raw <- get.nhl.data(modifier = "team.stats")

# select only odd rows (does not include ranks), then select useful columns
row.odd2 <- seq_len(nrow(division.raw)) %% 2  ## Create row indicator
row.odd2 ## Print row indicator
```

    ##  [1] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
    ## [39] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1

``` r
division <- division.raw[row.odd2 == 1, ] %>%
  select(franchiseId, name, abbreviation, teamName, venue.name, venue.city, 
         division.id, division.name, conference.id, conference.name)

# find common columns between the 2 data frames
common.names.dv <- intersect(names(team.total), names(division))
```

``` r
# combine the 2 tables (join matching rows from division to team.total) together using "franchiseId" as index
new.data <- left_join(team.total, division, by="franchiseId")
new.data
```

    ## # A tibble: 31 x 39
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId
    ##              <int>      <int>        <int> <int>         <int>       <int>
    ##  1               1          2           NA     1      19821983          23
    ##  2               1          2           NA     3      19721973          22
    ##  3               1          2           NA     5      19261927          10
    ##  4               1          2           NA     8      19671968          16
    ##  5               1          2           NA     9      19671968          17
    ##  6               1          2           NA    11      19241925           6
    ##  7               1          2           NA    13      19701971          19
    ##  8               1          2           NA    16      19171918           1
    ##  9               1          2           NA    17      19921993          30
    ## 10               1          2           NA    19      19271928           5
    ## # … with 21 more rows, and 33 more variables: gamesPlayed <int>,
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

    ## # A tibble: 31 x 45
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId
    ##              <int>      <int>        <int> <int>         <int>       <int>
    ##  1               1          2           NA     1      19821983          23
    ##  2               1          2           NA     3      19721973          22
    ##  3               1          2           NA     5      19261927          10
    ##  4               1          2           NA     8      19671968          16
    ##  5               1          2           NA     9      19671968          17
    ##  6               1          2           NA    11      19241925           6
    ##  7               1          2           NA    13      19701971          19
    ##  8               1          2           NA    16      19171918           1
    ##  9               1          2           NA    17      19921993          30
    ## 10               1          2           NA    19      19271928           5
    ## # … with 21 more rows, and 39 more variables: gamesPlayed <int>,
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

## Read data from 2 different endpoints and combine

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
# find common columns between the two tibbles
common.names <- intersect(names(goalie.records), names(skater.records))

# only keep common columns in each tibble
goalie.records <- goalie.records %>% select(all_of(common.names))
skater.records <- skater.records %>% select(all_of(common.names))

# combine the 2 tibbles together by binding rows
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
# read from goalie records endpoint for all teams
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
# read from skater records endpoint for all teams
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
# find common columns between the two tibbles
common.names <- intersect(names(goalie.records2), names(skater.records2))

# only keep common columns in each tibble
goalie.records2 <- goalie.records2 %>% select(all_of(common.names))
skater.records2 <- skater.records2 %>% select(all_of(common.names))

# combine the 2 tibbles together
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
# find common columns between the above tibble and the previous division tibble
common.names.players <- intersect(names(players.record.all), names(division))

# combine 2 tibbles together (matching rows from division to players.record.all) using franchiseId as index
new.data.players <- left_join(players.record.all, division, by="franchiseId")
```

## Contingency Tables

``` r
# see how many players are in each team
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
# see how many active and inactive players are in each time
table(players.record.all$activePlayer)
```

    ## 
    ## FALSE  TRUE 
    ## 16171  2116

``` r
# find the count of active/inactive players by franchise
contingency.table.players <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>% 
  kable(caption = "Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by Franchise")
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
# find the proportion by margin=2 (by column)
contingency.table.players.prop <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>%
  prop.table(margin=2)*100
contingency.table.players.prop %>% 
  kable(caption = "Proportion of all Active (TRUE) / Inactive (FALSE) Players by Franchise") 
```

<table>
<caption>
Proportion of all Active (TRUE) / Inactive (FALSE) Players by Franchise
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
# see how many teams are in each division
table(new.data$division.name)
```

    ## 
    ## Discover Central       Honda West  MassMutual East     Scotia North 
    ##                8                8                8                7

``` r
# see how many games are won among all teams
table(new.data$wins)
```

    ## 
    ##  173  214  382  678  759  827  852  889  971  985  990 1007 1070 1084 1394 1469 
    ##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
    ## 1497 1649 1688 1700 1754 1805 1903 1929 2079 2812 2873 2883 2891 3241 3473 
    ##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1

``` r
# find the count of franchises w/i a division that had over 1500 wins
contingency.table2 <- table(new.data$division.name, new.data$wins>1500) %>%
  kable(caption = "Contingency Table of Number of Franchises in a Division that had Over 1500 Wins")
contingency.table2
```

<table>
<caption>
Contingency Table of Number of Franchises in a Division that had Over
1500 Wins
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
6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

``` r
# find the proportion by margin=2 (by columns)
contingency.table2.prop <- table(new.data$division.name, new.data$wins>1500) %>%
  prop.table(margin=2)*100
contingency.table2.prop %>%
  kable(caption = "Proportion of Over 1500 Win Teams By Division")
```

<table>
<caption>
Proportion of Over 1500 Win Teams By Division
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
35.294118
</td>
<td style="text-align:right;">
14.28571
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
35.294118
</td>
<td style="text-align:right;">
14.28571
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
5.882353
</td>
<td style="text-align:right;">
50.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
23.529412
</td>
<td style="text-align:right;">
21.42857
</td>
</tr>
</tbody>
</table>

## Numerical Summaries for Categorical Variables

``` r
# find the average games played by each position
new.data.players %>% group_by(positionCode) %>% 
  summarize(avg.games.played = mean(gamesPlayed))
```

    ## # A tibble: 5 x 2
    ##   positionCode avg.games.played
    ##   <chr>                   <dbl>
    ## 1 C                       121. 
    ## 2 D                       117. 
    ## 3 G                        89.4
    ## 4 L                       111. 
    ## 5 R                       118.

``` r
# find the average games played by each position and division name
new.data.players %>% drop_na() %>% group_by(positionCode, division.name) %>% 
  summarize(avg.games.played = mean(gamesPlayed))
```

    ## # A tibble: 20 x 3
    ## # Groups:   positionCode [5]
    ##    positionCode division.name    avg.games.played
    ##    <chr>        <chr>                       <dbl>
    ##  1 C            Discover Central             150.
    ##  2 C            Honda West                   143.
    ##  3 C            MassMutual East              140.
    ##  4 C            Scotia North                 151.
    ##  5 D            Discover Central             139.
    ##  6 D            Honda West                   143.
    ##  7 D            MassMutual East              143.
    ##  8 D            Scotia North                 153.
    ##  9 G            Discover Central             329.
    ## 10 G            Honda West                   152.
    ## 11 G            MassMutual East              327.
    ## 12 G            Scotia North                 238.
    ## 13 L            Discover Central             125.
    ## 14 L            Honda West                   139.
    ## 15 L            MassMutual East              132.
    ## 16 L            Scotia North                 125.
    ## 17 R            Discover Central             132.
    ## 18 R            Honda West                   138.
    ## 19 R            MassMutual East              144.
    ## 20 R            Scotia North                 153.

``` r
# find the 5 number summary (and mean) of the percentage of total games won by all teams
new.data %>% select(perc.total.games.win) %>% summary()
```

    ##  perc.total.games.win
    ##  Min.   :39.93       
    ##  1st Qu.:44.16       
    ##  Median :45.94       
    ##  Mean   :46.56       
    ##  3rd Qu.:48.67       
    ##  Max.   :59.45

``` r
# find the 5 number summary (and mean) of the percentage of total games lost by all teams
new.data %>% select(perc.total.games.loss) %>% summary()
```

    ##  perc.total.games.loss
    ##  Min.   :32.30        
    ##  1st Qu.:38.55        
    ##  Median :40.38        
    ##  Mean   :40.02        
    ##  3rd Qu.:41.73        
    ##  Max.   :48.88

``` r
# find the average percent of games won for home vs. road games by division
new.data %>% select(-3) %>% drop_na() %>% 
  group_by(division.name) %>% 
  summarize(avg.perc.home.win = mean(perc.home.win), avg.perc.road.win = mean(perc.road.win))
```

    ## # A tibble: 4 x 3
    ##   division.name    avg.perc.home.win avg.perc.road.win
    ##   <chr>                        <dbl>             <dbl>
    ## 1 Discover Central              51.7              39.6
    ## 2 Honda West                    52.9              41.0
    ## 3 MassMutual East               53.6              39.4
    ## 4 Scotia North                  52.5              38.9

``` r
# find the average percent of games lost for home vs. road games by division
new.data %>% select(-3) %>% drop_na() %>% 
  group_by(division.name) %>% 
  summarize(avg.perc.home.loss = mean(perc.home.loss), avg.perc.road.loss = mean(perc.road.loss))
```

    ## # A tibble: 4 x 3
    ##   division.name    avg.perc.home.loss avg.perc.road.loss
    ##   <chr>                         <dbl>              <dbl>
    ## 1 Discover Central               34.4               46.3
    ## 2 Honda West                     33.9               45.5
    ## 3 MassMutual East                32.7               46.4
    ## 4 Scotia North                   33.8               47.2

## Plots

### Bar plot

``` r
ggplot(data = new.data, aes(x = division.name)) + 
  geom_bar(aes(fill = as.factor(conference.name)), position = "dodge") + 
  labs(x = "Division", title = "Bar Plot of Number of Franchise in Each Division with Indication of Conference") + 
  scale_fill_discrete(name = "Conference Name", labels = c("Eastern Conference", "Western Conference"))
```

![](Project1-1_files/figure-gfm/plot%20-%20bar%20-%20new.data-1.png)<!-- -->

``` r
ggplot(data = players.record.all, aes(x = positionCode)) + 
  geom_bar(aes(fill = as.factor(activePlayer)), position = "dodge") + 
  labs(x = "Position", title = "Number of Players by Position with Indication of Active or Inactive Status") + 
  scale_x_discrete(labels = c("Center", "Defense", "Goalie", "Left Wing", "Right Wing")) +
  scale_fill_discrete(name = "Active/Inactive Players", labels = c("Inactive", "Active"))
```

![](Project1-1_files/figure-gfm/plot%20-%20bar%20-%20players.record.all-1.png)<!-- -->

### Histogram

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  labs(x = "Games Played", title = "Number of Games Played by each (of all) Players")
```

![](Project1-1_files/figure-gfm/plot%20-%20histogram%20-%20players.record.all-1.png)<!-- -->

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  facet_grid(~ positionCode, 
             labeller = as_labeller(c(C = "Center", D = "Defense", G = "Goalie", L = "Left Wing", R = "Right Wing"))) +
  labs(x = "Games Played", title = "Number of Games Played by each (of all) Players by Position Played")
```

![](Project1-1_files/figure-gfm/plot%20-%20histogram%20facet%20-%20player.record.all-1.png)<!-- -->

### Boxplot

``` r
ggplot(data = new.data, aes(x = division.name, y = gamesPlayed)) +
  geom_boxplot(fill = "maroon") +
  stat_summary(fun = mean, geom = "line", aes(group = conference.name, col = conference.name)) +
  labs(x = "Division Name", y = "# of Games Played", title = "Number of Games Played for Each Division")
```

![](Project1-1_files/figure-gfm/plot%20-%20box%20plot%20-%20new.data-1.png)<!-- -->

### Scatter Plot

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Each Franchise")
```

![](Project1-1_files/figure-gfm/plot%20-%20scatter%20-%20new.data-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  facet_grid(~ conference.name, labeller = as_labeller(c(Eastern = "Eastern Conference", 
                                      Western = "Western Conference"))) +
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Franchies in Each Division")
```

![](Project1-1_files/figure-gfm/plot%20-%20scatter%20facet-1.png)<!-- -->

### Scatter Plot using Group

``` r
ggplot(data = new.data, aes(x = homeWins, y = homeLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "Home Games Won", y = "Home Games Lost", title = "Number of Home Games Won vs. Lost for Each Franchise by Conference")
```

![](Project1-1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20home%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = roadWins, y = roadLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "Road Games Won", y = "Road Games Lost", title = "Number of Road Games Won vs. Lost for Each Franchise by Conference")
```

![](Project1-1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20road%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.win, y = perc.road.win)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Won", y = "% Road Games Won", title = "% of Home Games Won vs. Road Games Won for Each Franchise by Conference")
```

![](Project1-1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20won%20games-1.png)<!-- -->

``` r
ggplot(data = new.data, aes(x = perc.home.loss, y = perc.road.loss)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +
  labs(x = "% of Home Games Loss", y = "% Road Games Loss", title = "% of Home Games Loss vs. Road Games Loss for Each Franchise by Conference")
```

![](Project1-1_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20lost%20games-1.png)<!-- -->
