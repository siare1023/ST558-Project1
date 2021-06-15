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
library(qwraps2)
library(rmarkdown)
library(data.table)
library(kableExtra)
```

## NHL records API

``` r
get.records <- function(table.name, id=NULL, most.recent.id=NULL, ...) {
  base.url <- "https://records.nhl.com/site/api"
  
  if (!is.null(table.name)) {  ## has a table name
    if (table.name %in% ("franchise") & ((!is.null(id)) | (!is.null(most.recent.id)))) {  ## table name is this one and given id
      return ("The table selected cannot be returned with the specified ID and or most recent team ID input.")
    }  
    
    if (is.null(id) & is.null(most.recent.id)) {  ## has table name but no id no most recent team id
      full.url <- paste0(base.url, "/", table.name)
    }
    
    if (table.name %in% c("franchise-team-totals",
                          "franchise-season-records", 
                          "franchise-goalie-records", 
                          "franchise-skater-records") & (!is.null(id)) & (is.null(most.recent.id))) {  ## table name is one of the 4 and given id
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=franchiseId=", id)
    }
    
    if (table.name %in% c("franchise-team-totals",
                          "franchise-season-records", 
                          "franchise-goalie-records", 
                          "franchise-skater-records") & (!is.null(id)) & (!is.null(most.recent.id))){  ## table name is one of the 4 and given id and given most.recent id
      return (("Only ID and table name are used, please delete most recent team ID entry."))
    }
    
    if (table.name %in% c("franchise-team-totals",
                          "franchise-season-records", 
                          "franchise-goalie-records", 
                          "franchise-skater-records") & (is.null(id)) & (!is.null(most.recent.id))){  ## table name is one of the 4 and only given most.recent id
      return (("Only ID and table name are used, please delete most recent team ID and try using ID instead."))
    }    

    if (table.name %in% ("franchise-detail") & (is.null(id)) & (!is.null(most.recent.id))) {  ## table name is this one and given most recent id
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id)
    }
    
    if (table.name %in% ("franchise-detail") & (!is.null(id)) & (!is.null(most.recent.id))) {  ## table name is this one and given most recent id and id
      return ("Only most recent team ID and table name are used, please delete ID entry.")
    }
    
    if (table.name %in% ("franchise-detail") & (!is.null(id)) & (is.null(most.recent.id))) {  ## table name is this one and given only id
      return ("Please provide the most recent team ID to get results.")
    }  
  
    nfl.records <- GET(full.url)
    nfl.records.txt <- content(nfl.records, "text")  ## convert to JSON text form
    nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
    
    return (nfl.records.json$data)
    
  }
  
  else {  ## if no table name
    return ("Table name is invalid.")
  }
}
```

test different scenarios

``` r
get.records("franchise-goalie-records", id=26)
```

    ##      id activePlayer firstName franchiseId       franchiseName gameTypeId gamesPlayed     lastName losses
    ## 1   336        FALSE       Tom          26 Carolina Hurricanes          2          34     Barrasso     12
    ## 2   363        FALSE   Richard          26 Carolina Hurricanes          2           6      Brodeur      2
    ## 3   369        FALSE      Sean          26 Carolina Hurricanes          2         256        Burke    120
    ## 4   411        FALSE      Mark          26 Carolina Hurricanes          2           3  Fitzpatrick      2
    ## 5   425        FALSE      John          26 Carolina Hurricanes          2         122      Garrett     57
    ## 6   430        FALSE     Mario          26 Carolina Hurricanes          2          23     Gosselin     13
    ## 7   470        FALSE       Pat          26 Carolina Hurricanes          2           5    Jablonski      4
    ## 8   490        FALSE      Mike          26 Carolina Hurricanes          2         252         Liut    111
    ## 9   508        FALSE      Kirk          26 Carolina Hurricanes          2           8       McLean      2
    ## 10  525        FALSE      Greg          26 Carolina Hurricanes          2         219       Millen    120
    ## 11  662        FALSE     Frank          26 Carolina Hurricanes          2          54  Pietrangelo     27
    ## 12  676        FALSE      Jeff          26 Carolina Hurricanes          2          37        Reese     17
    ## 13  698        FALSE     Peter          26 Carolina Hurricanes          2         178 Sidorkiewicz     79
    ## 14  704        FALSE        Al          26 Carolina Hurricanes          2          30        Smith     10
    ## 15  713        FALSE        Ed          26 Carolina Hurricanes          2          19   Staniowski      9
    ## 16  743        FALSE      Mike          26 Carolina Hurricanes          2          69       Veisor     37
    ## 17  758        FALSE     Steve          26 Carolina Hurricanes          2          94        Weeks     40
    ## 18  310        FALSE    Arturs          26 Carolina Hurricanes          2         309         Irbe    122
    ## 19  789        FALSE    Trevor          26 Carolina Hurricanes          2          72         Kidd     31
    ## 20  868        FALSE     Tyler          26 Carolina Hurricanes          2          12         Moss      6
    ## 21  872        FALSE     Kevin          26 Carolina Hurricanes          2         119       Weekes     54
    ## 22  879        FALSE     Jamie          26 Carolina Hurricanes          2          14        Storr      8
    ## 23  881        FALSE      Eric          26 Carolina Hurricanes          2           9      Fichaud      5
    ## 24  907        FALSE      John          26 Carolina Hurricanes          2          45      Grahame     20
    ## 25  920        FALSE     Brian          26 Carolina Hurricanes          2          10      Boucher      6
    ## 26  993        FALSE   Michael          26 Carolina Hurricanes          2          33     Leighton     14
    ## 27 1001        FALSE       Dan          26 Carolina Hurricanes          2          19        Ellis      8
    ## 28 1034        FALSE    Martin          26 Carolina Hurricanes          2          60       Gerber     14
    ## 29 1262         TRUE    Curtis          26 Carolina Hurricanes          2          33   McElhinney     11
    ## 30  277        FALSE       Cam          26 Carolina Hurricanes          2         668         Ward    244
    ## 31 1083         TRUE     Anton          26 Carolina Hurricanes          2          70     Khudobin     31
    ## 32 1287         TRUE     James          26 Carolina Hurricanes          2          47       Reimer     11
    ## 33 1121        FALSE     Scott          26 Carolina Hurricanes          2          51      Darling     25
    ## 34 1158        FALSE     Eddie          26 Carolina Hurricanes          2          54         Lack     21
    ##                             mostGoalsAgainstDates mostGoalsAgainstOneGame         mostSavesDates mostSavesOneGame
    ## 1              2001-12-30, 2001-12-18, 2001-11-29                       5             2001-12-10               40
    ## 2                                      1988-03-09                       4             1988-04-03               31
    ## 3                                      1992-12-11                       9             1994-01-01               51
    ## 4                                      2000-02-15                       5             2000-02-15               40
    ## 5  1982-01-02, 1980-10-11, 1980-03-08, 1980-02-26                       9             1981-02-14               50
    ## 6                          1993-04-10, 1993-03-24                       6             1993-03-22               45
    ## 7                                      1998-01-03                       6             1997-12-27               26
    ## 8                                      1985-10-23                       9             1985-12-19               45
    ## 9                          1998-03-06, 1998-01-06                       4             1998-03-06               46
    ## 10                         1983-02-23, 1982-12-06                      11             1982-01-30               52
    ## 11                                     1992-10-31                       7             1992-03-29               48
    ## 12                                     1994-02-02                       9 1995-01-22, 1994-04-10               38
    ## 13             1991-01-29, 1990-12-29, 1989-02-25                       8             1992-01-16               39
    ## 14                                     1980-03-26                       7             1980-04-01               36
    ## 15                         1984-01-03, 1983-12-27                       7             1984-02-05               38
    ## 16                                     1980-11-23                      11             1982-11-02               52
    ## 17                                     1984-12-03                       9             1987-04-05               43
    ## 18                                     2002-01-12                       7             1999-01-30               44
    ## 19                                     1999-02-13                       6             1998-03-02               43
    ## 20                                     2000-10-31                       5             2000-10-14               32
    ## 21                                     2003-02-07                       7             2003-02-18               43
    ## 22                                     2003-11-21                       5             2003-10-25               28
    ## 23                                     1999-11-03                       6             1999-12-23               31
    ## 24                                     2007-12-01                       8             2006-11-22               44
    ## 25             2012-03-31, 2011-10-29, 2011-10-22                       5             2011-12-03               37
    ## 26                                     2009-10-31                       6             2008-12-07               38
    ## 27                         2013-04-04, 2013-02-23                       5 2013-03-19, 2013-01-25               40
    ## 28 2006-04-08, 2006-04-03, 2005-12-28, 2005-12-26                       5             2005-10-24               45
    ## 29                                     2019-03-08                       8             2018-11-27               48
    ## 30             2017-01-20, 2007-01-27, 2005-11-12                       7             2008-10-25               57
    ## 31                                     2015-04-02                       6             2014-03-18               46
    ## 32 2021-02-07, 2021-02-04, 2020-02-08, 2019-12-27                       5             2019-10-08               47
    ## 33                                     2017-12-19                       8             2018-03-27               41
    ## 34                                     2016-10-22                       6             2016-03-17               44
    ##     mostShotsAgainstDates mostShotsAgainstOneGame mostShutoutsOneSeason                  mostShutoutsSeasonIds mostWinsOneSeason
    ## 1              2001-12-10                      43                     2                               20012002                13
    ## 2              1988-04-03                      34                     0                               19871988                 4
    ## 3  1996-01-27, 1994-01-01                      54                     4                     19951996, 19961997                28
    ## 4              2000-02-15                      45                     0                               19992000                 0
    ## 5              1981-02-14                      57                     0           19791980, 19801981, 19811982                16
    ## 6              1993-03-22                      50                     0                     19921993, 19931994                 5
    ## 7              1997-12-27                      27                     0                               19971998                 1
    ## 8              1987-03-10                      48                     4                               19861987                31
    ## 9              1998-03-06                      50                     0                               19971998                 4
    ## 10             1982-01-30                      54                     2                               19831984                21
    ## 11             1992-03-29                      50                     0           19911992, 19921993, 19931994                 5
    ## 12             1995-01-22                      40                     1                     19931994, 19951996                 5
    ## 13             1992-01-16                      43                     4                               19881989                22
    ## 14             1980-04-01                      41                     2                               19791980                11
    ## 15             1984-02-05                      41                     0                     19831984, 19841985                 6
    ## 16             1982-11-02                      59                     1                     19801981, 19821983                 6
    ## 17             1987-04-05                      49                     2                               19841985                13
    ## 18             1999-01-30                      45                     6                     19981999, 20002001                37
    ## 19             1998-03-02                      44                     3                               19971998                21
    ## 20             2000-10-14                      34                     0                               20002001                 1
    ## 21             2003-02-18                      47                     6                               20032004                23
    ## 22             2003-10-25                      32                     0                               20032004                 0
    ## 23             1999-12-23                      35                     1                               19992000                 3
    ## 24             2006-11-22                      47                     0                     20062007, 20072008                10
    ## 25             2011-12-03                      40                     0                               20112012                 1
    ## 26             2009-02-05                      40                     0 20072008, 20082009, 20092010, 20162017                 6
    ## 27             2013-03-19                      43                     1                               20122013                 6
    ## 28             2005-10-24                      47                     3                               20052006                38
    ## 29             2018-11-27                      49                     2                               20182019                20
    ## 30             2008-10-25                      60                     6                               20082009                39
    ## 31             2014-03-18                      47                     1                     20132014, 20142015                19
    ## 32             2019-10-08                      50                     3                               20192020                15
    ## 33             2018-03-27                      45                     0                     20172018, 20182019                13
    ## 34             2016-03-17                      48                     2                               20152016                12
    ##    mostWinsSeasonIds overtimeLosses playerId positionCode rookieGamesPlayed rookieShutouts rookieWins seasons shutouts ties wins
    ## 1           20012002             NA  8445275            G                NA             NA         NA       1        2    5   13
    ## 2           19871988             NA  8445694            G                NA             NA         NA       1        0    0    4
    ## 3           19951996             NA  8445769            G                NA             NA         NA       6       10   24  100
    ## 4           19992000             NA  8446829            G                NA             NA         NA       1        0    0    0
    ## 5           19791980             NA  8447066            G                NA             NA         NA       3        0   27   36
    ## 6           19921993             NA  8447303            G                NA             NA         NA       2        0    1    5
    ## 7           19971998             NA  8448207            G                NA             NA         NA       1        0    0    1
    ## 8           19861987             NA  8448865            G                NA             NA         NA       6       13   17  115
    ## 9           19971998             NA  8449474            G                NA             NA         NA       1        0    0    4
    ## 10          19831984             NA  8449627            G                NA             NA         NA       4        4   33   62
    ## 11          19931994             NA  8450390            G                NA             NA         NA       3        0    3   12
    ## 12          19931994             NA  8450743            G                NA             NA         NA       3        2    4    9
    ## 13          19881989             NA  8451369            G                44              4         22       5        8   24   71
    ## 14          19791980             NA  8451474            G                NA             NA         NA       1        2    8   11
    ## 15          19831984             NA  8451655            G                NA             NA         NA       2        0    1    6
    ## 16          19801981             NA  8452205            G                NA             NA         NA       4        2    9   17
    ## 17          19851986             NA  8452355            G                NA             NA         NA       4        4    6   41
    ## 18          20002001             NA  8456692            G                NA             NA         NA       6       20   44  130
    ## 19          19971998             NA  8456830            G                NA             NA         NA       2        5    9   28
    ## 20          20002001             NA  8459451            G                NA             NA         NA       1        0    0    1
    ## 21          20032004             NA  8459463            G                NA             NA         NA       3       11   20   39
    ## 22          20032004             NA  8460497            G                NA             NA         NA       1        0    2    0
    ## 23          19992000             NA  8460506            G                NA             NA         NA       1        1    1    3
    ## 24          20062007              3  8460715            G                NA             NA         NA       2        0   NA   15
    ## 25          20112012              1  8462052            G                NA             NA         NA       1        0   NA    1
    ## 26          20082009              2  8468038            G                NA             NA         NA       4        0   NA   10
    ## 27          20122013              2  8468540            G                NA             NA         NA       1        1   NA    6
    ## 28          20052006              6  8469675            G                NA             NA         NA       1        3   NA   38
    ## 29          20182019              2  8470147            G                NA             NA         NA       1        2    0   20
    ## 30          20082009             84  8470320            G                28              0         14      13       27    0  318
    ## 31          20132014              7  8471418            G                NA             NA         NA       2        2   NA   27
    ## 32          20202021              4  8473503            G                NA             NA         NA       2        3    0   29
    ## 33          20172018              9  8474152            G                NA             NA         NA       2        0    0   15
    ## 34          20152016              9  8475663            G                NA             NA         NA       2        3   NA   20
    ##  [ reached 'max' / getOption("max.print") -- omitted 4 rows ]

``` r
#get.records("franchise-team-totals", 26, 3)
#get.records("franchise-detail", id=23, most.recent.id = 23)
#get.records("franchise-detail", most.recent.id = 23)
```

## NHL stats API

``` r
get.stat <- function(modifier = "team.stats", most.recent.id=NULL, ...) {
  base.url2 <- "https://statsapi.web.nhl.com/api/v1/teams"
  
  if (!is.null(most.recent.id)) {  ## modifier given and id is given
    full.url2 <- paste0(base.url2, "/", most.recent.id, "?expand=team.stats")
  }  
  
  else if (is.null(most.rencent.id)) {  ## modifier given and no id
    full.url2 <- paste0(base.url2, "/", "?expand=team.stats")
  }
  
  else if (modifier != "team.stats") {
    return ("team.stats is the only accepted modifier, please try again.")
  }
  
  nfl.stats <- GET(full.url2)
  nfl.stats.txt <- content(nfl.stats, "text")  ## convert to JSON text form
  nfl.stats.json <- fromJSON(nfl.stats.txt, flatten=TRUE)  ## convert to list
    
  return (nfl.stats.json$teams)  
}
```

``` r
#get.stat(most.recent.id = 23)
#get.stat()
#get.stat(26)
```

## Wrapper Function

``` r
get.nhl.data <- function (table.name=NULL, modifier=NULL, id=NULL, most.recent.id=NULL, season=NULL, ...) {
  # yes table name, yes modifier
  if (!is.null(table.name) & !is.null(modifier)) {
    stop ("Cannot use both table name and modifier to get back information.")
  }
  
  # yes table name
  else if (!is.null(table.name) & is.null(id) & is.null(modifier)) {
    output <- get.records(table.name)
  }  
  
  # yes table name, yes id, no modifier
  else if (!is.null(table.name) & !is.null(id) & is.null(modifier)) {
    output <- get.records(table.name, id)
  }
  
  # yes table name, yes most recent id, no modifier
  else if (!is.null(table.name) & !is.null(most.recent.id) & is.null(modifier)) {
    output <- get.records(table.name, most.recent.id)
  }
  
  else if (!is.null(table.name) & !is.null(id) & !is.null(most.recent.id))
    return("Only ID and table name are used, please delete most recent team ID entry.")
  
  # yes modifier
  else if (!is.null(modifier) & is.null(table.name) & is.null(most.recent.id)) {
    output <- get.stat2(modifier)
  }
  
  # yes modifier, yes most recent id
  else if (!is.null(modifier) & is.null(table.name) & !is.null(most.recent.id)) {
    output <- get.stat2(modifier, most.recent.id)
  }
  
  # yes modifier, yes season
  else if (!is.null(modifier) & is.null(table.name) & !is.null(season)) {
    output <- get.stat2(modifier, season)
  }
  
  # yes modifier, yes multiple most recent id
  else if (!is.null(modifier) & is.null(table.name) & !is.null(most.recent.id) & !is.null(most.recent.id2)) {
    output <- get.stat2(modifier, most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4)
  }
  
  return(output)
}
```

``` r
get.stat2 <- function(modifier, most.recent.id=NULL, season=NULL, most.recent.id2=NULL, most.recent.id3=NULL, most.recent.id4=NULL, ...) {
  base.url3 <- "https://statsapi.web.nhl.com/api/v1/teams"
  
  if (modifier %in% c("team.roster",
                      "person.names",
                      "team.schedule.next", 
                      "team.schedule.previous",  
                      "team.stats") & is.null(most.recent.id)) {  ## modifier is 1 of the 5 and no most recent id given
    
    nfl.stats3 <- GET(paste0(base.url3, "?expand=", modifier))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }

  else if (modifier %in% c("team.roster",
                           "person.names",
                           "team.schedule.next",
                           "team.schedule.previous",
                           "team.stats") & !is.null(most.recent.id)) {  ## modifier is 1 of the 5 and yes most recent id

    nfl.stats3 <- GET(paste0(base.url3, "/", most.recent.id, "?expand=", modifier))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }
  
  else if (modifier %in% ("team.roster&season") & is.null(most.recent.id) & !is.null(season)) {  ## modifier is this and no most recent id and yes season
    nfl.stats3 <- GET(paste0(base.url3, "?expand=", modifier, "=", season))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }
  
  else if (modifier %in% ("team.roster&season") & !is.null(most.recent.id) & !is.null(season)) {  ## modifier is this and yes most recent id and yes season
    nfl.stats3 <- GET(paste0(base.url3, "/", most.recent.id, "?expand=", modifier, "=", season))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }  
  
  else if (modifier %in% ("team.roster&season") & is.null(season)) {  ## modifier is this and no season
    return ("Must provide a season input, please try again.")
  }    

    
  else if (modifier %in% ("teamId") & !is.null(c(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4))) {  ## modifier is this and multiple most recent team id given
    
    nfl.stats3 <- GET(paste0(base.url3, "?", modifier, "=", 
                             paste(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4, sep = ",")))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }
  
  
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.null(most.recent.id)) {  ## modifier is this and no most recent id given
    nfl.stats3 <- GET(paste0(base.url3, "?stats=", modifier))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }
  
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & !is.null(most.recent.id)) {  ## modifier is this and yes most recent id
    nfl.stats3 <- GET(paste0(base.url3, "/", most.recent.id, "?stats=", modifier))
    nfl.stats3.txt <- content(nfl.stats3, "text")
    nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
    
    return (nfl.stats3.json$teams)
  }  


  else {
    stop ("Invalid input, please try again.")  
  }
}
```

try out function

``` r
get.stat2("team.roster&season", 12, 20142015)
```

    ##   id                name             link abbreviation   teamName locationName firstYearOfPlay shortName
    ## 1 12 Carolina Hurricanes /api/v1/teams/12          CAR Hurricanes     Carolina            1997  Carolina
    ##                      officialSiteUrl franchiseId active venue.id venue.name          venue.link venue.city venue.timeZone.id
    ## 1 http://www.carolinahurricanes.com/          26   TRUE     5066  PNC Arena /api/v1/venues/5066    Raleigh  America/New_York
    ##   venue.timeZone.offset venue.timeZone.tz division.id division.name division.nameShort        division.link division.abbreviation
    ## 1                    -4               EDT          18  Metropolitan              Metro /api/v1/divisions/18                     M
    ##   conference.id conference.name       conference.link franchise.franchiseId franchise.teamName        franchise.link
    ## 1             6         Eastern /api/v1/conferences/6                    26         Hurricanes /api/v1/franchises/26
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            roster.roster
    ## 1 81, 26, 6, 18, NA, 13, 39, 21, 5, 24, 9, 11, 15, 20, 24, 73, 38, 22, 47, 15, 62, 41, 72, 53, 52, 48, 47, 49, 32, 56, 28, 38, 30, 35, 8468493, 8468639, 8469476, 8469508, 8469531, 8470120, 8470302, 8470595, 8471284, 8471804, 8473482, 8473533, 8474052, 8474062, 8474089, 8474135, 8474519, 8474576, 8474661, 8475250, 8475294, 8475732, 8475753, 8475784, 8475826, 8475980, 8476397, 8476437, 8476465, 8476944, 8477496, 8477887, 8470320, 8471418, Ron Hainsey, John-Michael Liles, Tim Gleason, Jay McClement, Jay Harrison, Alexander Semin, Patrick Dwyer, Eric Staal, Andrej Sekera, Nathan Gerbe, Jiri Tlusty, Jordan Staal, Chris Terry, Riley Nash, Brad Malone, Brett Bellemore, Jack Hillen, Zach Boychuk, Michal Jordan, Andrej Nestrasil, Rasmus Rissanen, Danny Biega, Justin Faulk, Jeff Skinner, Justin Shugg, Brody Sutter, Keegan Lowe, Victor Rask, Ryan Murphy, Brendan Woods, Elias Lindholm, Patrick Brown, Cam Ward, Anton Khudobin, /api/v1/people/8468493, /api/v1/people/8468639, /api/v1/people/8469476, /api/v1/people/8469508, /api/v1/people/8469531, /api/v1/people/8470120, /api/v1/people/8470302, /api/v1/people/8470595, /api/v1/people/8471284, /api/v1/people/8471804, /api/v1/people/8473482, /api/v1/people/8473533, /api/v1/people/8474052, /api/v1/people/8474062, /api/v1/people/8474089, /api/v1/people/8474135, /api/v1/people/8474519, /api/v1/people/8474576, /api/v1/people/8474661, /api/v1/people/8475250, /api/v1/people/8475294, /api/v1/people/8475732, /api/v1/people/8475753, /api/v1/people/8475784, /api/v1/people/8475826, /api/v1/people/8475980, /api/v1/people/8476397, /api/v1/people/8476437, /api/v1/people/8476465, /api/v1/people/8476944, /api/v1/people/8477496, /api/v1/people/8477887, /api/v1/people/8470320, /api/v1/people/8471418, D, D, D, C, D, R, R, C, D, C, L, C, L, C, C, D, D, L, D, C, D, D, D, L, R, C, D, C, D, L, C, C, G, G, Defenseman, Defenseman, Defenseman, Center, Defenseman, Right Wing, Right Wing, Center, Defenseman, Center, Left Wing, Center, Left Wing, Center, Center, Defenseman, Defenseman, Left Wing, Defenseman, Center, Defenseman, Defenseman, Defenseman, Left Wing, Right Wing, Center, Defenseman, Center, Defenseman, Left Wing, Center, Center, Goalie, Goalie, Defenseman, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Goalie, Goalie, D, D, D, C, D, RW, RW, C, D, C, LW, C, LW, C, C, D, D, LW, D, C, D, D, D, LW, RW, C, D, C, D, LW, C, C, G, G
    ##               roster.link
    ## 1 /api/v1/teams/12/roster

``` r
get.stat2("team.roster&season", NULL, 20142015)
```

    ##    id                  name             link abbreviation     teamName locationName firstYearOfPlay    shortName
    ## 1   1     New Jersey Devils  /api/v1/teams/1          NJD       Devils   New Jersey            1982   New Jersey
    ## 2   2    New York Islanders  /api/v1/teams/2          NYI    Islanders     New York            1972 NY Islanders
    ## 3   3      New York Rangers  /api/v1/teams/3          NYR      Rangers     New York            1926   NY Rangers
    ## 4   4   Philadelphia Flyers  /api/v1/teams/4          PHI       Flyers Philadelphia            1967 Philadelphia
    ## 5   5   Pittsburgh Penguins  /api/v1/teams/5          PIT     Penguins   Pittsburgh            1967   Pittsburgh
    ## 6   6         Boston Bruins  /api/v1/teams/6          BOS       Bruins       Boston            1924       Boston
    ## 7   7        Buffalo Sabres  /api/v1/teams/7          BUF       Sabres      Buffalo            1970      Buffalo
    ## 8   8    Montréal Canadiens  /api/v1/teams/8          MTL    Canadiens     Montréal            1917     Montréal
    ## 9   9       Ottawa Senators  /api/v1/teams/9          OTT     Senators       Ottawa            1992       Ottawa
    ## 10 10   Toronto Maple Leafs /api/v1/teams/10          TOR  Maple Leafs      Toronto            1926      Toronto
    ## 11 12   Carolina Hurricanes /api/v1/teams/12          CAR   Hurricanes     Carolina            1997     Carolina
    ## 12 13      Florida Panthers /api/v1/teams/13          FLA     Panthers      Florida            1993      Florida
    ## 13 14   Tampa Bay Lightning /api/v1/teams/14          TBL    Lightning    Tampa Bay            1992    Tampa Bay
    ## 14 15   Washington Capitals /api/v1/teams/15          WSH     Capitals   Washington            1974   Washington
    ## 15 16    Chicago Blackhawks /api/v1/teams/16          CHI   Blackhawks      Chicago            1926      Chicago
    ## 16 17     Detroit Red Wings /api/v1/teams/17          DET    Red Wings      Detroit            1932      Detroit
    ## 17 18   Nashville Predators /api/v1/teams/18          NSH    Predators    Nashville            1998    Nashville
    ## 18 19       St. Louis Blues /api/v1/teams/19          STL        Blues    St. Louis            1967     St Louis
    ## 19 20        Calgary Flames /api/v1/teams/20          CGY       Flames      Calgary            1980      Calgary
    ## 20 21    Colorado Avalanche /api/v1/teams/21          COL    Avalanche     Colorado            1995     Colorado
    ## 21 22       Edmonton Oilers /api/v1/teams/22          EDM       Oilers     Edmonton            1979     Edmonton
    ## 22 23     Vancouver Canucks /api/v1/teams/23          VAN      Canucks    Vancouver            1970    Vancouver
    ## 23 24         Anaheim Ducks /api/v1/teams/24          ANA        Ducks      Anaheim            1993      Anaheim
    ## 24 25          Dallas Stars /api/v1/teams/25          DAL        Stars       Dallas            1993       Dallas
    ## 25 26     Los Angeles Kings /api/v1/teams/26          LAK        Kings  Los Angeles            1967  Los Angeles
    ## 26 28       San Jose Sharks /api/v1/teams/28          SJS       Sharks     San Jose            1991     San Jose
    ## 27 29 Columbus Blue Jackets /api/v1/teams/29          CBJ Blue Jackets     Columbus            2000     Columbus
    ## 28 30        Minnesota Wild /api/v1/teams/30          MIN         Wild    Minnesota            2000    Minnesota
    ## 29 52         Winnipeg Jets /api/v1/teams/52          WPG         Jets     Winnipeg            2011     Winnipeg
    ## 30 53       Arizona Coyotes /api/v1/teams/53          ARI      Coyotes      Arizona            2014      Arizona
    ##                       officialSiteUrl franchiseId active                        venue.name          venue.link   venue.city
    ## 1     http://www.newjerseydevils.com/          23   TRUE                 Prudential Center /api/v1/venues/null       Newark
    ## 2    http://www.newyorkislanders.com/          22   TRUE Nassau Veterans Memorial Coliseum /api/v1/venues/null    Uniondale
    ## 3      http://www.newyorkrangers.com/          10   TRUE             Madison Square Garden /api/v1/venues/5054     New York
    ## 4  http://www.philadelphiaflyers.com/          16   TRUE                Wells Fargo Center /api/v1/venues/5096 Philadelphia
    ## 5      http://pittsburghpenguins.com/          17   TRUE                  PPG Paints Arena /api/v1/venues/5034   Pittsburgh
    ## 6        http://www.bostonbruins.com/           6   TRUE                         TD Garden /api/v1/venues/5085       Boston
    ## 7              http://www.sabres.com/          19   TRUE                    KeyBank Center /api/v1/venues/5039      Buffalo
    ## 8           http://www.canadiens.com/           1   TRUE                       Bell Centre /api/v1/venues/5028     Montréal
    ## 9      http://www.ottawasenators.com/          30   TRUE              Canadian Tire Centre /api/v1/venues/5031       Ottawa
    ## 10         http://www.mapleleafs.com/           5   TRUE                  Scotiabank Arena /api/v1/venues/null      Toronto
    ## 11 http://www.carolinahurricanes.com/          26   TRUE                         PNC Arena /api/v1/venues/5066      Raleigh
    ## 12    http://www.floridapanthers.com/          33   TRUE                       BB&T Center /api/v1/venues/5027      Sunrise
    ## 13  http://www.tampabaylightning.com/          31   TRUE                      AMALIE Arena /api/v1/venues/null        Tampa
    ## 14 http://www.washingtoncapitals.com/          24   TRUE                 Capital One Arena /api/v1/venues/5094   Washington
    ## 15  http://www.chicagoblackhawks.com/          11   TRUE                     United Center /api/v1/venues/5092      Chicago
    ## 16    http://www.detroitredwings.com/          12   TRUE              Little Caesars Arena /api/v1/venues/5145      Detroit
    ## 17 http://www.nashvillepredators.com/          34   TRUE                 Bridgestone Arena /api/v1/venues/5030    Nashville
    ## 18       http://www.stlouisblues.com/          18   TRUE                 Enterprise Center /api/v1/venues/5076    St. Louis
    ## 19      http://www.calgaryflames.com/          21   TRUE             Scotiabank Saddledome /api/v1/venues/5075      Calgary
    ## 20  http://www.coloradoavalanche.com/          27   TRUE                        Ball Arena /api/v1/venues/5064       Denver
    ## 21     http://www.edmontonoilers.com/          25   TRUE                      Rogers Place /api/v1/venues/5100     Edmonton
    ## 22            http://www.canucks.com/          20   TRUE                      Rogers Arena /api/v1/venues/5073    Vancouver
    ## 23       http://www.anaheimducks.com/          32   TRUE                      Honda Center /api/v1/venues/5046      Anaheim
    ## 24        http://www.dallasstars.com/          15   TRUE          American Airlines Center /api/v1/venues/5019       Dallas
    ## 25            http://www.lakings.com/          14   TRUE                    STAPLES Center /api/v1/venues/5081  Los Angeles
    ## 26           http://www.sjsharks.com/          29   TRUE            SAP Center at San Jose /api/v1/venues/null     San Jose
    ## 27        http://www.bluejackets.com/          36   TRUE                  Nationwide Arena /api/v1/venues/5059     Columbus
    ## 28               http://www.wild.com/          37   TRUE                Xcel Energy Center /api/v1/venues/5098     St. Paul
    ## 29           http://winnipegjets.com/          35   TRUE                    Bell MTS Place /api/v1/venues/5058     Winnipeg
    ## 30     http://www.arizonacoyotes.com/          28   TRUE                  Gila River Arena /api/v1/venues/5043     Glendale
    ##    venue.id   venue.timeZone.id venue.timeZone.offset venue.timeZone.tz division.id division.name division.nameShort
    ## 1        NA    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 2        NA    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 3      5054    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 4      5096    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 5      5034    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 6      5085    America/New_York                    -4               EDT          17      Atlantic                ATL
    ## 7      5039    America/New_York                    -4               EDT          17      Atlantic                ATL
    ## 8      5028    America/Montreal                    -4               EDT          17      Atlantic                ATL
    ## 9      5031    America/New_York                    -4               EDT          17      Atlantic                ATL
    ## 10       NA     America/Toronto                    -4               EDT          17      Atlantic                ATL
    ## 11     5066    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 12     5027    America/New_York                    -4               EDT          17      Atlantic                ATL
    ## 13       NA    America/New_York                    -4               EDT          17      Atlantic                ATL
    ## 14     5094    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 15     5092     America/Chicago                    -5               CDT          16       Central                CEN
    ## 16     5145     America/Detroit                    -4               EDT          17      Atlantic                ATL
    ## 17     5030     America/Chicago                    -5               CDT          16       Central                CEN
    ## 18     5076     America/Chicago                    -5               CDT          16       Central                CEN
    ## 19     5075      America/Denver                    -6               MDT          15       Pacific                PAC
    ## 20     5064      America/Denver                    -6               MDT          16       Central                CEN
    ## 21     5100    America/Edmonton                    -6               MDT          15       Pacific                PAC
    ## 22     5073   America/Vancouver                    -7               PDT          15       Pacific                PAC
    ## 23     5046 America/Los_Angeles                    -7               PDT          15       Pacific                PAC
    ## 24     5019     America/Chicago                    -5               CDT          16       Central                CEN
    ## 25     5081 America/Los_Angeles                    -7               PDT          15       Pacific                PAC
    ## 26       NA America/Los_Angeles                    -7               PDT          15       Pacific                PAC
    ## 27     5059    America/New_York                    -4               EDT          18  Metropolitan              Metro
    ## 28     5098     America/Chicago                    -5               CDT          16       Central                CEN
    ## 29     5058    America/Winnipeg                    -5               CDT          16       Central                CEN
    ## 30     5043     America/Phoenix                    -7               MST          15       Pacific                PAC
    ##           division.link division.abbreviation conference.id conference.name       conference.link franchise.franchiseId
    ## 1  /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    23
    ## 2  /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    22
    ## 3  /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    10
    ## 4  /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    16
    ## 5  /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    17
    ## 6  /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                     6
    ## 7  /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                    19
    ## 8  /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                     1
    ## 9  /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                    30
    ## 10 /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                     5
    ## 11 /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    26
    ## 12 /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                    33
    ## 13 /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                    31
    ## 14 /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    24
    ## 15 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    11
    ## 16 /api/v1/divisions/17                     A             6         Eastern /api/v1/conferences/6                    12
    ## 17 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    34
    ## 18 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    18
    ## 19 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    21
    ## 20 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    27
    ## 21 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    25
    ## 22 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    20
    ## 23 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    32
    ## 24 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    15
    ## 25 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    14
    ## 26 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    29
    ## 27 /api/v1/divisions/18                     M             6         Eastern /api/v1/conferences/6                    36
    ## 28 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    37
    ## 29 /api/v1/divisions/16                     C             5         Western /api/v1/conferences/5                    35
    ## 30 /api/v1/divisions/15                     P             5         Western /api/v1/conferences/5                    28
    ##    franchise.teamName        franchise.link
    ## 1              Devils /api/v1/franchises/23
    ## 2           Islanders /api/v1/franchises/22
    ## 3             Rangers /api/v1/franchises/10
    ## 4              Flyers /api/v1/franchises/16
    ## 5            Penguins /api/v1/franchises/17
    ## 6              Bruins  /api/v1/franchises/6
    ## 7              Sabres /api/v1/franchises/19
    ## 8           Canadiens  /api/v1/franchises/1
    ## 9            Senators /api/v1/franchises/30
    ## 10        Maple Leafs  /api/v1/franchises/5
    ## 11         Hurricanes /api/v1/franchises/26
    ## 12           Panthers /api/v1/franchises/33
    ## 13          Lightning /api/v1/franchises/31
    ## 14           Capitals /api/v1/franchises/24
    ## 15         Blackhawks /api/v1/franchises/11
    ## 16          Red Wings /api/v1/franchises/12
    ## 17          Predators /api/v1/franchises/34
    ## 18              Blues /api/v1/franchises/18
    ## 19             Flames /api/v1/franchises/21
    ## 20          Avalanche /api/v1/franchises/27
    ## 21             Oilers /api/v1/franchises/25
    ## 22            Canucks /api/v1/franchises/20
    ## 23              Ducks /api/v1/franchises/32
    ## 24              Stars /api/v1/franchises/15
    ## 25              Kings /api/v1/franchises/14
    ## 26             Sharks /api/v1/franchises/29
    ## 27       Blue Jackets /api/v1/franchises/36
    ## 28               Wild /api/v1/franchises/37
    ## 29               Jets /api/v1/franchises/35
    ## 30            Coyotes /api/v1/franchises/28
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              roster.roster
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  68, 26, 24, 9, 23, 17, 29, 15, 13, 22, NA, 28, 8, 14, 12, 5, 4, 37, 10, 14, 10, 44, 36, 28, 36, 10, 24, 6, 23, 28, 12, 34, 40, 35, 71, 8448208, 8460542, 8460626, 8464977, 8467351, 8467545, 8467899, 8469462, 8469500, 8469547, 8469622, 8469623, 8470609, 8471233, 8471636, 8471748, 8472382, 8472394, 8472410, 8474641, 8475185, 8475199, 8475274, 8475750, 8476209, 8476227, 8476423, 8476457, 8476870, 8476923, 8477059, 8477127, 8466339, 8471239, 8476234, Jaromir Jagr, Patrik Elias, Bryce Salvador, Dainius Zubrus, Scott Gomez, Michael Ryder, Martin Havlat, Tuomo Ruutu, Michael Cammalleri, Jordin Tootoo, Ryane Clowe, Marek Zidlicky, Steve Bernier, Travis Zajac, Tim Sestito, Mark Fraser, Andy Greene, Peter Harrold, Stephen Gionta, Adam Henrique, Jacob Josefson, Eric Gelinas, Seth Helgeson, Jon Merrill, Mike Sislo, Joe Whitney, Reid Boucher, Adam Larsson, Stefan Matteau, Damon Severson, Damien Brunner, Raman Hrabarenka, Scott Clemmensen, Cory Schneider, Keith Kinkaid, /api/v1/people/8448208, /api/v1/people/8460542, /api/v1/people/8460626, /api/v1/people/8464977, /api/v1/people/8467351, /api/v1/people/8467545, /api/v1/people/8467899, /api/v1/people/8469462, /api/v1/people/8469500, /api/v1/people/8469547, /api/v1/people/8469622, /api/v1/people/8469623, /api/v1/people/8470609, /api/v1/people/8471233, /api/v1/people/8471636, /api/v1/people/8471748, /api/v1/people/8472382, /api/v1/people/8472394, /api/v1/people/8472410, /api/v1/people/8474641, /api/v1/people/8475185, /api/v1/people/8475199, /api/v1/people/8475274, /api/v1/people/8475750, /api/v1/people/8476209, /api/v1/people/8476227, /api/v1/people/8476423, /api/v1/people/8476457, /api/v1/people/8476870, /api/v1/people/8476923, /api/v1/people/8477059, /api/v1/people/8477127, /api/v1/people/8466339, /api/v1/people/8471239, /api/v1/people/8476234, R, C, D, C, C, R, R, L, L, R, L, D, R, C, L, D, D, D, C, C, C, D, D, D, R, R, C, D, C, D, R, D, G, G, G, Right Wing, Center, Defenseman, Center, Center, Right Wing, Right Wing, Left Wing, Left Wing, Right Wing, Left Wing, Defenseman, Right Wing, Center, Left Wing, Defenseman, Defenseman, Defenseman, Center, Center, Center, Defenseman, Defenseman, Defenseman, Right Wing, Right Wing, Center, Defenseman, Center, Defenseman, Right Wing, Defenseman, Goalie, Goalie, Goalie, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Goalie, Goalie, Goalie, RW, C, D, C, C, RW, RW, LW, LW, RW, LW, D, RW, C, LW, D, D, D, C, C, C, D, D, D, RW, RW, C, D, C, D, RW, D, G, G, G
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      36, 11, 81, 55, 36, 48, 84, 21, 15, 40, 47, 86, 34, 12, 27, 46, 17, 91, 44, 2, 53, 27, 29, 22, 26, 89, 16, 8, 24, 41, 1, 30, 35, 8460720, 8468598, 8470144, 8470187, 8470644, 8471311, 8471362, 8473449, 8473504, 8473546, 8473565, 8473579, 8474066, 8474573, 8474612, 8474659, 8474709, 8475166, 8475177, 8475181, 8475231, 8475314, 8475754, 8476147, 8476158, 8476195, 8476458, 8476852, 8476429, 8470860, 8473434, 8473607, 8474690, Eric Boulton, Lubomir Visnovsky, Frans Nielsen, Johnny Boychuk, Colin McDonald, Tyler Kennedy, Mikhail Grabovski, Kyle Okposo, Cal Clutterbuck, Michael Grabner, Brian Strait, Nikolay Kulemin, Thomas Hickey, Josh Bailey, Travis Hamonic, Matt Donovan, Matt Martin, John Tavares, Calvin de Haan, Nick Leddy, Casey Cizikas, Anders Lee, Brock Nelson, Kael Mouillierat, Harry Zolnierczyk, Cory Conacher, Ryan Strome, Griffin Reinhart, Scott Mayfield, Jaroslav Halak, Chad Johnson, Michal Neuvirth, Kevin Poulin, /api/v1/people/8460720, /api/v1/people/8468598, /api/v1/people/8470144, /api/v1/people/8470187, /api/v1/people/8470644, /api/v1/people/8471311, /api/v1/people/8471362, /api/v1/people/8473449, /api/v1/people/8473504, /api/v1/people/8473546, /api/v1/people/8473565, /api/v1/people/8473579, /api/v1/people/8474066, /api/v1/people/8474573, /api/v1/people/8474612, /api/v1/people/8474659, /api/v1/people/8474709, /api/v1/people/8475166, /api/v1/people/8475177, /api/v1/people/8475181, /api/v1/people/8475231, /api/v1/people/8475314, /api/v1/people/8475754, /api/v1/people/8476147, /api/v1/people/8476158, /api/v1/people/8476195, /api/v1/people/8476458, /api/v1/people/8476852, /api/v1/people/8476429, /api/v1/people/8470860, /api/v1/people/8473434, /api/v1/people/8473607, /api/v1/people/8474690, L, D, C, D, R, C, C, R, R, L, D, L, D, R, D, D, L, C, D, D, C, L, C, L, L, C, C, D, D, G, G, G, G, Left Wing, Defenseman, Center, Defenseman, Right Wing, Center, Center, Right Wing, Right Wing, Left Wing, Defenseman, Left Wing, Defenseman, Right Wing, Defenseman, Defenseman, Left Wing, Center, Defenseman, Defenseman, Center, Left Wing, Center, Left Wing, Left Wing, Center, Center, Defenseman, Defenseman, Goalie, Goalie, Goalie, Goalie, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Goalie, Goalie, Goalie, Goalie, LW, D, C, D, RW, C, C, RW, RW, LW, D, LW, D, RW, D, D, LW, C, D, D, C, LW, C, LW, LW, C, C, D, D, G, G, G, G
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         26, 22, 12, 20, 61, 8, 18, 15, 48, 18, 3, 5, 45, 16, 55, 27, 62, 21, 38, 21, 20, 27, NA, 36, 24, 13, 20, 71, 9, NA, 91, 35, 33, NA, 8466378, 8467096, 8467988, 8468575, 8470041, 8470630, 8470740, 8470854, 8471436, 8471686, 8471735, 8471958, 8473536, 8473544, 8473589, 8474151, 8474176, 8474497, 8474535, 8474613, 8475184, 8475186, 8475243, 8475692, 8475715, 8475763, 8475795, 8475855, 8476468, 8477214, 8477407, 8468685, 8475660, 8477084, Martin St. Louis, Dan Boyle, Ryan Malone, Dominic Moore, Rick Nash, Kevin Klein, Lee Stempniak, Tanner Glass, Matt Hunwick, Marc Staal, Keith Yandle, Dan Girardi, James Sheppard, Derick Brassard, Chris Summers, Ryan McDonagh, Carl Hagelin, Michael Kostka, Chris Mueller, Derek Stepan, Chris Kreider, John Moore, Ryan Bourque, Mats Zuccarello, Oscar Lindberg, Kevin Hayes, Dylan McIlrath, Jesper Fast, J.T. Miller, Conor Allen, Anthony Duclair, Henrik Lundqvist, Cam Talbot, Mackenzie Skapski, /api/v1/people/8466378, /api/v1/people/8467096, /api/v1/people/8467988, /api/v1/people/8468575, /api/v1/people/8470041, /api/v1/people/8470630, /api/v1/people/8470740, /api/v1/people/8470854, /api/v1/people/8471436, /api/v1/people/8471686, /api/v1/people/8471735, /api/v1/people/8471958, /api/v1/people/8473536, /api/v1/people/8473544, /api/v1/people/8473589, /api/v1/people/8474151, /api/v1/people/8474176, /api/v1/people/8474497, /api/v1/people/8474535, /api/v1/people/8474613, /api/v1/people/8475184, /api/v1/people/8475186, /api/v1/people/8475243, /api/v1/people/8475692, /api/v1/people/8475715, /api/v1/people/8475763, /api/v1/people/8475795, /api/v1/people/8475855, /api/v1/people/8476468, /api/v1/people/8477214, /api/v1/people/8477407, /api/v1/people/8468685, /api/v1/people/8475660, /api/v1/people/8477084, R, D, L, C, L, D, R, L, D, D, D, D, C, C, D, D, L, D, C, C, L, D, C, R, L, C, D, R, C, D, L, G, G, G, Right Wing, Defenseman, Left Wing, Center, Left Wing, Defenseman, Right Wing, Left Wing, Defenseman, Defenseman, Defenseman, Defenseman, Center, Center, Defenseman, Defenseman, Left Wing, Defenseman, Center, Center, Left Wing, Defenseman, Center, Right Wing, Left Wing, Center, Defenseman, Right Wing, Center, Defenseman, Left Wing, Goalie, Goalie, Goalie, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, RW, D, LW, C, LW, D, RW, LW, D, D, D, D, C, C, D, D, LW, D, C, C, LW, D, C, RW, LW, C, D, RW, C, D, LW, G, G, G
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        44, 32, 55, 20, 25, 45, 8, 76, 42, 21, 28, 47, 93, 24, 2, 15, 36, 10, 38, 26, NA, 5, 20, 10, 21, 14, 21, 53, 17, 41, 72, 29, NA, 8467329, 8468309, 8468513, 8469469, 8469470, 8470601, 8471269, 8471762, 8471767, 8473466, 8473512, 8473584, 8474161, 8474190, 8474568, 8474584, 8474736, 8475170, 8475342, 8475430, 8475723, 8475729, 8475917, 8476177, 8476393, 8476461, 8476872, 8476906, 8477290, 8477930, 8467972, 8469548, 8473461, Vincent Lecavalier, Mark Streit, Nick Schultz, R.J. Umberger, Carlo Colaiacovo, Braydon Coburn, Nicklas Grossmann, Chris VandeVelde, Blair Jones, Ryan White, Claude Giroux, Andrew MacDonald, Jakub Voracek, Wayne Simmonds, Luke Schenn, Michael Del Zotto, Zac Rinaldo, Brayden Schenn, Oliver Lauridsen, Brandon Manning, Petr Straka, Mark Alt, Jason Akeson, Matt Read, Nick Cousins, Sean Couturier, Scott Laughton, Shayne Gostisbehere, Michael Raffl, Pierre-Edouard Bellemare, Rob Zepp, Ray Emery, Steve Mason, /api/v1/people/8467329, /api/v1/people/8468309, /api/v1/people/8468513, /api/v1/people/8469469, /api/v1/people/8469470, /api/v1/people/8470601, /api/v1/people/8471269, /api/v1/people/8471762, /api/v1/people/8471767, /api/v1/people/8473466, /api/v1/people/8473512, /api/v1/people/8473584, /api/v1/people/8474161, /api/v1/people/8474190, /api/v1/people/8474568, /api/v1/people/8474584, /api/v1/people/8474736, /api/v1/people/8475170, /api/v1/people/8475342, /api/v1/people/8475430, /api/v1/people/8475723, /api/v1/people/8475729, /api/v1/people/8475917, /api/v1/people/8476177, /api/v1/people/8476393, /api/v1/people/8476461, /api/v1/people/8476872, /api/v1/people/8476906, /api/v1/people/8477290, /api/v1/people/8477930, /api/v1/people/8467972, /api/v1/people/8469548, /api/v1/people/8473461, C, D, D, C, D, D, D, C, C, C, C, D, R, R, D, D, C, C, D, D, R, D, R, R, C, C, C, D, L, C, G, G, G, Center, Defenseman, Defenseman, Center, Defenseman, Defenseman, Defenseman, Center, Center, Center, Center, Defenseman, Right Wing, Right Wing, Defenseman, Defenseman, Center, Center, Defenseman, Defenseman, Right Wing, Defenseman, Right Wing, Right Wing, Center, Center, Center, Defenseman, Left Wing, Center, Goalie, Goalie, Goalie, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Goalie, Goalie, Goalie, C, D, D, C, D, D, D, C, C, C, C, D, RW, RW, D, D, C, C, D, D, RW, D, RW, RW, C, C, C, D, LW, C, G, G, G
    ## 5                                                                                                                                                                                                                                                  27, 9, 7, 7, 57, 55, 14, 40, 71, 15, 26, 87, 17, 44, 58, 70, 25, 21, 12, 28, 20, 16, 57, 41, 23, 6, 8, 78, 17, 33, 26, 46, 4, 44, 6, 51, 12, 29, 29, 35, 8465166, 8466393, 8467452, 8468542, 8469473, 8469555, 8470543, 8470654, 8471215, 8471260, 8471476, 8471675, 8471703, 8471710, 8471724, 8471887, 8473682, 8473933, 8473970, 8474013, 8474091, 8474096, 8474102, 8474145, 8475119, 8475155, 8475208, 8475761, 8475810, 8476062, 8476293, 8476339, 8476449, 8476769, 8476874, 8476884, 8477126, 8470594, 8471306, 8473553, Craig Adams, Pascal Dupuis, Rob Scuderi, Paul Martin, Marcel Goc, Christian Ehrhoff, Chris Kunitz, Maxim Lapierre, Evgeni Malkin, Blake Comeau, Daniel Winnik, Sidney Crosby, Steve Downie, Taylor Chorney, Kris Letang, Patric Hornqvist, Andrew Ebbett, Ben Lovejoy, Rob Klinkhammer, Ian Cole, Brandon Sutter, Nick Spaling, David Perron, Robert Bortuzzo, Zach Sill, Simon Despres, Brian Dumoulin, Beau Bennett, Bryan Rust, Mark Arcobello, Scott Wilson, Dominik Uher, Scott Harrington, Bobby Farnham, Olli Maatta, Derrick Pouliot, Jayson Megna, Marc-Andre Fleury, Thomas Greiss, Jeff Zatkoff, /api/v1/people/8465166, /api/v1/people/8466393, /api/v1/people/8467452, /api/v1/people/8468542, /api/v1/people/8469473, /api/v1/people/8469555, /api/v1/people/8470543, /api/v1/people/8470654, /api/v1/people/8471215, /api/v1/people/8471260, /api/v1/people/8471476, /api/v1/people/8471675, /api/v1/people/8471703, /api/v1/people/8471710, /api/v1/people/8471724, /api/v1/people/8471887, /api/v1/people/8473682, /api/v1/people/8473933, /api/v1/people/8473970, /api/v1/people/8474013, /api/v1/people/8474091, /api/v1/people/8474096, /api/v1/people/8474102, /api/v1/people/8474145, /api/v1/people/8475119, /api/v1/people/8475155, /api/v1/people/8475208, /api/v1/people/8475761, /api/v1/people/8475810, /api/v1/people/8476062, /api/v1/people/8476293, /api/v1/people/8476339, /api/v1/people/8476449, /api/v1/people/8476769, /api/v1/people/8476874, /api/v1/people/8476884, /api/v1/people/8477126, /api/v1/people/8470594, /api/v1/people/8471306, /api/v1/people/8473553, R, R, D, D, C, D, L, C, C, L, L, C, R, D, D, R, C, D, L, D, C, C, L, D, C, D, D, R, R, R, C, C, D, R, D, D, C, G, G, G, Right Wing, Right Wing, Defenseman, Defenseman, Center, Defenseman, Left Wing, Center, Center, Left Wing, Left Wing, Center, Right Wing, Defenseman, Defenseman, Right Wing, Center, Defenseman, Left Wing, Defenseman, Center, Center, Left Wing, Defenseman, Center, Defenseman, Defenseman, Right Wing, Right Wing, Right Wing, Center, Center, Defenseman, Right Wing, Defenseman, Defenseman, Center, Goalie, Goalie, Goalie, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Goalie, Goalie, Goalie, RW, RW, D, D, C, D, LW, C, C, LW, LW, C, RW, D, D, RW, C, D, LW, D, C, C, LW, D, C, D, D, RW, RW, RW, C, C, D, RW, D, D, C, G, G, G
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    33, 12, 22, 44, 9, 25, NA, 21, 37, 34, 46, 54, 64, 63, 17, 55, 44, 33, 19, 22, NA, 15, 20, 55, 86, 51, 76, 19, 70, 23, 47, NA, 88, 40, 35, 30, 8465009, 8467346, 8467967, 8469619, 8470169, 8470171, 8470230, 8470626, 8470638, 8471262, 8471276, 8471717, 8472365, 8473419, 8473473, 8474660, 8474749, 8475150, 8475191, 8475414, 8475671, 8475727, 8475792, 8475902, 8476191, 8476363, 8476435, 8476462, 8476476, 8476495, 8476792, 8477228, 8477956, 8471695, 8476844, 8476876, Zdeno Chara, Simon Gagne, Chris Kelly, Dennis Seidenberg, Gregory Campbell, Max Talbot, Daniel Paille, Loui Eriksson, Patrice Bergeron, Carl Soderberg, David Krejci, Adam McQuaid, Bobby Robins, Brad Marchand, Milan Lucic, David Warsofsky, Matt Bartkowski, Jordan Caron, Reilly Smith, Craig Cunningham, Matt Fraser, Ryan Spooner, Brett Connolly, Zach Trotman, Kevan Miller, Brian Ferlin, Alex Khokhlachev, Dougie Hamilton, Joe Morrow, Seth Griffith, Torey Krug, Matt Lindblad, David Pastrnak, Tuukka Rask, Niklas Svedberg, Malcolm Subban, /api/v1/people/8465009, /api/v1/people/8467346, /api/v1/people/8467967, /api/v1/people/8469619, /api/v1/people/8470169, /api/v1/people/8470171, /api/v1/people/8470230, /api/v1/people/8470626, /api/v1/people/8470638, /api/v1/people/8471262, /api/v1/people/8471276, /api/v1/people/8471717, /api/v1/people/8472365, /api/v1/people/8473419, /api/v1/people/8473473, /api/v1/people/8474660, /api/v1/people/8474749, /api/v1/people/8475150, /api/v1/people/8475191, /api/v1/people/8475414, /api/v1/people/8475671, /api/v1/people/8475727, /api/v1/people/8475792, /api/v1/people/8475902, /api/v1/people/8476191, /api/v1/people/8476363, /api/v1/people/8476435, /api/v1/people/8476462, /api/v1/people/8476476, /api/v1/people/8476495, /api/v1/people/8476792, /api/v1/people/8477228, /api/v1/people/8477956, /api/v1/people/8471695, /api/v1/people/8476844, /api/v1/people/8476876, D, L, C, D, C, C, L, L, C, C, C, D, R, L, L, D, D, R, R, L, R, C, R, D, D, R, C, D, D, C, D, C, R, G, G, G, Defenseman, Left Wing, Center, Defenseman, Center, Center, Left Wing, Left Wing, Center, Center, Center, Defenseman, Right Wing, Left Wing, Left Wing, Defenseman, Defenseman, Right Wing, Right Wing, Left Wing, Right Wing, Center, Right Wing, Defenseman, Defenseman, Right Wing, Center, Defenseman, Defenseman, Center, Defenseman, Center, Right Wing, Goalie, Goalie, Goalie, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Goalie, Goalie, Goalie, D, LW, C, D, C, C, LW, LW, C, C, C, D, RW, LW, LW, D, D, RW, RW, LW, RW, C, RW, D, D, RW, C, D, D, C, D, C, RW, G, G, G
    ## 7                                                                                                                                                                                                                                                                                                                     12, 8, 37, 4, 48, 28, NA, 18, 41, 71, 36, NA, 44, 22, 11, 57, 63, 26, 17, 20, 49, 44, 22, 13, 40, 32, 28, 25, 19, 59, 2, 55, 16, 23, 31, 30, 39, 31, 1, 8467407, 8469591, 8470018, 8470324, 8470378, 8470729, 8470852, 8471226, 8471236, 8471338, 8471388, 8473426, 8473485, 8474567, 8474570, 8474574, 8474589, 8474610, 8475220, 8475235, 8475309, 8475321, 8475728, 8475796, 8476469, 8476808, 8476878, 8476888, 8476931, 8477213, 8477244, 8477499, 8477507, 8477933, 8473523, 8473607, 8474765, 8475252, 8477092, Brian Gionta, Cody McCormick, Matt Ellis, Josh Gorges, Andre Benoit, Tyson Strachan, Matt Moulson, Drew Stafford, Andrej Meszaros, Torrey Mitchell, Patrick Kaleta, Mike Weber, Chris Stewart, Zach Bogosian, Cody Hodgson, Tyler Myers, Tyler Ennis, Zac Dalpe, Marcus Foligno, Nicolas Deslauriers, Jerry D'Amigo, Phil Varone, Johan Larsson, Mark Pysyk, Joel Armia, Brian Flynn, Zemgus Girgensons, Mikhail Grigorenko, Jake McCabe, Tim Schaller, Chad Ruhwedel, Rasmus Ristolainen, Nikita Zadorov, Sam Reinhart, Jhonas Enroth, Michal Neuvirth, Anders Lindback, Matt Hackett, Andrey Makarov, /api/v1/people/8467407, /api/v1/people/8469591, /api/v1/people/8470018, /api/v1/people/8470324, /api/v1/people/8470378, /api/v1/people/8470729, /api/v1/people/8470852, /api/v1/people/8471226, /api/v1/people/8471236, /api/v1/people/8471338, /api/v1/people/8471388, /api/v1/people/8473426, /api/v1/people/8473485, /api/v1/people/8474567, /api/v1/people/8474570, /api/v1/people/8474574, /api/v1/people/8474589, /api/v1/people/8474610, /api/v1/people/8475220, /api/v1/people/8475235, /api/v1/people/8475309, /api/v1/people/8475321, /api/v1/people/8475728, /api/v1/people/8475796, /api/v1/people/8476469, /api/v1/people/8476808, /api/v1/people/8476878, /api/v1/people/8476888, /api/v1/people/8476931, /api/v1/people/8477213, /api/v1/people/8477244, /api/v1/people/8477499, /api/v1/people/8477507, /api/v1/people/8477933, /api/v1/people/8473523, /api/v1/people/8473607, /api/v1/people/8474765, /api/v1/people/8475252, /api/v1/people/8477092, R, C, L, D, D, D, L, R, D, C, R, D, R, D, C, D, C, C, L, L, R, C, L, D, R, C, L, C, D, L, D, D, D, R, G, G, G, G, G, Right Wing, Center, Left Wing, Defenseman, Defenseman, Defenseman, Left Wing, Right Wing, Defenseman, Center, Right Wing, Defenseman, Right Wing, Defenseman, Center, Defenseman, Center, Center, Left Wing, Left Wing, Right Wing, Center, Left Wing, Defenseman, Right Wing, Center, Left Wing, Center, Defenseman, Left Wing, Defenseman, Defenseman, Defenseman, Right Wing, Goalie, Goalie, Goalie, Goalie, Goalie, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Goalie, Goalie, Goalie, Goalie, Goalie, RW, C, LW, D, D, D, LW, RW, D, C, RW, D, RW, D, C, D, C, C, LW, LW, RW, C, LW, D, RW, C, LW, C, D, LW, D, D, D, RW, G, G, G, G, G
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          55, 6, 20, 79, 43, 27, 14, 11, 6, 9, 25, 71, 17, 51, 26, 34, 76, 38, 67, 20, 22, 2, 28, 15, 39, 25, 84, 11, 88, 32, 12, 10, 61, 36, 31, 31, 8458951, 8467332, 8467334, 8467496, 8468478, 8468635, 8469521, 8469707, 8470104, 8471283, 8471296, 8471338, 8471504, 8471976, 8473507, 8474025, 8474056, 8474137, 8474157, 8474189, 8474668, 8474688, 8475254, 8475739, 8475749, 8475758, 8475797, 8475848, 8476470, 8476808, 8476851, 8477413, 8477455, 8478137, 8471679, 8474682, Sergei Gonchar, Bryan Allen, Manny Malhotra, Andrei Markov, Mike Weaver, Travis Moen, Tomas Plekanec, PA Parenteau, Tom Gilbert, Brandon Prust, Alexei Emelin, Torrey Mitchell, Rene Bourque, David Desharnais, Jeff Petry, Eric Tangradi, P.K. Subban, Drayson Bowman, Max Pacioretty, Lars Eller, Dale Weise, Greg Pateryn, Gabriel Dumont, Michael Bournival, Christian Thomas, Devante Smith-Pelly, Jarred Tinordi, Brendan Gallagher, Nathan Beaulieu, Brian Flynn, Alex Galchenyuk, Sven Andrighetto, Jacob de la Rose, Jiri Sekac, Carey Price, Dustin Tokarski, /api/v1/people/8458951, /api/v1/people/8467332, /api/v1/people/8467334, /api/v1/people/8467496, /api/v1/people/8468478, /api/v1/people/8468635, /api/v1/people/8469521, /api/v1/people/8469707, /api/v1/people/8470104, /api/v1/people/8471283, /api/v1/people/8471296, /api/v1/people/8471338, /api/v1/people/8471504, /api/v1/people/8471976, /api/v1/people/8473507, /api/v1/people/8474025, /api/v1/people/8474056, /api/v1/people/8474137, /api/v1/people/8474157, /api/v1/people/8474189, /api/v1/people/8474668, /api/v1/people/8474688, /api/v1/people/8475254, /api/v1/people/8475739, /api/v1/people/8475749, /api/v1/people/8475758, /api/v1/people/8475797, /api/v1/people/8475848, /api/v1/people/8476470, /api/v1/people/8476808, /api/v1/people/8476851, /api/v1/people/8477413, /api/v1/people/8477455, /api/v1/people/8478137, /api/v1/people/8471679, /api/v1/people/8474682, D, D, C, D, D, L, C, R, D, L, D, C, R, C, D, L, D, L, L, C, R, D, C, L, L, R, D, R, D, C, C, R, L, L, G, G, Defenseman, Defenseman, Center, Defenseman, Defenseman, Left Wing, Center, Right Wing, Defenseman, Left Wing, Defenseman, Center, Right Wing, Center, Defenseman, Left Wing, Defenseman, Left Wing, Left Wing, Center, Right Wing, Defenseman, Center, Left Wing, Left Wing, Right Wing, Defenseman, Right Wing, Defenseman, Center, Center, Right Wing, Left Wing, Left Wing, Goalie, Goalie, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Goalie, Goalie, D, D, C, D, D, LW, C, RW, D, LW, D, C, RW, C, D, LW, D, LW, LW, C, RW, D, C, LW, LW, RW, D, RW, D, C, C, RW, LW, LW, G, G
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             4, 17, 25, 18, 16, 33, 54, 38, 2, 27, 8, 15, 65, 90, 90, 68, 39, 29, 61, 11, 44, 93, 54, 4, 20, 31, 90, 60, 35, 8464956, 8467330, 8467493, 8470599, 8470667, 8470760, 8471676, 8471862, 8473468, 8473588, 8474068, 8474250, 8474578, 8474605, 8474697, 8474884, 8475163, 8475174, 8475913, 8476386, 8476419, 8476459, 8476477, 8476879, 8477508, 8467950, 8475215, 8476904, 8477202, Chris Phillips, David Legwand, Chris Neil, Milan Michalek, Clarke MacArthur, Marc Methot, Bobby Ryan, Colin Greening, Eric Gryba, Erik Condra, Kyle Turris, Zack Smith, Erik Karlsson, Patrick Wiercioch, Mark Borowiecki, Mike Hoffman, Alex Chiasson, Jared Cowen, Mark Stone, Shane Prince, Jean-Gabriel Pageau, Mika Zibanejad, Matt Puempel, Cody Ceci, Curtis Lazar, Craig Anderson, Robin Lehner, Chris Driedger, Andrew Hammond, /api/v1/people/8464956, /api/v1/people/8467330, /api/v1/people/8467493, /api/v1/people/8470599, /api/v1/people/8470667, /api/v1/people/8470760, /api/v1/people/8471676, /api/v1/people/8471862, /api/v1/people/8473468, /api/v1/people/8473588, /api/v1/people/8474068, /api/v1/people/8474250, /api/v1/people/8474578, /api/v1/people/8474605, /api/v1/people/8474697, /api/v1/people/8474884, /api/v1/people/8475163, /api/v1/people/8475174, /api/v1/people/8475913, /api/v1/people/8476386, /api/v1/people/8476419, /api/v1/people/8476459, /api/v1/people/8476477, /api/v1/people/8476879, /api/v1/people/8477508, /api/v1/people/8467950, /api/v1/people/8475215, /api/v1/people/8476904, /api/v1/people/8477202, D, C, R, L, L, D, R, C, D, R, C, C, D, D, D, C, R, D, R, L, C, C, L, D, C, G, G, G, G, Defenseman, Center, Right Wing, Left Wing, Left Wing, Defenseman, Right Wing, Center, Defenseman, Right Wing, Center, Center, Defenseman, Defenseman, Defenseman, Center, Right Wing, Defenseman, Right Wing, Left Wing, Center, Center, Left Wing, Defenseman, Center, Goalie, Goalie, Goalie, Goalie, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, Goalie, D, C, RW, LW, LW, D, RW, C, D, RW, C, C, D, D, D, C, RW, D, RW, LW, C, C, LW, D, C, G, G, G, G
    ## 10                                                                                                                                                                                                                                         12, 13, 2, 28, 15, 19, 3, 40, 71, 17, 25, 45, 26, 11, 47, 81, 22, 23, 25, 39, 39, 51, 27, 21, 23, 44, 37, 91, NA, 24, 67, 18, 8, 39, 27, 10, 44, 37, 47, 45, 8462196, 8466140, 8466142, 8468778, 8470152, 8470207, 8470602, 8470867, 8470920, 8471266, 8471390, 8471392, 8471476, 8471742, 8473463, 8473548, 8473560, 8473942, 8474037, 8474113, 8474192, 8474581, 8474748, 8475098, 8475119, 8475148, 8475154, 8475172, 8475180, 8475209, 8475295, 8475735, 8475774, 8475842, 8476410, 8476478, 8476853, 8478376, 8473503, 8473541, Stephane Robidas, Olli Jokinen, Eric Brewer, Colton Orr, Joakim Lindstrom, Joffrey Lupul, Dion Phaneuf, Troy Bodie, David Clarkson, David Booth, Mike Santorelli, Roman Polak, Daniel Winnik, Cody Franson, Leo Komarov, Phil Kessel, Korbinian Holzer, Trevor Smith, James van Riemsdyk, TJ Brennan, Matt Frattin, Jake Gardiner, Andrew MacWilliam, Tyler Bozak, Zach Sill, Tim Erixon, Carter Ashton, Nazem Kadri, Peter Holland, Richard Panik, Brandon Kozun, Greg McKegg, Petter Granberg, Sam Carrick, Josh Leivo, Stuart Percy, Morgan Rielly, Casey Bailey, James Reimer, Jonathan Bernier, /api/v1/people/8462196, /api/v1/people/8466140, /api/v1/people/8466142, /api/v1/people/8468778, /api/v1/people/8470152, /api/v1/people/8470207, /api/v1/people/8470602, /api/v1/people/8470867, /api/v1/people/8470920, /api/v1/people/8471266, /api/v1/people/8471390, /api/v1/people/8471392, /api/v1/people/8471476, /api/v1/people/8471742, /api/v1/people/8473463, /api/v1/people/8473548, /api/v1/people/8473560, /api/v1/people/8473942, /api/v1/people/8474037, /api/v1/people/8474113, /api/v1/people/8474192, /api/v1/people/8474581, /api/v1/people/8474748, /api/v1/people/8475098, /api/v1/people/8475119, /api/v1/people/8475148, /api/v1/people/8475154, /api/v1/people/8475172, /api/v1/people/8475180, /api/v1/people/8475209, /api/v1/people/8475295, /api/v1/people/8475735, /api/v1/people/8475774, /api/v1/people/8475842, /api/v1/people/8476410, /api/v1/people/8476478, /api/v1/people/8476853, /api/v1/people/8478376, /api/v1/people/8473503, /api/v1/people/8473541, D, C, D, R, C, L, D, R, R, L, C, D, L, D, R, R, D, C, L, D, R, D, D, C, C, D, R, C, C, R, R, C, D, C, L, D, D, R, G, G, Defenseman, Center, Defenseman, Right Wing, Center, Left Wing, Defenseman, Right Wing, Right Wing, Left Wing, Center, Defenseman, Left Wing, Defenseman, Right Wing, Right Wing, Defenseman, Center, Left Wing, Defenseman, Right Wing, Defenseman, Defenseman, Center, Center, Defenseman, Right Wing, Center, Center, Right Wing, Right Wing, Center, Defenseman, Center, Left Wing, Defenseman, Defenseman, Right Wing, Goalie, Goalie, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Goalie, Goalie, D, C, D, RW, C, LW, D, RW, RW, LW, C, D, LW, D, RW, RW, D, C, LW, D, RW, D, D, C, C, D, RW, C, C, RW, RW, C, D, C, LW, D, D, RW, G, G
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  81, 26, 6, 18, NA, 13, 39, 21, 5, 24, 9, 11, 15, 20, 24, 73, 38, 22, 47, 15, 62, 41, 72, 53, 52, 48, 47, 49, 32, 56, 28, 38, 30, 35, 8468493, 8468639, 8469476, 8469508, 8469531, 8470120, 8470302, 8470595, 8471284, 8471804, 8473482, 8473533, 8474052, 8474062, 8474089, 8474135, 8474519, 8474576, 8474661, 8475250, 8475294, 8475732, 8475753, 8475784, 8475826, 8475980, 8476397, 8476437, 8476465, 8476944, 8477496, 8477887, 8470320, 8471418, Ron Hainsey, John-Michael Liles, Tim Gleason, Jay McClement, Jay Harrison, Alexander Semin, Patrick Dwyer, Eric Staal, Andrej Sekera, Nathan Gerbe, Jiri Tlusty, Jordan Staal, Chris Terry, Riley Nash, Brad Malone, Brett Bellemore, Jack Hillen, Zach Boychuk, Michal Jordan, Andrej Nestrasil, Rasmus Rissanen, Danny Biega, Justin Faulk, Jeff Skinner, Justin Shugg, Brody Sutter, Keegan Lowe, Victor Rask, Ryan Murphy, Brendan Woods, Elias Lindholm, Patrick Brown, Cam Ward, Anton Khudobin, /api/v1/people/8468493, /api/v1/people/8468639, /api/v1/people/8469476, /api/v1/people/8469508, /api/v1/people/8469531, /api/v1/people/8470120, /api/v1/people/8470302, /api/v1/people/8470595, /api/v1/people/8471284, /api/v1/people/8471804, /api/v1/people/8473482, /api/v1/people/8473533, /api/v1/people/8474052, /api/v1/people/8474062, /api/v1/people/8474089, /api/v1/people/8474135, /api/v1/people/8474519, /api/v1/people/8474576, /api/v1/people/8474661, /api/v1/people/8475250, /api/v1/people/8475294, /api/v1/people/8475732, /api/v1/people/8475753, /api/v1/people/8475784, /api/v1/people/8475826, /api/v1/people/8475980, /api/v1/people/8476397, /api/v1/people/8476437, /api/v1/people/8476465, /api/v1/people/8476944, /api/v1/people/8477496, /api/v1/people/8477887, /api/v1/people/8470320, /api/v1/people/8471418, D, D, D, C, D, R, R, C, D, C, L, C, L, C, C, D, D, L, D, C, D, D, D, L, R, C, D, C, D, L, C, C, G, G, Defenseman, Defenseman, Defenseman, Center, Defenseman, Right Wing, Right Wing, Center, Defenseman, Center, Left Wing, Center, Left Wing, Center, Center, Defenseman, Defenseman, Left Wing, Defenseman, Center, Defenseman, Defenseman, Defenseman, Left Wing, Right Wing, Center, Defenseman, Center, Defenseman, Left Wing, Center, Center, Goalie, Goalie, Defenseman, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Goalie, Goalie, D, D, D, C, D, RW, RW, C, D, C, LW, C, LW, C, C, D, D, LW, D, C, D, D, D, LW, RW, C, D, C, D, LW, C, C, G, G
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             68, 33, 22, 51, 17, 28, 82, 36, 12, 9, 23, 34, NA, 44, 48, 11, 4, 70, 73, 28, 15, 27, 44, 16, 23, 11, 16, 5, 1, 39, 35, 8448208, 8465185, 8465978, 8466285, 8468001, 8468504, 8468518, 8469638, 8470039, 8470105, 8470176, 8470839, 8471245, 8474000, 8474606, 8474625, 8475153, 8475179, 8475204, 8475253, 8475755, 8475760, 8475790, 8476389, 8476428, 8476456, 8477493, 8477932, 8466141, 8468540, 8471219, Jaromir Jagr, Willie Mitchell, Shawn Thornton, Brian Campbell, Derek MacKenzie, Brad Boyes, Tomas Kopecky, Jussi Jokinen, Tomas Fleischmann, Scottie Upshall, Sean Bergenheim, Shane O'Brien, Dave Bolland, Steven Kampfer, Colby Robak, Jimmy Hayes, Dylan Olsen, Dmitry Kulikov, Brandon Pirri, Garrett Wilson, Alexander Petrovic, Nick Bjugstad, Erik Gudbranson, Vincent Trocheck, Rocco Grimaldi, Jonathan Huberdeau, Aleksander Barkov, Aaron Ekblad, Roberto Luongo, Dan Ellis, Al Montoya, /api/v1/people/8448208, /api/v1/people/8465185, /api/v1/people/8465978, /api/v1/people/8466285, /api/v1/people/8468001, /api/v1/people/8468504, /api/v1/people/8468518, /api/v1/people/8469638, /api/v1/people/8470039, /api/v1/people/8470105, /api/v1/people/8470176, /api/v1/people/8470839, /api/v1/people/8471245, /api/v1/people/8474000, /api/v1/people/8474606, /api/v1/people/8474625, /api/v1/people/8475153, /api/v1/people/8475179, /api/v1/people/8475204, /api/v1/people/8475253, /api/v1/people/8475755, /api/v1/people/8475760, /api/v1/people/8475790, /api/v1/people/8476389, /api/v1/people/8476428, /api/v1/people/8476456, /api/v1/people/8477493, /api/v1/people/8477932, /api/v1/people/8466141, /api/v1/people/8468540, /api/v1/people/8471219, R, D, L, D, C, R, R, L, L, R, L, D, C, D, D, R, D, D, C, L, D, C, D, C, R, L, C, D, G, G, G, Right Wing, Defenseman, Left Wing, Defenseman, Center, Right Wing, Right Wing, Left Wing, Left Wing, Right Wing, Left Wing, Defenseman, Center, Defenseman, Defenseman, Right Wing, Defenseman, Defenseman, Center, Left Wing, Defenseman, Center, Defenseman, Center, Right Wing, Left Wing, Center, Defenseman, Goalie, Goalie, Goalie, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Goalie, Goalie, Goalie, RW, D, LW, D, C, RW, RW, LW, LW, RW, LW, D, C, D, D, RW, D, D, C, LW, D, C, D, C, RW, LW, C, D, G, G, G
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            2, 10, 51, 45, 9, 25, 24, 20, 6, 10, 17, NA, 91, 44, 28, 9, 77, 7, 20, 18, 89, 86, 92, 81, 23, 20, 18, 62, 92, 20, 30, 88, 8466142, 8466160, 8470047, 8470601, 8470619, 8470640, 8471339, 8471663, 8471873, 8471916, 8473986, 8474520, 8474564, 8474717, 8474722, 8474870, 8475167, 8475462, 8475792, 8476292, 8476300, 8476453, 8476480, 8476539, 8476806, 8476886, 8476975, 8477205, 8477494, 8460705, 8471750, 8476883, Eric Brewer, Brenden Morrow, Valtteri Filppula, Braydon Coburn, Brian Boyle, Matt Carle, Ryan Callahan, Mike Blunden, Anton Stralman, Mike Angelidis, Alex Killorn, Jason Garrison, Steven Stamkos, Mark Barberio, Luke Witkowski, Tyler Johnson, Victor Hedman, Radko Gudas, Brett Connolly, Ondrej Palat, Nikita Nesterov, Nikita Kucherov, Vladislav Namestnikov, Jonathan Marchessault, JT Brown, Slater Koekkoek, Cedric Paquette, Andrej Sustr, Jonathan Drouin, Evgeni Nabokov, Ben Bishop, Andrei Vasilevskiy, /api/v1/people/8466142, /api/v1/people/8466160, /api/v1/people/8470047, /api/v1/people/8470601, /api/v1/people/8470619, /api/v1/people/8470640, /api/v1/people/8471339, /api/v1/people/8471663, /api/v1/people/8471873, /api/v1/people/8471916, /api/v1/people/8473986, /api/v1/people/8474520, /api/v1/people/8474564, /api/v1/people/8474717, /api/v1/people/8474722, /api/v1/people/8474870, /api/v1/people/8475167, /api/v1/people/8475462, /api/v1/people/8475792, /api/v1/people/8476292, /api/v1/people/8476300, /api/v1/people/8476453, /api/v1/people/8476480, /api/v1/people/8476539, /api/v1/people/8476806, /api/v1/people/8476886, /api/v1/people/8476975, /api/v1/people/8477205, /api/v1/people/8477494, /api/v1/people/8460705, /api/v1/people/8471750, /api/v1/people/8476883, D, L, C, D, C, D, R, R, D, L, L, D, C, D, R, C, D, D, R, L, D, R, C, C, R, D, C, D, L, G, G, G, Defenseman, Left Wing, Center, Defenseman, Center, Defenseman, Right Wing, Right Wing, Defenseman, Left Wing, Left Wing, Defenseman, Center, Defenseman, Right Wing, Center, Defenseman, Defenseman, Right Wing, Left Wing, Defenseman, Right Wing, Center, Center, Right Wing, Defenseman, Center, Defenseman, Left Wing, Goalie, Goalie, Goalie, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, D, LW, C, D, C, D, RW, RW, D, LW, LW, D, C, D, RW, C, D, D, RW, LW, D, RW, C, C, RW, D, C, D, LW, G, G, G
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             20, 42, 44, 6, 25, 21, 22, 8, 27, 36, 15, 38, 19, 27, 83, 38, 74, 90, NA, NA, 17, 92, 16, 43, 38, 43, 38, 88, 95, 40, 49, 31, 8466251, 8468208, 8468498, 8469476, 8469639, 8470611, 8471185, 8471214, 8471242, 8471426, 8471702, 8472368, 8473563, 8473991, 8474291, 8474519, 8474590, 8475149, 8475161, 8475247, 8475619, 8475744, 8475816, 8476162, 8476799, 8476880, 8477070, 8477220, 8477444, 8471251, 8474651, 8475831, Jason Chimera, Joel Ward, Brooks Orpik, Tim Gleason, Brooks Laich, Eric Fehr, Curtis Glencross, Alex Ovechkin, Mike Green, Troy Brouwer, Matt Niskanen, Chris Conner, Nicklas Backstrom, Karl Alzner, Jay Beagle, Jack Hillen, John Carlson, Marcus Johansson, Chris Brown, Michael Latta, Aaron Volpatti, Evgeny Kuznetsov, Stanislav Galiev, Steve Oleksy, Cameron Schilling, Tom Wilson, Liam O'Brien, Nate Schmidt, Andre Burakovsky, Justin Peters, Braden Holtby, Philipp Grubauer, /api/v1/people/8466251, /api/v1/people/8468208, /api/v1/people/8468498, /api/v1/people/8469476, /api/v1/people/8469639, /api/v1/people/8470611, /api/v1/people/8471185, /api/v1/people/8471214, /api/v1/people/8471242, /api/v1/people/8471426, /api/v1/people/8471702, /api/v1/people/8472368, /api/v1/people/8473563, /api/v1/people/8473991, /api/v1/people/8474291, /api/v1/people/8474519, /api/v1/people/8474590, /api/v1/people/8475149, /api/v1/people/8475161, /api/v1/people/8475247, /api/v1/people/8475619, /api/v1/people/8475744, /api/v1/people/8475816, /api/v1/people/8476162, /api/v1/people/8476799, /api/v1/people/8476880, /api/v1/people/8477070, /api/v1/people/8477220, /api/v1/people/8477444, /api/v1/people/8471251, /api/v1/people/8474651, /api/v1/people/8475831, L, R, D, D, C, C, L, L, D, R, D, R, C, D, C, D, D, C, R, C, L, C, R, D, D, R, C, D, L, G, G, G, Left Wing, Right Wing, Defenseman, Defenseman, Center, Center, Left Wing, Left Wing, Defenseman, Right Wing, Defenseman, Right Wing, Center, Defenseman, Center, Defenseman, Defenseman, Center, Right Wing, Center, Left Wing, Center, Right Wing, Defenseman, Defenseman, Right Wing, Center, Defenseman, Left Wing, Goalie, Goalie, Goalie, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, LW, RW, D, D, C, C, LW, LW, D, RW, D, RW, C, D, C, D, D, C, RW, C, LW, C, RW, D, D, RW, C, D, LW, G, G, G
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                                                  44, 32, 81, 17, 50, 10, 29, 2, 7, 13, 29, 12, 10, 4, 26, 11, 19, 88, 18, 44, 5, 33, 16, 20, 65, 44, 6, 6, 20, 24, 86, 38, 57, NA, 50, 33, 32, 8459670, 8465058, 8466148, 8467389, 8468535, 8469544, 8469665, 8470281, 8470607, 8470666, 8471254, 8471299, 8471346, 8471769, 8471879, 8471956, 8473604, 8474141, 8474727, 8475148, 8475182, 8475214, 8475323, 8475807, 8476381, 8476394, 8476403, 8476431, 8476438, 8476479, 8476882, 8477451, 8477845, 8478393, 8470645, 8474152, 8477293, Kimmo Timonen, Michal Rozsival, Marian Hossa, Brad Richards, Antoine Vermette, Patrick Sharp, Johnny Oduya, Duncan Keith, Brent Seabrook, Daniel Carcillo, Bryan Bickell, Peter Regin, Kris Versteeg, Niklas Hjalmarsson, Kyle Cumiskey, Andrew Desjardins, Jonathan Toews, Patrick Kane, Ben Smith, Tim Erixon, David Rundblad, Jeremy Morin, Marcus Kruger, Joakim Nordstrom, Andrew Shaw, Michael Paliotta, Klas Dahlbeck, Adam Clendening, Brandon Saad, Phillip Danault, Teuvo Teravainen, Ryan Hartman, Trevor van Riemsdyk, Kyle Baun, Corey Crawford, Scott Darling, Antti Raanta, /api/v1/people/8459670, /api/v1/people/8465058, /api/v1/people/8466148, /api/v1/people/8467389, /api/v1/people/8468535, /api/v1/people/8469544, /api/v1/people/8469665, /api/v1/people/8470281, /api/v1/people/8470607, /api/v1/people/8470666, /api/v1/people/8471254, /api/v1/people/8471299, /api/v1/people/8471346, /api/v1/people/8471769, /api/v1/people/8471879, /api/v1/people/8471956, /api/v1/people/8473604, /api/v1/people/8474141, /api/v1/people/8474727, /api/v1/people/8475148, /api/v1/people/8475182, /api/v1/people/8475214, /api/v1/people/8475323, /api/v1/people/8475807, /api/v1/people/8476381, /api/v1/people/8476394, /api/v1/people/8476403, /api/v1/people/8476431, /api/v1/people/8476438, /api/v1/people/8476479, /api/v1/people/8476882, /api/v1/people/8477451, /api/v1/people/8477845, /api/v1/people/8478393, /api/v1/people/8470645, /api/v1/people/8474152, /api/v1/people/8477293, D, D, R, C, C, L, D, D, D, L, L, C, R, D, D, L, C, R, R, D, D, L, C, C, R, D, D, D, L, C, L, R, D, R, G, G, G, Defenseman, Defenseman, Right Wing, Center, Center, Left Wing, Defenseman, Defenseman, Defenseman, Left Wing, Left Wing, Center, Right Wing, Defenseman, Defenseman, Left Wing, Center, Right Wing, Right Wing, Defenseman, Defenseman, Left Wing, Center, Center, Right Wing, Defenseman, Defenseman, Defenseman, Left Wing, Center, Left Wing, Right Wing, Defenseman, Right Wing, Goalie, Goalie, Goalie, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, D, D, RW, C, C, LW, D, D, D, LW, LW, C, RW, D, D, LW, C, RW, RW, D, D, LW, C, C, RW, D, D, D, LW, C, LW, RW, D, RW, G, G, G
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   11, 72, NA, 40, 55, 90, 28, 52, 27, 20, 93, 46, 8, 43, 42, 18, 14, 32, 41, 90, 15, 15, 67, 3, 13, 61, 41, 65, 35, 38, 50, 34, 8466149, 8467396, 8467514, 8468083, 8468509, 8469457, 8469623, 8470318, 8470724, 8470778, 8471309, 8471693, 8471716, 8471794, 8474090, 8474168, 8474679, 8474873, 8475157, 8475193, 8475250, 8475772, 8475800, 8476289, 8476430, 8476443, 8476822, 8477215, 8470657, 8474591, 8475361, 8475852, Daniel Cleary, Erik Cole, Pavel Datsyuk, Henrik Zetterberg, Niklas Kronwall, Stephen Weiss, Marek Zidlicky, Jonathan Ericsson, Kyle Quincey, Drew Miller, Johan Franzen, Jakub Kindl, Justin Abdelkader, Darren Helm, Brendan Smith, Joakim Andersson, Gustav Nyquist, Brian Lashoff, Landon Ferraro, Tomas Tatar, Andrej Nestrasil, Riley Sheahan, Teemu Pulkkinen, Alexey Marchenko, Tomas Jurco, Xavier Ouellet, Luke Glendening, Danny DeKeyser, Jimmy Howard, Tom McCollum, Jonas Gustavsson, Petr Mrazek, /api/v1/people/8466149, /api/v1/people/8467396, /api/v1/people/8467514, /api/v1/people/8468083, /api/v1/people/8468509, /api/v1/people/8469457, /api/v1/people/8469623, /api/v1/people/8470318, /api/v1/people/8470724, /api/v1/people/8470778, /api/v1/people/8471309, /api/v1/people/8471693, /api/v1/people/8471716, /api/v1/people/8471794, /api/v1/people/8474090, /api/v1/people/8474168, /api/v1/people/8474679, /api/v1/people/8474873, /api/v1/people/8475157, /api/v1/people/8475193, /api/v1/people/8475250, /api/v1/people/8475772, /api/v1/people/8475800, /api/v1/people/8476289, /api/v1/people/8476430, /api/v1/people/8476443, /api/v1/people/8476822, /api/v1/people/8477215, /api/v1/people/8470657, /api/v1/people/8474591, /api/v1/people/8475361, /api/v1/people/8475852, R, L, C, C, D, C, D, D, D, L, R, D, L, L, D, C, C, D, C, L, C, C, L, D, R, D, C, D, G, G, G, G, Right Wing, Left Wing, Center, Center, Defenseman, Center, Defenseman, Defenseman, Defenseman, Left Wing, Right Wing, Defenseman, Left Wing, Left Wing, Defenseman, Center, Center, Defenseman, Center, Left Wing, Center, Center, Left Wing, Defenseman, Right Wing, Defenseman, Center, Defenseman, Goalie, Goalie, Goalie, Goalie, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Goalie, Goalie, Goalie, Goalie, RW, LW, C, C, D, C, D, D, D, LW, RW, D, LW, LW, D, C, C, D, C, LW, C, C, LW, D, RW, D, C, D, G, G, G, G
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             7, 13, 12, 63, 20, 28, 8, 24, 6, 25, 18, 25, 11, 24, 28, 49, 22, 59, 4, 28, 14, 12, 57, 19, 22, 33, 10, 9, 3, 22, 33, 35, 40, 31, 8464989, 8466140, 8467370, 8467371, 8468501, 8468700, 8469485, 8470180, 8470642, 8471390, 8471707, 8471733, 8471742, 8473537, 8473658, 8473921, 8474569, 8474600, 8475176, 8475192, 8475218, 8475225, 8475268, 8475714, 8475868, 8476062, 8476447, 8476887, 8477495, 8477942, 8478042, 8471469, 8475622, 8476992, Matt Cullen, Olli Jokinen, Mike Fisher, Mike Ribeiro, Anton Volchenkov, Paul Gaustad, Derek Roy, Eric Nystrom, Shea Weber, Mike Santorelli, James Neal, Rich Clune, Cody Franson, Viktor Stalberg, Victor Bartley, Joe Piskula, Colin Wilson, Roman Josi, Ryan Ellis, Taylor Beck, Mattias Ekholm, Craig Smith, Gabriel Bourque, Calle Jarnkrok, Anthony Bitetto, Mark Arcobello, Miikka Salomaki, Filip Forsberg, Seth Jones, Kevin Fiala, Viktor Arvidsson, Pekka Rinne, Carter Hutton, Marek Mazanec, /api/v1/people/8464989, /api/v1/people/8466140, /api/v1/people/8467370, /api/v1/people/8467371, /api/v1/people/8468501, /api/v1/people/8468700, /api/v1/people/8469485, /api/v1/people/8470180, /api/v1/people/8470642, /api/v1/people/8471390, /api/v1/people/8471707, /api/v1/people/8471733, /api/v1/people/8471742, /api/v1/people/8473537, /api/v1/people/8473658, /api/v1/people/8473921, /api/v1/people/8474569, /api/v1/people/8474600, /api/v1/people/8475176, /api/v1/people/8475192, /api/v1/people/8475218, /api/v1/people/8475225, /api/v1/people/8475268, /api/v1/people/8475714, /api/v1/people/8475868, /api/v1/people/8476062, /api/v1/people/8476447, /api/v1/people/8476887, /api/v1/people/8477495, /api/v1/people/8477942, /api/v1/people/8478042, /api/v1/people/8471469, /api/v1/people/8475622, /api/v1/people/8476992, C, C, C, C, D, C, C, L, D, C, L, L, D, L, D, D, C, D, D, L, D, C, L, C, D, R, R, L, D, L, R, G, G, G, Center, Center, Center, Center, Defenseman, Center, Center, Left Wing, Defenseman, Center, Left Wing, Left Wing, Defenseman, Left Wing, Defenseman, Defenseman, Center, Defenseman, Defenseman, Left Wing, Defenseman, Center, Left Wing, Center, Defenseman, Right Wing, Right Wing, Left Wing, Defenseman, Left Wing, Right Wing, Goalie, Goalie, Goalie, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Goalie, Goalie, Goalie, C, C, C, C, D, C, C, LW, D, C, LW, LW, D, LW, D, D, C, D, D, LW, D, C, LW, C, D, RW, RW, LW, D, LW, RW, G, G, G
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      13, 5, 33, 92, 57, 4, 19, 15, 20, 40, 21, 29, 7, 25, 77, 25, 75, 10, 28, 22, 4, 41, 7, 15, 56, 91, 17, 8, 23, 48, 30, 37, 34, 8466140, 8467890, 8467917, 8468505, 8469473, 8469760, 8470151, 8470152, 8470257, 8470654, 8470655, 8470662, 8470871, 8471669, 8471698, 8471761, 8471817, 8473534, 8474013, 8474031, 8474125, 8474145, 8474565, 8474627, 8475175, 8475765, 8475768, 8476427, 8476436, 8476989, 8455710, 8470880, 8474596, Olli Jokinen, Barret Jackman, Jordan Leopold, Steve Ott, Marcel Goc, Zbynek Michalek, Jay Bouwmeester, Joakim Lindstrom, Alexander Steen, Maxim Lapierre, David Backes, Colin Fraser, Chris Porter, Paul Stastny, T.J. Oshie, Chris Butler, Ryan Reaves, Patrik Berglund, Ian Cole, Kevin Shattenkirk, Carl Gunnarsson, Robert Bortuzzo, Alex Pietrangelo, Jori Lehtera, Magnus Paajarvi, Vladimir Tarasenko, Jaden Schwartz, Ty Rattie, Dmitrij Jaskin, Petteri Lindbohm, Martin Brodeur, Brian Elliott, Jake Allen, /api/v1/people/8466140, /api/v1/people/8467890, /api/v1/people/8467917, /api/v1/people/8468505, /api/v1/people/8469473, /api/v1/people/8469760, /api/v1/people/8470151, /api/v1/people/8470152, /api/v1/people/8470257, /api/v1/people/8470654, /api/v1/people/8470655, /api/v1/people/8470662, /api/v1/people/8470871, /api/v1/people/8471669, /api/v1/people/8471698, /api/v1/people/8471761, /api/v1/people/8471817, /api/v1/people/8473534, /api/v1/people/8474013, /api/v1/people/8474031, /api/v1/people/8474125, /api/v1/people/8474145, /api/v1/people/8474565, /api/v1/people/8474627, /api/v1/people/8475175, /api/v1/people/8475765, /api/v1/people/8475768, /api/v1/people/8476427, /api/v1/people/8476436, /api/v1/people/8476989, /api/v1/people/8455710, /api/v1/people/8470880, /api/v1/people/8474596, C, D, D, C, C, D, D, C, L, C, R, C, L, C, R, D, R, C, D, D, D, D, D, C, L, R, L, R, R, D, G, G, G, Center, Defenseman, Defenseman, Center, Center, Defenseman, Defenseman, Center, Left Wing, Center, Right Wing, Center, Left Wing, Center, Right Wing, Defenseman, Right Wing, Center, Defenseman, Defenseman, Defenseman, Defenseman, Defenseman, Center, Left Wing, Right Wing, Left Wing, Right Wing, Right Wing, Defenseman, Goalie, Goalie, Goalie, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Goalie, Goalie, Goalie, C, D, D, C, C, D, D, C, LW, C, RW, C, LW, C, RW, D, RW, C, D, D, D, D, D, C, LW, RW, LW, RW, RW, D, G, G, G
    ## 19                                                                                                                                                                                                                                                                                                                                   40, 5, 6, 18, 22, 21, 12, 5, 22, 15, 39, 10, 4, 21, 41, 11, 8, 17, 78, 29, 10, 52, 23, NA, 79, 33, 13, 60, 26, 47, 77, 28, 23, 36, 45, 9, 31, 1, 37, 8467977, 8468674, 8469770, 8470162, 8470201, 8470714, 8470877, 8470966, 8471185, 8471222, 8471664, 8471682, 8471729, 8473673, 8474038, 8474150, 8474577, 8474642, 8474673, 8475213, 8475260, 8475650, 8475733, 8475829, 8475907, 8476244, 8476346, 8476440, 8476452, 8476466, 8476967, 8477486, 8477497, 8477591, 8477913, 8477935, 8471403, 8473972, 8475299, Brian McGrattan, Deryk Engelland, Dennis Wideman, Matt Stajan, Jiri Hudler, Corey Potter, David Jones, Mark Giordano, Curtis Glencross, Ladislav Smid, Mason Raymond, Devin Setoguchi, Kris Russell, David Schlemko, Paul Byron, Mikael Backlund, Joe Colborne, Lance Bouma, TJ Brodie, Drew Shore, Corban Knight, Brandon Bollig, Max Reinhart, John Ramage, Micheal Ferland, Raphael Diaz, Johnny Gaudreau, Markus Granlund, Tyler Wotherspoon, Sven Baertschi, Brett Kulak, Emile Poirier, Sean Monahan, Josh Jooris, David Wolf, Sam Bennett, Karri Ramo, Jonas Hiller, Joni Ortio, /api/v1/people/8467977, /api/v1/people/8468674, /api/v1/people/8469770, /api/v1/people/8470162, /api/v1/people/8470201, /api/v1/people/8470714, /api/v1/people/8470877, /api/v1/people/8470966, /api/v1/people/8471185, /api/v1/people/8471222, /api/v1/people/8471664, /api/v1/people/8471682, /api/v1/people/8471729, /api/v1/people/8473673, /api/v1/people/8474038, /api/v1/people/8474150, /api/v1/people/8474577, /api/v1/people/8474642, /api/v1/people/8474673, /api/v1/people/8475213, /api/v1/people/8475260, /api/v1/people/8475650, /api/v1/people/8475733, /api/v1/people/8475829, /api/v1/people/8475907, /api/v1/people/8476244, /api/v1/people/8476346, /api/v1/people/8476440, /api/v1/people/8476452, /api/v1/people/8476466, /api/v1/people/8476967, /api/v1/people/8477486, /api/v1/people/8477497, /api/v1/people/8477591, /api/v1/people/8477913, /api/v1/people/8477935, /api/v1/people/8471403, /api/v1/people/8473972, /api/v1/people/8475299, R, D, D, C, R, D, R, D, L, D, L, R, D, D, L, C, C, L, D, C, C, L, C, D, L, D, L, C, D, L, D, L, C, R, L, C, G, G, G, Right Wing, Defenseman, Defenseman, Center, Right Wing, Defenseman, Right Wing, Defenseman, Left Wing, Defenseman, Left Wing, Right Wing, Defenseman, Defenseman, Left Wing, Center, Center, Left Wing, Defenseman, Center, Center, Left Wing, Center, Defenseman, Left Wing, Defenseman, Left Wing, Center, Defenseman, Left Wing, Defenseman, Left Wing, Center, Right Wing, Left Wing, Center, Goalie, Goalie, Goalie, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Goalie, Goalie, Goalie, RW, D, D, C, RW, D, RW, D, LW, D, LW, RW, D, D, LW, C, C, LW, D, C, C, LW, C, D, LW, D, LW, C, D, LW, D, LW, C, RW, LW, C, G, G, G
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                           12, 48, 17, 40, 25, 34, 8, 7, 58, 55, 24, 6, 88, 44, 34, 22, 38, 33, 90, 95, 22, 29, 81, 26, 38, 13, 23, 36, 92, 15, 37, 46, 29, 45, 71, 1, 40, 31, 8462042, 8464975, 8467331, 8467338, 8470171, 8470317, 8470699, 8470750, 8471326, 8471657, 8471718, 8473446, 8473465, 8473700, 8474008, 8474207, 8474743, 8475150, 8475158, 8475168, 8475197, 8475206, 8475266, 8475461, 8475767, 8475878, 8475958, 8476043, 8476455, 8476464, 8476536, 8476798, 8477492, 8477902, 8477916, 8473499, 8473575, 8475717, Jarome Iginla, Daniel Briere, Brad Stuart, Alex Tanguay, Max Talbot, Nate Guenin, Jan Hejda, John Mitchell, Patrick Bordeleau, Cody McLeod, Marc-Andre Cliche, Erik Johnson, Jamie McGinn, Ryan Wilson, Paul Carey, Nick Holden, Zach Redmond, Jordan Caron, Ryan O'Reilly, Matt Duchene, Tyson Barrie, Stefan Elliott, Tomas Vincour, Andrew Agozzino, Joey Hishon, Freddie Hamilton, Michael Sgarbossa, Ben Street, Gabriel Landeskog, Duncan Siemens, Colin Smith, Karl Stollery, Nathan MacKinnon, Dennis Everberg, Borna Rendulic, Reto Berra, Semyon Varlamov, Calvin Pickard, /api/v1/people/8462042, /api/v1/people/8464975, /api/v1/people/8467331, /api/v1/people/8467338, /api/v1/people/8470171, /api/v1/people/8470317, /api/v1/people/8470699, /api/v1/people/8470750, /api/v1/people/8471326, /api/v1/people/8471657, /api/v1/people/8471718, /api/v1/people/8473446, /api/v1/people/8473465, /api/v1/people/8473700, /api/v1/people/8474008, /api/v1/people/8474207, /api/v1/people/8474743, /api/v1/people/8475150, /api/v1/people/8475158, /api/v1/people/8475168, /api/v1/people/8475197, /api/v1/people/8475206, /api/v1/people/8475266, /api/v1/people/8475461, /api/v1/people/8475767, /api/v1/people/8475878, /api/v1/people/8475958, /api/v1/people/8476043, /api/v1/people/8476455, /api/v1/people/8476464, /api/v1/people/8476536, /api/v1/people/8476798, /api/v1/people/8477492, /api/v1/people/8477902, /api/v1/people/8477916, /api/v1/people/8473499, /api/v1/people/8473575, /api/v1/people/8475717, R, C, D, L, C, D, D, C, L, L, C, D, L, D, C, D, D, R, C, C, D, D, C, L, C, C, C, C, L, D, C, D, C, R, R, G, G, G, Right Wing, Center, Defenseman, Left Wing, Center, Defenseman, Defenseman, Center, Left Wing, Left Wing, Center, Defenseman, Left Wing, Defenseman, Center, Defenseman, Defenseman, Right Wing, Center, Center, Defenseman, Defenseman, Center, Left Wing, Center, Center, Center, Center, Left Wing, Defenseman, Center, Defenseman, Center, Right Wing, Right Wing, Goalie, Goalie, Goalie, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Goalie, Goalie, Goalie, RW, C, D, LW, C, D, D, C, LW, LW, C, D, LW, D, C, D, D, RW, C, C, D, D, C, LW, C, C, C, C, LW, D, C, D, C, RW, RW, G, G, G
    ## 21 21, 15, 8, 27, 86, 67, 5, 48, 26, 6, 13, 9, 12, 17, 22, 57, 7, 2, 51, NA, 52, 70, 17, 71, 88, 33, 41, 26, 85, 93, 77, 77, 64, 72, 41, 25, 82, 29, 32, 40, 34, 30, 35, 8466333, 8468611, 8469485, 8470159, 8471348, 8471678, 8471816, 8472424, 8473507, 8473514, 8473911, 8473962, 8473970, 8473989, 8474048, 8474102, 8474586, 8474602, 8475165, 8475671, 8475716, 8475734, 8475752, 8475791, 8475869, 8476062, 8476179, 8476336, 8476426, 8476454, 8476472, 8476779, 8476855, 8477249, 8477410, 8477498, 8477851, 8477934, 8473614, 8475681, 8475781, 8476316, 8476839, Andrew Ference, Matt Hendricks, Derek Roy, Boyd Gordon, Nikita Nikitin, Benoit Pouliot, Mark Fayne, Ryan Hamilton, Jeff Petry, Jesse Joensuu, Steven Pinizzotto, Teddy Purcell, Rob Klinkhammer, Luke Gazdic, Keith Aulie, David Perron, Jordan Eberle, Justin Schultz, Anton Lander, Matt Fraser, Martin Marincin, Curtis Hamilton, Tyler Pitlick, Taylor Hall, Brandon Davidson, Mark Arcobello, Will Acton, Iiro Pakarinen, David Musil, Ryan Nugent-Hopkins, Oscar Klefbom, Brad Hunt, Nail Yakupov, Andrew Miller, Bogdan Yakimov, Darnell Nurse, Jordan Oesterle, Leon Draisaitl, Richard Bachman, Ben Scrivens, Tyler Bunz, Laurent Brossoit, Viktor Fasth, /api/v1/people/8466333, /api/v1/people/8468611, /api/v1/people/8469485, /api/v1/people/8470159, /api/v1/people/8471348, /api/v1/people/8471678, /api/v1/people/8471816, /api/v1/people/8472424, /api/v1/people/8473507, /api/v1/people/8473514, /api/v1/people/8473911, /api/v1/people/8473962, /api/v1/people/8473970, /api/v1/people/8473989, /api/v1/people/8474048, /api/v1/people/8474102, /api/v1/people/8474586, /api/v1/people/8474602, /api/v1/people/8475165, /api/v1/people/8475671, /api/v1/people/8475716, /api/v1/people/8475734, /api/v1/people/8475752, /api/v1/people/8475791, /api/v1/people/8475869, /api/v1/people/8476062, /api/v1/people/8476179, /api/v1/people/8476336, /api/v1/people/8476426, /api/v1/people/8476454, /api/v1/people/8476472, /api/v1/people/8476779, /api/v1/people/8476855, /api/v1/people/8477249, /api/v1/people/8477410, /api/v1/people/8477498, /api/v1/people/8477851, /api/v1/people/8477934, /api/v1/people/8473614, /api/v1/people/8475681, /api/v1/people/8475781, /api/v1/people/8476316, /api/v1/people/8476839, D, C, C, C, D, L, D, L, D, L, R, L, L, L, D, L, R, D, C, R, D, L, C, L, D, R, C, R, D, C, D, D, R, C, C, D, D, C, G, G, G, G, G, Defenseman, Center, Center, Center, Defenseman, Left Wing, Defenseman, Left Wing, Defenseman, Left Wing, Right Wing, Left Wing, Left Wing, Left Wing, Defenseman, Left Wing, Right Wing, Defenseman, Center, Right Wing, Defenseman, Left Wing, Center, Left Wing, Defenseman, Right Wing, Center, Right Wing, Defenseman, Center, Defenseman, Defenseman, Right Wing, Center, Center, Defenseman, Defenseman, Center, Goalie, Goalie, Goalie, Goalie, Goalie, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Goalie, Goalie, Goalie, Goalie, Goalie, D, C, C, C, D, LW, D, LW, D, LW, RW, LW, LW, LW, D, LW, RW, D, C, RW, D, LW, C, LW, D, RW, C, RW, D, C, D, D, RW, C, C, D, D, C, G, G, G, G, G
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          22, 33, 71, 5, 3, 20, 14, 15, 23, 36, 3, NA, 25, 16, 13, 3, 55, 21, 44, 16, 20, 8, 43, 18, 6, 47, 39, 53, 41, 30, 25, 31, 8467875, 8467876, 8468085, 8469465, 8469598, 8470274, 8470358, 8470755, 8471303, 8471498, 8473415, 8473432, 8473542, 8473574, 8474009, 8474134, 8474579, 8474646, 8475178, 8475223, 8475598, 8475690, 8476214, 8476302, 8476431, 8476466, 8476482, 8477500, 8477589, 8468011, 8474593, 8475663, Daniel Sedin, Henrik Sedin, Radim Vrbata, Dan Hamhuis, Kevin Bieksa, Chris Higgins, Alexandre Burrows, Brad Richardson, Alexander Edler, Jannik Hansen, Alex Biega, Derek Dorsett, Tom Sestito, Shawn Matthias, Nick Bonino, Yannick Weber, Luca Sbisa, Brandon McMillan, Zack Kassian, Linden Vey, Ryan Stanton, Christopher Tanev, Brandon DeFazio, Frank Corrado, Adam Clendening, Sven Baertschi, Nicklas Jensen, Bo Horvat, Ronalds Kenins, Ryan Miller, Jacob Markstrom, Eddie Lack, /api/v1/people/8467875, /api/v1/people/8467876, /api/v1/people/8468085, /api/v1/people/8469465, /api/v1/people/8469598, /api/v1/people/8470274, /api/v1/people/8470358, /api/v1/people/8470755, /api/v1/people/8471303, /api/v1/people/8471498, /api/v1/people/8473415, /api/v1/people/8473432, /api/v1/people/8473542, /api/v1/people/8473574, /api/v1/people/8474009, /api/v1/people/8474134, /api/v1/people/8474579, /api/v1/people/8474646, /api/v1/people/8475178, /api/v1/people/8475223, /api/v1/people/8475598, /api/v1/people/8475690, /api/v1/people/8476214, /api/v1/people/8476302, /api/v1/people/8476431, /api/v1/people/8476466, /api/v1/people/8476482, /api/v1/people/8477500, /api/v1/people/8477589, /api/v1/people/8468011, /api/v1/people/8474593, /api/v1/people/8475663, L, C, R, D, D, L, R, R, D, R, D, R, L, L, C, D, D, L, R, R, D, D, L, D, D, L, R, C, L, G, G, G, Left Wing, Center, Right Wing, Defenseman, Defenseman, Left Wing, Right Wing, Right Wing, Defenseman, Right Wing, Defenseman, Right Wing, Left Wing, Left Wing, Center, Defenseman, Defenseman, Left Wing, Right Wing, Right Wing, Defenseman, Defenseman, Left Wing, Defenseman, Defenseman, Left Wing, Right Wing, Center, Left Wing, Goalie, Goalie, Goalie, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Goalie, Goalie, Goalie, LW, C, RW, D, D, LW, RW, RW, D, RW, D, RW, LW, LW, C, D, D, LW, RW, RW, D, D, LW, D, D, LW, RW, C, LW, G, G, G
    ## 23                                                                                                                                                                                                          2, 6, 23, 15, NA, 12, 21, 15, 17, 94, 11, 4, 28, 17, 11, 39, 21, 14, 48, 21, 6, 5, 33, 77, 45, 25, 4, 26, 14, 42, 49, 71, 26, 67, 47, 36, 45, 80, 31, 36, 8466142, 8467332, 8467400, 8468482, 8469490, 8470039, 8470222, 8470612, 8470616, 8470621, 8470775, 8470886, 8471241, 8471504, 8471699, 8473492, 8473933, 8474034, 8474606, 8475151, 8475155, 8475162, 8475164, 8475203, 8475222, 8475758, 8475764, 8475770, 8475780, 8476312, 8476384, 8476448, 8476474, 8476483, 8476854, 8478137, 8467391, 8468524, 8475883, 8476434, Eric Brewer, Bryan Allen, Francois Beauchemin, Dany Heatley, Tim Jackman, Tomas Fleischmann, James Wisniewski, Ryan Getzlaf, Ryan Kesler, Corey Perry, Nate Thompson, Clayton Stoner, Mark Fistric, Rene Bourque, Andrew Cogliano, Matt Beleskey, Ben Lovejoy, Pat Maroon, Colby Robak, Kyle Palmieri, Simon Despres, Mat Clark, Jakob Silfverberg, Jesse Blacker, Sami Vatanen, Devante Smith-Pelly, Cam Fowler, Emerson Etem, Chris Wagner, Josh Manson, Max Friberg, William Karlsson, Stefan Noesen, Rickard Rakell, Hampus Lindholm, Jiri Sekac, Jason LaBarbera, Ilya Bryzgalov, Frederik Andersen, John Gibson, /api/v1/people/8466142, /api/v1/people/8467332, /api/v1/people/8467400, /api/v1/people/8468482, /api/v1/people/8469490, /api/v1/people/8470039, /api/v1/people/8470222, /api/v1/people/8470612, /api/v1/people/8470616, /api/v1/people/8470621, /api/v1/people/8470775, /api/v1/people/8470886, /api/v1/people/8471241, /api/v1/people/8471504, /api/v1/people/8471699, /api/v1/people/8473492, /api/v1/people/8473933, /api/v1/people/8474034, /api/v1/people/8474606, /api/v1/people/8475151, /api/v1/people/8475155, /api/v1/people/8475162, /api/v1/people/8475164, /api/v1/people/8475203, /api/v1/people/8475222, /api/v1/people/8475758, /api/v1/people/8475764, /api/v1/people/8475770, /api/v1/people/8475780, /api/v1/people/8476312, /api/v1/people/8476384, /api/v1/people/8476448, /api/v1/people/8476474, /api/v1/people/8476483, /api/v1/people/8476854, /api/v1/people/8478137, /api/v1/people/8467391, /api/v1/people/8468524, /api/v1/people/8475883, /api/v1/people/8476434, D, D, D, L, R, L, D, C, C, R, C, D, D, R, C, L, D, L, D, R, D, D, R, D, D, R, D, R, R, D, R, C, R, L, D, L, G, G, G, G, Defenseman, Defenseman, Defenseman, Left Wing, Right Wing, Left Wing, Defenseman, Center, Center, Right Wing, Center, Defenseman, Defenseman, Right Wing, Center, Left Wing, Defenseman, Left Wing, Defenseman, Right Wing, Defenseman, Defenseman, Right Wing, Defenseman, Defenseman, Right Wing, Defenseman, Right Wing, Right Wing, Defenseman, Right Wing, Center, Right Wing, Left Wing, Defenseman, Left Wing, Goalie, Goalie, Goalie, Goalie, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, Goalie, D, D, D, LW, RW, LW, D, C, C, RW, C, D, D, RW, C, LW, D, LW, D, RW, D, D, RW, D, D, RW, D, RW, RW, D, RW, C, RW, LW, D, LW, G, G, G, G
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     55, 72, 22, 27, 19, 83, 83, 83, 18, 33, 39, 21, 14, 7, 55, 40, 26, 20, 44, 81, 4, 24, 91, NA, 3, 16, 23, 24, 2, 13, 32, 31, 39, 40, 8458951, 8467396, 8467423, 8468635, 8469455, 8469466, 8469759, 8470110, 8470622, 8471274, 8471474, 8473673, 8473994, 8474098, 8474218, 8474818, 8474849, 8475236, 8475246, 8475310, 8475455, 8475747, 8475794, 8475896, 8475906, 8476116, 8476279, 8476439, 8476467, 8477501, 8470140, 8473523, 8474765, 8475680, Sergei Gonchar, Erik Cole, Shawn Horcoff, Travis Moen, Jason Spezza, Ales Hemsky, Vernon Fiddler, Trevor Daley, Patrick Eaves, Alex Goligoski, Travis Morin, David Schlemko, Jamie Benn, Colton Sceviour, Jason Demers, Jordie Benn, Antoine Roussel, Cody Eakin, Kevin Connauton, Curtis McKenzie, Brenden Dillon, Patrik Nemeth, Tyler Seguin, Brendan Ranford, John Klingberg, Ryan Garbutt, Jyrki Jokipakka, Brett Ritchie, Jamie Oleksiak, Valeri Nichushkin, Kari Lehtonen, Jhonas Enroth, Anders Lindback, Jussi Rynnas, /api/v1/people/8458951, /api/v1/people/8467396, /api/v1/people/8467423, /api/v1/people/8468635, /api/v1/people/8469455, /api/v1/people/8469466, /api/v1/people/8469759, /api/v1/people/8470110, /api/v1/people/8470622, /api/v1/people/8471274, /api/v1/people/8471474, /api/v1/people/8473673, /api/v1/people/8473994, /api/v1/people/8474098, /api/v1/people/8474218, /api/v1/people/8474818, /api/v1/people/8474849, /api/v1/people/8475236, /api/v1/people/8475246, /api/v1/people/8475310, /api/v1/people/8475455, /api/v1/people/8475747, /api/v1/people/8475794, /api/v1/people/8475896, /api/v1/people/8475906, /api/v1/people/8476116, /api/v1/people/8476279, /api/v1/people/8476439, /api/v1/people/8476467, /api/v1/people/8477501, /api/v1/people/8470140, /api/v1/people/8473523, /api/v1/people/8474765, /api/v1/people/8475680, D, L, C, L, C, R, C, D, R, D, C, D, L, R, D, D, L, C, D, L, D, D, C, L, D, C, D, R, D, R, G, G, G, G, Defenseman, Left Wing, Center, Left Wing, Center, Right Wing, Center, Defenseman, Right Wing, Defenseman, Center, Defenseman, Left Wing, Right Wing, Defenseman, Defenseman, Left Wing, Center, Defenseman, Left Wing, Defenseman, Defenseman, Center, Left Wing, Defenseman, Center, Defenseman, Right Wing, Defenseman, Right Wing, Goalie, Goalie, Goalie, Goalie, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, Goalie, D, LW, C, LW, C, RW, C, D, RW, D, C, D, LW, RW, D, D, LW, C, D, LW, D, D, C, LW, D, C, D, RW, D, RW, G, G, G, G
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          44, 12, 14, 19, 2, 36, 77, 23, 10, 55, 5, 11, 23, 2, 21, 8, 23, 8, 26, 13, 3, 71, 73, 10, 21, 70, 32, 31, 8467344, 8468483, 8468508, 8468526, 8470121, 8470189, 8470604, 8470606, 8470617, 8471240, 8471284, 8471685, 8473453, 8473571, 8474100, 8474162, 8474166, 8474563, 8474594, 8475160, 8475188, 8475325, 8475726, 8476404, 8476406, 8476871, 8471734, 8474889, Robyn Regehr, Marian Gaborik, Justin Williams, Jarret Stoll, Matt Greene, David Van Der Gulik, Jeff Carter, Dustin Brown, Mike Richards, Jeff Schultz, Andrej Sekera, Anze Kopitar, Trevor Lewis, Jamie McBain, Dwight King, Jake Muzzin, Alec Martinez, Drew Doughty, Slava Voynov, Kyle Clifford, Brayden McNabb, Jordan Nolan, Tyler Toffoli, Andy Andreoff, Nicholas Shore, Tanner Pearson, Jonathan Quick, Martin Jones, /api/v1/people/8467344, /api/v1/people/8468483, /api/v1/people/8468508, /api/v1/people/8468526, /api/v1/people/8470121, /api/v1/people/8470189, /api/v1/people/8470604, /api/v1/people/8470606, /api/v1/people/8470617, /api/v1/people/8471240, /api/v1/people/8471284, /api/v1/people/8471685, /api/v1/people/8473453, /api/v1/people/8473571, /api/v1/people/8474100, /api/v1/people/8474162, /api/v1/people/8474166, /api/v1/people/8474563, /api/v1/people/8474594, /api/v1/people/8475160, /api/v1/people/8475188, /api/v1/people/8475325, /api/v1/people/8475726, /api/v1/people/8476404, /api/v1/people/8476406, /api/v1/people/8476871, /api/v1/people/8471734, /api/v1/people/8474889, D, R, R, C, D, L, C, R, C, D, D, C, C, D, L, D, D, D, D, L, D, C, R, C, C, L, G, G, Defenseman, Right Wing, Right Wing, Center, Defenseman, Left Wing, Center, Right Wing, Center, Defenseman, Defenseman, Center, Center, Defenseman, Left Wing, Defenseman, Defenseman, Defenseman, Defenseman, Left Wing, Defenseman, Center, Right Wing, Center, Center, Left Wing, Goalie, Goalie, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Goalie, Goalie, D, RW, RW, C, D, LW, C, RW, C, D, D, C, C, D, LW, D, D, D, D, LW, D, C, RW, C, C, LW, G, G
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               97, 12, 27, 37, 88, 16, 48, 13, 44, 11, 45, 22, 61, 39, 55, 38, 11, 18, 57, 4, 44, 44, 13, 42, 83, NA, 19, 46, 7, 48, 71, 76, 25, 68, 32, 37, 1, 8466138, 8466139, 8466158, 8470063, 8470613, 8470794, 8471311, 8471371, 8471709, 8471956, 8473536, 8473722, 8474027, 8474053, 8474218, 8474230, 8474516, 8474727, 8474739, 8475455, 8475625, 8475775, 8475878, 8476166, 8476442, 8476549, 8476624, 8476798, 8476807, 8476881, 8476919, 8477227, 8477509, 8477922, 8471774, 8474550, 8477234, Joe Thornton, Patrick Marleau, Scott Hannan, Adam Burish, Brent Burns, Joe Pavelski, Tyler Kennedy, Mike Brown, Marc-Edouard Vlasic, Andrew Desjardins, James Sheppard, John Scott, Justin Braun, Logan Couture, Jason Demers, Micheal Haley, Bryan Lerg, Ben Smith, Tommy Wingels, Brenden Dillon, Matt Irwin, Tye McGinn, Freddie Hamilton, Taylor Fedun, Matt Nieto, Daniil Tarasov, Barclay Goodrow, Karl Stollery, Matt Tennyson, Tomas Hertl, Chris Tierney, Eriah Hayes, Mirco Mueller, Melker Karlsson, Alex Stalock, Antti Niemi, Troy Grosenick, /api/v1/people/8466138, /api/v1/people/8466139, /api/v1/people/8466158, /api/v1/people/8470063, /api/v1/people/8470613, /api/v1/people/8470794, /api/v1/people/8471311, /api/v1/people/8471371, /api/v1/people/8471709, /api/v1/people/8471956, /api/v1/people/8473536, /api/v1/people/8473722, /api/v1/people/8474027, /api/v1/people/8474053, /api/v1/people/8474218, /api/v1/people/8474230, /api/v1/people/8474516, /api/v1/people/8474727, /api/v1/people/8474739, /api/v1/people/8475455, /api/v1/people/8475625, /api/v1/people/8475775, /api/v1/people/8475878, /api/v1/people/8476166, /api/v1/people/8476442, /api/v1/people/8476549, /api/v1/people/8476624, /api/v1/people/8476798, /api/v1/people/8476807, /api/v1/people/8476881, /api/v1/people/8476919, /api/v1/people/8477227, /api/v1/people/8477509, /api/v1/people/8477922, /api/v1/people/8471774, /api/v1/people/8474550, /api/v1/people/8477234, C, C, D, R, D, C, C, R, D, L, C, L, D, C, D, L, C, R, C, D, D, L, C, D, L, R, C, D, D, C, C, R, D, C, G, G, G, Center, Center, Defenseman, Right Wing, Defenseman, Center, Center, Right Wing, Defenseman, Left Wing, Center, Left Wing, Defenseman, Center, Defenseman, Left Wing, Center, Right Wing, Center, Defenseman, Defenseman, Left Wing, Center, Defenseman, Left Wing, Right Wing, Center, Defenseman, Defenseman, Center, Center, Right Wing, Defenseman, Center, Goalie, Goalie, Goalie, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Goalie, Goalie, Goalie, C, C, D, RW, D, C, C, RW, D, LW, C, LW, D, C, D, LW, C, RW, C, D, D, LW, C, D, LW, RW, C, D, D, C, C, RW, D, C, G, G, G
    ## 27                                                                                           33, 17, 51, 12, 21, 71, 17, 27, 17, 27, 9, 40, 71, 51, 28, 22, 42, 34, 41, 17, NA, 11, 13, 53, 5, 44, 33, 58, 44, 92, 26, 29, 38, 71, 22, 17, 56, 21, 39, 35, 72, 31, 8467917, 8468486, 8469492, 8470065, 8470222, 8470920, 8471273, 8471490, 8471504, 8471677, 8471681, 8471766, 8473422, 8473573, 8473647, 8473914, 8473992, 8474095, 8474130, 8474597, 8474609, 8474685, 8474715, 8474744, 8474774, 8475148, 8475214, 8475233, 8475246, 8475793, 8475808, 8476207, 8476432, 8476448, 8476850, 8476981, 8477448, 8477505, 8477510, 8470147, 8475683, 8476341, Jordan Leopold, Scott Hartnell, Fedor Tyutin, Ryan Craig, James Wisniewski, David Clarkson, Brandon Dubinsky, Adam Cracknell, Rene Bourque, Jack Johnson, Jack Skille, Jared Boll, Nick Foligno, Artem Anisimov, Frédéric St-Denis, Mark Letestu, Justin Falk, Dana Tyrell, Corey Tropp, Cody Goloubef, Luke Adam, Matt Calvert, Cam Atkinson, Sean Collins, Dalton Prout, Tim Erixon, Jeremy Morin, David Savard, Kevin Connauton, Ryan Johansen, Michael Chaput, Brian Gibbons, Boone Jenner, William Karlsson, Ryan Murray, Josh Anderson, Marko Dano, Alex Wennberg, Kerby Rychel, Curtis McElhinney, Sergei Bobrovsky, Anton Forsberg, /api/v1/people/8467917, /api/v1/people/8468486, /api/v1/people/8469492, /api/v1/people/8470065, /api/v1/people/8470222, /api/v1/people/8470920, /api/v1/people/8471273, /api/v1/people/8471490, /api/v1/people/8471504, /api/v1/people/8471677, /api/v1/people/8471681, /api/v1/people/8471766, /api/v1/people/8473422, /api/v1/people/8473573, /api/v1/people/8473647, /api/v1/people/8473914, /api/v1/people/8473992, /api/v1/people/8474095, /api/v1/people/8474130, /api/v1/people/8474597, /api/v1/people/8474609, /api/v1/people/8474685, /api/v1/people/8474715, /api/v1/people/8474744, /api/v1/people/8474774, /api/v1/people/8475148, /api/v1/people/8475214, /api/v1/people/8475233, /api/v1/people/8475246, /api/v1/people/8475793, /api/v1/people/8475808, /api/v1/people/8476207, /api/v1/people/8476432, /api/v1/people/8476448, /api/v1/people/8476850, /api/v1/people/8476981, /api/v1/people/8477448, /api/v1/people/8477505, /api/v1/people/8477510, /api/v1/people/8470147, /api/v1/people/8475683, /api/v1/people/8476341, D, L, D, C, D, R, C, R, R, D, R, R, L, C, D, C, D, C, R, D, C, L, R, C, D, D, L, D, D, C, C, C, C, C, D, R, C, C, L, G, G, G, Defenseman, Left Wing, Defenseman, Center, Defenseman, Right Wing, Center, Right Wing, Right Wing, Defenseman, Right Wing, Right Wing, Left Wing, Center, Defenseman, Center, Defenseman, Center, Right Wing, Defenseman, Center, Left Wing, Right Wing, Center, Defenseman, Defenseman, Left Wing, Defenseman, Defenseman, Center, Center, Center, Center, Center, Defenseman, Right Wing, Center, Center, Left Wing, Goalie, Goalie, Goalie, Defenseman, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Goalie, Goalie, Goalie, D, LW, D, C, D, RW, C, RW, RW, D, RW, RW, LW, C, D, C, D, C, RW, D, C, LW, RW, C, D, D, LW, D, D, C, C, C, C, C, D, RW, C, C, LW, G, G, G
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         24, 33, 9, 29, 19, 2, 23, 26, 20, 11, 28, 27, 44, 18, 42, 7, 6, 46, 4, 11, 56, 39, 16, 13, 64, 21, 14, 44, 25, 24, 32, 40, 32, 33, 35, 8465951, 8467917, 8469459, 8469506, 8469542, 8470054, 8470176, 8470598, 8470600, 8470610, 8470803, 8471840, 8473485, 8473646, 8473992, 8474164, 8474618, 8474716, 8474772, 8475147, 8475287, 8475613, 8475722, 8475745, 8475798, 8475799, 8476235, 8476344, 8476463, 8476856, 8477850, 8471227, 8473404, 8474202, 8475311, Matt Cooke, Jordan Leopold, Mikko Koivu, Jason Pominville, Stephane Veilleux, Keith Ballard, Sean Bergenheim, Thomas Vanek, Ryan Suter, Zach Parise, Kyle Brodziak, Brett Sutter, Chris Stewart, Ryan Carter, Justin Falk, Jonathon Blum, Marco Scandella, Jared Spurgeon, Stu Bickel, Jordan Schroeder, Erik Haula, Nate Prosser, Jason Zucker, Charlie Coyle, Mikael Granlund, Nino Niederreiter, Justin Fontaine, Tyler Graovac, Jonas Brodin, Matt Dumba, Christian Folin, Devan Dubnyk, Niklas Backstrom, John Curry, Darcy Kuemper, /api/v1/people/8465951, /api/v1/people/8467917, /api/v1/people/8469459, /api/v1/people/8469506, /api/v1/people/8469542, /api/v1/people/8470054, /api/v1/people/8470176, /api/v1/people/8470598, /api/v1/people/8470600, /api/v1/people/8470610, /api/v1/people/8470803, /api/v1/people/8471840, /api/v1/people/8473485, /api/v1/people/8473646, /api/v1/people/8473992, /api/v1/people/8474164, /api/v1/people/8474618, /api/v1/people/8474716, /api/v1/people/8474772, /api/v1/people/8475147, /api/v1/people/8475287, /api/v1/people/8475613, /api/v1/people/8475722, /api/v1/people/8475745, /api/v1/people/8475798, /api/v1/people/8475799, /api/v1/people/8476235, /api/v1/people/8476344, /api/v1/people/8476463, /api/v1/people/8476856, /api/v1/people/8477850, /api/v1/people/8471227, /api/v1/people/8473404, /api/v1/people/8474202, /api/v1/people/8475311, L, D, C, R, L, D, L, L, D, L, C, L, R, L, D, D, D, D, D, C, L, D, L, C, C, R, R, C, D, D, D, G, G, G, G, Left Wing, Defenseman, Center, Right Wing, Left Wing, Defenseman, Left Wing, Left Wing, Defenseman, Left Wing, Center, Left Wing, Right Wing, Left Wing, Defenseman, Defenseman, Defenseman, Defenseman, Defenseman, Center, Left Wing, Defenseman, Left Wing, Center, Center, Right Wing, Right Wing, Center, Defenseman, Defenseman, Defenseman, Goalie, Goalie, Goalie, Goalie, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Goalie, Goalie, Goalie, Goalie, LW, D, C, RW, LW, D, LW, LW, D, LW, C, LW, RW, LW, D, D, D, D, D, C, LW, D, LW, C, C, RW, RW, C, D, D, D, G, G, G, G
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 22, NA, 19, 5, 18, 39, 33, 16, 26, 18, 5, 24, 18, 9, 67, 85, 21, 16, 14, 15, 7, 22, 57, NA, 28, NA, 9, 8, 49, 17, 55, 8, 9, 31, 30, 8469501, 8469531, 8470289, 8470614, 8470740, 8470828, 8470834, 8471217, 8471218, 8471226, 8471385, 8471482, 8473412, 8473482, 8473564, 8473618, 8474001, 8474061, 8474074, 8474094, 8474184, 8474567, 8474574, 8474603, 8474616, 8475159, 8475169, 8475279, 8475567, 8476392, 8476460, 8476885, 8477429, 8471715, 8474636, Chris Thorburn, Jay Harrison, Jim Slater, Mark Stuart, Lee Stempniak, Toby Enstrom, Dustin Byfuglien, Andrew Ladd, Blake Wheeler, Drew Stafford, Adam Pardy, Grant Clitsome, Bryan Little, Jiri Tlusty, Michael Frolik, Mathieu Perreault, TJ Galiardi, Anthony Peluso, Paul Postma, Matt Halischuk, Keaton Ellerby, Zach Bogosian, Tyler Myers, Eric O'Dell, Patrice Cormier, Carl Klingberg, Evander Kane, Ben Chiarot, Julien Brouillette, Adam Lowry, Mark Scheifele, Jacob Trouba, Andrew Copp, Ondrej Pavelec, Michael Hutchinson, /api/v1/people/8469501, /api/v1/people/8469531, /api/v1/people/8470289, /api/v1/people/8470614, /api/v1/people/8470740, /api/v1/people/8470828, /api/v1/people/8470834, /api/v1/people/8471217, /api/v1/people/8471218, /api/v1/people/8471226, /api/v1/people/8471385, /api/v1/people/8471482, /api/v1/people/8473412, /api/v1/people/8473482, /api/v1/people/8473564, /api/v1/people/8473618, /api/v1/people/8474001, /api/v1/people/8474061, /api/v1/people/8474074, /api/v1/people/8474094, /api/v1/people/8474184, /api/v1/people/8474567, /api/v1/people/8474574, /api/v1/people/8474603, /api/v1/people/8474616, /api/v1/people/8475159, /api/v1/people/8475169, /api/v1/people/8475279, /api/v1/people/8475567, /api/v1/people/8476392, /api/v1/people/8476460, /api/v1/people/8476885, /api/v1/people/8477429, /api/v1/people/8471715, /api/v1/people/8474636, R, D, C, D, R, D, D, L, R, R, D, D, C, L, R, L, L, R, D, R, D, D, D, C, C, L, L, D, D, C, C, D, C, G, G, Right Wing, Defenseman, Center, Defenseman, Right Wing, Defenseman, Defenseman, Left Wing, Right Wing, Right Wing, Defenseman, Defenseman, Center, Left Wing, Right Wing, Left Wing, Left Wing, Right Wing, Defenseman, Right Wing, Defenseman, Defenseman, Defenseman, Center, Center, Left Wing, Left Wing, Defenseman, Defenseman, Center, Center, Defenseman, Center, Goalie, Goalie, Forward, Defenseman, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Goalie, Goalie, RW, D, C, D, RW, D, D, LW, RW, RW, D, D, C, LW, RW, LW, LW, RW, D, RW, D, D, D, C, C, LW, LW, D, D, C, C, D, C, G, G
    ## 30                                                        19, 10, 50, 18, 4, 44, 49, 46, 24, 29, 40, 3, 97, 55, 21, 12, 89, 89, 26, 6, 21, 37, 23, 27, 26, 27, 22, NA, 44, 39, 33, 13, 6, 53, 5, 55, 48, 32, 41, 56, 40, 70, 8462038, 8468064, 8468535, 8469664, 8469760, 8470647, 8470719, 8470798, 8471231, 8471232, 8471691, 8471735, 8471856, 8473589, 8473673, 8473970, 8474040, 8474571, 8474628, 8474638, 8474646, 8474793, 8475171, 8475186, 8475194, 8475224, 8475414, 8475759, 8475775, 8475996, 8476062, 8476356, 8476403, 8476451, 8476473, 8476868, 8476921, 8477715, 8469608, 8470093, 8471227, 8475839, Shane Doan, Martin Erat, Antoine Vermette, David Moss, Zbynek Michalek, B.J. Crombeen, Alexandre Bolduc, Dylan Reese, Kyle Chipchura, Lauri Korpikoski, Martin Hanzal, Keith Yandle, Joe Vitale, Chris Summers, David Schlemko, Rob Klinkhammer, Sam Gagner, Mikkel Boedker, Michael Stone, Andrew Campbell, Brandon McMillan, Justin Hodgman, Oliver Ekman-Larsson, John Moore, Philip Samuelsson, Jordan Szwarz, Craig Cunningham, Brandon Gormley, Tye McGinn, Brendan Shinnimin, Mark Arcobello, Tobias Rieder, Klas Dahlbeck, Lucas Lessio, Connor Murphy, Henrik Samuelsson, Jordan Martinook, Tyler Gaudet, Mike Smith, Mike McKenna, Devan Dubnyk, Louis Domingue, /api/v1/people/8462038, /api/v1/people/8468064, /api/v1/people/8468535, /api/v1/people/8469664, /api/v1/people/8469760, /api/v1/people/8470647, /api/v1/people/8470719, /api/v1/people/8470798, /api/v1/people/8471231, /api/v1/people/8471232, /api/v1/people/8471691, /api/v1/people/8471735, /api/v1/people/8471856, /api/v1/people/8473589, /api/v1/people/8473673, /api/v1/people/8473970, /api/v1/people/8474040, /api/v1/people/8474571, /api/v1/people/8474628, /api/v1/people/8474638, /api/v1/people/8474646, /api/v1/people/8474793, /api/v1/people/8475171, /api/v1/people/8475186, /api/v1/people/8475194, /api/v1/people/8475224, /api/v1/people/8475414, /api/v1/people/8475759, /api/v1/people/8475775, /api/v1/people/8475996, /api/v1/people/8476062, /api/v1/people/8476356, /api/v1/people/8476403, /api/v1/people/8476451, /api/v1/people/8476473, /api/v1/people/8476868, /api/v1/people/8476921, /api/v1/people/8477715, /api/v1/people/8469608, /api/v1/people/8470093, /api/v1/people/8471227, /api/v1/people/8475839, R, R, C, R, D, R, C, D, C, L, C, D, C, D, D, L, C, L, D, D, L, C, D, D, D, C, L, D, L, C, R, L, D, L, D, C, L, C, G, G, G, G, Right Wing, Right Wing, Center, Right Wing, Defenseman, Right Wing, Center, Defenseman, Center, Left Wing, Center, Defenseman, Center, Defenseman, Defenseman, Left Wing, Center, Left Wing, Defenseman, Defenseman, Left Wing, Center, Defenseman, Defenseman, Defenseman, Center, Left Wing, Defenseman, Left Wing, Center, Right Wing, Left Wing, Defenseman, Left Wing, Defenseman, Center, Left Wing, Center, Goalie, Goalie, Goalie, Goalie, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Forward, Forward, Defenseman, Defenseman, Defenseman, Forward, Forward, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Defenseman, Forward, Forward, Forward, Goalie, Goalie, Goalie, Goalie, RW, RW, C, RW, D, RW, C, D, C, LW, C, D, C, D, D, LW, C, LW, D, D, LW, C, D, D, D, C, LW, D, LW, C, RW, LW, D, LW, D, C, LW, C, G, G, G, G
    ##                roster.link
    ## 1   /api/v1/teams/1/roster
    ## 2   /api/v1/teams/2/roster
    ## 3   /api/v1/teams/3/roster
    ## 4   /api/v1/teams/4/roster
    ## 5   /api/v1/teams/5/roster
    ## 6   /api/v1/teams/6/roster
    ## 7   /api/v1/teams/7/roster
    ## 8   /api/v1/teams/8/roster
    ## 9   /api/v1/teams/9/roster
    ## 10 /api/v1/teams/10/roster
    ## 11 /api/v1/teams/12/roster
    ## 12 /api/v1/teams/13/roster
    ## 13 /api/v1/teams/14/roster
    ## 14 /api/v1/teams/15/roster
    ## 15 /api/v1/teams/16/roster
    ## 16 /api/v1/teams/17/roster
    ## 17 /api/v1/teams/18/roster
    ## 18 /api/v1/teams/19/roster
    ## 19 /api/v1/teams/20/roster
    ## 20 /api/v1/teams/21/roster
    ## 21 /api/v1/teams/22/roster
    ## 22 /api/v1/teams/23/roster
    ## 23 /api/v1/teams/24/roster
    ## 24 /api/v1/teams/25/roster
    ## 25 /api/v1/teams/26/roster
    ## 26 /api/v1/teams/28/roster
    ## 27 /api/v1/teams/29/roster
    ## 28 /api/v1/teams/30/roster
    ## 29 /api/v1/teams/52/roster
    ## 30 /api/v1/teams/53/roster

``` r
get.stat2("person.names")
```

    ##    id                  name             link abbreviation       teamName locationName firstYearOfPlay    shortName
    ## 1   1     New Jersey Devils  /api/v1/teams/1          NJD         Devils   New Jersey            1982   New Jersey
    ## 2   2    New York Islanders  /api/v1/teams/2          NYI      Islanders     New York            1972 NY Islanders
    ## 3   3      New York Rangers  /api/v1/teams/3          NYR        Rangers     New York            1926   NY Rangers
    ## 4   4   Philadelphia Flyers  /api/v1/teams/4          PHI         Flyers Philadelphia            1967 Philadelphia
    ## 5   5   Pittsburgh Penguins  /api/v1/teams/5          PIT       Penguins   Pittsburgh            1967   Pittsburgh
    ## 6   6         Boston Bruins  /api/v1/teams/6          BOS         Bruins       Boston            1924       Boston
    ## 7   7        Buffalo Sabres  /api/v1/teams/7          BUF         Sabres      Buffalo            1970      Buffalo
    ## 8   8    Montréal Canadiens  /api/v1/teams/8          MTL      Canadiens     Montréal            1909     Montréal
    ## 9   9       Ottawa Senators  /api/v1/teams/9          OTT       Senators       Ottawa            1990       Ottawa
    ## 10 10   Toronto Maple Leafs /api/v1/teams/10          TOR    Maple Leafs      Toronto            1917      Toronto
    ## 11 12   Carolina Hurricanes /api/v1/teams/12          CAR     Hurricanes     Carolina            1979     Carolina
    ## 12 13      Florida Panthers /api/v1/teams/13          FLA       Panthers      Florida            1993      Florida
    ## 13 14   Tampa Bay Lightning /api/v1/teams/14          TBL      Lightning    Tampa Bay            1991    Tampa Bay
    ## 14 15   Washington Capitals /api/v1/teams/15          WSH       Capitals   Washington            1974   Washington
    ## 15 16    Chicago Blackhawks /api/v1/teams/16          CHI     Blackhawks      Chicago            1926      Chicago
    ## 16 17     Detroit Red Wings /api/v1/teams/17          DET      Red Wings      Detroit            1926      Detroit
    ## 17 18   Nashville Predators /api/v1/teams/18          NSH      Predators    Nashville            1997    Nashville
    ## 18 19       St. Louis Blues /api/v1/teams/19          STL          Blues    St. Louis            1967     St Louis
    ## 19 20        Calgary Flames /api/v1/teams/20          CGY         Flames      Calgary            1980      Calgary
    ## 20 21    Colorado Avalanche /api/v1/teams/21          COL      Avalanche     Colorado            1979     Colorado
    ## 21 22       Edmonton Oilers /api/v1/teams/22          EDM         Oilers     Edmonton            1979     Edmonton
    ## 22 23     Vancouver Canucks /api/v1/teams/23          VAN        Canucks    Vancouver            1970    Vancouver
    ## 23 24         Anaheim Ducks /api/v1/teams/24          ANA          Ducks      Anaheim            1993      Anaheim
    ## 24 25          Dallas Stars /api/v1/teams/25          DAL          Stars       Dallas            1967       Dallas
    ## 25 26     Los Angeles Kings /api/v1/teams/26          LAK          Kings  Los Angeles            1967  Los Angeles
    ## 26 28       San Jose Sharks /api/v1/teams/28          SJS         Sharks     San Jose            1990     San Jose
    ## 27 29 Columbus Blue Jackets /api/v1/teams/29          CBJ   Blue Jackets     Columbus            1997     Columbus
    ## 28 30        Minnesota Wild /api/v1/teams/30          MIN           Wild    Minnesota            1997    Minnesota
    ## 29 52         Winnipeg Jets /api/v1/teams/52          WPG           Jets     Winnipeg            2011     Winnipeg
    ## 30 53       Arizona Coyotes /api/v1/teams/53          ARI        Coyotes      Arizona            1979      Arizona
    ## 31 54  Vegas Golden Knights /api/v1/teams/54          VGK Golden Knights        Vegas            2016        Vegas
    ## 32 55        Seattle Kraken /api/v1/teams/55          SEA         Kraken      Seattle            <NA>         <NA>
    ##                       officialSiteUrl franchiseId active                        venue.name          venue.link   venue.city
    ## 1     http://www.newjerseydevils.com/          23   TRUE                 Prudential Center /api/v1/venues/null       Newark
    ## 2    http://www.newyorkislanders.com/          22   TRUE Nassau Veterans Memorial Coliseum /api/v1/venues/null    Uniondale
    ## 3      http://www.newyorkrangers.com/          10   TRUE             Madison Square Garden /api/v1/venues/5054     New York
    ## 4  http://www.philadelphiaflyers.com/          16   TRUE                Wells Fargo Center /api/v1/venues/5096 Philadelphia
    ## 5      http://pittsburghpenguins.com/          17   TRUE                  PPG Paints Arena /api/v1/venues/5034   Pittsburgh
    ## 6        http://www.bostonbruins.com/           6   TRUE                         TD Garden /api/v1/venues/5085       Boston
    ## 7              http://www.sabres.com/          19   TRUE                    KeyBank Center /api/v1/venues/5039      Buffalo
    ## 8           http://www.canadiens.com/           1   TRUE                       Bell Centre /api/v1/venues/5028     Montréal
    ## 9      http://www.ottawasenators.com/          30   TRUE              Canadian Tire Centre /api/v1/venues/5031       Ottawa
    ## 10         http://www.mapleleafs.com/           5   TRUE                  Scotiabank Arena /api/v1/venues/null      Toronto
    ## 11 http://www.carolinahurricanes.com/          26   TRUE                         PNC Arena /api/v1/venues/5066      Raleigh
    ## 12    http://www.floridapanthers.com/          33   TRUE                       BB&T Center /api/v1/venues/5027      Sunrise
    ## 13  http://www.tampabaylightning.com/          31   TRUE                      AMALIE Arena /api/v1/venues/null        Tampa
    ## 14 http://www.washingtoncapitals.com/          24   TRUE                 Capital One Arena /api/v1/venues/5094   Washington
    ## 15  http://www.chicagoblackhawks.com/          11   TRUE                     United Center /api/v1/venues/5092      Chicago
    ## 16    http://www.detroitredwings.com/          12   TRUE              Little Caesars Arena /api/v1/venues/5145      Detroit
    ## 17 http://www.nashvillepredators.com/          34   TRUE                 Bridgestone Arena /api/v1/venues/5030    Nashville
    ## 18       http://www.stlouisblues.com/          18   TRUE                 Enterprise Center /api/v1/venues/5076    St. Louis
    ## 19      http://www.calgaryflames.com/          21   TRUE             Scotiabank Saddledome /api/v1/venues/5075      Calgary
    ## 20  http://www.coloradoavalanche.com/          27   TRUE                        Ball Arena /api/v1/venues/5064       Denver
    ## 21     http://www.edmontonoilers.com/          25   TRUE                      Rogers Place /api/v1/venues/5100     Edmonton
    ## 22            http://www.canucks.com/          20   TRUE                      Rogers Arena /api/v1/venues/5073    Vancouver
    ## 23       http://www.anaheimducks.com/          32   TRUE                      Honda Center /api/v1/venues/5046      Anaheim
    ## 24        http://www.dallasstars.com/          15   TRUE          American Airlines Center /api/v1/venues/5019       Dallas
    ## 25            http://www.lakings.com/          14   TRUE                    STAPLES Center /api/v1/venues/5081  Los Angeles
    ## 26           http://www.sjsharks.com/          29   TRUE            SAP Center at San Jose /api/v1/venues/null     San Jose
    ## 27        http://www.bluejackets.com/          36   TRUE                  Nationwide Arena /api/v1/venues/5059     Columbus
    ## 28               http://www.wild.com/          37   TRUE                Xcel Energy Center /api/v1/venues/5098     St. Paul
    ## 29           http://winnipegjets.com/          35   TRUE                    Bell MTS Place /api/v1/venues/5058     Winnipeg
    ## 30     http://www.arizonacoyotes.com/          28   TRUE                  Gila River Arena /api/v1/venues/5043     Glendale
    ## 31 http://www.vegasgoldenknights.com/          38   TRUE                    T-Mobile Arena /api/v1/venues/5178    Las Vegas
    ## 32        https://www.nhl.com/seattle          39  FALSE                              <NA>                <NA>         <NA>
    ##    venue.id   venue.timeZone.id venue.timeZone.offset venue.timeZone.tz division.id    division.name          division.link
    ## 1        NA    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 2        NA    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 3      5054    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 4      5096    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 5      5034    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 6      5085    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 7      5039    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 8      5028    America/Montreal                    -4               EDT          28     Scotia North   /api/v1/divisions/28
    ## 9      5031    America/New_York                    -4               EDT          28     Scotia North   /api/v1/divisions/28
    ## 10       NA     America/Toronto                    -4               EDT          28     Scotia North   /api/v1/divisions/28
    ## 11     5066    America/New_York                    -4               EDT          26 Discover Central   /api/v1/divisions/26
    ## 12     5027    America/New_York                    -4               EDT          26 Discover Central   /api/v1/divisions/26
    ## 13       NA    America/New_York                    -4               EDT          26 Discover Central   /api/v1/divisions/26
    ## 14     5094    America/New_York                    -4               EDT          25  MassMutual East   /api/v1/divisions/25
    ## 15     5092     America/Chicago                    -5               CDT          26 Discover Central   /api/v1/divisions/26
    ## 16     5145     America/Detroit                    -4               EDT          26 Discover Central   /api/v1/divisions/26
    ## 17     5030     America/Chicago                    -5               CDT          26 Discover Central   /api/v1/divisions/26
    ## 18     5076     America/Chicago                    -5               CDT          27       Honda West   /api/v1/divisions/27
    ## 19     5075      America/Denver                    -6               MDT          28     Scotia North   /api/v1/divisions/28
    ## 20     5064      America/Denver                    -6               MDT          27       Honda West   /api/v1/divisions/27
    ## 21     5100    America/Edmonton                    -6               MDT          28     Scotia North   /api/v1/divisions/28
    ## 22     5073   America/Vancouver                    -7               PDT          28     Scotia North   /api/v1/divisions/28
    ## 23     5046 America/Los_Angeles                    -7               PDT          27       Honda West   /api/v1/divisions/27
    ## 24     5019     America/Chicago                    -5               CDT          26 Discover Central   /api/v1/divisions/26
    ## 25     5081 America/Los_Angeles                    -7               PDT          27       Honda West   /api/v1/divisions/27
    ## 26       NA America/Los_Angeles                    -7               PDT          27       Honda West   /api/v1/divisions/27
    ## 27     5059    America/New_York                    -4               EDT          26 Discover Central   /api/v1/divisions/26
    ## 28     5098     America/Chicago                    -5               CDT          27       Honda West   /api/v1/divisions/27
    ## 29     5058    America/Winnipeg                    -5               CDT          28     Scotia North   /api/v1/divisions/28
    ## 30     5043     America/Phoenix                    -7               MST          27       Honda West   /api/v1/divisions/27
    ## 31     5178 America/Los_Angeles                    -7               PDT          27       Honda West   /api/v1/divisions/27
    ## 32       NA                <NA>                    NA              <NA>          NA             <NA> /api/v1/divisions/null
    ##    conference.id conference.name          conference.link franchise.franchiseId franchise.teamName        franchise.link
    ## 1              6         Eastern    /api/v1/conferences/6                    23             Devils /api/v1/franchises/23
    ## 2              6         Eastern    /api/v1/conferences/6                    22          Islanders /api/v1/franchises/22
    ## 3              6         Eastern    /api/v1/conferences/6                    10            Rangers /api/v1/franchises/10
    ## 4              6         Eastern    /api/v1/conferences/6                    16             Flyers /api/v1/franchises/16
    ## 5              6         Eastern    /api/v1/conferences/6                    17           Penguins /api/v1/franchises/17
    ## 6              6         Eastern    /api/v1/conferences/6                     6             Bruins  /api/v1/franchises/6
    ## 7              6         Eastern    /api/v1/conferences/6                    19             Sabres /api/v1/franchises/19
    ## 8              6         Eastern    /api/v1/conferences/6                     1          Canadiens  /api/v1/franchises/1
    ## 9              6         Eastern    /api/v1/conferences/6                    30           Senators /api/v1/franchises/30
    ## 10             6         Eastern    /api/v1/conferences/6                     5        Maple Leafs  /api/v1/franchises/5
    ## 11             6         Eastern    /api/v1/conferences/6                    26         Hurricanes /api/v1/franchises/26
    ## 12             6         Eastern    /api/v1/conferences/6                    33           Panthers /api/v1/franchises/33
    ## 13             6         Eastern    /api/v1/conferences/6                    31          Lightning /api/v1/franchises/31
    ## 14             6         Eastern    /api/v1/conferences/6                    24           Capitals /api/v1/franchises/24
    ## 15             5         Western    /api/v1/conferences/5                    11         Blackhawks /api/v1/franchises/11
    ## 16             6         Eastern    /api/v1/conferences/6                    12          Red Wings /api/v1/franchises/12
    ## 17             5         Western    /api/v1/conferences/5                    34          Predators /api/v1/franchises/34
    ## 18             5         Western    /api/v1/conferences/5                    18              Blues /api/v1/franchises/18
    ## 19             5         Western    /api/v1/conferences/5                    21             Flames /api/v1/franchises/21
    ## 20             5         Western    /api/v1/conferences/5                    27          Avalanche /api/v1/franchises/27
    ## 21             5         Western    /api/v1/conferences/5                    25             Oilers /api/v1/franchises/25
    ## 22             5         Western    /api/v1/conferences/5                    20            Canucks /api/v1/franchises/20
    ## 23             5         Western    /api/v1/conferences/5                    32              Ducks /api/v1/franchises/32
    ## 24             5         Western    /api/v1/conferences/5                    15              Stars /api/v1/franchises/15
    ## 25             5         Western    /api/v1/conferences/5                    14              Kings /api/v1/franchises/14
    ## 26             5         Western    /api/v1/conferences/5                    29             Sharks /api/v1/franchises/29
    ## 27             6         Eastern    /api/v1/conferences/6                    36       Blue Jackets /api/v1/franchises/36
    ## 28             5         Western    /api/v1/conferences/5                    37               Wild /api/v1/franchises/37
    ## 29             5         Western    /api/v1/conferences/5                    35               Jets /api/v1/franchises/35
    ## 30             5         Western    /api/v1/conferences/5                    28            Coyotes /api/v1/franchises/28
    ## 31             5         Western    /api/v1/conferences/5                    38     Golden Knights /api/v1/franchises/38
    ## 32            NA            <NA> /api/v1/conferences/null                    39             Kraken /api/v1/franchises/39

``` r
get.stat2("teamId", most.recent.id = 12, most.recent.id2 = 21)
```

    ##   id                name             link abbreviation   teamName locationName firstYearOfPlay shortName
    ## 1 21  Colorado Avalanche /api/v1/teams/21          COL  Avalanche     Colorado            1979  Colorado
    ## 2 12 Carolina Hurricanes /api/v1/teams/12          CAR Hurricanes     Carolina            1979  Carolina
    ##                      officialSiteUrl franchiseId active venue.id venue.name          venue.link venue.city venue.timeZone.id
    ## 1  http://www.coloradoavalanche.com/          27   TRUE     5064 Ball Arena /api/v1/venues/5064     Denver    America/Denver
    ## 2 http://www.carolinahurricanes.com/          26   TRUE     5066  PNC Arena /api/v1/venues/5066    Raleigh  America/New_York
    ##   venue.timeZone.offset venue.timeZone.tz division.id    division.name        division.link conference.id conference.name
    ## 1                    -6               MDT          27       Honda West /api/v1/divisions/27             5         Western
    ## 2                    -4               EDT          26 Discover Central /api/v1/divisions/26             6         Eastern
    ##         conference.link franchise.franchiseId franchise.teamName        franchise.link
    ## 1 /api/v1/conferences/5                    27          Avalanche /api/v1/franchises/27
    ## 2 /api/v1/conferences/6                    26         Hurricanes /api/v1/franchises/26

## Wrapper Function

``` r
get.nhl.data <- function (table.name=NULL, modifier=NULL, id=NULL, most.recent.id=NULL, season=NULL, ...) {
  # yes table name, yes modifier
  if (!is.null(table.name) & !is.null(modifier)) {
    stop ("Cannot use both table name and modifier to get back information.")
  }
  
  # yes table name
  else if (!is.null(table.name) & is.null(id) & is.null(modifier)) {
    output <- get.records(table.name)
  }  
  
  # yes table name, yes id, no modifier
  else if (!is.null(table.name) & !is.null(id) & is.null(modifier)) {
    output <- get.records(table.name, id)
  }
  
  # yes table name, yes most recent id, no modifier
  else if (!is.null(table.name) & !is.null(most.recent.id) & is.null(modifier)) {
    output <- get.records(table.name, most.recent.id)
  }
  
  else if (!is.null(table.name) & !is.null(id) & !is.null(most.recent.id))
    return("Only ID and table name are used, please delete most recent team ID entry.")
  
  # yes modifier
  else if (!is.null(modifier) & is.null(table.name) & is.null(most.recent.id)) {
    output <- get.stat2(modifier)
  }
  
  # yes modifier, yes most recent id
  else if (!is.null(modifier) & is.null(table.name) & !is.null(most.recent.id)) {
    output <- get.stat2(modifier, most.recent.id)
  }
  
  # yes modifier, yes season
  else if (!is.null(modifier) & is.null(table.name) & !is.null(season)) {
    output <- get.stat2(modifier, season)
  }
  
  # yes modifier, yes multiple most recent id
  else if (!is.null(modifier) & is.null(table.name) & !is.null(most.recent.id) & !is.null(most.recent.id2)) {
    output <- get.stat2(modifier, most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4)
  }
  
  return(output)
}
```

``` r
get.nhl.data(table.name = "franchise-team-totals")  ## only id used
```

    ##    id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses
    ## 1   1               1      19821983          23          2        2993         8902     8792        525                 85
    ## 2   2               1      19821983          23          3         257          634      697         53                  0
    ## 3   3               1      19721973          22          2        3788        11907    12045        678                 84
    ## 4   4               1      19721973          22          3         307          891      980         52                  1
    ## 5   5               1      19261927          10          2        6560        20020    20041       1143                 76
    ## 6   6               1      19261927          10          3         518         1447     1404        104                  0
    ## 7   7               1      19671968          16          3         449         1332     1335         97                  0
    ## 8   8               1      19671968          16          2        4171        12255    13690        584                 93
    ## 9   9               1      19671968          17          2        4171        14049    13874        683                 60
    ## 10 10               1      19671968          17          3         391         1131     1190         85                  0
    ## 11 11               1      19241925           6          2        6626        19137    21112        960                 92
    ## 12 12               1      19241925           6          3         675         1907     1956        151                  2
    ## 13 13               1      19701971          19          2        3945        11966    12471        639                 84
    ## 14 14               1      19701971          19          3         256          765      763         54                  0
    ## 15 15               1      19171918           1          3         771         1955     2300        133                  0
    ## 16 16               1      19171918           1          2        6787        18260    21791        881                 95
    ## 17 17               1      19921993          30          2        2195         6580     6250        413                 93
    ## 18 18               1      19921993          30          3         151          372      357         35                  0
    ## 19 19               1      19271928           5          2        6516        19953    19980       1082                 85
    ## 20 20               1      19271928           5          3         545         1491     1398        120                  0
    ## 21 21               1      19992000          35          2         902         3014     2465        204                 38
    ## 22 22               1      19992000          35          3           4           17        6          2                  0
    ## 23 23               1      19971998          26          3         112          282      272         24                  0
    ## 24 24               1      19971998          26          2        1812         5140     4914        323                 77
    ## 25 25               1      19931994          33          2        2109         6122     5665        390                115
    ## 26 26               1      19931994          33          3          54          152      132         15                  0
    ## 27 27               1      19921993          31          2        2194         6646     6216        414                 67
    ## 28 28               1      19921993          31          3         174          455      479         43                  0
    ## 29 29               1      19741975          24          2        3633        11553    11516        620                 83
    ## 30 30               1      19741975          24          3         295          837      836         77                  1
    ## 31 31               1      19261927          11          3         548         1669     1566        104                  0
    ## 32 32               1      19261927          11          2        6560        19687    19537       1128                 86
    ## 33 33               1      19321933          12          2        6293        18881    19550        940                 99
    ##    homeTies homeWins lastSeasonId losses overtimeLosses penaltyMinutes pointPctg points roadLosses roadOvertimeLosses roadTies
    ## 1        96      790           NA   1211            169          44773    0.5306   3176        686                 84      123
    ## 2        NA       74           NA    120              0           4266    0.0039      2         67                  0       NA
    ## 3       170      963           NA   1587            166          57792    0.5133   3889        909                 82      177
    ## 4        NA       94           NA    137              0           5654    0.0130      8         85                  2       NA
    ## 5       448     1614           NA   2716            153          86129    0.5127   6727       1573                 77      360
    ## 6         1      137           NA    266              0           8181    0.0000      0        162                  0        7
    ## 7        NA      135           NA    218              0           9104    0.0045      4        121                  0       NA
    ## 8       193     1216           NA   1452            183          76208    0.5752   4798        868                 90      264
    ## 9       205     1138           NA   1734            151          66221    0.5203   4340       1051                 91      178
    ## 10       NA      113           NA    182              0           6106    0.0153     12         97                  1       NA
    ## 11      376     1885           NA   2403            191          88570    0.5632   7464       1443                 99      415
    ## 12        3      194           NA    337              0          10607    0.0296     40        186                  2        3
    ## 13      197     1053           NA   1564            167          60671    0.5305   4186        925                 83      212
    ## 14       NA       73           NA    132              0           4692    0.0000      0         78                  0       NA
    ## 15        3      257           NA    321              0          12138    0.0000      0        188                  0        5
    ## 16      381     2038           NA   2302            175          87484    0.5863   7958       1421                 80      456
    ## 17       60      533           NA    940            169          29684    0.5071   2226        527                 76       55
    ## 18       NA       37           NA     79              0           2102    0.0000      0         44                  0       NA
    ## 19      388     1702           NA   2696            174          92331    0.5136   6693       1614                 89      385
    ## 20        2      149           NA    283              0           8550    0.0110     12        163                  1        1
    ## 21       26      183     20102011    437             78          13727    0.4473    807        233                 40       19
    ## 22       NA        0     20102011      4              0            115    0.0000      0          2                  0       NA
    ## 23       NA       32           NA     54              0           1310    0.0714     16         30                  2       NA
    ## 24       52      453           NA    725            174          19429    0.5281   1914        402                 97       34
    ## 25       65      485           NA    870            208          29171    0.5045   2128        480                 93       77
    ## 26       NA       13           NA     33              0            775    0.0000      0         18                  0       NA
    ## 27       56      559           NA    947            150          31086    0.5087   2232        533                 83       56
    ## 28       NA       47           NA     75              0           2421    0.0632     22         32                  0       NA
    ## 29      153      959           NA   1467            163          57455    0.5321   3866        847                 80      150
    ## 30       NA       75           NA    156              1           5152    0.0644     38         79                  2       NA
    ## 31        1      166           NA    275              0           8855    0.0000      0        171                  1        4
    ## 32      410     1655           NA   2761            173          92285    0.5039   6611       1633                 87      404
    ## 33      368     1741           NA   2446            183          84403    0.5354   6738       1506                 84      405
    ##    roadWins shootoutLosses shootoutWins shutouts teamId            teamName ties triCode wins
    ## 1       604             84           78      196      1   New Jersey Devils  219     NJD 1394
    ## 2        63              0            0       25      1   New Jersey Devils   NA     NJD  137
    ## 3       725             70           86      177      2  New York Islanders  347     NYI 1688
    ## 4        76              0            0       12      2  New York Islanders   NA     NYI  170
    ## 5      1269             68           79      408      3    New York Rangers  808     NYR 2883
    ## 6       107              0            0       44      3    New York Rangers    8     NYR  244
    ## 7        96              0            0       33      4 Philadelphia Flyers   NA     PHI  231
    ## 8       863             92           53      248      4 Philadelphia Flyers  457     PHI 2079
    ## 9       765             54           83      189      5 Pittsburgh Penguins  383     PIT 1903
    ## 10       96              0            0       30      5 Pittsburgh Penguins   NA     PIT  209
    ## 11     1356             82           68      506      6       Boston Bruins  791     BOS 3241
    ## 12      138              0            0       49      6       Boston Bruins    6     BOS  332
    ## 13      752             74           81      194      7      Buffalo Sabres  409     BUF 1805
    ## 14       51              0            0       18      7      Buffalo Sabres   NA     BUF  124
    ## 15      185              0            0       68      8  Montréal Canadiens    8     MTL  442
    ## 16     1435             66           69      543      8  Montréal Canadiens  837     MTL 3473
    ## 17      438             79           58      137      9     Ottawa Senators  115     OTT  971
    ## 18       35              0            0       12      9     Ottawa Senators   NA     OTT   72
    ## 19     1171             77           59      422     10 Toronto Maple Leafs  773     TOR 2873
    ## 20      110              0            0       50     10 Toronto Maple Leafs    3     TOR  259
    ## 21      159             29           37       41     11   Atlanta Thrashers   45     ATL  342
    ## 22        0              0            0        0     11   Atlanta Thrashers   NA     ATL    0
    ## 23       26              0            0       11     12 Carolina Hurricanes   NA     CAR   58
    ## 24      374             61           50       99     12 Carolina Hurricanes   86     CAR  827
    ## 25      404             97           71      115     13    Florida Panthers  142     FLA  889
    ## 26        8              0            0        3     13    Florida Panthers   NA     FLA   21
    ## 27      426             59           68      124     14 Tampa Bay Lightning  112     TBL  985
    ## 28       52              0            1       14     14 Tampa Bay Lightning   NA     TBL   99
    ## 29      741             71           68      178     15 Washington Capitals  303     WSH 1700
    ## 30       63              1            0       19     15 Washington Capitals   NA     WSH  138
    ## 31      102              0            0       32     16  Chicago Blackhawks    5     CHI  268
    ## 32     1157             70           75      439     16  Chicago Blackhawks  814     CHI 2812
    ## 33     1150             76           71      423     17   Detroit Red Wings  773     DET 2891
    ##  [ reached 'max' / getOption("max.print") -- omitted 72 rows ]

``` r
get.nhl.data(modifier = "person.names", most.recent.id = 12, season = 20142015)
```

    ##   id                name             link abbreviation   teamName locationName firstYearOfPlay shortName
    ## 1 12 Carolina Hurricanes /api/v1/teams/12          CAR Hurricanes     Carolina            1979  Carolina
    ##                      officialSiteUrl franchiseId active venue.id venue.name          venue.link venue.city venue.timeZone.id
    ## 1 http://www.carolinahurricanes.com/          26   TRUE     5066  PNC Arena /api/v1/venues/5066    Raleigh  America/New_York
    ##   venue.timeZone.offset venue.timeZone.tz division.id    division.name        division.link conference.id conference.name
    ## 1                    -4               EDT          26 Discover Central /api/v1/divisions/26             6         Eastern
    ##         conference.link franchise.franchiseId franchise.teamName        franchise.link
    ## 1 /api/v1/conferences/6                    26         Hurricanes /api/v1/franchises/26

## Grabbing data and analyzing

``` r
# get all information from "franchise-team-totals"
team.total.raw <- get.nhl.data(table.name = "franchise-team-totals") %>% rename(abbreviation = triCode)

# combine the duplicate rows in team.total so that each team only has 1 row (instead of 2+ rows)

# convert into a data table format
team.total.dt <- as.data.table(team.total.raw)
# identify which columns are numeric 
numeric_cols <- which(sapply(team.total.dt, is.numeric))
# sum up all the values and combine duplicate rows into 1
team.total <- team.total.dt[, lapply(.SD, FUN = sum), by = abbreviation, .SDcols = numeric_cols]
write_csv(team.total,"new.team.total.csv")

# get division data for different teams from "team.stats"
division <- get.nhl.data(modifier = "team.stats") %>% select(franchiseId, name, abbreviation, teamName, venue.name, venue.city, division.id, division.name, conference.id, conference.name)
```

``` r
# combine the 2 tables together using "abbreviation" as index. (there are instances where the same franchiseId were used for many different teams, so using "abbreviation" as index)
new.data <- left_join(team.total, division, by="abbreviation")
new.data.na <- left_join(team.total, division, by="abbreviation")
write_csv(new.data, "new.data.csv")

head(new.data)
```

    ##    abbreviation id activeFranchise firstSeasonId franchiseId.x gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses
    ## 1:          NJD  3               2      39643966            46          5        3250         9536     9489        578
    ## 2:          NYI  7               2      39443946            44          5        4095        12798    13025        730
    ## 3:          NYR 11               2      38523854            20          5        7078        21467    21445       1247
    ## 4:          PHI 15               2      39343936            32          5        4620        13587    15025        681
    ## 5:          PIT 19               2      39343936            34          5        4562        15180    15064        768
    ## 6:          BOS 23               2      38483850            12          5        7301        21044    23068       1111
    ##    homeOvertimeLosses homeTies homeWins lastSeasonId losses overtimeLosses penaltyMinutes pointPctg points roadLosses
    ## 1:                 85       NA      864           NA   1331            169          49039    0.5345   3178        753
    ## 2:                 85       NA     1057           NA   1724            166          63446    0.5263   3897        994
    ## 3:                 76      449     1751           NA   2982            153          94310    0.5127   6727       1735
    ## 4:                 93       NA     1351           NA   1670            183          85312    0.5797   4802        989
    ## 5:                 60       NA     1251           NA   1916            151          72327    0.5356   4352       1148
    ## 6:                 94      379     2079           NA   2740            191          99177    0.5928   7504       1629
    ##    roadOvertimeLosses roadTies roadWins shootoutLosses shootoutWins shutouts teamId ties wins franchiseId.y                name
    ## 1:                 84       NA      667             84           78      221      2   NA 1531            23   New Jersey Devils
    ## 2:                 84       NA      801             70           86      189      4   NA 1858            22  New York Islanders
    ## 3:                 77      367     1376             68           79      452      6  816 3127            10    New York Rangers
    ## 4:                 90       NA      959             92           53      281      8   NA 2310            16 Philadelphia Flyers
    ## 5:                 92       NA      861             54           83      219     10   NA 2112            17 Pittsburgh Penguins
    ## 6:                101      418     1494             82           68      555     12  797 3573             6       Boston Bruins
    ##     teamName                        venue.name   venue.city division.id   division.name conference.id conference.name
    ## 1:    Devils                 Prudential Center       Newark          25 MassMutual East             6         Eastern
    ## 2: Islanders Nassau Veterans Memorial Coliseum    Uniondale          25 MassMutual East             6         Eastern
    ## 3:   Rangers             Madison Square Garden     New York          25 MassMutual East             6         Eastern
    ## 4:    Flyers                Wells Fargo Center Philadelphia          25 MassMutual East             6         Eastern
    ## 5:  Penguins                  PPG Paints Arena   Pittsburgh          25 MassMutual East             6         Eastern
    ## 6:    Bruins                         TD Garden       Boston          25 MassMutual East             6         Eastern

## Read data from 2 different API’s

``` r
franchise.data <- get.records("franchise"); franchise.data
```

    ##    id firstSeasonId              fullName lastSeasonId mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ## 1   1      19171918    Montréal Canadiens           NA                8        MTL      Canadiens      Montréal
    ## 2   2      19171918    Montreal Wanderers     19171918               41        MWN      Wanderers      Montreal
    ## 3   3      19171918      St. Louis Eagles     19341935               45        SLE         Eagles     St. Louis
    ## 4   4      19191920       Hamilton Tigers     19241925               37        HAM         Tigers      Hamilton
    ## 5   5      19171918   Toronto Maple Leafs           NA               10        TOR    Maple Leafs       Toronto
    ## 6   6      19241925         Boston Bruins           NA                6        BOS         Bruins        Boston
    ## 7   7      19241925      Montreal Maroons     19371938               43        MMR        Maroons      Montreal
    ## 8   8      19251926    Brooklyn Americans     19411942               51        BRK      Americans      Brooklyn
    ## 9   9      19251926  Philadelphia Quakers     19301931               39        QUA        Quakers  Philadelphia
    ## 10 10      19261927      New York Rangers           NA                3        NYR        Rangers      New York
    ## 11 11      19261927    Chicago Blackhawks           NA               16        CHI     Blackhawks       Chicago
    ## 12 12      19261927     Detroit Red Wings           NA               17        DET      Red Wings       Detroit
    ## 13 13      19671968      Cleveland Barons     19771978               49        CLE         Barons     Cleveland
    ## 14 14      19671968     Los Angeles Kings           NA               26        LAK          Kings   Los Angeles
    ## 15 15      19671968          Dallas Stars           NA               25        DAL          Stars        Dallas
    ## 16 16      19671968   Philadelphia Flyers           NA                4        PHI         Flyers  Philadelphia
    ## 17 17      19671968   Pittsburgh Penguins           NA                5        PIT       Penguins    Pittsburgh
    ## 18 18      19671968       St. Louis Blues           NA               19        STL          Blues     St. Louis
    ## 19 19      19701971        Buffalo Sabres           NA                7        BUF         Sabres       Buffalo
    ## 20 20      19701971     Vancouver Canucks           NA               23        VAN        Canucks     Vancouver
    ## 21 21      19721973        Calgary Flames           NA               20        CGY         Flames       Calgary
    ## 22 22      19721973    New York Islanders           NA                2        NYI      Islanders      New York
    ## 23 23      19741975     New Jersey Devils           NA                1        NJD         Devils    New Jersey
    ## 24 24      19741975   Washington Capitals           NA               15        WSH       Capitals    Washington
    ## 25 25      19791980       Edmonton Oilers           NA               22        EDM         Oilers      Edmonton
    ## 26 26      19791980   Carolina Hurricanes           NA               12        CAR     Hurricanes      Carolina
    ## 27 27      19791980    Colorado Avalanche           NA               21        COL      Avalanche      Colorado
    ## 28 28      19791980       Arizona Coyotes           NA               53        ARI        Coyotes       Arizona
    ## 29 29      19911992       San Jose Sharks           NA               28        SJS         Sharks      San Jose
    ## 30 30      19921993       Ottawa Senators           NA                9        OTT       Senators        Ottawa
    ## 31 31      19921993   Tampa Bay Lightning           NA               14        TBL      Lightning     Tampa Bay
    ## 32 32      19931994         Anaheim Ducks           NA               24        ANA          Ducks       Anaheim
    ## 33 33      19931994      Florida Panthers           NA               13        FLA       Panthers       Florida
    ## 34 34      19981999   Nashville Predators           NA               18        NSH      Predators     Nashville
    ## 35 35      19992000         Winnipeg Jets           NA               52        WPG           Jets      Winnipeg
    ## 36 36      20002001 Columbus Blue Jackets           NA               29        CBJ   Blue Jackets      Columbus
    ## 37 37      20002001        Minnesota Wild           NA               30        MIN           Wild     Minnesota
    ## 38 38      20172018  Vegas Golden Knights           NA               54        VGK Golden Knights         Vegas
    ## 39 39      20212022        Seattle Kraken           NA               55        SEA         Kraken       Seattle

``` r
franchise.data <- franchise.data %>% select(id, mostRecentTeamId, teamCommonName, teamAbbrev, teamPlaceName)

goalie.data.hurricanes <- get.records("franchise-goalie-records", id=26) %>% select(activePlayer, firstName, lastName, gamesPlayed, wins, ties, losses, seasons, shutouts, mostShotsAgainstOneGame, mostGoalsAgainstOneGame, mostSavesOneGame, mostShutoutsOneSeason, mostWinsOneSeason)
```

## Create new variables

``` r
# replace NA as 0 in new.data
new.data[is.na(new.data)] = 0


new.data$perc.total.games.win <- (new.data$wins / new.data$gamesPlayed * 100) %>% round(2)

new.data$perc.total.games.loss <- (new.data$losses / new.data$gamesPlayed * 100) %>% round(2)

new.data$perc.home.win <- (new.data$homeWins / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

new.data$perc.home.loss <- (new.data$homeLosses / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

new.data$perc.road.win <- (new.data$roadWins / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)

new.data$perc.road.loss <- (new.data$roadLosses / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)
```

## Contingency Tables

``` r
goalie.data <- get.records("franchise-goalie-records") %>% select(franchiseName, activePlayer, firstName, lastName, gamesPlayed, wins, ties, losses, seasons, shutouts, mostShotsAgainstOneGame, mostGoalsAgainstOneGame, mostSavesOneGame, mostShutoutsOneSeason, mostWinsOneSeason)
goalie.data
```

    ##            franchiseName activePlayer      firstName   lastName gamesPlayed wins ties losses seasons shutouts
    ## 1           Dallas Stars        FALSE            Don    Beaupre         315  126   45    125       9        3
    ## 2        Arizona Coyotes        FALSE            Bob    Essensa         281  116   32    114       7       14
    ## 3     Chicago Blackhawks        FALSE           Tony   Esposito         873  418  148    302      15       74
    ## 4        Edmonton Oilers        FALSE          Grant       Fuhr         423  226   54    117      10        9
    ## 5    Philadelphia Flyers        FALSE            Ron    Hextall         489  240   58    172      11       18
    ## 6        St. Louis Blues        FALSE         Curtis     Joseph         280  137   34     96       6        5
    ## 7    Washington Capitals        FALSE           Olie     Kolzig         711  301   63    293      16       35
    ## 8        St. Louis Blues        FALSE           Mike       Liut         347  151   52    133       6       10
    ## 9      Vancouver Canucks        FALSE           Kirk     McLean         516  211   62    228      11       20
    ## 10      Cleveland Barons        FALSE         Gilles    Meloche         250   58   48    140       7        8
    ## 11       St. Louis Blues        FALSE           Greg     Millen         209   85   33     87       6        9
    ## 12   Toronto Maple Leafs        FALSE           Turk      Broda         629  304  102    222      14       61
    ## 13         Boston Bruins        FALSE          Gerry   Cheevers         416  226   76    103      12       26
    ## 14      St. Louis Eagles        FALSE           Alec    Connell         294  140   47    106       8       64
    ## 15       Hamilton Tigers        FALSE           Jake     Forbes          78   34    1     43       3        7
    ## 16    Montreal Wanderers        FALSE           Bert    Lindsay           4    1    0      3       1        0
    ## 17       Hamilton Tigers        FALSE         Howard   Lockhart           1    0    0      1       3        0
    ## 18          Dallas Stars        FALSE         Cesare    Maniago         420  145   70    190       9       26
    ## 19  Philadelphia Quakers        FALSE            Joe     Miller          87   14   11     62       3       11
    ## 20     Detroit Red Wings        FALSE          Terry    Sawchuk         734  350  132    245      14       85
    ## 21      Montreal Maroons        FALSE           Flat      Walsh         107   46   13     44       7        9
    ## 22    Brooklyn Americans        FALSE            Roy    Worters         360  118   70    171       9       45
    ## 23  Philadelphia Quakers        FALSE            Roy    Worters         123   52   12     59       3       22
    ## 24   Tampa Bay Lightning        FALSE          Daren      Puppa         206   77   26     91       7       12
    ## 25       Edmonton Oilers        FALSE           Bill    Ranford         449  167   54    193      10        8
    ## 26      New York Rangers        FALSE           Mike    Richter         666  301   73    258      14       24
    ## 27    Montréal Canadiens        FALSE        Patrick        Roy         551  289   66    175      12       29
    ## 28    Colorado Avalanche        FALSE        Patrick        Roy         478  262   65    140       8       37
    ## 29    New York Islanders        FALSE          Billy      Smith         674  304  104    230      17       22
    ## 30       St. Louis Blues        FALSE             Ed Staniowski         137   37   14     68       6        0
    ## 31        Calgary Flames        FALSE           Mike     Vernon         527  262   57    188      13       13
    ## 32     New Jersey Devils        FALSE         Martin    Brodeur        1259  688  105    394      21      124
    ## 33     Detroit Red Wings        FALSE          Chris     Osgood         565  317   46    149      14       39
    ## 34          Dallas Stars        FALSE          Marty      Turco         509  262   26    154       9       40
    ## 35       San Jose Sharks        FALSE         Evgeni    Nabokov         563  293   29    178      10       50
    ## 36         Anaheim Ducks        FALSE Jean-Sebastien    Giguere         447  206   23    163       9       32
    ## 37      Florida Panthers        FALSE        Roberto     Luongo         572  230   32    241      11       38
    ## 38       Ottawa Senators         TRUE          Craig   Anderson         435  202    0    168      10       28
    ## 39        Buffalo Sabres         TRUE           Ryan     Miller         540  284    1    186      11       28
    ## 40          Dallas Stars        FALSE           Kari   Lehtonen         445  216    0    150       9       24
    ## 41        Minnesota Wild        FALSE           Josh    Harding         151   60   NA     59       9       10
    ## 42   Carolina Hurricanes        FALSE            Cam       Ward         668  318    0    244      13       27
    ## 43   Pittsburgh Penguins         TRUE     Marc-Andre     Fleury         691  375    2    216      13       44
    ## 44  Vegas Golden Knights         TRUE     Marc-Andre     Fleury         192  117    0     60       4       23
    ## 45   Nashville Predators         TRUE          Pekka      Rinne         683  369    0    213      15       60
    ## 46         Winnipeg Jets        FALSE         Ondrej    Pavelec         119   41   NA     51      10        6
    ## 47     Los Angeles Kings         TRUE       Jonathan      Quick         666  336    0    249      14       54
    ## 48        Minnesota Wild        FALSE         Niklas  Backstrom         409  194   NA    142       9       28
    ## 49 Columbus Blue Jackets         TRUE         Sergei  Bobrovsky         374  213    0    130       7       33
    ## 50  Vegas Golden Knights         TRUE         Maxime     Lagace          17    6    0      8       2        0
    ## 51  Vegas Golden Knights         TRUE          Oscar      Dansk           6    4    0      1       3        1
    ## 52  Vegas Golden Knights         TRUE        Malcolm     Subban          63   30    0     21       3        1
    ## 53  Vegas Golden Knights         TRUE          Dylan   Ferguson           1    0    0      0       1        0
    ## 54      Montreal Maroons        FALSE          Clint   Benedict         204   92   25     84       6       38
    ## 55         Boston Bruins        FALSE           Tiny   Thompson         468  252   63    153      11       74
    ## 56        Calgary Flames        FALSE         Miikka  Kiprusoff         576  305    4    192       9       41
    ## 57      New York Rangers         TRUE         Henrik  Lundqvist         887  459    0    310      15       64
    ## 58         Winnipeg Jets        FALSE           Kari   Lehtonen         204   94    0     83       5       14
    ## 59    Montréal Canadiens         TRUE          Carey      Price         707  360    0    257      14       49
    ## 60   Tampa Bay Lightning         TRUE            Ben     Bishop         227  131   NA     64       5       17
    ## 61    Montréal Canadiens        FALSE        Jacques     Plante         556  314  107    133      11       58
    ## 62     Vancouver Canucks        FALSE        Roberto     Luongo         448  252   NA    137       8       38
    ## 63       Arizona Coyotes        FALSE           Ilya  Bryzgalov         257  130   NA     93       4       21
    ## 64         Anaheim Ducks        FALSE            Guy     Hebert         441  173   52    202       8       27
    ## 65         Boston Bruins        FALSE          Eddie   Johnston         444  182   54    192      11       27
    ## 66 Columbus Blue Jackets        FALSE           Marc      Denis         266   84   24    146       5       12
    ##    mostShotsAgainstOneGame mostGoalsAgainstOneGame mostSavesOneGame mostShutoutsOneSeason mostWinsOneSeason
    ## 1                       55                      10               52                     1                25
    ## 2                       50                       8               49                     5                33
    ## 3                       53                      10               50                    15                38
    ## 4                       54                       9               49                     4                40
    ## 5                       50                       9               45                     5                37
    ## 6                       54                       8               51                     2                36
    ## 7                       54                       8               52                     6                41
    ## 8                       48                       9               44                     3                33
    ## 9                       52                       9               48                     5                38
    ## 10                      58                      11               55                     4                19
    ## 11                      46                       9               43                     6                22
    ## 12                      NA                       9               NA                     9                32
    ## 13                      48                       8               45                     4                30
    ## 14                      NA                      10               NA                    15                30
    ## 15                      NA                       9               NA                     6                19
    ## 16                      NA                      11               NA                     0                 1
    ## 17                      NA                      13               NA                     1                 6
    ## 18                      59                      10               54                     6                22
    ## 19                      NA                       9               NA                    11                 9
    ## 20                      53                      10               50                    12                44
    ## 21                      NA                       6               NA                     2                17
    ## 22                      NA                      10               NA                    13                18
    ## 23                      NA                       7               NA                    11                19
    ## 24                      46                       7               45                     5                29
    ## 25                      59                       8               56                     2                27
    ## 26                      62                      10               59                     5                42
    ## 27                      53                       9               49                     7                36
    ## 28                      53                       7               51                     9                40
    ## 29                      60                       9               55                     3                32
    ## 30                      54                       9               45                     0                10
    ## 31                      44                       8               41                     3                39
    ## 32                      51                       6               51                    12                48
    ## 33                      49                       7               46                     6                39
    ## 34                      52                       6               49                     9                41
    ## 35                      52                       7               50                     9                46
    ## 36                      57                       6               51                     8                36
    ## 37                      60                       6               57                     7                35
    ## 38                      53                       7               49                     5                33
    ## 39                      51                       7               49                     6                41
    ## 40                      50                       7               47                     5                34
    ## 41                      49                       8               47                     3                18
    ## 42                      60                       7               57                     6                39
    ## 43                      51                       7               47                    10                42
    ## 44                      47                       7               45                     8                35
    ## 45                      50                       8               48                     8                43
    ## 46                      55                       8               50                     5                29
    ## 47                      51                       8               51                    10                40
    ## 48                      51                       7               48                     8                37
    ## 49                      55                       8               52                     9                41
    ## 50                      39                       7               36                     0                 6
    ## 51                      37                       6               32                     1                 3
    ## 52                      44                       7               42                     1                13
    ## 53                       2                       1                1                     0                 0
    ## 54                      NA                       7               NA                    13                24
    ## 55                      NA                       9               NA                    12                38
    ## 56                      46                       7               44                    10                45
    ## 57                      55                       8               50                    11                39
    ## 58                      50                       7               49                     4                34
    ## 59                      55                       8               53                     9                44
    ## 60                      51                       6               48                     6                40
    ## 61                      52                       8               52                     9                42
    ## 62                      54                       8               50                     9                47
    ## 63                      48                       7               44                     8                42
    ## 64                      50                       7               47                     6                31
    ## 65                      56                      11               48                     6                30
    ## 66                      51                       8               48                     5                27
    ##  [ reached 'max' / getOption("max.print") -- omitted 1012 rows ]

``` r
table(goalie.data$franchiseName)
```

    ## 
    ##         Anaheim Ducks       Arizona Coyotes         Boston Bruins    Brooklyn Americans        Buffalo Sabres 
    ##                    26                    45                    51                    11                    33 
    ##        Calgary Flames   Carolina Hurricanes    Chicago Blackhawks      Cleveland Barons    Colorado Avalanche 
    ##                    37                    38                    48                     5                    35 
    ## Columbus Blue Jackets          Dallas Stars     Detroit Red Wings       Edmonton Oilers      Florida Panthers 
    ##                    16                    37                    49                    43                    29 
    ##       Hamilton Tigers     Los Angeles Kings        Minnesota Wild    Montréal Canadiens      Montreal Maroons 
    ##                     3                    44                    16                    38                     6 
    ##    Montreal Wanderers   Nashville Predators     New Jersey Devils    New York Islanders      New York Rangers 
    ##                     1                    11                    28                    30                    43 
    ##       Ottawa Senators   Philadelphia Flyers  Philadelphia Quakers   Pittsburgh Penguins       San Jose Sharks 
    ##                    29                    34                     4                    38                    20 
    ##       St. Louis Blues      St. Louis Eagles   Tampa Bay Lightning   Toronto Maple Leafs     Vancouver Canucks 
    ##                    42                     4                    34                    54                    40 
    ##  Vegas Golden Knights   Washington Capitals         Winnipeg Jets 
    ##                     8                    31                    17

``` r
table(goalie.data$activePlayer)
```

    ## 
    ## FALSE  TRUE 
    ##   929   149

``` r
# find the count of active/inactive players by franchise
contingency.table1 <- table(goalie.data$franchiseName, goalie.data$activePlayer) %>% kable(caption = "Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by Franchise")

# find the proportion by margin=1 (rows)
contingency.table1.prop <- table(goalie.data$franchiseName, goalie.data$activePlayer) %>% prop.table(margin=1)*100
contingency.table1.prop %>% kable(caption = "Proportion of Active (TRUE) / Inactive (FALSE) Players by Franchise")
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
69.23077
</td>
<td style="text-align:right;">
30.769231
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
82.22222
</td>
<td style="text-align:right;">
17.777778
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
96.07843
</td>
<td style="text-align:right;">
3.921569
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
100.00000
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
81.81818
</td>
<td style="text-align:right;">
18.181818
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
86.48649
</td>
<td style="text-align:right;">
13.513514
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
86.84211
</td>
<td style="text-align:right;">
13.157895
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
91.66667
</td>
<td style="text-align:right;">
8.333333
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
100.00000
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
74.28571
</td>
<td style="text-align:right;">
25.714286
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
87.50000
</td>
<td style="text-align:right;">
12.500000
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
91.89189
</td>
<td style="text-align:right;">
8.108108
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
93.87755
</td>
<td style="text-align:right;">
6.122449
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
93.02326
</td>
<td style="text-align:right;">
6.976744
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
86.20690
</td>
<td style="text-align:right;">
13.793103
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
100.00000
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
88.63636
</td>
<td style="text-align:right;">
11.363636
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
68.75000
</td>
<td style="text-align:right;">
31.250000
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
92.10526
</td>
<td style="text-align:right;">
7.894737
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
100.00000
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
100.00000
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
63.63636
</td>
<td style="text-align:right;">
36.363636
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
85.71429
</td>
<td style="text-align:right;">
14.285714
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
86.66667
</td>
<td style="text-align:right;">
13.333333
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
90.69767
</td>
<td style="text-align:right;">
9.302326
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
75.86207
</td>
<td style="text-align:right;">
24.137931
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
88.23529
</td>
<td style="text-align:right;">
11.764706
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
100.00000
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
86.84211
</td>
<td style="text-align:right;">
13.157895
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
80.00000
</td>
<td style="text-align:right;">
20.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
85.71429
</td>
<td style="text-align:right;">
14.285714
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
100.00000
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
85.29412
</td>
<td style="text-align:right;">
14.705882
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
85.18519
</td>
<td style="text-align:right;">
14.814815
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
87.50000
</td>
<td style="text-align:right;">
12.500000
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
12.50000
</td>
<td style="text-align:right;">
87.500000
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
87.09677
</td>
<td style="text-align:right;">
12.903226
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
82.35294
</td>
<td style="text-align:right;">
17.647059
</td>
</tr>
</tbody>
</table>

``` r
table(new.data$division.name)
```

    ## 
    ##                0 Discover Central       Honda West  MassMutual East     Scotia North 
    ##               26                8                8                8                7

``` r
table(new.data$wins)
```

    ## 
    ##    1    4   11   16   22   27   34   47   64   68   69  113  116  210  218  245  270  276  291  342  398  525  532  552  637  693 
    ##    1    2    1    1    1    1    1    2    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
    ##  789  838  885  906  910 1043 1079 1084 1131 1189 1531 1600 1629 1760 1838 1858 1865 1929 2111 2112 2310 3080 3127 3132 3216 3573 
    ##    1    1    1    1    1    1    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
    ## 3915 
    ##    1

``` r
# find the count of franchises w/i a division that had over 2000 wins
contingency.table2 <- table(new.data$division.name, new.data$wins>2000) %>% kable(caption = "Contingency Table of Number of Franchises in a Division that had Over 200 Wins")

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
0
</td>
<td style="text-align:right;">
54.166667
</td>
<td style="text-align:right;">
0.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
Discover Central
</td>
<td style="text-align:right;">
12.500000
</td>
<td style="text-align:right;">
22.22222
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
14.583333
</td>
<td style="text-align:right;">
11.11111
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
8.333333
</td>
<td style="text-align:right;">
44.44444
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
10.416667
</td>
<td style="text-align:right;">
22.22222
</td>
</tr>
</tbody>
</table>

## Plots

### Bar plot

``` r
new.data.no.na <- new.data.na %>% drop_na(division.name)
ggplot(data = new.data.no.na, aes(x = division.name)) + 
  geom_bar(aes(fill = as.factor(conference.name)), position = "dodge") + 
  labs(x = "Division", title = "Bar Plot of Number of Franchise in Each Division") + 
  scale_fill_discrete(name = "Conference Name", labels = c("Eastern Conference", "Western Conference"))
```

![](README_files/figure-gfm/plot%20-%20bar-1.png)<!-- -->

## Individual tables for reference

## NHL stats API
