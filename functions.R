getSurvey <- function() {
  library(tidyverse)
  library(lubridate)
  library(stringr)
  raw <- read_csv("survey.csv")
  
  # sample size to work with the data
  #raw <- raw[1:1000,]
  
  # handle NAs
  raw[is.na(raw)] <- "Unknown"
  
  # format data
  data <- raw %>%
    mutate(`Date Submitted`=mdy_hms(`Date Submitted`),
           `Time Started`=mdy_hms(`Time Started`),
           Age=as.factor(`Age (optional)`),
           `Frequency of Use`=case_when(
             str_detect(`How often do you use Facebook?`,"I’m not on Facebook") ~ "Not on Facebook",
             str_detect(`How often do you use Facebook?`,"constantly") ~ "Constant User",
             str_detect(`How often do you use Facebook?`,"day") ~ "Daily User",
             TRUE ~ "Occasional User"),
           `Tech Knowledge` = as.factor(`I consider myself:`),
           `Facebook Users and Non-users` = case_when(
             `Frequency of Use` == "Not on Facebook" ~ "Non-user",
             TRUE ~ "User"),
           `I know how to: Change Facebook privacy settings` = case_when(
             str_detect(`Change your Facebook privacy settings:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Opt out of allowing third-party apps to track me and my friends` = case_when(
             str_detect(`Opt out of allowing third-party apps to track you and your friends:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Publish something so only a few of my friends can see it` = case_when(
             str_detect(`Publish something so only a few of your friends can see it:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Browse Facebook while limiting its ability to track me` = case_when(
             str_detect(`Browse Facebook while limiting its ability to track you:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Delete my Facebook page` = case_when(
             str_detect(`Delete your Facebook page:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Create a targeted Facebook ad` = case_when(
             str_detect(`Create a targeted Facebook ad:Check all the items you currently know how to do:`, "Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Find out all the data Facebook has collected on me` = case_when(
             str_detect(`Find out all the data Facebook has collected on me already:Check all the items you currently know how to do:`,"Unknown") ~ "No",
             TRUE ~ "Yes"),
           `I know how to: Make a backup of all of my content on Facebook (photos, videos, etc.)` = case_when(
             str_detect(`Make a backup of all my content on Facebook (photos, videos, etc):Check all the items you currently know how to do:`, "Unknown") ~ "No",
             TRUE ~ "Yes")
           ) %>%
    select(c(`Response ID`:Country),
           `Link Name`,
           c(Age:`Facebook Users and Non-users`),
           c(`I consider myself:`:`Do you know what types of personal information Facebook collects about you?`),
           `Would you consider paying for a version of Facebook that doesn’t make money off you by collecting and selling your data?`,
           `Have you made any changes on Facebook as a result of the recent Cambridge Analytica and Facebook revelations?`,
           c(`I know how to: Change Facebook privacy settings`:`I know how to: Make a backup of all of my content on Facebook (photos, videos, etc.)`)
           ) %>%
    gather(`I consider myself:`:`I know how to: Make a backup of all of my content on Facebook (photos, videos, etc.)`,key="Question",value="Answer") %>%
    mutate(Chart = case_when(
      str_detect(Question,"How concerned are you") ~ "Histogram",
      str_detect(Question,"what types of personal information") ~ "Stacked",
      str_detect(Question,"Have you made any changes") ~ "Stacked",
      TRUE ~ "Stacked"
    ))

    
  return(data)
}

getDownloadableSurvey <- function() {
  library(tidyverse)
  library(lubridate)
  library(stringr)
  raw <- read_csv("survey.csv")
  
  # sample size to work with the data
  #raw <- raw[1:1000,]
  
  # handle NAs
  raw[is.na(raw)] <- "Unknown"
  data <- raw %>%
    select(c(`Response ID`),
           Country,
           c(`I consider myself:`:`Would you consider paying for a version of Facebook that doesn’t make money off you by collecting and selling your data?`),
           `Age (optional)`
    )
  
  return(data)
}
