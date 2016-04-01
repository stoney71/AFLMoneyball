# Ensure csv file is already in current directory.

AFLstats <- read.csv("./all_stats_2013_2016.csv", stringsAsFactors = FALSE)

# load some packages that may be useful

library(dplyr)
library(tidyr)
library(ggplot2)


AFLstats <- select(AFLstats, -X)

clubs <- c("Adelaide", "Brisbane Lions", "Carlton", "Collingwood", "Essendon",
                   "Fremantle", "Geelong", "Gold Coast", "Greater Western Sydney",
                   "Hawthorn", "Melbourne", "North Melbourne", "Port Adelaide",
                   "Richmond", "St Kilda", "Sydney", "West Coast", "Western Bulldogs")


# a function to detemine the main influencers for a given team & stat

influencers <- function(team, stat = "Kicks") {
        player_list <- unique(filter(AFLstats, Team == team & Season == 2015)$Player)
        infl_df <- data.frame(Player = as.character(NULL), Team = as.character(NULL), 
                              B1 = as.numeric(NULL), B2 = as.numeric(NULL),
                              stringsAsFactors = FALSE)
        for (i in 1:length(player_list)) {
                df <- AFLstats %>% filter(Player == player_list[i]) %>% select(Margin, contains(stat))
                y <- df$Margin
                x <- df[[2]]
                if (length(x) < 10)
                        next
                fit <- lm(y ~ x)
                B2 <- summary(fit)$coef[2, 1]
                B1 <- summary(fit)$coef[1, 1]
                Pr_t <- summary(fit)$coef[2, 4]
                if (B2 >= 1.0 & Pr_t < 0.1) {
                        infl_df[nrow(infl_df) + 1, ] <- c(player_list[i], team, B1, B2)
                }
        }
        names(infl_df) <- c("Player", "Team", "B1", "B2")
        infl_df$B1 <- as.numeric(infl_df$B1)
        infl_df$B2 <- as.numeric(infl_df$B2)
        infl_df
}


# a function to determine the impact of a key player, for a given stat (Kicks). The
# impact is based on a linear regression fit where the predictor is the stat and the 
# variable or outcome is the Margin

impact <- function(player, B1, B2, venue, opposition, stat) {
        df1 <- AFLstats %>% filter(Player == player) %>% 
                select(Player, Opposition, Venue, Margin, contains(stat, ignore.case = FALSE))
        df2 <- df1 %>% filter(Opposition == opposition)
        df3 <- df1 %>% filter(Venue == venue)
        if (nrow(df2) == 0 & nrow(df3) == 0)
                return(0)
        imp <- mean(c(df2[[5]], df3[[5]]), na.rm = TRUE) * B2 + B1
        imp
}

## find the main influencers for each team, for various key stats
## so far this is just kicks

infl_kicks <- bind_rows(influencers("Adelaide", "Kicks"), influencers("Brisbane Lions", "Kicks"),
                        influencers("Carlton", "Kicks"), influencers("Collingwood", "Kicks"),
                        influencers("Essendon", "Kicks"), influencers("Fremantle", "Kicks"),
                        influencers("Geelong", "Kicks"), influencers("Gold Coast", "Kicks"),
                        influencers("Greater Western Sydney", "Kicks"), influencers("Hawthorn", "Kicks"),
                        influencers("Melbourne", "Kicks"), influencers("North Melbourne", "Kicks"),
                        influencers("Port Adelaide", "Kicks"), influencers("Richmond", "Kicks"),
                        influencers("St Kilda", "Kicks"), influencers("Sydney", "Kicks"),
                        influencers("West Coast", "Kicks"), influencers("Western Bulldogs", "Kicks"))

## manually update entries for influential players who retired or were delisted.

infl_kicks <- infl_kicks[infl_kicks$Player != "Ellard, David" & infl_kicks$Player != "Watson, Matthew" &
                                 infl_kicks$Player != "Petterd, Ricky" & infl_kicks$Player != "McDonough, Matt", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Cross, Daniel" & infl_kicks$Player != "Chapman, Paul" &
                                 infl_kicks$Player != "Winderlich, Jason", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Cornes, Kane" & infl_kicks$Player != "Jaensch, Matthew" &
                                 infl_kicks$Player != "McPharlin, Luke" & infl_kicks$Player != "Schneider, Adam" &
                                 infl_kicks$Player != "Staker, Brent", ]

## manually update entries for influential players who changed clubs in 2016

infl_kicks[infl_kicks$Player == "Kerridge, Sam", 2] <- "Carlton"
infl_kicks[infl_kicks$Player == "Henderson, Lachie", 2] <- "Geelong"
infl_kicks[infl_kicks$Player == "Johnson, Steve", 2] <- "Greater Western Sydney"
infl_kicks[infl_kicks$Player == "Bugg, Tomas", 2] <- "Melbourne"
infl_kicks[infl_kicks$Player == "Jamar, Mark", 2] <- "Essendon"
infl_kicks[infl_kicks$Player == "Fitzpatrick, Jack", 2] <- "Hawthorn"
infl_kicks[infl_kicks$Player == "Carlisle, Jake", 2] <- "St Kilda"
infl_kicks[infl_kicks$Player == "Giles, Jonathan", 2] <- "West Coast"
infl_kicks[infl_kicks$Player == "Smith, Zac", 2] <- "Geelong"
infl_kicks[infl_kicks$Player == "Bird, Craig", 2] <- "Essendon"
infl_kicks[infl_kicks$Player == "Brown, Mitch", 2] <- "Essendon"
infl_kicks[infl_kicks$Player == "Kelly, James", 2] <- "Essendon"
infl_kicks[infl_kicks$Player == "Leuenberger, Matthew", 2] <- "Essendon"
infl_kicks[infl_kicks$Player == "Suckling, Matt", 2] <- "Western Bulldogs"
infl_kicks[infl_kicks$Player == "Sinclair, Callum", 2] <- "Sydney"
infl_kicks[infl_kicks$Player == "Walker, Josh", 2] <- "Brisbane Lions"


infl_kicks <- arrange(infl_kicks, Team)

## produce a function that takes a home side (h), an away side (a) and a venue to predict
## the result. Print the result to screen.

predict_result <- function(h, a, venue) {
        h_mar <- 0
        a_mar <- 0
        h_infl_kicks <- filter(infl_kicks, Team == h)
        for (i in 1:nrow(h_infl_kicks)) 
                h_mar <- h_mar + impact(h_infl_kicks$Player[i], h_infl_kicks$B1[i],
                                       h_infl_kicks$B2[i], venue, a, "Kicks")
        a_infl_kicks <- filter(infl_kicks, Team == a)
        for (i in 1:nrow(a_infl_kicks)) 
                a_mar <- a_mar + impact(a_infl_kicks$Player[i], a_infl_kicks$B1[i],
                                        a_infl_kicks$B2[i], venue, h, "Kicks")
        paste(h, " predicted to beat ", a, " at ", venue, " by ", (h_mar - a_mar) / 10, " points.", sep = "")
}


## Predict the Results for R1, 2016.

## Remove key players who are not playing this round
# Richmond/Carlton
infl_kicks <- infl_kicks[infl_kicks$Player != "Wood, Cameron" & infl_kicks$Player != "Batchelor, Jake" &
                                 infl_kicks$Player != "Maric, Ivan" & infl_kicks$Player != "Deledio, Brett" &
                                 infl_kicks$Player != "Conca, Reece", ]
# Melb/GWS
infl_kicks <- infl_kicks[infl_kicks$Player != "Cameron, Jeremy" & infl_kicks$Player != "Haynes, Nick" &
                                 infl_kicks$Player != "McCarthy, Cam" & infl_kicks$Player != "Patfull, Joel" &
                                 infl_kicks$Player != "Lumumba, Heritier" & infl_kicks$Player != "Grimes, Jack" &
                                 infl_kicks$Player != "Spencer, Jake", ]

# Gold Coast/Essendon
infl_kicks <- infl_kicks[infl_kicks$Player != "Broughton, Greg" & infl_kicks$Player != "Hibberd, Michael" &
                                 infl_kicks$Player != "Howlett, Ben" & infl_kicks$Player != "Dempsey, Courtenay", ]

# North Melbourne/Adelaide
infl_kicks <- infl_kicks[infl_kicks$Player != "Black, Aaron" & infl_kicks$Player != "Hansen, Lachlan" &
                                 infl_kicks$Player != "Cameron, Charlie" & infl_kicks$Player != "Lyons, Jarryd", ]

#Sydney / Collingwood
infl_kicks <- infl_kicks[infl_kicks$Player != "Rohan, Gary" & infl_kicks$Player != "McVeigh, Jarrad" &
                                 infl_kicks$Player != "McGlynn, Ben", ]

#Western Bulldogs / Fremantle
infl_kicks <- infl_kicks[infl_kicks$Player != "Johnson, Michael" & infl_kicks$Player != "McPharlin, Luke" &
                                 infl_kicks$Player != "Pearce, Clancee" & infl_kicks$Player != "Crameri, Stewart" &
                                 infl_kicks$Player != "Dickson, Tory", ]

# Port St Kilda
infl_kicks <- infl_kicks[infl_kicks$Player != "Butcher, John" & infl_kicks$Player != "Stewart, Paul" &
                                 infl_kicks$Player != "Carlisle, Jake" & infl_kicks$Player != "Ray, Farren" &
                                 infl_kicks$Player != "Saunders, Josh", ]

# West Coast Brisb
infl_kicks <- infl_kicks[infl_kicks$Player != "Giles, Jonathan" & infl_kicks$Player != "Masten, Chris" &
                                 infl_kicks$Player != "Beams, Dayne" & infl_kicks$Player != "Close, Michael" &
                                 infl_kicks$Player != "Staker, Brent", ]


predict_result("Richmond", "Carlton", "M.C.G.")
predict_result("Melbourne", "Greater Western Sydney", "M.C.G.")
predict_result("Gold Coast", "Essendon", "Carrara")
predict_result("North Melbourne", "Adelaide", "Docklands")
predict_result("Sydney", "Collingwood", "S.C.G.")
predict_result("Western Bulldogs", "Fremantle", "Docklands")
predict_result("Port Adelaide", "St Kilda", "Adelaide Oval")
predict_result("West Coast", "Brisbane Lions", "Subiaco")
predict_result("Geelong", "Hawthorn", "M.C.G.")

#############################################################

## Predict the Results for R2, 2016.

names_infl_kicks <- arrange(infl_kicks, Player)

## Remove key players who are not playing this round
# Collingwood / Richmond
infl_kicks <- infl_kicks[infl_kicks$Player != "Swan, Dane" & infl_kicks$Player != "Maynard, Brayden", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Batchelor, Jake" & infl_kicks$Player != "Maric, Ivan" &
                                 infl_kicks$Player != "Deledio, Brett" & infl_kicks$Player != "Conca, Reece", ]

# Adelaide / Port Adelaide

infl_kicks <- infl_kicks[infl_kicks$Player != "Cameron, Charlie" & infl_kicks$Player != "Lyons, Jarryd", ]
infl_kicks <- infl_kicks[infl_kicks$Player != "Butcher, John" & infl_kicks$Player != "Stewart, Paul" &
                                 infl_kicks$Player != "Carlisle, Jake" & infl_kicks$Player != "White, Matt" &
                                 infl_kicks$Player != "Schulz, Jay" & infl_kicks$Player != "Hartlett, Hamish", ]

# Essendon / Melbourne
infl_kicks <- infl_kicks[infl_kicks$Player != "Hibberd, Michael" & infl_kicks$Player != "Howlett, Ben" & 
                                 infl_kicks$Player != "Dempsey, Courtenay" & infl_kicks$Player != "McKernan, Shaun" &
                                 infl_kicks$Player != "Bird, Craig" & infl_kicks$Player != "Jamar, Mark", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Grimes, Jack" & infl_kicks$Player != "Spencer, Jake" &
                                 infl_kicks$Player != "Frost, Sam", ]


# Brisbane / North Melbourne
infl_kicks <- infl_kicks[infl_kicks$Player != "Beams, Dayne" & infl_kicks$Player != "Close, Michael" &
                                 infl_kicks$Player != "Freeman, Jonathan", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Black, Aaron" & infl_kicks$Player != "Hansen, Lachlan", ]


# Fremantle / Gold Coast
infl_kicks <- infl_kicks[infl_kicks$Player != "Mzungu, Tendai" & infl_kicks$Player != "Clarke, Zac", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Broughton, Greg", ]

# St Kilda / Western Bulldogs
infl_kicks <- infl_kicks[infl_kicks$Player != "Ray, Farren" & infl_kicks$Player != "Saunders, Josh", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Crameri, Stewart" & infl_kicks$Player != "Dickson, Tory", ]

# GWS / Geelong
#

infl_kicks <- infl_kicks[infl_kicks$Player != "Rivers, Jared" & infl_kicks$Player != "Smedts, Billie", ]

# Hawthorn / West Coast

infl_kicks <- infl_kicks[infl_kicks$Player != "Hodge, Luke" & infl_kicks$Player != "Hill, Bradley" &
                                 infl_kicks$Player != "Roughead, Jarryd" & infl_kicks$Player != "Shiels, Liam" &
                                 infl_kicks$Player != "Fitzpatrick, Jack", ]


infl_kicks <- infl_kicks[infl_kicks$Player != "Giles, Jonathan" & infl_kicks$Player != "Masten, Chris", ]

# Carlton / Sydney 
infl_kicks <- infl_kicks[infl_kicks$Player != "Wood, Cameron", ]

infl_kicks <- infl_kicks[infl_kicks$Player != "Rohan, Gary" & infl_kicks$Player != "McVeigh, Jarrad" &
                                 infl_kicks$Player != "McGlynn, Ben", ]



predict_result("Collingwood", "Richmond", "M.C.G.")
predict_result("Adelaide", "Port Adelaide", "Adelaide Oval")
predict_result("Essendon", "Melbourne", "M.C.G.")
predict_result("Brisbane Lions", "North Melbourne", "Gabba")
predict_result("Fremantle", "Gold Coast", "Subiaco")
predict_result("St Kilda", "Western Bulldogs", "Docklands")
predict_result("Greater Western Sydney", "Geelong", "Manuka Oval")
predict_result("Hawthorn", "West Coast", "M.C.G.")
predict_result("Carlton", "Sydney", "Docklands")
