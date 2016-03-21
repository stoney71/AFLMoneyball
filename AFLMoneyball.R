# Ensure csv file is already in current directory.

AFLstats <- read.csv("./all_stats_2013_2015.csv", stringsAsFactors = FALSE)

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
        paste(h, " predicted to beat ", a, " at ", venue, " by ", h_mar - a_mar, " points.")
}


## Predict the Results for R1, 2016.

predict_result("Richmond", "Carlton", "M.C.G.")
predict_result("Melbourne", "Greater Western Sydney", "M.C.G.")
predict_result("Gold Coast", "Essendon", "Carrara")
predict_result("North Melbourne", "Adelaide", "Docklands")
predict_result("Sydney", "Collingwood", "S.C.G.")
predict_result("Western Bulldogs", "Fremantle", "Docklands")
predict_result("Port Adelaide", "St Kilda", "Adelaide Oval")
predict_result("West Coast", "Brisbane Lions", "Subiaco")
predict_result("Geelong", "Hawthorn", "M.C.G.")