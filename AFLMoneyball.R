# Ensure csv file is already in current directory.

AFLstats <- read.csv("./all_stats_2013_2015.csv", stringsAsFactors = FALSE)

# load some packages that may be useful

library(dplyr)
library(tidyr)
library(ggplot2)


AFLstats <- select(AFLstats, -X)

###########################################################################
# First look at upcoming R1, 2016 Adelaide vs North Melbourne @ Docklands #
###########################################################################

# Load the key players - those who have the most impact from Kicks stats

adel_infl_kicks <- c("Betts, Eddie", "Brown, Luke","Cameron, Charlie", "Jacobs, Sam", 
                     "Jenkins, Josh", "Lyons, Jarryd", "Sloane, Rory", "Talia, Daniel",
                     "Walker, Taylor","Thompson, Scott" )

nm_infl_kicks <- c("Bastinac, Ryan", "Goldstein, Todd", "Harvey, Brent", "Petrie, Drew",
                   "Thomas, Lindsay", "Thompson, Scott", "Waite, Jarrad", "Ziebell, Jack",
                   "Black, Aaron", "Hansen, Lachlan")

# a function to determine the impact of a key player, for a given stat (Kicks). The
# impact is based on a linear regression fit where the predictor is the stat and the 
# variable or outcome is the Margin

impact <- function(player, venue, opposition, stat) {
        df1 <- AFLstats %>% filter(Player == player) %>% select(Player, Opposition, Venue, Margin, contains(stat))
        x <- df1[[5]]
        y <- df1$Margin
        fit <- lm(y ~ x)
        b2 <- summary(fit)$coef[2, 1]
        b1 <- summary(fit)$coef[1, 1]
        df2 <- df1 %>% filter(Opposition == opposition | Venue == venue)
        imp <- mean(df2[[5]]) * b2 + b1
        imp
}

# produce a vector for the resulting impact of each key player against the opposition at this venue.
# at the moment, this is for R1, 2016: Adelaide vs North Melbourne, Docklands stadium

nm_adel <- sapply(nm_infl_kicks,  impact, "Docklands", "Adelaide",  "Kicks")
adel_nm <- sapply(adel_infl_kicks, impact, "Docklands", "North Melbourne", "Kicks")
paste("North Melbourne predicted to beat Adelaide by: ", sum(nm_adel) - sum(adel_nm), " points.")
