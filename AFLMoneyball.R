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
        infl_list <- NULL
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
                        infl_list <- c(player_list[i], infl_list)
                }
        }
        infl_list
}




# a function to determine the impact of a key player, for a given stat (Kicks). The
# impact is based on a linear regression fit where the predictor is the stat and the 
# variable or outcome is the Margin

impact <- function(player, venue, opposition, stat) {
        df1 <- AFLstats %>% filter(Player == player) %>% 
                select(Player, Opposition, Venue, Margin, contains(stat, ignore.case = FALSE))
        x <- df1[[5]]
        y <- df1$Margin
        fit <- lm(y ~ x)
        b2 <- summary(fit)$coef[2, 1]
        b1 <- summary(fit)$coef[1, 1]
        df2 <- df1 %>% filter(Opposition == opposition)
        df3 <- df1 %>% filter(Venue == venue)
        if (nrow(df2) == 0 & nrow(df3) == 0)
                return(0)
        imp <- mean(c(df2[[5]], df3[[5]]), na.rm = TRUE) * b2 + b1
        imp
}

## find the main influencers for each team, for various key stats
## so far this is just kicks

adel_infl_kicks <- influencers("Adelaide", "Kicks")
bris_infl_kicks <- influencers("Brisbane Lions", "Kicks")
carl_infl_kicks <- influencers("Carlton", "Kicks")
coll_infl_kicks <- influencers("Collingwood", "Kicks")
ess_infl_kicks <- influencers("Essendon", "Kicks")
fre_infl_kicks <- influencers("Fremantle", "Kicks")
geel_infl_kicks <- influencers("Geelong", "Kicks")
gc_infl_kicks <- influencers("Gold Coast", "Kicks")
gws_infl_kicks <- influencers("Greater Western Sydney", "Kicks")
haw_infl_kicks <- influencers("Hawthorn", "Kicks")
mel_infl_kicks <- influencers("Melbourne", "Kicks")
nm_infl_kicks <- influencers("North Melbourne", "Kicks")
pa_infl_kicks <- influencers("Port Adelaide", "Kicks")
rich_infl_kicks <- influencers("Richmond", "Kicks")
stk_infl_kicks <- influencers("St Kilda", "Kicks")
syd_infl_kicks <- influencers("Sydney", "Kicks")
wc_infl_kicks <- influencers("West Coast", "Kicks")
wb_infl_kicks <- influencers("Western Bulldogs", "Kicks")


# produce a vector for the resulting impact of each key player against the opposition at this venue.
# at the moment, this is for R1, 2016.

rich_carl <- sapply(rich_infl_kicks,  impact, "M.C.G.", "Carlton", "Kicks")
carl_rich <- sapply(carl_infl_kicks,  impact, "M.C.G.", "Richmond", "Kicks")
paste("Richmond predicted to beat Carlton by: ", sum(rich_carl) - sum(carl_rich), " points.")

mel_gws <- sapply(mel_infl_kicks,  impact, "M.C.G.", "Greater Western Sydney", "Kicks")
gws_mel <- sapply(gws_infl_kicks,  impact, "M.C.G.", "Melbourne", "Kicks")
paste("Melbourne predicted to beat GWS by: ", sum(mel_gws) - sum(gws_mel), " points.")

gc_ess <- sapply(gc_infl_kicks,  impact, "Carrara", "Essendon", "Kicks")
ess_gc <- sapply(ess_infl_kicks,  impact, "Carrara", "Gold Coast", "Kicks")
paste("Gold Coast predicted to beat Essendon by: ", sum(gc_ess) - sum(ess_gc), " points.")

nm_adel <- sapply(nm_infl_kicks,  impact, "Docklands", "Adelaide", "Kicks")
adel_nm <- sapply(adel_infl_kicks, impact, "Docklands", "North Melbourne", "Kicks")
paste("North Melbourne predicted to beat Adelaide by: ", sum(nm_adel) - sum(adel_nm), " points.")

syd_coll <- sapply(syd_infl_kicks,  impact, "S.C.G.", "Collingwood", "Kicks")
coll_syd <- sapply(coll_infl_kicks,  impact, "S.C.G.", "Sydney", "Kicks")
paste("Sydney predicted to beat Collingwood by: ", sum(syd_coll) - sum(coll_syd), " points.")

wb_fre <- sapply(wb_infl_kicks,  impact, "Docklands", "Fremantle", "Kicks")
fre_wb <- sapply(fre_infl_kicks,  impact, "Docklands", "Western Bulldogs", "Kicks")
paste("Western Bulldogs predicted to beat Fremantle by: ", sum(wb_fre) - sum(fre_wb), " points.")

pa_stk <- sapply(pa_infl_kicks,  impact, "Adelaide Oval", "St Kilda", "Kicks")
stk_pa <- sapply(stk_infl_kicks,  impact, "Adelaide Oval", "Port Adelaide", "Kicks")
paste("Port Adelaide predicted to beat St Kilda by: ", sum(pa_stk) - sum(stk_pa), " points.")

wc_bris <- sapply(wc_infl_kicks,  impact, "Subiaco", "Brisbane", "Kicks")
bris_wc <- sapply(bris_infl_kicks,  impact, "Subiaco", "West Coast", "Kicks")
paste("West Coast predicted to beat Brisbane by: ", sum(wc_bris) - sum(bris_wc), " points.")

geel_haw <- sapply(geel_infl_kicks,  impact, "M.C.G.", "Hawthorn", "Kicks")
haw_geel <- sapply(haw_infl_kicks,  impact, "M.C.G.", "Geelong", "Kicks")
paste("Geelong predicted to beat Hawthorn by: ", sum(geel_haw) - sum(haw_geel), " points.")
