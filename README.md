# AFLMoneyball
Predict Australian Football League (AFL) Results

This project uses historical AFL statistics to predict results for the 2016 AFL season. It builds on my other AFL Project: AFLPlayerStatsGCD which produces stats for the previous 3 AFL seasons into a csv file.

In AFLMoneyball the first step is to import the csv file into a R dataframe, then use key players and key stats to predict the outcome.

In this commit the only stat or KPI used is the number of Kicks for the key players. Their past performance against the upcoming opposition team (at any venue) and at the upcoming venue (against any team) is compared with their overall average. If the key players have performed above avaerage against this team or at this venue overall then that will increases their team's chances of winning. Conversely if they perform poorly against this team or at this ground it will reflect poorly on their team's chances.
