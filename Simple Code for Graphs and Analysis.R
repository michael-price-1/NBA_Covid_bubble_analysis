#Data Collection
game_log_2020<-read.csv("Desktop/Thesis/2020 game data.csv", header=T, sep = ",")
game_log_2017_2019<-read.csv("Desktop/Thesis/2017 2019 game data.csv", header=T)

#Data Cleaning
game_log_2020$W.L<-ifelse(game_log_2020$W.L=="W", 1, 0)
game_log_2017_2019$W.L<-ifelse(game_log_2017_2019$W.L=="W", 1, 0)

game_log_2020_home<-game_log_2020[game_log_2020$Home.Away=="FALSE",]
game_log_2020_away<-game_log_2020[game_log_2020$Home.Away=="TRUE",]
game_log_2017_2019_home<-game_log_2017_2019[game_log_2017_2019$Home.Away=="FALSE",]
game_log_2017_2019_away<-game_log_2017_2019[game_log_2017_2019$Home.Away=="TRUE",]

#Home Winning Percentage Over Time
years<-2010:2020
winpct<-c(.6707, .667, .679, .635, .562, .593, .6744, .5696, .707, .561, .482)

plot(years, winpct, ylim = c(0,1), title("NBA Home Playoff Win % Since 2010"), col = ifelse(winpct==.494, "Red", "Black"), pch=16)
text(years, winpct, labels = winpct, cex = 0.5, pos = 1)
text(2010, 0.51, labels = "0.5", cex = 0.6, col = "Green")
abline(h=0.5, col = "Green")

#histograms
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,2))
hist(game_log_2020_home$PTS, xlab = "Points", col = "green",  xlim = c(60, 160), main = "Home 2020")
hist(game_log_2020_away$PTS,  xlab = "Points", col = "red",  xlim = c(60, 160), main = "Away 2020")
hist(game_log_2017_2019_home$PTS, xlab = "Points", col="green",  xlim = c(60, 160), main = "Home 2017-2019" )
hist(game_log_2017_2019_away$PTS, xlab = "Points", col = "red",  xlim = c(60, 160), main = "Home 2017-2019", breaks = 8 )
par(opar)

opar<-par(no.readonly = TRUE)
par(mfrow=c(2,3))
hist(game_log_2020_home$X2P., xlab = "Shooting Percentage", col= "green", main = "2020 Home 2 pointers",  xlim = c(20, 70))
hist(game_log_2020_home$X3P., xlab = "Shooting Percentage", col= "green", main = "2020 Home 3 pointers", xlim = c(10, 70))
hist(game_log_2020_home$FT., xlab = "Shooting Percentage", col= "green", main = "2020 Home Free Throws", xlim = c( 50, 100))
hist(game_log_2017_2019_home$X2P., xlab = "Shooting Percentage", col= "green", main = "2017-2019 2 pointers", xlim = c(20, 70))
hist(game_log_2017_2019_home$X3P., xlab = "Shooting Percentage", col= "green", main = "2017-2019 3 pointers", xlim = c(10, 70))
hist(game_log_2017_2019_home$FT., xlab = "Shooting Percentage", col= "green", main = "2017-2019 Free Throws", xlim = c( 50, 100))
par(opar)

opar<-par(no.readonly = TRUE)
par(mfrow=c(2,3))
hist(game_log_2020_away$X2P., xlab = "Shooting Percentage", col= "red", main = "2020 Away 2 pointers", xlim= c(30, 80))
hist(game_log_2020_away$X3P., xlab = "Shooting Percentage", col= "red", main = "2020 Away 3 pointers", xlim = c(10, 60))
hist(game_log_2020_away$FT., xlab = "Shooting Percentage", col= "red", main = "2020 Away Free Throws", xlim = c(40, 100))
hist(game_log_2017_2019_away$X2P., xlab = "Shooting Percentage", col= "red", main = "2017-19 Away 2 pointers", xlim= c(30, 80))
hist(game_log_2017_2019_away$X3P., xlab = "Shooting Percentage", col= "red", main = "2017-19 Away 3 pointers", xlim = c(10, 60))
hist(game_log_2017_2019_away$FT., xlab = "Shooting Percentage", col= "red", main = "2017-19 Away Free Throwa", xlim = c(40, 100))
par(opar)

#Analysis
library(BSDA)
prop.test(x=c(sum(game_log_2020_home$W.L), sum(game_log_2017_2019_home$W.L)), n=c(nrow(game_log_2020_home), nrow(game_log_2017_2019_home)), correct=FALSE)
z.test(x=game_log_2020_home$PTS, y=game_log_2017_2019_home$PTS, sigma.x = sd(game_log_2020_home$PTS), sigma.y = sd(game_log_2017_2019_home$PTS))
z.test(x=game_log_2020_away$PTS, y=game_log_2017_2019_away$PTS, sigma.x = sd(game_log_2020_away$PTS), sigma.y = sd(game_log_2017_2019_away$PTS))
prop.test(x=c(sum(game_log_2020_home$X2PM), sum(game_log_2017_2019_home$X2PM)), n=c(sum(game_log_2020_home$X2PA), sum(game_log_2017_2019_home$X2PA)))
prop.test(x=c(sum(game_log_2020_home$X3PM), sum(game_log_2017_2019_home$X3PM)), n=c(sum(game_log_2020_home$X3PA), sum(game_log_2017_2019_home$X3PA)))
prop.test(x=c(sum(game_log_2020_home$FTM), sum(game_log_2017_2019_home$FTM)), n=c(sum(game_log_2020_home$FTA), sum(game_log_2017_2019_home$FTA)))
prop.test(x=c(sum(game_log_2020_away$X2PM), sum(game_log_2017_2019_away$X2PM)), n=c(sum(game_log_2020_away$X2PA), sum(game_log_2017_2019_away$X2PA)))
prop.test(x=c(sum(game_log_2020_away$X3PM), sum(game_log_2017_2019_away$X3PM)), n=c(sum(game_log_2020_away$X3PA), sum(game_log_2017_2019_away$X3PA)))
prop.test(x=c(sum(game_log_2020_away$FTM), sum(game_log_2017_2019_away$FTM)), n=c(sum(game_log_2020_away$FTA), sum(game_log_2017_2019_away$FTA)))

