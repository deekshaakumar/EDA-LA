matches <- read.csv("C:/Users/lalit/Downloads/fifa dataset/WorldCupMatches.csv")
players <- read.csv("C:/Users/lalit/Downloads/fifa dataset/WorldCupPlayers.csv")
worldcups <- read.csv("C:/Users/lalit/Downloads/fifa dataset/WorldCups.csv")



library(ggplot2)
library(gcookbook)
library(ggthemes)


top <- head(matches, 25)

ggplot(top, aes(x = Attendance, y = Stadium,)) +
  geom_point() +
  labs(title = "Fifa world cup matches",
       x = "Attendance",
       y = "Stadium") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none")



fifamatchesyear <- data.frame(Year=matches$Year,Attendance = matches$Attendance)
ggplot(fifamatchesyear, aes(x = Year, y = Attendance)) +geom_area(colour = "black",fill = "green", alpha = .2) +geom_line()



baloonplot <- ggplot(worldcups, aes(x = MatchesPlayed, y = Year, size = GoalsScored)) +geom_point(shape = 21, colour = "black", fill = "skyblue")
baloonplot


library(dplyr)
worldcups_num <- select(worldcups, -Country, -Winner, -Runners.Up, -Third, -Fourth, -Attendance)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

pairs(worldcups_num,upper.panel = panel.cor,diag.panel  = panel.hist,lower.panel = panel.smooth)


matchesfilter <- matches[matches$Stage %in% c('Semi-finals','Final','Quarter-finals','Match for third place','Group 1'),]
violin <- ggplot(matchesfilter, aes(x = Stage, y = Attendance))
violin + geom_violin()




ggplot(matches, aes(x = Home.Team.Goals)) +geom_density(fill = "blue", alpha = .2, colour = NA) +xlim(0, 10) +geom_line(stat = "density")


ggplot(matches, aes(x = Home.Team.Goals, y = ..density..)) +geom_histogram(fill = "cornsilk", colour = "grey60", size = .2)+geom_density() +xlim(0, 10)


ggplot(matches, aes(x = Home.Team.Goals)) +geom_bar()
ggplot(players, aes(x = Shirt.Number)) +geom_bar()


ggplot(matches, aes(x = Home.Team.Goals)) +geom_bar()
ggplot(players, aes(x = Shirt.Number)) +geom_bar()


ggplot(matches, aes(x=Home.Team.Goals)) +
  geom_freqpoly()


ggplot(matchesfilter, aes(x = factor(Stage), y = Home.Team.Goals)) +geom_boxplot() +stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "orange")



ggplot(matchesfilter, aes(x = factor(Stage), y = Home.Team.Goals)) +geom_boxplot(notch = TRUE)


ggplot(matches, aes(x = Year, y = Attendance)) +geom_point() +geom_rug(position = "jitter", size = 0.2)


ggplot(matches, aes(x = Year, y = Home.Team.Goals)) +geom_line() +geom_point(size = 4, shape = 22, colour = "darkred", fill = "orange")