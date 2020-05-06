music = read.csv('/Users/morganpmckee/OneDrive/2019 - 2020/STA 371/Project/top10s.csv')
View(music)

install.packages('dplyr')
install.packages('ggplot2')
install.packages('reshape2')
install.packages("wakefield")
install.packages("RcppArmadillo")
install.packages('Amelia')
install.packages('corrplot')
install.packages('treemap')

library('dplyr')
library('ggplot2')
library('reshape2')
library(Rcpp)
library('Amelia')

# Data Cleaning
# check if there are any missing values in the dataset
missmap(music)
# there are no mussing values


# Data Exploration
names(music) <- c('ID', 'Title', 'Artist', 'Genre', 
                  'Year', 'BPM', 'Energy', 'Danceability', 'dB', 'Liveness', 
                  'Valence', 'Duration', 'Acousticness', 'Speechiness', 'Popularity')
View(music)
names(music) # see column names
dim(music) # shape 
summary(music) 


library(corrplot)
correlations <- cor(music[,5:14])
corrplot(correlations, method="circle", legend=FALSE)


unique(music$Artist)
# 366 distinct artists made it into the top

# Create table of unique values of artists in the dataset and how many songs they have
#plots of top genres and artists 
plot(as.table(topgenre))
plot(as.table(topartist))

table1 = table(music$Artist, useNA = 'ifany')
table1
max(table1)
# Katy Perry has the most top songs in this dataset

#top 5 artists 
tail(names(sort(table(music$Artist))),5)
#top5 genres 
tail(names(sort(table(music$Genre))),5)

# Pair Plot

upper.panel<-function(x, y){
  points(x, y, pch=19)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(music[,6:10], lower.panel = NULL, 
      upper.panel = upper.panel)

# Tree Map
library(treemap)
tree = treemap(music, index=c('Year', "Genre"),  
        vSize = "Popularity",  
        type="index", 
        palette = "Blues",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Genre Popularity by Year", fontsize.title = 14)
tree

cor(music$Genre == 'electro', music$Acousticness)
cor(music$Genre == 'electro', music$Speechiness)
cor(music$Genre == 'electro', music$Speechiness)

# Data Modeling

year.f = as.factor(music$Year)
model.matrix(~year.f)
model2 <- lm(Year~ BPM + dB + Danceability + Duration + Liveness + Valence + 
               Speechiness + Popularity, data = music)
summary(model2)


#histograms of the variables
hist(music$BPM) 
hist(music$Danceability)  
hist(music$Enrgy)  
hist(music$dB)  # skewed left
hist(music$Liveness) # skewed right
hist(music$Valence)
hist(music$Duration) # skewed right
hist(music$Acousticness) # skewed right
hist(music$Speechiness) # skewed right
hist(musicPopularity) # skewed left
#basic correlations
plot(music$Danceability,music$Valence) # theres a positive correlation between danceability and valance 
plot(music$BPM,music$Valence)
plot(music$Duration,music$Popularity)
plot(music$Valence,music$Enrgy) # theres a positive correlation between valence and energy 

# ggplots
library(ggplot2)
year1= subset(music, Year<2015)
year1=as.factor(year1)

ggplot(data = music, aes(x=music$Duration, y=music$Popularity)) +
  geom_point(alpha = 0.5, aes(color=music$BPM, size=music$Danceability, shape=music$Year)) +
  xlab('Duration') + ylab('Popularity')

ggplot(data = music, aes(x=music$Valence, y=music$Enrgy)) +
  geom_point(alpha = 0.5, aes(color=music$Liveness, size=music$Acousticness)) +
  xlab('Valence') + ylab('Energy')

# square pop
music$PopularitySq= music$Popularity^2
model2 = lm(PopularitySq ~ BPM + Energy + dB + Danceability + Duration + Liveness + Valence + Speechiness + Acousticness, data = music)
qqnorm(residuals(model2))
qqline(residuals(model2))
hist(residuals(model2))
summary(model2) # About 6.575% of variation in Popularity can be explained by this model

selected_reg4 = regsubsets(PopularitySq ~ BPM + Energy + dB + Danceability + Duration + Liveness + Valence + Speechiness + Acousticness,
                           data=music,method = "exhaustive",nvmax=10)
plot(selected_reg4, scale="r2")

#can we do better
music$BPMlog = log(music$BPM)
music$Energylog = log(music$Energy)
music$Danceabilitylog = log(music$Danceability)

model.1 = lm(PopularitySq ~ BPMlog + Energylog + dB + Danceabilitylog + Duration + Liveness + Valence + Speechiness + Acousticness, data = music)
qqnorm(residuals(model.1))
qqline(residuals(model.1))
hist(residuals(model.1))
summary(model.1) # About 4% of variation in Popularity can be explained by this model


