music = read.csv('/Users/morganpmckee/OneDrive/2019 - 2020/STA 371/Project/top10s.csv')
View(music)

install.packages('dplyr')
library('dplyr')


length(music$artist)
# 603 artists made it into the top

# Create table of unique values of artists in the dataset and how many songs they have
a <- table(music$artist)
a
max(a)

# Katy Perry has the most top songs in this dataset


# return shape
dim(music)
summary(music)


# trinity - my df name is tops
#histograms of the variables
hist(tops$bpm) 
mean(tops$bpm) #average beats per minute is 118.5456 beats
hist(tops$dnce)  
hist(tops$nrgy)  
hist(tops$dB)  # skewed left
hist(tops$live) # skewed right
hist(tops$val)
hist(tops$dur) # skewed right
mean(tops$dur) # average song is 3 minutes and 44 seconds
hist(tops$acous) # skewed right
hist(tops$spch) # skewed right
hist(tops$pop) # skewed left
#basic correlations
plot(tops$dnce,tops$val) # theres a positive correlation between danceability and valance 
plot(tops$bpm,tops$val)
plot(tops$dur,tops$pop)
plot(tops$val,tops$nrgy) # theres a positive correlation between valence and energy 
tops$year= as.factor(tops$year)
# ggplots
library(ggplot2)
year1= subset(tops, year<2015)
year1=as.factor(year1)

ggplot(data = tops, aes(x=tops$dur, y=tops$pop)) +
  geom_point(alpha = 0.5, aes(color=tops$bpm, size=tops$dnce, shape=tops$year)) +
  xlab('Duration') + ylab('Popularity')

ggplot(data = tops, aes(x=tops$val, y=tops$nrgy)) +
  geom_point(alpha = 0.5, aes(color=tops$live, size=tops$acous)) +
  xlab('Valence') + ylab('Energy')


# Mohammad

# anna

# Noelia


