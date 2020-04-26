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


# trinity


# Mohammad

# anna
# called my dataset top10
model1 <- lm(top10$pop~top10$bpm+top10$dnce+top10$live+top10$val+top10$spch)
summary(model1) 
hist(top10$dnce)
hist(top10$bpm)
hist(top10$live)
hist(top10$val)

year.f = as.factor(top10$year)
model.matrix(~year.f)
model2 <- lm(top10$year~top10$bpm+top10$dnce+top10$live+top10$val+top10$spch+top10$pop)
summary(model2)

cor(top10$bpm,top10$dnce) # negative
cor(top10$bpm,top10$pop)

# Noelia

mean(top10s$bpm)
#average duration in seconds
mean(top10s$dur)
#artists with numbers of charted songs 
topartist = table(top10s$artist, useNA = 'ifany')
#top genres
topgenre = table(top10s$top.genre, useNA = 'ifany')
#plots of top genres and artists 
plot(as.table(topgenre))
plot(as.table(topartist))
#top 5 artists 
tail(names(sort(table(top10s$artist))),5)
#top5 genries 
tail(names(sort(table(top10s$genre))),5)
