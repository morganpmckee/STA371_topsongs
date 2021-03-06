music = read.csv('/Users/morganpmckee/OneDrive/2019 - 2020/STA 371/Project/top10s.csv')
View(music)

install.packages('dplyr')
install.packages('ggplot2')
install.packages('reshape2')

library('ggplot2')
library('reshape2')

# Data Cleaning / feature engineering
# check if there are any missing values in the dataset

install.packages('Amelia')
library('Amelia')
missmap(music)
# there are no mussing values

# Row 443 has mostly zero values, so delete
music = music[-c(443),]


names(music) <- c('ID', 'Title', 'Artist', 'Genre', 
                  'Year', 'BPM', 'Energy', 'Danceability', 'dB', 'Liveness', 
                  'Valence', 'Duration', 'Acousticness', 'Speechiness', 'Popularity')

# change popularity scores that are 0 with the mean of the column based on other cols
library(data.table)
setDT(music)[Popularity==0, Popularity := NA,]
music[, Popularity := replace(Popularity, is.na(Popularity), mean(Popularity, na.rm = TRUE)) 
      , by = .(BPM, Energy, Danceability, dB, Liveness, Valence, Duration, Acousticness, Speechiness)]
View(music)

# Data Exploration

View(music)
names(music) # see column names
dim(music) # shape 
summary(music) 

install.packages('corrplot')
library(corrplot)
correlations <- cor(music[,5:14])
corrplot(correlations, method="circle", legend=FALSE)

unique(music$Artist)
# 366 distinct artists made it into the top

table1 = table(music$Artist, useNA = 'ifany')
table1
max(table1)
# Katy Perry has the most top songs in this dataset

#top 5 artists 
tail(names(sort(table(music$Artist))),5)
#top5 genres 
tail(names(sort(table(music$Genre))),5)

# Pair Plot
install.packages('psych')
library(psych)
pairs.panels(music[,6:15], 
             method = "pearson", 
             hist.col = "red",
             density = TRUE,
             ellipses = TRUE 
)


# Tree Map
install.packages('treemap')
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

hist(music$Danceability)

h1 = hist(music$BPM)
h2 = hist(music$Liveness)
h3 = hist(music$Valence)
h4 = hist(music$Popularity)
h5 = hist(music$dB)
h6 = hist(music$Energy)
h7 = hist(music$Speechiness)

# Data Modeling

year.f = as.factor(music$Year)
model.matrix(~year.f)
model2 <- lm(Year~ BPM + dB + Danceability + Duration + Liveness + Valence + 
               Speechiness + Popularity, data = music)
summary(model2)

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


# Cross Validation

install.packages('leaps')
install.packages('boot')
library('leaps')
library('boot')

#dance model 

select_dancemodel = regsubsets(Danceabilitylog ~ BPMlog + Energylog + Liveness + Valence + PopularitySq,  data=music)
plot(select_dancemodel, scale= 'r2', main = 'Dance model')
summary(select_dancemodel)

dance = lm(Danceabilitylog ~ BPMlog + Energylog + Liveness + Valence + Popularity, data = music)

dancemodel = lm(Danceabilitylog ~ BPMlog + Energylog + Liveness + Valence + Popularity, data = training_set)

predicted_y = predict(dancemodel, validation_set)
mean((validation_set$Danceabilitylog - predicted_y)^2) #.0363



#Energy model


select_energy = regsubsets(Energylog~ BPMlog + dB + Danceabilitylog + Duration + Liveness + Valence + 
                             + PopularitySq,  data=music)
plot(select_energy, scale= 'r2', main = 'Energy model')
summary(select_energy)

energy = lm(Energylog~ BPMlog + dB + Danceabilitylog + Duration + Liveness + Valence + 
              + PopularitySq, data = music)

energymodel = lm(Energylog~ BPMlog + dB + Danceabilitylog + Duration + Liveness + Valence + 
                   + PopularitySq, data = training_set)

predicted_y = predict(energymodel, validation_set)
mean((validation_set$Energylog - predicted_y)^2) #.041




#duration model 

select_duration = regsubsets(Duration ~ BPMlog + dB + Danceabilitylog +  Liveness + Valence + Speechiness
                            + PopularitySq,  data=music)
plot(select_duration, scale= 'r2', main = 'Duration model')
summary(select_energy)

duration = lm(Duration ~ BPMlog + dB + Danceabilitylog +  Liveness + Valence + Speechiness
              + PopularitySq, data = music)

durationmodel = lm(Duration ~ BPMlog + dB + Danceabilitylog +  Liveness + Valence + Speechiness
                   + PopularitySq, data = training_set)

predicted_y = predict(durationmodel, validation_set)
mean((validation_set$Duration - predicted_y)^2) #939.772


# Popularity model
selected_reg = regsubsets(PoularitySq ~ BPMlog + Energylog
                          + Danceabilitylog + Liveness +Valence + dB + Duration + 
                            Acousticness + Speechiness,data=music,method="backward")
plot(selected_reg, scale="r2")

summary(lm(PopularitySq ~ BPMlog + Energylog + Danceabilitylog + Liveness + 
             Valence + dB + Duration + Acousticness + Speechiness, data=music))


pop_model = lm(PopularitySq ~ BPMlog + Energylog + Danceabilitylog + 
                 Liveness + dB + Duration + Acousticness, data=music)

predicted_y = predict(pop_model, validation_set) 
mean((validation_set$PopularitySq - predicted_y)^2)


# live model
selected_reg = regsubsets(Liveness ~ BPMlog + Energylog + Danceabilitylog + PopularitySq 
                          + Valence + dB + Duration + Acousticness + 
                            Speechiness, data=music, method="backward")
plot(selected_reg, scale="r2")

summary(lm(Liveness~ BPMlog + Energylog + Danceabilitylog + PopularitySq + Valence + 
             Duration + Speechiness,data=music))

live_model = lm(Liveness~ BPMlog + Energylog + Danceabilitylog + PopularitySq + Valence
                + Duration + Speechiness, data=music)

predicted_y = predict(live_model, validation_set) 
mean((validation_set$Liveness - predicted_y)^2) 


# bpm model
selected_reg = regsubsets(BPMlog~ PopularitySq + Energylog + Danceabilitylog +
                            Liveness + Valence + dB + Duration + Acousticness + 
                            Speechiness,data=music,method="backward")
plot(selected_reg, scale="r2")

summary(lm(BPMlog~ Energylog + Danceabilitylog + Liveness + dB 
           + Duration + Acousticness + Speechiness, data=music))

bpm_model = lm(BPMlog~Energylog+Danceabilitylog+Liveness+dB+Duration+Acousticness+Speechiness, data=music)

predicted_y = predict(bpm_model, validation_set) 
mean((validation_set$BPMlog - predicted_y)^2) 

#loudness model
select_db = regsubsets(dB ~ BPMlog + Energylog + PopularitySq + 
                         Danceabilitylog + Duration + Liveness + Valence + 
                         Speechiness + Acousticness, data = music, method =  "backward")
plot(select_db, scale= 'r2', main = "Loudness Model" )
summary(select_db)

loudness = lm(dB~ BPMlog + Energylog + PopularitySq + Danceabilitylog + 
                Duration + Liveness + Valence + Speechiness + Acousticness, data = music)
summary(loudness)
#47.41% of model is explained by model 
#valence,speechiness, acousticness, and popularity are the most significant 

loud_model = lm(dB ~ PopularitySq + BPMlog + Energylog + Danceabilitylog + 
                  Duration + Liveness + Valence + Speechiness + 
                  Acousticness, data = train_set)
predicted_ydb= predict(loud_model, val_set)
mean((val_set$dB - predicted_ydb)^2)
#mean sse is 1.73, low value signifying a good model 

#acous model 
acousmodel = lm(Acousticness ~ dB + PopularitySq + BPMlog + Energylog + 
                  Danceabilitylog + Duration + Liveness + Valence 
                + Speechiness, data = music)
summary(acousmodel)
#db, nrgy, dnce are statistically significant 

select_acous = regsubsets(Acousticness ~ dB + PopularitySq + BPMlog + Energylog + 
                            Danceabilitylog + Duration + Liveness + Valence +
                            Speechiness, data= music, method = "backward")
plot(select_acous, scale= 'r2', main = "Acousticness Model")


acous_model = lm(Acousticness ~ dB + PopularitySq + BPMlog + Energylog + Danceabilitylog 
                 + Duration + Liveness + Valence + Speechiness,data = training_set )

predicted_yacous = predict(acous_model, validation_set)
mean((validation_set$Acousticness - predicted_yacous)^2)
#mean sse 207.21

#spch 
spch = lm(Speechiness~ dB + PopularitySq + BPMlog + Energylog + Danceabilitylog + 
            Duration + Liveness + Valence + Acousticness, data = music)
summary(spch)
#nrgy, live, val are statistically significant, 9.35% is represented by model 

select_spch = regsubsets(Speechiness~dB + PopularitySq + BPMlog + Energylog + 
                           Danceabilitylog + Duration + Liveness + Valence + 
                           Acousticness, data= music, method = "backward")
plot(select_spch, scale= 'r2', main = "Speechiness Model")


spch_model = lm(Speechiness ~ dB + PopularitySq + BPMlog + Energylog + Danceabilitylog + 
                  Duration + Liveness + Valence + Acousticness, data = training_set)

predict_yspch = predict(spch_model, validation_set)
mean((validation_set$Speechiness - predict_yspch)^2)
#mean sse 82.74


# Extra Portion

# HAC Clustering 
#split the dataset into features and labels
labels = music$Popularity
drops = c("Popularity")
data = subset(music, select = -c(Popularity, Title, Artist, ID, Genre, Year))
dim(data)

as.numeric(music$dB)

install.packages('tidyverse')
library(tidyverse)
library(cluster)
install.packages('factoextra')
library(factoextra)

# Ward's Method Proximity Measure'
hd = dist(scale(data), method = "euclidean")
hc = hclust(hd, method = "ward.D2")

fviz_dend(hc, k = 4,                 
          cex = 0.5,                 
          k_colors = c("red", "blue", "green", "yellow"),
          color_labels_by_k = TRUE,  
          ggtheme = theme_gray()     
)

# Complete (Max) Link Proximity Measure
hd2 = dist(scale(data), method = "euclidean")
hc2 = hclust(hd, method = "complete")

fviz_dend(hc2, k = 4,                 
          cex = 0.5,                 
          k_colors = c("red", "blue", "green", "yellow"),
          color_labels_by_k = TRUE,  
          ggtheme = theme_gray()     
)


# K Means Clustering

library(cluster)
library(gridExtra)

# scale data 
data_s = scale(data)

k2 = kmeans(data_s, centers = 2, nstart = 25)
k3 = kmeans(data_s, centers = 3, nstart = 25)  
k4 = kmeans(data_s, centers = 4, nstart = 25)  
k5 = kmeans(data_s, centers = 5, nstart = 25)  

p1 = fviz_cluster(k2, geom = "point", data = data_s) + ggtitle("k = 2")
p2 = fviz_cluster(k3, geom = "point", data = data_s) + ggtitle("k = 3")
p3 = fviz_cluster(k4, geom = "point", data = data_s) + ggtitle("k = 4")
p4 = fviz_cluster(k5, geom = "point", data = data_s) + ggtitle("k = 5")
grid.arrange(p1, p2, p3, p4, nrow = 2)

# k means did not perform well because of differing clusters with different levels of densities or clusters 
# of different sizes as exhibited in HAC
