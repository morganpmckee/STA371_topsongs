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

# Noelia


