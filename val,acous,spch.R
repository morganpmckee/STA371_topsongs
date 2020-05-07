#loudness, acous, spch 
set.seed(104)
artistmodel = glm(topartist~., data = top10s, family = 'binomial')
summary(artistmodel)

install.packages('boot')
library(boot)
install.packages('leaps')
library(leaps)

#loudness model
select_db = regsubsets(dB~bpm+nrgy+dnce+live+val+dur+acous+spch+pop, data= top10s)
plot(select_db, scale= 'r2')

loudness = lm(dB~ bpm+nrgy+dnce+live+val+dur+acous+spch+pop, data = top10s)
summary(loudness)
#bpm, nrgy, dnce, acous, spch, pop are the most significant 


val_case = sample(1:301, 50, replace = FALSE)
train_case = setdiff(1:301, val_case)
train_set = top10s[train_case,]
val_set= top10s[val_case,]

loud_model = lm(dB~bpm+nrgy+dnce+live+val+dur+acous+spch+pop, data = train_set)
predicted_ydb= predict(loud_model, val_set)
mean((val_set$dB - predicted_ydb)^2)
#mean sse is 1.67, low value signifying a good model 

#acous model 
acous = lm(acous ~ dB+nrgy+dnce+live+val+dur+spch+pop, data = top10s)
summary(acousmodel)
#db, nrgy, dnce are statistically significant 

select_acous = regsubsets(acous~bpm+nrgy+dnce+live+val+dur+dB+spch+pop, data= top10s)
plot(select_acous, scale= 'r2')


acous_model = lm(acous~ dB+nrgy+dnce+live+val+dur+spch+pop,data = train_set )

predicted_yacous = predict(acous_model, val_set)
mean((val_set$acous - predicted_yacous)^2)
#mean sse 226.22

#spch 
spch = lm(spch~dB+nrgy+dnce+live+val+dur+acous+pop, data = top10s)
summary(spch)
#nrgy, live, val are statistically significant 

select_spch = regsubsets(spch~bpm+nrgy+dnce+live+val+dur+dB+acous+pop, data= top10s)
plot(select_spch, scale= 'r2')

spch_model = lm(spch~dB+nrgy+dnce+live+val+dur+acous+pop, data = train_set)

predict_yspch = predict(spch_model, val_set)
mean((val_set$spch - predict_yspch)^2)
#mean sse 79.85 