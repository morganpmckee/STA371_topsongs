#average bpm
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
