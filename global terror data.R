library(ggplot2)
library(leaflet)
library(treemap)
library(corrplot)
library(tm)
library(tidytext)
library(tidyr)
library(wordcloud)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
library(topicmodels)

df <- read.csv("globalterrorismdb_0718dist.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)

#killing worldwide 
df %>% filter(nkill > 0) -> dfk
treemap(dfk, 
        index=c("iyear"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)

#killings by country
treemap(dfk, 
        index=c("country_txt"), 
        vSize = "nkill",  
        palette = "Reds",  
        title="Killings in Global Terrorism", 
        fontsize.title = 14 
)
#killings by coutry and year
treemap(dfk, 
        index=c("country_txt", "iyear"),  
        type = "value",
        vSize = "nkill", 
        vColor="nwound",
        palette = "RdBu", 
        title="Killings in Global terrorism  (Countries/Years) - size is proportional with the number of killings", 
        title.legend = "Number of wounded",
        fontsize.title = 10 
)
#killings per year and region
dfk %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkill)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

#Killings per province/state and years
dfk %>% group_by(provstate) %>% summarise(nk = sum(nkill)) %>% 
  top_n(50, wt=nk) %>% ungroup -> dfkp_top

dfk %>% filter(provstate %in% dfkp_top$provstate) %>% group_by(provstate, iyear) %>% 
  summarise(nk = sum(nkill), nw = sum(nwound)) %>% ungroup -> dfkp


treemap(dfkp, 
        index=c("provstate", "iyear"),  
        type = "value",
        vSize = "nk", 
        vColor="nw",
        palette = "RdBu", 
        title="Killings in Provinces/Years - size is proportional with the number of killings (top 50 regions only)", 
        
        title.legend = "Number of wounded",
        fontsize.title = 10 
)
#killings per cities and years
dfk %>% group_by(city) %>% summarise(nk = sum(nkill)) %>% 
  top_n(20, wt=nk) %>% ungroup -> dfkc_top

dfk %>% filter(city %in% dfkc_top$city) %>% group_by(city, iyear) %>% 
  summarise(nk = sum(nkill), nw = sum(nwound)) %>% ungroup -> dfkc


treemap(dfkc, 
        index=c("city", "iyear"),  
        type = "value",
        vSize = "nk", 
        vColor="nw",
        palette = "RdBu", 
        title="Killings in Cities/Years - size is proportional with the number of killings (top 20 cities only)", 
        
        title.legend = "Number of wounded",
        fontsize.title = 10 
)
#amercian citizen killed
df %>% filter(nkillus > 0) -> dfk_us
treemap(dfk_us,
        index=c("country_txt", "iyear"),  
        type = "value",
        vSize = "nkillus",  
        vColor="nwoundus",
        palette = "RdBu",  
        title="Killings in Global terrorism - US Citizens (Countries/Years) - size is proportional with the number of killings", 
        title.legend = "Number of wounded US citizens",
        fontsize.title = 12
)

#amrcian killed per year and region
dfk_us %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkillus)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

#amercian citizen wounded
df %>% filter(nwoundus > 0) -> dfw_us
treemap(dfw_us, 
        index=c("country_txt", "iyear"),  
        type = "value",
        vSize = "nwoundus",  
        vColor="nkillus",
        palette = "RdBu",  
        title="Wounded in Global terrorism - US Citizens (Countries/Years) - size ~  number of wounded", 
        title.legend = "Number of killed US citizens",
        fontsize.title = 11
)
#map with citizen killed
p <- leaflet(data = dfk_us) %>%
  addTiles() %>%
  addMarkers(lat=dfk_us$latitude, lng=dfk_us$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", dfk_us$iday,"/",dfk_us$imonth,"/", dfk_us$iyear,
                          "<br><br><strong>Place: </strong>", dfk_us$city,"-",dfk_us$country_txt,
                          "<br><strong>Killed: </strong>", dfk_us$nkill,
                          "<br><strong>Wounded: </strong>", dfk_us$nwound
             ))

#amercian wounded per year and region

dfw_us %>% group_by(iyear,region_txt) %>% summarise(nwounds = sum(nwoundus)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Wounded")
ggplot(data = dfyr, aes(x = Year, y = Wounded, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()
 

# type of attack

df %>% group_by(iyear,attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> dfya
colnames(dfya)<-c("Year","Type of attack","Number of events")
ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  geom_line() + geom_point() + theme_bw()

#events alternative classifiacation
df %>% filter(alternative_txt != "") %>% group_by(alternative_txt) %>% 
  summarise(n = length(alternative_txt))  %>% ungroup() -> dfa
ggplot(data = dfa, aes(x = reorder(alternative_txt,n), y = n)) +  
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="", x ="Alternative classification", y = "Number of events")

# suicide attack
df %>% filter(suicide ==1) -> dfs
treemap(dfs, 
        index=c("iyear","country_txt"), 
        type = "value",
        vSize = "nkill",  
        vColor="nwound",
        palette = "RdBu",  
        title="Suicide attacks - size is proportional with the number of kills", 
        title.legend = "Number of wounded",
        fontsize.title = 14
)
leaflet(data = dfs) %>%
  addTiles() %>%
  addMarkers(lat=dfs$latitude, lng=dfs$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", dfs$iday,"/",dfs$imonth,"/", dfs$iyear,
                          "<br><br><strong>Place: </strong>", dfs$city,"-",dfs$country_txt,
                          "<br><strong>Killed: </strong>", dfs$nkill,
                          "<br><strong>Wounded: </strong>", dfs$nwound,
                          "<br><strong>Killed US citizens: </strong>", dfs$nkillus,
                          "<br><strong>Wounded US citizens: </strong>", dfs$nwoundus
             ))

dfs %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkill)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

#terror attack with ransom demand

df %>% filter(ransompaid > 0) -> dfr
treemap(dfr, 
        index=c("country_txt", "iyear"), 
        type = "value",
        vSize = "ransompaid", 
        vColor="ransomamt",
        palette = "RdBu",  
        title="Ransom paid in Global terrorism  - size is proportional with the ransom paid", 
        title.legend = "Ransom demand",
        fontsize.title = 12
)

#map with ransom demands

leaflet(data = dfr) %>%
  addTiles() %>%
  addMarkers(lat=dfr$latitude, lng=dfr$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", dfr$iday,"/",dfr$imonth,"/", dfr$iyear,
                          "<br><br><strong>Place: </strong>", dfr$city,"-",dfr$country_txt,
                          "<br><strong>Killed: </strong>", dfr$nkill,
                          "<br><strong>Wounded: </strong>", dfr$nwound,
                          "<br><strong>Killed US citizens: </strong>", dfr$nkillus,
                          "<br><strong>Wounded US citizens: </strong>", dfr$nwoundus,
                          "<br><strong>Suicide attack(0-No/1-Yes): </strong>", dfr$suicide,
                          "<br><strong>Ransom paid: </strong>", dfr$ransompaid,
                          "<br><strong>Ransom note: </strong>", dfr$ransomnote,
                          "<br><strong>Hostages/kidnapped: </strong>", dfr$nhostkid,
                          "<br><strong>Hostages/kidnapped outcome: </strong>", dfr$hostkidoutcome_txt
             ))

#ransom demands with grouped by region and year
dfr %>% group_by(iyear,region_txt) %>% summarise(ransomsum = sum(ransompaid)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Ransom")
ggplot(data = dfyr, aes(x = Year, y = Ransom, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

#Hostage/kidnapping events with a ransom note
df %>% filter(!is.na(ransomnote)) -> dfho
dfho %>% filter(ransompaid > 0) -> dfho1
treemap(dfho1, 
        index=c("hostkidoutcome_txt","country_txt"), 
        type = "value",
        vSize = "ransompaid",  
        vColor="nhostkid",
        palette = "RdBu",  
        title="Ransom paid in Global terrorism  - size is proportional with the ransom paid", 
        title.legend = "Number of hostages/kidnapped",
        fontsize.title = 12 
)
dfho1 %>% group_by(iyear,region_txt) %>% summarise(nhostkidnp = sum(nhostkid)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Hostages_Kidnapped")
ggplot(data = dfyr, aes(x = Year, y = Hostages_Kidnapped, colour = Region)) +       
  geom_line() + geom_point() + theme_bw() +
  labs(title="Hostages and Kidnapped when there is a ransom note", x ="Year", y = "Hostages and Kidnapped")

#Hostage/kidnapping events outcome
treemap(dfo, 
        index=c("hostkidoutcome_txt","country_txt"),  
        type = "value",
        vSize = "cnt",  
        vColor="sum",
        palette = "RdBu",  
        title="Outcome of hostage events in Global terrorism - size is proportional with the number of events", 
        title.legend = "Number of hostages or kidnapped in the events",
        fontsize.title = 12
)
# correlation between value
terrorCor <- df[,c("iyear","imonth","iday","country", "nkill", "ransompaid","ransompaidus",
                   "nhostkidus","nhours","ndays")]
terrorCor <- na.omit(terrorCor)
correlations <- cor(terrorCor)
p <- corrplot(correlations, method="circle")

#ransom note (text analysis)
df %>% filter(!is.na(ransomnote)) -> dfn0
dfn0 %>% filter(ransomnote != "") -> dfn
text <- dfn$ransomnote
myCorpus <- Corpus(VectorSource(text))
myCorpus = tm_map(myCorpus, content_transformer(tolower))

#remove puncutation
myCorpus = tm_map(myCorpus, removePunctuation)

#remove numbers
myCorpus = tm_map(myCorpus, removeNumbers)

#remove stopwords for english
myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), stopwords("SMART")))

#create dtm
myDtm = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))

# frequent term and association
freqTerms <- findFreqTerms(myDtm, lowfreq=1)
m <- as.matrix(myDtm)

#calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wctop <-wordcloud(d$word, d$freq, min.freq=5, colors=brewer.pal(9,"Set1"))

mydata.df <- as.data.frame(inspect(removeSparseTerms(myDtm, sparse=0.99)))

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit, xaxt = 'n', yaxt='n', xlab = "Word clustering using ward.D method", ylab = "",
     main="Cluster Dendogram for words used in ransom notes") # display dendogram?
groups <- cutree(fit, k=5) 
#cut tree in 5 cluster

# draw dendogram with blue borders around the 5 clusters
rect.hclust(fit, k=5, border="blue")
  
# map with ransom note
leaflet(data = dfr) %>%
  addTiles() %>%
  addMarkers(lat=dfr$latitude, lng=dfr$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", dfr$iday,"/",dfr$imonth,"/", dfr$iyear,
                          "<br><br><strong>Place: </strong>", dfr$city,"-",dfr$country_txt,
                          "<br><strong>Killed: </strong>", dfr$nkill,
                          "<br><strong>Wounded: </strong>", dfr$nwound,
                          "<br><strong>Killed US citizens: </strong>", dfr$nkillus,
                          "<br><strong>Wounded US citizens: </strong>", dfr$nwoundus,
                          "<br><strong>Suicide attack(0-No/1-Yes): </strong>", dfr$suicide,
                          "<br><strong>Ransom paid: </strong>", dfr$ransompaid,
                          "<br><strong>Ransom note: </strong>", dfr$ransomnote,
                          "<br><strong>Hostages/kidnapped: </strong>", dfr$nhostkid,
                          "<br><strong>Hostages/kidnapped outcome: </strong>", dfr$hostkidoutcome_txt
             ))

#ransom notes topic modelling
topic_model_extract <- function(inputText, nTopics = 5) {
  
  # create a corpus
  myCorpus <- Corpus(VectorSource(inputText))
  # convert to lowercase
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus = tm_map(myCorpus, removePunctuation)
  # remove numbers
  myCorpus = tm_map(myCorpus, removeNumbers)
  # define archaisms to remove
  # remove stopwords, 'SMART' english
  myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), 
                                            stopwords('SMART')))
  # create a document-term matrix
  myDtm <- tm::DocumentTermMatrix(myCorpus, control = list(stemming = TRUE, stopwords = TRUE,
                                                           minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
  
  # remove rows with 0 words in each document
  rowTotals <- apply(myDtm , 1, sum) #find the sum of words in each Document
  myDtm   <- myDtm[rowTotals> 1, ]  
  
  # apply LDA
  system.time(model <- topicmodels::LDA(myDtm, 5 , method = "Gibbs", control = list(iter=2000, seed =42)))
  return (model)
}
model = topic_model_extract(dfr$ransomnote)
terms <- as.data.frame(topicmodels::terms(model, 5), stringsAsFactors = FALSE)
head(t(terms), 5) %>%
  kable( "html", escape=F, align="c") %>%
  column_spec(1:1, bold = T) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center")

# summary (text analysis)
summary wordcloud
df %>% filter(!is.na(summary)) -> dfn0
dfn0 %>% filter(summary != "") -> dfn
text <- sample(dfn$summary, nrow(dfn)/100)
myCorpus <- Corpus(VectorSource(text))
#myCorpus = tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus = tm_map(myCorpus, removePunctuation)
## Warning in tm_map.SimpleCorpus(myCorpus, removePunctuation): transformation
## drops documents
# remove numbers
myCorpus = tm_map(myCorpus, removeNumbers)
## Warning in tm_map.SimpleCorpus(myCorpus, removeNumbers): transformation
## drops documents
# remove stopwords for English
myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), stopwords("SMART"), "the"))
## Warning in tm_map.SimpleCorpus(myCorpus, removeWords,
## c(stopwords("english"), : transformation drops documents
#create DTM
myDtm = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 3))
#Frequent Terms and Associations
freqTerms <- findFreqTerms(myDtm, lowfreq=1)
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wctop <-wordcloud(d$word, d$freq, min.freq=50, colors=brewer.pal(9,"Set1"))

summary cluster dendogram
mydata.df <- as.data.frame(inspect(removeSparseTerms(myDtm, sparse=0.99)))

mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit, xaxt = 'n', yaxt='n', xlab = "Word clustering using ward.D method", ylab = "",
     main="Cluster Dendogram for words used in summary description") # display dendogram?
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with blue borders around the 5 clusters
rect.hclust(fit, k=5, border="blue")

#motive (text analysis)
motive wordcloud
df %>% filter(!is.na(motive)) -> dfm0
dfm0 %>% filter(motive != "") -> dfm
text <- sample(dfm$motive, nrow(dfm)/50)
specificWords <- c("The", "Unknown", "attack", "specific", "motive", "sources", "unknown", "claimed", "targeted",
                   "carried", "noted", "incident", "stated", "responsibility", "the")

text<-sapply(text, function(x) gsub("\n"," ",x))

myCorpus<-VCorpus(VectorSource(text))
myCorpusClean <- myCorpus %>% 
  #tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removeNumbers)) %>% 
  tm_map(content_transformer(removePunctuation)) %>%
  tm_map(content_transformer(removeWords),tidytext::stop_words$word) %>%
  tm_map(content_transformer(removeWords),specificWords)
myDtm = TermDocumentMatrix(myCorpusClean,
                           control = list(minWordLength = 3))
freqTerms <- findFreqTerms(myDtm, lowfreq=1)
m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
wctop <-wordcloud(d$word, d$freq, min.freq=5, colors=brewer.pal(9,"Set1"))


motive 2-gram
#define 2-gram
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
tdm_2<- TermDocumentMatrix(myCorpusClean, control = list(tokenize = BigramTokenizer))
m_tdm_2 <-as.matrix(tdm_2)
word.freq.2<-sort(rowSums(m_tdm_2), decreasing=T)
set.seed(314)
wordcloud(words=names(word.freq.2),freq = word.freq.2,random.order=F,colors=brewer.pal(9,"Set1"),max.words=100)
title(paste0('Most frequent 2-grams in Motive'),col.main='black',cex.main=2)

motive 3 gram 
TrigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
tdm_3 <- TermDocumentMatrix(myCorpusClean, control = list(tokenize = TrigramTokenizer))
m_tdm_3 <-as.matrix(tdm_3)
word.freq.3<-sort(rowSums(m_tdm_3), decreasing=T)
set.seed(314)
wordcloud(words=names(word.freq.3),freq = word.freq.3,random.order=F,colors=brewer.pal(9,"Set1"),max.words=100)
title(paste0('Most frequent 3-grams in Motive'),col.main='black',cex.main=2)

#Perpetrators group
df %>% group_by(gname) %>% summarise(nkills = sum(nkill)) %>% top_n(n=10) %>% ungroup() -> dfg
## Selecting by nkills
ggplot(data = dfg, aes(x = reorder(gname,nkills), y = nkills)) +  
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Group", y = "Killed")

df %>% group_by(gname) %>% summarise(nevents = length(gname)) %>% top_n(n=10) %>% ungroup() -> dfg
## Selecting by nevents
ggplot(data = dfg, aes(x = reorder(gname,nevents), y = nevents)) +  
  geom_bar(stat="identity", fill="yellow", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Group", y = "Terrorist attacks")

#eliminate the unknown 
dfg %>% filter(gname != "Unknown") -> fGroup
df %>%  filter(gname %in% fGroup$gname) %>% group_by(iyear,gname) %>% summarise(nevents = length(gname)) %>% ungroup() -> dfyg
colnames(dfyg)<-c("Year","Group","Events")
ggplot(data = dfyg, aes(x = Year, y = Events, colour = Group)) +       
  geom_line() + geom_point() + theme_bw() + theme(legend.position=c(0.25,0.8))

#Perpetrators nationality

df %>% group_by(natlty1_txt) %>% summarise(nevents = length(natlty1_txt)) %>% top_n(n=20) %>% ungroup() -> dfg
## Selecting by nevents
ggplot(data = dfg, aes(x = reorder(natlty1_txt,nevents), y = nevents)) +  
  geom_bar(stat="identity", fill="blue", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Perpetrators nationality", y = "Terrorist attacks")

# location for escape for kidnapping and hijack
df %>% filter(kidhijcountry != "") %>% group_by(kidhijcountry) %>% summarise(nevents = length(kidhijcountry)) %>% top_n(n=20) %>% ungroup() -> dfc
## Selecting by nevents
ggplot(data = dfc, aes(x = reorder(kidhijcountry,nevents), y = nevents)) +  
  geom_bar(stat="identity", fill="blue", colour="black") +
  coord_flip() + theme_bw(base_size = 8)  +
  labs(title="", x ="Location of escape", y = "Terrorist attacks")




















































































































































































































































































































































































































































































































