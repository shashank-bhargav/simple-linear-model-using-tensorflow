library(ggplot2) 
library(readr) 
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(scales)
library(corrplot)

city_data <- read.csv("cities_r2.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
str(city_data)
summary(city_data)

city_name = city_data$name_of_city[which(duplicated(city_data$name_of_city))]
city_data %>% filter(name_of_city==city_name)

sum(is.na(city_data)) 

head(city_data)

colnames(city_data)

#replace the extra spoace in cities
city_data$name_of_city #Few cities have space in their names. Will replace it for easy use.
city_data$name_of_city <- gsub(" ", "", city_data$name_of_city)

length(unique(city_data$state_code)) #Total 29 states codes and 
length(unique(city_data$state_name)) #Total 29 unique state names

nrow(unique(city_data[,c("state_code", "state_name")]))
unique(city_data[,c("state_code", "state_name")])

city_data$state_code <- as.factor(city_data$state_code)
city_data$state_name <- as.factor(city_data$state_name)

#Districts Analysis
length(unique(city_data$dist_code)) 
sort(unique(city_data$dist_code)) #Not continuous 1-99 codes
sum(is.na(city_data$dist_code)) #no empty
city_data$dist_code <- as.factor(city_data$dist_code)

#No of cities for each dist_code in each state.
temp = city_data %>% group_by(state_code, state_name, dist_code) %>% summarise(total_cities = n()) %>% arrange(total_cities)
#In one district there are 22 maximum cities considered for westbengal's district 22

ggplot(temp, aes(dist_code, total_cities, fill=state_name)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  geom_text(aes(label=total_cities), stat = "identity", vjust=-0.35) + 
  labs(x="State", y="Count", title="Number of Cities choosed from each State and district", 
       caption=paste("Total number of Cities considered =", sum(state_df$total_cities)))


##Treat the outliers from beginning

#Consider the total population will have the outlier

#we will exclude the outlier and maybe later we will analize them separately

#finding the 1st and 3rd quantiles
quantiles <- quantile(city_data$population_total,c(0.25,0.75))
quantiles

#creating a range which could be tolerated

range <- 1.5*IQR(city_data$population_total)
range

#Filtering lower outliers if any

lower_outliers <- city_data[which(city_data$population_total < (quantiles[1] - range)),]
nrow(lower_outliers)

#removing it from datasets

if(nrow(lower_outliers)!=0) {
  without_outlier_data <- city_data[-which(city_data$population_total < (quantiles[1] - range)),]
}

upper_outliers <- city_data[which(city_data$population_total > (quantiles[2] + range)),]
nrow(upper_outliers)
if (nrow(upper_outliers)!=0) {
  without_outlier_data <- without_outlier_data[-which(city_data$population_total > (quantiles[2] + range)),]
}

#aggreate data on state level
state_df <- city_data %>% group_by(state_code, state_name) %>% summarise(total_cities = n(),
                                                                         total_population = sum(population_total), 
                                                                         male_population = sum(population_male),
                                                                         female_population = sum(population_female),
                                                                         state_sex_ratio = round((female_population/male_population)*1000,0),
                                                                         child_population = sum(X0.6_population_total),
                                                                         male_child_population=sum(X0.6_population_male),
                                                                         female_child_population=sum(X0.6_population_female),
                                                                         state_Child_sex_ratio = round((female_child_population/male_child_population)*1000,0),
                                                                         total_literates = sum(literates_total),
                                                                         male_literates = sum(literates_male),
                                                                         female_literates = sum(literates_female),
                                                                         effective_literacy_rate = round((total_literates/total_population)*100,1),
                                                                         male_effective_literacy_rate = round((male_literates/total_population)*100,1),
                                                                         female_effective_literacy_rate = round((female_literates/total_population)*100,1),
                                                                         
                                                                         graduates_total = sum(total_graduates),
                                                                         graduates_male = sum(male_graduates),
                                                                         graduates_female = sum(female_graduates),
                                                                         total_graduation_rate = round((graduates_total/total_population)*100,2),
                                                                         male_graduation_rate = round((graduates_male/male_population)*100,2),
                                                                         female_graduation_rate = round((graduates_female/female_population)*100,2)) %>% arrange(desc(total_population))
   

ggplot(state_df, aes(reorder(state_name, total_cities, desc), total_cities, fill=state_name)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  geom_text(aes(label=total_cities), stat = "identity", vjust=-0.35) + 
  labs(x="State", y="Count", title="Number of Cities choosed from each State", 
       caption=paste("Total number of Cities considered =", sum(state_df$total_cities)))


population_data <- melt(state_df[, c("state_name", "total_population", "male_population", "female_population")], id="state_name")

ggplot(population_data, aes(state_name, value/10^6, col=variable)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom") +
  geom_line(aes(group=variable)) + geom_point(alpha=0.3) +
  labs(x="State", y="Population in Millions", title="Total, Male and Female Population in Each State")


state_df$total_population <- 100*state_df$total_population/sum(state_df$total_population)
state_df$male_population <- 100*state_df$male_population/sum(state_df$male_population)
state_df$female_population <- 100*state_df$female_population/sum(state_df$female_population)
population_data <- melt(state_df[, c("state_name", "total_population", "male_population", "female_population")], id="state_name")


ggplot(population_data, aes(state_name, value, col=variable)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom") +
  geom_line(aes(group=variable)) + geom_point(alpha=0.3) +
  labs(x="State", y="Population in %", title="Total, Male and Female Population in Each State")


ggplot(state_df, aes(reorder(state_name, state_sex_ratio, desc), state_sex_ratio, fill=state_name)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none") + geom_bar(stat="identity") +
  geom_text(aes(label=state_sex_ratio), stat = "identity",hjust=0, angle=90) +
  labs(x="State", y="Sex Ratio of Population", title="Sex Ratio of Population in each State")


## Plot for Child Population study in states ##
### Filter Child Poplation Data from the aggregated data ###
child_population_data <- melt(state_df[, c("state_name", "child_population", "male_child_population", "female_child_population")], id="state_name")

ggplot(child_population_data, aes(state_name, value/(10^6), col=variable)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom") + 
  geom_line(aes(group=variable)) + geom_point(alpha=0.3) +
  labs(x="State", y="Child Population in Million", title="Total, Male and Female Child Population in Each State")

state_df$child_population <- 100*state_df$child_population/sum(state_df$child_population)

state_df$male_child_population <- 100*state_df$male_child_population/sum(state_df$male_child_population)

state_df$female_child_population <- 100*state_df$female_child_population/sum(state_df$female_child_population)

child_population_data <- melt(state_df[, c("state_name", "child_population", "male_child_population", "female_child_population")], id="state_name")


ggplot(child_population_data, aes(state_name, value, col=variable)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom") + 
  geom_line(aes(group=variable)) + geom_point(alpha=0.3) +
  labs(x="State", y="Child Population in %", title="Total, Male and Female Child Population in Each State")


### Ploting sex ratio for each state

ggplot(state_df, aes(reorder(state_name, state_Child_sex_ratio, desc), state_Child_sex_ratio, fill=state_name)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none") + geom_bar(stat="identity") +
  geom_text(aes(label=state_Child_sex_ratio), stat = "identity",hjust=0, angle=90) +
  labs(x="State", y="Sex Ratio of Child Population", title="Sex Ratio of Child Population in each State")

#graph expalining 

literacy_data <- melt(state_df[, c("state_name", "total_literates", "male_literates", "female_literates")], id="state_name")

ggplot(literacy_data, aes(state_name, value/(10^6), col=variable)) + 
  geom_line(aes(group=variable)) + geom_point(alpha=0.3) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom") +
  labs(x="State", y="Literate Population in Million", title="Literate Population in States")


### Plot showing the female to male literates ratio in each state ###

ggplot(state_df, aes(state_name, round((female_literates/male_literates),2), fill=state_name)) + geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha=0.8) +
  geom_text(label=round((state_df$female_literates/state_df$male_literates),2), angle=90, hjust=0) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  labs(x="State", y="Female to Male\nLiterate population", title="Female Literate to Male Literate population in States")

#graph expalning the population litaeracy

literacy_rate_data <- melt(state_df[, c("state_name", "effective_literacy_rate", "male_effective_literacy_rate", "female_effective_literacy_rate")], id="state_name")

ggplot(state_df, aes(reorder(state_name, effective_literacy_rate, desc), effective_literacy_rate, fill=state_name)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha=0.8) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
  geom_text(aes(label=effective_literacy_rate), stat = "identity", hjust=0, angle=90) +
  labs(x="State", y="Literate rate of population", title="Literate Rate of Total Population in States")


ggplot(literacy_rate_data, aes(state_name, value, col=variable)) + geom_line(aes(group = variable)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  labs(x="State", y="Literate rate of population", title="Literate Rate of Total Population, Male and Female Population in States")

ggplot(city_data, aes(sex_ratio, child_sex_ratio)) + geom_point() + geom_smooth() +
  labs(x="Sex Ratio", y="Child Sex Ratio", title="Sex Ratio Vs. Child Sex Ratio")


ggplot(city_data, aes(sex_ratio, effective_literacy_rate_total)) + geom_point() + geom_smooth() +
  labs(x="Sex Ratio", y="Effective Literacy Rate", title="Sex Ratio Vs. Effective Literacy Rate")

ggplot(city_data, aes(sex_ratio, total_graduates*100/population_total)) + geom_point() + geom_smooth() +
  labs(x="Sex Ratio", y="Graduation Rate", title="Sex Ratio Vs. Graduation Rate")

ggplot(city_data, aes(effective_literacy_rate_total, total_graduates*100/population_total)) + geom_point() + geom_smooth() +
  labs(x="Literacy Rate", y="Graduation Rate", title="Literacy Rate  Vs. Graduation Rate")


## Finding correlation between the numeric variables ##
city_num_data <- city_data[,!(colnames(city_data) %in% c("name_of_city","state_code", "state_name", "dist_code", "location"))]
cordata <- cor(city_num_data)
corrplot(cordata, method="shade", type="upper")

operate_on <- city_data

#Population based clustering for adults

operates_on <- operate_on[,colnames(city_data) %in% c("sex_ratio", "effective_literacy_rate_total")]
str(operates_on)
scaled_df <- scale(operates_on)
dist_df <- dist(scaled_df)
str(dist_df)

sum(is.na(dist_df))

test <- as.data.frame(sapply(operates_on$location, function(x) strsplit(x,',')))
test <- t(test)
operates_on <- cbind(operates_on, test)

colnames(operates_on)[25] <-"longitude"
colnames(operates_on)[26] <- "latitude"
str(operates_on)
operates_on$longitude <- as.numeric(operates_on$longitude)
operates_on$latitude <- as.numeric(as.character(operates_on$latitude))

hcl <- hclust(dist_df, method = "ward.D2")
plot(hcl)
rect.hclust(hcl, k=5, border="red")


clusterCut <- cutree(hcl, k=5)
operates_on <- cbind(city_data,clusterCut)

cluster_level_output <- operates_on %>% group_by(clusterCut) %>% summarise(total_cities = n(),
                                                                           total_population = mean(population_total),
                                                                           sex_ratio = mean(sex_ratio),
                                                                           effective_literacy_rate = mean(effective_literacy_rate_total))  %>% arrange(desc(total_population))


cluster_level_output
ggplot(operates_on, aes(effective_literacy_rate_total, sex_ratio, col=as.factor(clusterCut))) + geom_point() + 
  geom_text(aes(label=name_of_city), stat = "identity", hjust=0)

ggplot(cluster_level_output, aes(clusterCut, total_population, fill=as.factor(clusterCut))) + geom_bar(stat="identity")
ggplot(cluster_level_output, aes(clusterCut, sex_ratio, fill=as.factor(clusterCut))) + geom_bar(stat="identity")
ggplot(cluster_level_output, aes(clusterCut, effective_literacy_rate, fill=as.factor(clusterCut))) + geom_bar(stat="identity")


r_sq <- vector()
for (number in 1:20){clus <- kmeans(scaled_df, centers = number, nstart = 50, iter.max = 50)
r_sq[number]<- clus$betweenss/clus$totss
}
ggplot() + geom_point(aes(1:20, r_sq))

KM_Clus <- kmeans(scaled_df, centers = 5, nstart = 50, iter.max = 50)
operates_on <- cbind(operates_on, kclus=KM_Clus$cluster)


cluster_level_output <- operates_on %>% group_by(kclus) %>% summarise(total_cities = n(),
                                                                      total_population = mean(population_total),
                                                                      sex_ratio = mean(sex_ratio),
                                                                      effective_literacy_rate = mean(effective_literacy_rate_total),
                                                                      total_literates = mean(literates_total))  %>% arrange(desc(total_population))
cluster_level_output


ggplot(operates_on, aes(effective_literacy_rate_total, sex_ratio, col=as.factor(kclus))) + geom_point() + 
  geom_text(aes(label=name_of_city), stat = "identity", hjust=0)

ggplot(cluster_level_output, aes(kclus, total_population, fill=as.factor(kclus))) + geom_bar(stat="identity")
ggplot(cluster_level_output, aes(kclus, sex_ratio, fill=as.factor(kclus))) + geom_bar(stat="identity")
ggplot(cluster_level_output, aes(kclus, effective_literacy_rate, fill=as.factor(kclus))) + geom_bar(stat="identity")



















































































































































































                                                                         
                                                                         
                                                                         
                                                                                                                                                  
                                                                                                                                                 
















































