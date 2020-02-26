library(data.table)
library(ggplot2)
library(lme4)
library(AICcmodavg)
library(dplyr)


######Build Data set for final project#####
##Read in different .csv
laws <- read.csv('C:/Users/wonha/Desktop/SMU Class Material/6302 Experimental Statistics II/Final Project/state-firearms/raw_data.csv')
shootings <- read.csv('C:/Users/wonha/Desktop/SMU Class Material/6302 Experimental Statistics II/Final Project/awram-us-mass-shootings/awram-us-mass-shootings/data/mother_jones_mass_shootings_database_1982_2019.2.csv')
shootings <- shootings[c(3:4,7:9)]

##Create a master data set by merging variables I need.
master.set <- merge(x = laws, y = shootings, by = c("state", "year"), all.x = TRUE)

##dealing with NA that I merged into master data set.
master.set$fatalities[is.na(master.set$fatalities)] <- 0
sum(is.na(master.set$fatalities))
master.set$injured[is.na(master.set$injured)] <- 0
sum(is.na(master.set$injured))
master.set$totalvictims[is.na(master.set$totalvictims)] <- 0
sum(is.na(master.set$totalvictims))

##read in population data, rename variables, reshape long, and merge to master.set
population <- read.csv('C:/Users/wonha/Desktop/SMU Class Material/6302 Experimental Statistics II/Final Project/PEP_2018_PEPANNRES/PEP_2018_PEPANNRES_with_ann.csv')
population <- population[-c(1),]
population$rescen42010<-NULL
population$resbase42010<-NULL
setnames(population, old=c("GEO.display.label","respop72010", "respop72011", "respop72012", "respop72013", "respop72014", "respop72015", "respop72016", "respop72017", "respop72018"), 
         new=c("state", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))
population$`2010`<- as.numeric(population$`2010`)
population$`2011`<- as.numeric(population$`2011`)
population$`2012`<- as.numeric(population$`2012`)
population$`2013`<- as.numeric(population$`2013`)
population$`2014`<- as.numeric(population$`2014`)
population$`2015`<- as.numeric(population$`2015`)
population$`2016`<- as.numeric(population$`2016`)
population$`2017`<- as.numeric(population$`2017`)
population$`2018`<- as.numeric(population$`2018`)
str(population)

##Convert from wide to long
population.L <- reshape(data = population, varying = 4:12, v.names = "population", timevar = "year",
                  times = 2010:2018, idvar = "state", direction = "long")
##Check headings for long format, n=10 tells R to show you 10 obs.
head(population.L, n = 10)

##Reorder new data by subject then time point
population.LS <- population.L[order(population.L$state, population.L$year), ]

#Merge
master.set <- merge(x = master.set, y = population.LS, by = c("state", "year"), all.x = TRUE)
master.set$GEO.id<-NULL
master.set$GEO.id2<-NULL
str(master.set)

#####Create lag: difference in total gun laws in case I need it
master.set <- as.data.table(master.set)
setkey(master.set, state, year)
master.set[, law.diff := lawtotal - shift(lawtotal, fill = first(lawtotal)), by = state]
str(master.set)
master.set <- as.data.frame(master.set)

#All variables have been set up to have NAs deleted
##Easy way to check proportion of missing data
##colMeans,rowMeans, colSums, rowSums is to find means and sums of rows and columns
mymiss <- colMeans(is.na(master.set[,3:141]))
mymiss #Population is the only one that has missing values.
#GOing to proceed with analysis for only 2000-2017 since they are have population 
#and I can't impute since the missing values are not missing at random.
master.set <- na.omit(master.set)
str(master.set) #data set size reduced from 1350 obs to 400 obs.

####Creating a region variable to try to account for similiar cultures and future exploratory analysis section.
##Regions defined by National Geographics presentation.
master.set$region[master.set$state == "California" |
                    master.set$state == "Oregon"|
                    master.set$state == "Washington"|
                    master.set$state == "Nevada"|
                    master.set$state == "Idaho"|
                    master.set$state == "Utah"|
                    master.set$state == "Colorado"|
                    master.set$state == "Wyoming"|
                    master.set$state == "Montana"|
                    master.set$state == "Hawaii"|
                    master.set$state == "Alaska"] <- "West"
master.set$region[master.set$state == "Arizona" |
                    master.set$state == "New Mexico"|
                    master.set$state == "Texas"|
                    master.set$state == "Oklahoma"] <- "Southwest"
master.set$region[master.set$state == "North Dakota" |
                    master.set$state == "South Dakota"|
                    master.set$state == "Nebraska"|
                    master.set$state == "Kansas"|
                    master.set$state == "Missouri"|
                    master.set$state == "Iowa"|
                    master.set$state == "Minnesota"|
                    master.set$state == "Wisconsin"|
                    master.set$state == "Illinois"|
                    master.set$state == "Indiana"|
                    master.set$state == "Michigan"|
                    master.set$state == "Ohio"] <- "Midwest"
master.set$region[master.set$state == "Arkansas" |
                    master.set$state == "Louisiana"|
                    master.set$state == "Mississippi"|
                    master.set$state == "Tennessee"|
                    master.set$state == "Kentucky"|
                    master.set$state == "Alabama"|
                    master.set$state == "Georgia"|
                    master.set$state == "Florida"|
                    master.set$state == "South Carolina"|
                    master.set$state == "North Carolina"|
                    master.set$state == "Virginia"|
                    master.set$state == "West Virginia"|
                    master.set$state == "Maryland"|
                    master.set$state == "Delaware"] <- "Southeast"
master.set$region[master.set$state == "Pennsylvania" |
                    master.set$state == "New Jersey"|
                    master.set$state == "New York"|
                    master.set$state == "Connecticut"|
                    master.set$state == "Rhode Island"|
                    master.set$state == "Massachusetts"|
                    master.set$state == "Vermont"|
                    master.set$state == "New Hampshire"|
                    master.set$state == "Maine"] <- "Northeast"
#There is not D.C. in the data set
##Check to see if there are any missing values in the new region variable
countRegion.na <- sum(is.na(master.set$region))
countRegion.na # No missing values

##Summary statistics
master.set$region <-as.factor(master.set$region)
summary(master.set)


###Exploratory analysis and visualization using usmap
library(usmap)
##Subsetting for last year
set2017 <- master.set[ which(master.set$year==2017), ]
length(set2017$state) #to see if I have one observation per state

#create casuality rate variable
mapset2017 <- set2017[, c(1:2, 136:142)]
mapset2017$vic.per.100000 <- mapset2017$totalvictims/mapset2017$population*100000
summary(mapset2017$vic.per.100000) #Looks like there are plenty of no events states, makes sense

##visualization to verify that mass shootings are rare events
plot_usmap(data = mapset2017, values = "lawtotal") + labs(title = "Total Gun Laws in 2017") +
  scale_fill_continuous(low = "white", high = "blue", name = "Gun Laws") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))

plot_usmap(data = mapset2017, values = "totalvictims") + labs(title = "Mass Shooting Violence in 2017") +
  scale_fill_continuous(low = "white", high = "orange", name = "Total Victims") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))
#Nevada is skewing the data, so I will exclude it
plot_usmap(data = mapset2017, exclude = c("NV"), values = "totalvictims") + labs(title = "Mass Shooting Violence in 2017 (excluding Nevada and Texas)") +
  scale_fill_continuous(low = "white", high = "orange", name = "Total Victims") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))
#Want to see what it would look like with Texas excluded as well
plot_usmap(data = mapset2017, exclude = c("NV", "TX"), values = "totalvictims") + labs(title = "Mass Shooting Violence in 2017 (excluding Nevada and Texas)") +
  scale_fill_continuous(low = "white", high = "orange", name = "Total Victims") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))
#Still see that mass shootings are rare events

##Now visualize based on population rate instead
plot_usmap(data = mapset2017, values = "vic.per.100000") + labs(title = "Mass Shooting Violence in 2017") +
  scale_fill_continuous(low = "white", high = "red", name = "Victims per 100,000") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))
#Nevada skewing the visualization again
plot_usmap(data = mapset2017, exclude = "NV", values = "vic.per.100000") + labs(title = "Mass Shooting Violence in 2017 (excluding Nevada)") +
  scale_fill_continuous(low = "white", high = "red", name = "Victims per 100,000") +
  theme(legend.position = "right", panel.background = element_rect(fill = "skyblue1"))


###Now I want to check to see if the continuous variables are skewed
ggplot(master.set,aes(x=lawtotal))+geom_histogram(binwidth=5,colour="black", fill="blue", alpha= .8)+
  labs(title = "Histogram of Total Laws in State", x= "Number of Laws", y= "Count")
ggplot(master.set,aes(x=population))+geom_histogram(binwidth=5000000,colour="black", fill="azure4", alpha= .8)+
  labs(title = "Histogram of Population in State", x= "Population", y= "Count")
#Adjust population so R can display simple numbers
master.set$pop_millions <- master.set$population/1000000
ggplot(master.set,aes(x=pop_millions))+geom_histogram(binwidth=5,colour="black", fill="skyblue", alpha= .8)+
  labs(title = "Histogram of Population in State", x= "Population (by million)", y= "Count")
ggplot(master.set,aes(x=totalvictims))+geom_histogram(bins = 7, colour="black", fill="firebrick", alpha= .8)+
  labs(title = "Histogram of Mass Shooting Victims in State", x= "Victims", y= "Count")
#Will create a set without the Las Vegas shooting, since the number of vicitms there is a clear outlier
#Since all the continuous variables have skewed distribution, I use the median and IQR to report
#I will also create logged versions of the variables to account for skewing
master.set$log_lawtotal <- log(master.set$lawtotal)
master.set$log_pop <- log(master.set$population)
#cannot lag total victims because there are a lot of 0's, but you can't log 0.

##Check histograms of the logged variables
ggplot(master.set,aes(x=log_lawtotal))+geom_histogram(bins = 8, colour="black", fill="blue", alpha= .8)+
  labs(title = "Histogram of Total Laws in State", x= "Logged Number of Laws", y= "Count")
ggplot(master.set,aes(x=log_pop))+geom_histogram(bins = 9, colour="black", fill="azure4", alpha= .8)+
  labs(title = "Histogram of Population in State", x= "Logged Population", y= "Count")
ggplot(master.set,aes(x=log_tot_victims))+geom_histogram(bins = 8, colour="black", fill="firebrick", alpha= .8)+
  labs(title = "Histogram of Mass Shooting Victims in State", x= "Logged # of Victims", y= "Count")
##They look better, though mass shooting victims is still skewed. See what I should do about this.


###Removing Las Vegas outlier
which.max(master.set$totalvictims)
master.set[224,1:2] #This is the correct observation
no.Vegas.set <- master.set[-224,]
#Histogram of victims in states without Nevada 2017
ggplot(no.Vegas.set,aes(x=totalvictims))+geom_histogram(binwidth = 20, colour="black", fill="firebrick", alpha= .8)+
  labs(title = "Histogram of Mass Shooting Victims in State", subtitle = "Nevada 20017 removed", x= "Victims", y= "Count")
#Skewness is still there because mass shootings are rare events, but can see a little better without Nevada 2017


####More Data Exploration####
###Create spaghetti plot of random 25 states: x-axis is year, y-axis is total victims
set.seed(124)
mysubset.25 <- subset(master.set, state %in% sample(unique(master.set$state), size = 25))
g1 <- ggplot(data = mysubset.25, aes(x = year, y = totalvictims , group = state, colour = state)) + 
  geom_line()
print(g1)

g2 <- g1 + theme_light() + ggtitle("Number of Victims Over Time") + scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020), name = "Year") +
  scale_y_continuous(name = "Total Victims")
print(g2)
###Create loess curve across 25 states and all states
g3 <- ggplot(data = mysubset.25, aes(x = year, y = totalvictims , group = state, colour = state)) + ggtitle("Number of Victims Over Time") + geom_line() + geom_point() + ggtitle("Number of Victims Over Time") +
  scale_x_continuous(breaks = c(2010, 2017, 2020), name = "Year") + 
  scale_y_continuous(name = "Total Victims") + stat_smooth(se = FALSE) + 
  stat_summary(fun.y = "mean", aes(group = 1), geom = "line", size = 1.5)
print(g3)
g4 <- g3 + labs(fill = "States") + theme(legend.background = element_rect(fill = "honeydew2"))
print((g4))
g5 <- g4 + labs(color = "States") + theme(legend.background = element_rect(fill = "honeydew2"))
print(g5)

###Creating spaghetti plot with all 50 states and loess
g6 <- ggplot(data = master.set, aes(x = year, y = totalvictims , group = state, colour = state)) + 
  theme_light() + ggtitle("Number of Victims Over Time") + geom_line() + geom_point() + ggtitle("Number of Victims Over Time") +
  scale_x_continuous(breaks = c(2010, 2017, 2020), name = "Year") + 
  scale_y_continuous(name = "Total Victims") + stat_smooth(se = FALSE) + 
  stat_summary(fun.y = "mean", aes(group = 1), geom = "line", size = 1.5)
print(g6)
g7 <- g6 + labs(color = "States") + theme(legend.background = element_rect(fill = "honeydew2"))
print(g7)
###Facet by Collapsed Region variable
g8 <- g3 + ggtitle("Number of Victims Over Time by Region") + facet_grid(. ~ region, margins = TRUE) + theme(axis.text.x = element_text(angle = 45), legend.position = "none")
print(g8)
g9 <- g6 + ggtitle("Number of Victims Over Time by Region") + facet_grid(. ~ region, margins = TRUE) + theme(axis.text.x = element_text(angle = 45), legend.position = "none")
print(g9)
#We can see the various spikes from region to region. Each of the spikes was one of the well known mass shootings.



#Make sure to use models with the logged variables, models without Vegas since that was such and extreme outlier

####Data Analysis####
#Simple model using time variable as indicator variable and total victims as dependent variable
dynam.1 <- lmer(totalvictims ~ year + (year | state), data = master.set, REML = FALSE)
summary(dynam.1)
#Fixed effects: Each year in the study is associated with a increase of 1.25 victims of mass shootings
#Correlation between random slopes and random intercepts: -1. This means that
#in general, those with lower intercepts have higher slopes.

#1) Adding my variable of interest - number of laws to a linear model
dynam.2 <- lmer(totalvictims ~ year + lawtotal + (year | state), data = master.set, REML = FALSE)
summary(dynam.2)#This one has slightly higher AIC score 3893.2 vs 3895.2, a little concerning but I'll continue
#Fixed effects: Each year passed is associated with an increase of .01 victims of mass shootings.
#Each law passed in a state is associated with .01 increase in total victims of mass shootings.
#Correlation between random slopes and random intercepts: -1. This means that
#in general, those with lower intercepts have higher slopes.

#2) Interaction model taking the year and total number of laws
dynam.3 <- lmer(totalvictims ~ year * lawtotal + (year | state), data = master.set, REML = FALSE)
summary(dynam.3)

#Compare these two models
anova(dynam.2, dynam.3)
print(aictab(list(dynam.2, dynam.3), c("Main Effects", "Interaction")), LL = FALSE)
#Will proceed with the linear Main effects model because it looks like the interaction does not give any added information
#p-value of .7512 in ANOVA analysis

#3) Adding population and region to best of those models
dynam.4 <- lmer(totalvictims ~ year + lawtotal + population + region + (year | state), data = master.set, REML = FALSE)
summary(dynam.4)

###Create table of model fit statistics
mynames <- paste("Model", as.character(1:3), sep = "")
mymodels <- list(dynam.1, dynam.2, dynam.4)
myaicc <- as.data.frame(aictab(cand.set = mymodels, modnames = mynames,
                               sort = FALSE)[,-c(5,7)])
myaicc$Eratio <- max(myaicc$AICcWt) / myaicc$AICcWt
data.frame(Modnames = myaicc$Modnames, round(myaicc[,-1], 2))
#Need to make a table of this in word.
#Model 1 has lowest AICc score, so best. Model 1 delta of 0, so most plausible.
#Model 1 has 70% chance of being the best model among the three competeing models
#Model 2 is close with 26% chance.
#Model 3 has the largest evidence ratio, indicating that it has the highest odds of being the worst.
#Model 1 has the lowest evidence ration, making it have the least odds of being the worst model.

#Now trying only Model 2 and 3
mynames2 <- paste("Model", as.character(2:3), sep = "")
mymodels2 <- list(dynam.2, dynam.4)
myaicc2 <- as.data.frame(aictab(cand.set = mymodels2, modnames = mynames2,
                               sort = FALSE)[,-c(5,7)])
myaicc2$Eratio <- max(myaicc2$AICcWt) / myaicc2$AICcWt
data.frame(Modnames = myaicc2$Modnames, round(myaicc2[,-1], 2))
#Need to make a table of this in word.
#Model 2 has lowest AICc score, so best. Model 1 delta of 0, so most plausible.
#Model 2 has 87% chance of being the best model among the three competeing models
#Model 3 is close with 13% chance.
#Model 3 has the largest evidence ratio, indicating that it has the highest odds of being the worst.
#Model 2 has the lowest evidence ration, making it have the least odds of being the worst model.
#Because of this I'll go with Model 2

#Now I will repeat all of this with the logged variables, to get these variables back to normality
#1) Adding my variable of interest - number of logged laws to a linear model and logging total victims variables
dynam.5 <- lmer(totalvictims ~ year + log_lawtotal + (year | state), data = master.set, REML = FALSE)
summary(dynam.5)
#This model is still has a slighty higher AIC score than the simple model using only the state and year.
#However, the logged version of the model is has a sligtly lower AIC than the non logged law totals. 3894.7 here vs. 3895.2
#Fixed effects: Each year passed is associated with an increase of 1.2 victims of mass shootings.
#Each 1 unit increase in the logged laws passed in a state is associated with 1.3 increase in total victims of mass shootings.
#Correlation between random slopes and random intercepts: -1. This means that
#in general, those with lower intercepts have higher slopes.

#2) Interaction model taking the year and logged total number of laws
dynam.6 <- lmer(totalvictims ~ year * log_lawtotal + (year | state), data = master.set, REML = FALSE)
summary(dynam.6) #Looks as though the interaction effect is not significant.  Compare with previous model to verify.

#Compare these two models
anova(dynam.5, dynam.6)
print(aictab(list(dynam.5, dynam.6), c("Main Effects", "Interaction")), LL = FALSE)
#Will proceed with the linear Main effects model because it looks like the interaction does not give any added information
#p-value of .7231 in ANOVA analysis, indicating that there is no difference in what we learn from the models.  Keep the simple one.

#3) Adding logged population and region to best of those models (Main Effects model)
dynam.7 <- lmer(totalvictims ~ year + log_lawtotal + log_pop + region + (year | state), data = master.set, REML = FALSE)
summary(dynam.7)

###Create table of model fit statistics to compare all 3 of the logged models
mynames3 <- paste("Model", as.character(5:7), sep = "")
mymodels3 <- list(dynam.5, dynam.6, dynam.7)
myaicc3 <- as.data.frame(aictab(cand.set = mymodels3, modnames = mynames3,
                               sort = FALSE)[,-c(5,7)])
myaicc3$Eratio <- max(myaicc3$AICcWt) / myaicc3$AICcWt
data.frame(Modnames = myaicc3$Modnames, round(myaicc3[,-1], 2))
#Need to make a table of this in word.
#Model 5 has lowest AICc score, so best. Model 1 delta of 0, so most plausible.
#Model 5 has 65% chance of being the best model among the three competeing models
#Model 6 is close with 25% chance.
#Model 7 has the largest evidence ratio, indicating that it has the highest odds of being the worst.
#Model 5 has the lowest evidence ration, making it have the least odds of being the worst model.

#Now trying only Model 5 and 7 because model 6 had interaction that was not significant
mynames4 <- paste("Model", as.character(c(5,7)), sep = "")
mymodels4 <- list(dynam.5, dynam.7)
myaicc4 <- as.data.frame(aictab(cand.set = mymodels4, modnames = mynames4,
                                sort = FALSE)[,-c(5,7)])
myaicc4$Eratio <- max(myaicc4$AICcWt) / myaicc4$AICcWt
data.frame(Modnames = myaicc4$Modnames, round(myaicc4[,-1], 2))
#Need to make a table of this in word.
#Model 5 has lowest AICc score, so best. Model 1 delta of 0, so most plausible.
#Model 5 has 73% chance of being the best model among the three competeing models
#Model 6 has 27% chance.
#Model 5 has the lowest evidence ration, making it have the least odds of being the worst model.
#Because of this I'll go with Model 5

#Now compare the non-log with the logged model
mynames5 <- paste("Model", as.character(c(2, 5)), sep = "")
mymodel5 <- list(dynam.2, dynam.5)
myaicc5 <- as.data.frame(aictab(cand.set = mymodel5, modnames = mynames5,
                                sort = FALSE)[,-c(5,7)])
myaicc5$Eratio <- max(myaicc5$AICcWt) / myaicc5$AICcWt
data.frame(Modnames = myaicc5$Modnames, round(myaicc5[,-1], 2))










