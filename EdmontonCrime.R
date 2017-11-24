#Import required libraries

setwd("D:\\ALLWORK\\PROJECTS\\Essays\\Crime in Edmonton\\")

#Source: https://data.edmonton.ca/dataset/EPS-Neighbourhood-Criminal-Incidents/xthe-mnvi/data
f1 <- read.csv("crime edmonton.csv")

#source: open data Edmonton (municipalcensus data)
f2 <- read.csv("population_2009.csv")
f3 <- read.csv("population_2012.csv")
f4 <- read.csv("population_2014.csv")
f5 <- read.csv("population_2016.csv")

#aggregate the populations to neighbourhoods
f2$pop <- f2$FEMALE + f2$MALE
pop_2009 <-aggregate(f2$pop, by=list(f2$NEIGHBOURHOOD), FUN=sum)
names(pop_2009)[1] <- "NEIGHBOURHOOD"
names(pop_2009)[2] <- "pop2009"

f3$pop <- as.numeric(as.character(f3$MALE)) + as.numeric(as.character(f3$FEMALE))#imported as factor, this turns into number
pop_2012 <-aggregate(f3$pop, by=list(f3$NEIGHBOURHOOD_NAME), FUN=sum)
names(pop_2012)[1] <- "NEIGHBOURHOOD"
names(pop_2012)[2] <- "pop2012"

f4$pop <- as.numeric(as.character(f4$MALE)) + as.numeric(as.character(f4$FEMALE))
pop_2014 <-aggregate(f4$pop, by=list(f4$NEIGHBOURHOOD_NAME), FUN=sum)
names(pop_2014)[1] <- "NEIGHBOURHOOD"
names(pop_2014)[2] <- "pop2014"

f5$pop <- as.numeric(as.character(f5$Male)) + as.numeric(as.character(f5$Female)) + as.numeric(as.character(f5$Other))
pop_2016 <-aggregate(f5$pop, by=list(f5$Neighbourhood.Name), FUN=sum)
names(pop_2016)[1] <- "NEIGHBOURHOOD"
names(pop_2016)[2] <- "pop2016"

#Merge the population data together
pops <- merge(pop_2016, pop_2014, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, pop_2012, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, pop_2009, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)

#linear interpolations of missing years
pops$pop2010 <- (pops$pop2012 - pops$pop2009)/3 + pops$pop2009
pops$pop2011 <- (pops$pop2012 - pops$pop2009)/3 + pops$pop2010
pops$pop2013 <- (pops$pop2014 - pops$pop2012)/2 + pops$pop2012
pops$pop2015 <- (pops$pop2016 - pops$pop2014)/2 + pops$pop2014

#aggregate by type and clean up each file to prepare for final merge with population data
library(reshape2)

crimedata <-aggregate(f1$X..Incidents, by=list(f1$Neighbourhood.Description..Occurrence., f1$Incident.Reported.Year, f1$UCR.Violation.Type.Group..Incident.), FUN=sum)

#assaults
assaults <- crimedata[crimedata$Group.3 == "Assault",]
names(assaults)[1] <- "NEIGHBOURHOOD"
names(assaults)[2] <- "YEAR"
names(assaults)[4] <- "Assaults"
assaults <- assaults[c(-3)]
assaults <- reshape(assaults, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#b and e
be <- crimedata[crimedata$Group.3 == "Break and Enter",]
names(be)[1] <- "NEIGHBOURHOOD"
names(be)[2] <- "YEAR"
names(be)[4] <- "B and E"
be <- be[c(-3)]
be <- reshape(be, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#robbery
robbery <- crimedata[crimedata$Group.3 == "Robbery",]
names(robbery)[1] <- "NEIGHBOURHOOD"
names(robbery)[2] <- "YEAR"
names(robbery)[4] <- "Robbery"
robbery <- robbery[c(-3)]
robbery <- reshape(robbery, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#Theft From Vehicle
tfv <- crimedata[crimedata$Group.3 == "Theft From Vehicle",]
names(tfv)[1] <- "NEIGHBOURHOOD"
names(tfv)[2] <- "YEAR"
names(tfv)[4] <- "tfv"
tfv <- tfv[c(-3)]
tfv <- reshape(tfv, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#Theft of Vehicle
tov <- crimedata[crimedata$Group.3 == "Theft Of Vehicle",]
names(tov)[1] <- "NEIGHBOURHOOD"
names(tov)[2] <- "YEAR"
names(tov)[4] <- "tov"
tov <- tov[c(-3)]
tov <- reshape(tov, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#Theft over 5k
t5k <- crimedata[crimedata$Group.3 == "Theft Over $5000",]
names(t5k)[1] <- "NEIGHBOURHOOD"
names(t5k)[2] <- "YEAR"
names(t5k)[4] <- "t5k"
t5k <- t5k[c(-3)]
t5k <- reshape(t5k, idvar = "NEIGHBOURHOOD", timevar = "YEAR", direction = "wide")

#more the data together into one massive file in steps
pops <- merge(pops, assaults, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, be, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, robbery, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, tfv, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, tov, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)
pops <- merge(pops, t5k, by.x="NEIGHBOURHOOD",by.y="NEIGHBOURHOOD", all.x=T)

#-------------------------------------------------------
# Now we will import a shapefile of Edmonton neighbourhoods
#-------------------------------------------------------

library(rgdal)

#import the shapefile
shapefile <- readOGR("geo_export_fc3e872e-1f60-4d7b-889c-0c84bcce12dc.shp")
shapefile <- spTransform(shapefile,CRS("+proj=longlat +datum=WGS84"))
plot(shapefile)

#standardize the names to uppercase
shapefile@data$name <- toupper(shapefile@data$name)

#I will reduce the data file to one crime type to simplify things for now
#theft from car in 2016

thenames <- c("NEIGHBOURHOOD", "pop2016", "Assaults.2016")
df <- pops[thenames]

#delete records with 0 population
df <- df[df$pop2016>0,]

#delete records with NA
df <- df[!is.na(df$pop2016),]

#now lets merge these data to the shapefile
shapefile@data <- merge(df, shapefile@data, by.x="NEIGHBOURHOOD",by.y="name", all.y=T)

#create a simple identifier for the adjacency steps below
shapefile@data$ID <- c(1:394)

#-------------------------------------------------------
# Now we want to create a spatial lag variable where adjacent neighbourhood
# populations are put into a new variable for each neighbourhood
#-------------------------------------------------------

library(spdep)
library(data.table)

#create an adjacency matrix as a list
adjacency <- poly2nb(shapefile,queen=F,row.names=shapefile@data$ID)

#now nest the adjacency matrix list in two other functions to get it into the desired format
#it turns the neighbour list into a matrix, and then into a dataframe of nxn size (big!)
adj_mat<-data.frame(nb2mat(poly2nb(shapefile,queen=F,row.names=shapefile@data$ID),style="B",zero.policy=T))#library(spdep)

#some reformating, and renaming the first column
adj_mat <- setDT(adj_mat, keep.rownames = TRUE)[]
colnames(adj_mat)[1] <- "ID"

#Now reshape the matrix into long format
adj_mat <- melt(adj_mat, id.vars=c("ID"))
colnames(adj_mat)[1] <- "From"
colnames(adj_mat)[2] <- "To"

#reformat edge data into proper form
adj_mat$From <- as.numeric(adj_mat$From)
adj_mat$To <- as.numeric(gsub("X",'',adj_mat$To))
adj_mat <- as.data.frame(adj_mat[which(adj_mat$To != adj_mat$From & adj_mat$value == 1),])
adj_mat <- adj_mat[c("From","To")]

#create the neighbour file with total neighbour population
neighbours <- merge(adj_mat, shapefile@data, by.x="From", by.y="ID", all.y=T)
n2 <- neighbours[,c("To","pop2016")]
n2 <-aggregate(n2$pop2016, by=list(n2$To), FUN=sum, na.rm=T)
colnames(n2)[1] <- "ID"
colnames(n2)[2] <- "Npop"

#merge back to the shapefile
shapefile@data <- merge(n2, shapefile@data, by.x="ID",by.y="ID", all.y=T)

#--------------------------------------------------
#Now we can analyze the data!
#--------------------------------------------------

#Create a df for analysis
#delete areas with NA pop or NA crime

df <- shapefile@data[,c("ID","pop2016","Assaults.2016","Npop")]
df <- df[!is.na(df$pop2016),]
df <- df[!is.na(df$Assaults.2016),]
df$lognpop <- log(df$pop2016)
df$Npop10k <- df$Npop/10000
df$lograte <- log(df$Assaults.2016/df$pop2016)

out <- glm.nb(Assaults.2016 ~ Npop10k + offset(lognpop), data=df)
summary(out)

est <- cbind(Estimate = coef(out), confint(out))
exp(est)

#For every 10,000 people living in neighbouring areas, we expect a 1.6 times higher risk of assault
#(confidence interval 1.20, 2.23)

#generate the model predictions
df$p <- predict(out, df, type = "response")

#now merge the data to to the shapefile
shapefile@data <- merge(shapefile@data, df[,c("ID", "p")], by.x="ID",by.y="ID", all.x=T)
spplot(shapefile, "p", main = "Predicted Assault rate")

#crude assault rate
spplot(shapefile, "Assaults.2016", main = "Number of Assaults")

sum(df)

#------------------
sum(pops$Assaults.2016, na.rm=T)/sum(pops$pop2016, na.rm=T)

