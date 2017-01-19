library(ggplot2)
library(dplyr)
library(reshape2)
setwd("")

age <- read.csv("KAUFFMAN-KAUFF_AGE.csv")
edu <- read.csv("KAUFFMAN-KAUFF_EDU.csv")
ind <- read.csv("KAUFFMAN-KAUFF_IND.csv")
native <- read.csv("KAUFFMAN-KAUFF_NATIVE.csv")
race <- read.csv("KAUFFMAN-KAUFF_RACE.csv")
region <- read.csv("KAUFFMAN-KAUFF_REGION.csv")
gend <- read.csv("KAUFFMAN-KAUFF_SEX.csv")
veteran <- read.csv("KAUFFMAN-KAUFF_VETERAN.csv")


####age##########
age01 <- melt(age, id=c("Date"))

age01$type <- grepl("Index..", age01$variable, ignore.case = TRUE)

age02 <- subset(age01, type == TRUE)

age02 <- data.frame(age02)

age02$Date <- as.Date(as.character(age02$Date))
age02$variable <- as.character(age02$variable)

age02 <- age02 %>% 
  mutate(variable= replace(variable, variable == "Total.Index..", "Total")) %>%
  mutate(variable= replace(variable, variable == "X20.34.Index..", "20-34yrs")) %>%
  mutate(variable= replace(variable, variable == "X35.44.Index..", "35-44yrs")) %>%
  mutate(variable= replace(variable, variable == "X45.54.Index..", "45-54yrs")) %>%
  mutate(variable= replace(variable, variable == "X55.64.Index..", "55-64yrs"))

g.age <- ggplot(age02, aes(Date, value))
p.age <- g.age + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Age") 

p.age

dev.copy(png, file = "age.png", width=800)  
dev.off()


####edu####

str(edu)
edu01 <- melt(edu, id=c("Date"))

edu01

edu01$type <- grepl("Index..", edu01$variable, ignore.case = TRUE)

edu02 <- subset(edu01, type == TRUE)

edu02 <- data.frame(edu02)

edu02$Date <- as.Date(as.character(edu02$Date))
edu02$variable <- as.character(edu02$variable)


edu02
edu02 <- edu02 %>% 
  mutate(variable= replace(variable, variable == "Total.Index..", "Total")) %>%
  mutate(variable= replace(variable, variable == "Less.Than.High.School.Index..", "< High School")) %>%
  mutate(variable= replace(variable, variable == "High.School.Graduate.Index..", "High School Grad")) %>%
  mutate(variable= replace(variable, variable == "Some.College.Index..", "Some College")) %>%
  mutate(variable= replace(variable, variable == "College.Graduate.Index..", "College Graduate"))

g.edu <- ggplot(edu02, aes(Date, value))
p.edu <- g.edu + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Education") 

p.edu

dev.copy(png, file = "edu.png", width=800)  
dev.off()

###ind##########

ind01 <- melt(ind, id=c("Year"))

ind01$type <- grepl("Index..", ind01$variable, ignore.case = TRUE)

ind02 <- subset(ind01, type == TRUE)

ind02 <- data.frame(ind02)

ind02$Year <- as.Date(as.character(ind02$Year))
ind02$variable <- as.character(ind02$variable)

table(ind02$variable)

ind02 <- ind02 %>% 
  mutate(variable= replace(variable, variable == "Construction.Index..", "Construction")) %>%
  mutate(variable= replace(variable, variable == "Manufacturing.Index..", "Manufacturing")) %>%
  mutate(variable= replace(variable, variable == "Other.Index..", "Other")) %>%
  mutate(variable= replace(variable, variable == "Services.Index..", "Services")) %>%
  mutate(variable= replace(variable, variable == "Trade.Index..", "Trade"))

g.ind <- ggplot(ind02, aes(Year, value))
p.ind <- g.ind + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Year, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Industry") 

p.ind

dev.copy(png, file = "ind.png", width=800)  
dev.off()

###native##########

str(native)
native01 <- melt(native, id=c("Date"))

native01$type <- grepl("Index..", native01$variable, ignore.case = TRUE)

native02 <- subset(native01, type == TRUE)

native02 <- data.frame(native02)

native02$Date <- as.Date(as.character(native02$Date))
native02$variable <- as.character(native02$variable)

table(native02$variable)

native02 <- native02 %>% 
  mutate(variable= replace(variable, variable == "Immigrant.Index..", "Immigrant")) %>%
  mutate(variable= replace(variable, variable == "Native.Born.Index..", "Native Born")) %>%
  mutate(variable= replace(variable, variable == "Total.Index..", "Total")) 

n.ind <- ggplot(native02, aes(Date, value))
p.ind <- n.ind + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Immigrant vs Native") 

p.ind

dev.copy(png, file = "native.png", width=800)  
dev.off()

###race##########

str(race)
race01 <- melt(race, id=c("Date"))

race01$type <- grepl("Index..", race01$variable, ignore.case = TRUE)

race02 <- subset(race01, type == TRUE)

race02 <- data.frame(race02)

race02$Date <- as.Date(as.character(race02$Date))
race02$variable <- as.character(race02$variable)

table(race02$variable)

race02 <- race02 %>% 
  mutate(variable= replace(variable, variable == "Asian.Index..", "Asian")) %>%
  mutate(variable= replace(variable, variable == "Black.Index..", "Black")) %>%
  mutate(variable= replace(variable, variable == "Latino.Index..", "Latino")) %>%
  mutate(variable= replace(variable, variable == "Total.Index..", "Total")) %>%
  mutate(variable= replace(variable, variable == "White.Index..", "White"))

n.race <- ggplot(race02, aes(Date, value))
p.race <- n.race + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Race") 

p.race

dev.copy(png, file = "native.png", width=800)  
dev.off()

###region##########

str(region)
region01 <- melt(region, id=c("Year"))

region01$type <- grepl("Index..", region01$variable, ignore.case = TRUE)

region02 <- subset(region01, type == TRUE)

region02 <- data.frame(region02)

region02$Year <- as.Date(as.character(region02$Year))
region02$variable <- as.character(region02$variable)

table(region02$variable)

region02 <- region02 %>% 
  mutate(variable= replace(variable, variable == "Midwest.Index..", "Midwest")) %>%
  mutate(variable= replace(variable, variable == "Northeast.Index..", "Northeast")) %>%
  mutate(variable= replace(variable, variable == "South.Index..", "South")) %>%
  mutate(variable= replace(variable, variable == "Total.Index..", "Total")) %>%
  mutate(variable= replace(variable, variable == "West.Index..", "West"))

n.region <- ggplot(region02, aes(Year, value))
p.region <- n.region + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Year, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Race") 

p.region

dev.copy(png, file = "region.png", width=800)  
dev.off()

###gend##########

str(gend)
gend01 <- melt(gend, id=c("Date"))

gend01$type <- grepl("Index..", gend01$variable, ignore.case = TRUE)

gend02 <- subset(gend01, type == TRUE)

gend02 <- data.frame(gend02)

gend02$Date <- as.Date(as.character(gend02$Date))
gend02$variable <- as.character(gend02$variable)

table(gend02$variable)

gend02 <- gend02 %>% 
  mutate(variable= replace(variable, variable == "Female.Index..", "Female")) %>%
  mutate(variable= replace(variable, variable == "Male.Index..", "Male")) %>%
  mutate(variable= replace(variable, variable == "Total.Index..", "Total"))


n.gend <- ggplot(gend02, aes(Date, value))
p.gend <- n.gend + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Gender") 

p.gend

dev.copy(png, file = "gender.png", width=800)  
dev.off()


###veteran##########

str(veteran)
veteran01 <- melt(veteran, id=c("Date"))

veteran01$type <- grepl("Index..", veteran01$variable, ignore.case = TRUE)

veteran02 <- subset(veteran01, type == TRUE)

veteran02 <- data.frame(veteran02)

veteran02$Date <- as.Date(as.character(veteran02$Date))
veteran02$variable <- as.character(veteran02$variable)

table(veteran02$variable)

veteran02 <- veteran02 %>% 
  mutate(variable= replace(variable, variable == "Non.Veteran.Index..", "Non-Veteran")) %>%
  mutate(variable= replace(variable, variable == "Veteran.Index..", "Veteran")) %>%
  mutate(variable= replace(variable, variable == "Total.Index..", "Total"))


n.veteran <- ggplot(veteran02, aes(Date, value))
p.veteran <- n.veteran + geom_point(size=2, aes(color=variable)) + facet_grid(. ~variable) + 
  geom_smooth(aes(Date, value), method = "lm", color="brown") + ylab("Index") + 
  theme(legend.position = "none") + labs(title = "Gender") 

p.veteran

dev.copy(png, file = "veteran.png", width=800)  
dev.off()

