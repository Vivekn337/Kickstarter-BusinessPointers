install.packages("ggplot2")
library("dplyr")
library('ggplot2')
kickstart <- read.csv("DATA.csv")

subset <- kickstart %>% select(main_category,category,state)
distinct(kickstart,state)

subset$dummy <- ifelse((subset$state=='failed'),1,
                       ifelse((subset$state=='canceled'),2,
                              ifelse((subset$state=='successful'),3,
                                     ifelse((subset$state=='live'),4,5))))
agg1 <- subset %>% 
select(main_category,state)%>%
group_by(main_category,state)%>%
summarise(count=n())

agg2 <-  subset %>% 
  select(category,state)%>%
  group_by(category,state)%>%
  summarise(count=n())

agg1 %>% filter(state=='canceled')

ggplot(data=(agg1 %>% filter(state=='canceled')), aes( x=count,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of canceled Projects") + ylab("Main category")

ggplot(data=(agg1 %>% filter(state=='failed')), aes( x=count,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of failed Projects") + ylab("Main category")

ggplot(data=(agg1 %>% filter(state=='successful')), aes( x=count,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of successful Projects") + ylab("Main category")

ggplot(data=(agg1 %>% filter(state=='live')), aes( x=count,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of live Projects") + ylab("Main category")

ggplot(data=(agg1 %>% filter(state=='suspended')), aes( x=count,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of suspended Projects") + ylab("Main category")

subset2 <- kickstart %>% select(deadline,launched,state)

subset2$deadline <- as.POSIXct(subset2$deadline,format="%m/%d/%Y",tz=Sys.timezone())
class(subset2$deadline)

subset2$launched <- as.POSIXct(subset2$launched,format="%m/%d/%Y %H:%M",tz=Sys.timezone())


subset2$time_difference <- difftime(subset2$deadline, subset2$launched, units = c("days"))

agg3 <-  subset2 %>% 
  select(deadline,launched,state,time_difference)%>%
  group_by(state)%>%
  summarise(total=mean(time_difference))

ggplot(data=agg3, aes(x=state, y=total , group=1)) +
  geom_line(linetype='dashed', color="red")+ xlab("state") + ylab("Average number of days")+
  geom_point()

subset3 <- kickstart %>% select(main_category,backers)
agg4 <-  subset3 %>% 
  select(main_category,backers)%>%
  group_by(main_category)%>%
  summarise(no_backers=n())

ggplot(data=agg4, aes( x=no_backers,y=main_category)) +
  geom_bar( stat="identity",fill='blue') +
  xlab("No of canceled Projects") + ylab("Main category")

distinct(kickstart,country)

subset4 <- kickstart %>% select(country,state)
agg5 <- subset4 %>% 
  select(country,state) %>%
  group_by(state,country) %>%
  summarise(no_of_country=n())
  
agg5_updated <-  filter(agg5,state=="successful")


ggplot(agg5_updated, aes(x="", y=no_of_country ,fill=country)) +
  geom_bar(stat="identity", width=2)+
  coord_polar("y", start=0)

subset5 <- kickstart %>% select(main_category,usd_goal_real)
agg6 <- subset5 %>% 
  select(main_category,usd_goal_real) %>%
  group_by(main_category) %>%
  summarise(Average_cost=mean(usd_goal_real))

ggplot(data=agg6, aes( y=main_category,x=Average_cost)) +
  geom_bar( stat="identity",fill='red') +
  xlab("Average project cost in each category") + ylab("Main category")

subset5 <- kickstart %>% 
  filter(state=='successful') %>%
  select(main_category,usd_goal_real)
agg6 <- subset5 %>% 
  select(main_category,usd_goal_real) %>%
  group_by(main_category) %>%
  summarise(Average_cost=mean(usd_goal_real))

ggplot(data=agg6, aes( y=main_category,x=Average_cost)) +
  geom_bar( stat="identity",fill='red') +
  xlab("Average project cost in each category") + ylab("Main category")
#######Pair wise test#######3
cat_test <- kickstart %>%
  filter(state=="successful") %>%
  select(main_category)
myfreq <-  as.data.frame(table(cat_test))
comb <- combn(myfreq$cat_test,2)  
comb2 <- data.frame(t(combn(myfreq$Freq,2)))
comb2$pairN <-comb2$X1 +comb2$X2 
comb2$sig <- apply(comb2,1, function(x) binom.test(x[1],x[3])$p.value)
npair <- dim(comb2)[1]
comb2$adjsig <- comb2$sig*npair
comb <- as.data.frame(t(comb))
result <- cbind(comb,comb2)
last <-2 *npair
result[-c(npair+1:last),]
result
####2#####
tech <- kickstart %>%
  filter(main_category=='Technology') %>%
  select(usd_goal_real)
head(tech$usd_goal_real)

t.test(tech,mu = 25000,alternative = "greater")
######9######
platform <- kickstart %>%
  filter(state=="successful" | state=="failed" ) %>%
  select(usd_goal_real,usd_pledged_real)

var.test(platform$usd_goal_real, platform$usd_pledged_real, alternative = "two.sided")
t.test(platform$usd_goal_real, platform$usd_pledged_real, alternative = "greater", var.equal = FALSE)

######1#######
prob_suc <- kickstart %>%
  filter(state=='successful') %>%
  select(main_category,state) %>%
  group_by(main_category) %>%
  summarise(count_suc = n())
prob_suc1 <- kickstart %>%
  select(main_category,state) %>%
  group_by(main_category) %>%
  summarise(count_t = n())
dummy_prob <- cbind(prob_suc,prob_suc1)  
dummy_prob$prob <- dummy_prob$count_suc/dummy_prob$count_t #probability of success in dance is 0.62
head(dummy_prob2)
dummy_prob2 <- as.data.frame(dummy_prob$prob)
dummy_prob2 <- cbind(dummy_prob2,prob_suc$main_category)
ggplot(data=dummy_prob2, aes( x=dummy_prob$prob,y=prob_suc$main_category)) +
  geom_bar( stat="identity",fill='green') +
  xlab("Probability of success") + ylab("Main category")
####2####(test for sample size and stats previous material)
prop.test(2338,3767,p=0.6,alternative = "greater",correct = TRUE) #dance probability of success is more
####3####
prop.test(6434,32566,p=0.2,alternative = "less",correct = TRUE) #technology probability of success is less

agg3 <-  subset2 %>% 
  select(deadline,launched,state,time_difference)%>%
  group_by(state)%>%
  summarise(total=mean(time_difference))

####4####
time <- subset2 %>%
  filter(state=='successful')
t.test(time$time_difference,mu = 35,alternative = "greater")
####5######
install.packages("lubridate")
library("lubridate")
lucky_month <- month(subset2$launched)
mean(lucky_month)
t.test(lucky_month,mu=6,alternative = "greater")

######6######
backers <- kickstart %>%
  select(main_category,backers) %>%
  group_by(main_category) %>%
  summarise(average =mean(backers))
max(backers$average)#Games is the sector with more backers average for each project.

backer_data <-kickstart %>%
  filter(main_category=='Games') %>%
  select(backers)
mean(backer_data$backers)

t.test(backer_data,mu=321,alternative = "greater") 

######7#######

sample_table <- table(kickstart$state,kickstart$usd_pledged_real,kickstart$usd_pledged_real)

amt_suc <- kickstart %>%
  filter(state=='successful' | state=='failed') %>%
  select(state,usd_pledged_real,usd_goal_real)

amt_suc <-as.data.frame(amt_suc)

group_by(amt_suc, state) %>%
  summarise(
    count = n(),
    mean = mean(usd_pledged_real, na.rm = TRUE),
    sd = sd(usd_pledged_real, na.rm = TRUE)
  )

hist(amt_suc$usd_pledged_real[amt_suc$state == "successful"])
hist(amt_suc$usd_pledged_real[amt_suc$state == "failed"])


var.test(amt_suc$usd_pledged_real,amt_suc$usd_goal_real)
t.test(amt_suc$usd_goal_real,amt_suc$usd_pledged_real,,var.equal=FALSE,alternative="two.sided")
#mean pledge of amount in failed less than successful

####8####
logit<- kickstart %>%
  filter(state=='successful' | state=='failed') %>%
  select(state,usd_goal_real)

logit$binom <- ifelse((logit$state=='successful'),1,0)
mylogit <- glm(binom ~ usd_goal_real , data = logit, family = "binomial")
summary(mylogit)
