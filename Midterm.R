#install library
install.packages("MatchIt")
library(MatchIt)
library(dplyr)
library(ggplot2)
library(corrplot)

data <- read.csv("HighNote Data Midterm.csv", header = TRUE)

# Question #1 Descriptive Statistic
overall_stat <- summary(data)
overall_stat

by(data,data$adopter, summary)
    
# Question 2
data$adopter<-as.character(data$adopter)

# 1.Demographics 
ggplot(data, aes(x = adopter, y = age, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Age")

ggplot(data, aes(x = adopter, y = avg_friend_age, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Avg_friend_age")

# 2. Peer Influence
ggplot(data, aes(x = adopter, y = friend_cnt, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Friend_cnt")

ggplot(data, aes(x = adopter, y = friend_country_cnt, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Friend's Country")

ggplot(data, aes(x = adopter, y = subscriber_friend_cnt, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Subscriber's Friend")

## 3. user engagement
ggplot(data, aes(x = adopter, y = songsListened, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Song Listened")

ggplot(data, aes(x = adopter, y = lovedTracks, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Loved Tracks")

ggplot(data, aes(x = adopter, y = posts, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Posts")

ggplot(data, aes(x = adopter, y = shouts, fill = adopter)) + 
  geom_boxplot(outlier.color = "Black", outlier.size = 0.2) + 
  scale_fill_brewer(palette = "Blues") + theme_grey() + ggtitle("Boxplot of Shouts")

# Question 3
data$adopter<-as.numeric(data$adopter)

data<-mutate(data, subscriber_friend=ifelse(subscriber_friend_cnt==0, 0, 1))

# t-test(1) for subscriber_friend and adopter
t.test(data$subscriber_friend,data$adopter)

# create matched treatment and control sample
data_cov <- c("age","male","friend_cnt","avg_friend_age","avg_friend_male",
"friend_country_cnt","songsListened","lovedTracks","posts","playlists","shouts",
"tenure","good_country")

# t-test(2) for subscriber and data_cov's variables
lapply(data_cov, function(v) {
  t.test(data[, v] ~ data$subscriber_friend)
})

# Let's calculate the mean for each covariate by treatment status
data %>%
  group_by(adopter) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

# Propensity score estimation
# 1. test whether there is a significant average treatment effect
test1 <- glm(subscriber_friend~age+male+friend_cnt+avg_friend_age+avg_friend_male+
               friend_country_cnt+songsListened+lovedTracks+posts+playlists+
               shouts+tenure+good_country,family = binomial(), data = data)
summary(test1)

## remove no significant variables - male, posts, playlists, shouts. good_country
test2 <- glm(subscriber_friend~age+friend_cnt+avg_friend_age+avg_friend_male+
               friend_country_cnt+songsListened+lovedTracks+tenure,
             family = binomial(), data = data)

summary(test2)

# 2.Using this model, we can now calculate the propensity score for subscriber_friend==1 or ==0
prs_df <- data.frame(pr_score = predict(test2, type = "response"),
                     subscriber_friend = test2$model$subscriber_friend)
head(prs_df)

score_1<-mean(prs_df$pr_score[prs_df$subscriber_friend==1])
score_1

score_2<-mean(prs_df$pr_score[prs_df$subscriber_friend==0])
score_2

# After estimating the propensity score, it is useful to plot histograms of the estimated propensity scores by treatment status
labs <- paste("Subscription Type:", c("Adopter", "Non-adopter"))
prs_df %>%
  mutate(subscriber_friend = ifelse(subscriber_friend == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~subscriber_friend) +
  xlab("Probability of pay for subscription") +
  theme_bw()

# 3. MatchIt
# remove missing values
data_nomiss <- data %>%  
  select(adopter,subscriber_friend_cnt,subscriber_friend, one_of(data_cov)) %>%
  na.omit()

#run match it
mod_match <- matchit(subscriber_friend ~ age+male+friend_cnt+avg_friend_age+
                       avg_friend_male+friend_country_cnt+songsListened+lovedTracks+
                       posts+playlists+shouts+tenure+good_country,
                     method = "nearest", data = data_nomiss,caliper=0.0003) 

# how successful the matching
summary(mod_match)
plot(mod_match)

# Create a dataframe containing only the matched observations
dta_m <- match.data(mod_match)
dim(dta_m)

# 4. Examining covariate balance in the matched sample
# Difference of means
dta_m %>%
  group_by(subscriber_friend) %>%
  select(one_of(data_cov)) %>%
  summarise_all(funs(mean))

lapply(data_cov, function(v) {
  t.test(dta_m[, v] ~ dta_m$subscriber_friend)
})

# 5. Estimating treatment effects
with(dta_m, t.test(adopter ~ subscriber_friend))

lm_treat1 <- lm(adopter~subscriber_friend, data = dta_m)
summary(lm_treat1)

lm_treat2 <- lm(adopter~age+male+friend_cnt+avg_friend_age+avg_friend_male+
                  friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                  shouts+tenure+good_country+subscriber_friend, data = dta_m)
summary(lm_treat2)


# Question 4
regression_1 <- glm(adopter~age+male+friend_cnt+avg_friend_age+avg_friend_male+
                      friend_country_cnt+songsListened+lovedTracks+posts+playlists+
                      shouts+tenure+good_country+subscriber_friend,
                    family = binomial(), data = dta_m)
summary(regression_1)
exp(coef(regression_1))

# correlation
corr <- subset(dta_m,select=c("age","male","avg_friend_age","friend_country_cnt",
                             "songsListened","lovedTracks","playlists","tenure",
                             "good_country","subscriber_friend"))

correlation_dta_m <- cor(corr, method = c("pearson"))
correlation_dta_m

corrplot(correlation_dta_m, method="number",type="lower", addCoef.col="black",
         tl.col="black", number.cex = .7)

# remove not significant and higher correlation - friend_cnt,avg_friend_male,avg_friend_age,posts, shouts
regression_2 <- glm(adopter~avg_friend_age+male+friend_country_cnt+songsListened+
                      lovedTracks+playlists+tenure+good_country+subscriber_friend,
                    family = binomial(), data = dta_m)
summary(regression_2)
exp(coef(regression_2))

# remove not significant - friend_country_cnt
regression_3 <- glm(adopter~avg_friend_age+male+songsListened+lovedTracks+
                      playlists+tenure+good_country+subscriber_friend,
                    family = binomial(), data = dta_m)
summary(regression_3)
exp(coef(regression_3))



