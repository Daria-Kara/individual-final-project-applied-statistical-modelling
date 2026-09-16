library(tidyverse)
library(psych)
library(car)

steam <- read_csv("C:/Users/daria/Desktop/Final Project Applied Statistical Modelling/steam.csv")
summary(steam)

glimpse(steam)
sapply(steam,class)

unique(steam$owners)

steam <- steam %>%
  separate(owners, into = c("owners_min", "owners_max"), sep = "-", remove = FALSE) %>%
  mutate(
    owners_min = as.numeric(owners_min),
    owners_max = as.numeric(owners_max),
    owners_estimate = (owners_min + owners_max) / 2
  )


steam %>% select(owners, owners_min, owners_max, owners_estimate) 

colSums(is.na(steam))

sum(duplicated(steam$appid))


steam <- steam %>%
  mutate(primary_genre = str_split(genres, ";", simplify = TRUE)[, 1])

steam %>% select(genres, primary_genre) %>% head(10)
steam %>% filter(str_detect(genres, ";")) %>% select(genres, primary_genre) %>% head(10)


steam <- steam %>%
  mutate(windows_only = platforms == "windows")
steam %>% select(platforms, windows_only) %>% head(10)


steam <- steam %>%
  mutate(total_ratings = positive_ratings + negative_ratings)

steam <- steam %>%
  mutate(review_ratio = positive_ratings / total_ratings)
sum(is.nan(steam$review_ratio))


steam <- steam %>%
  mutate(free_to_play = price == 0)
steam %>% select(price, free_to_play) %>% head(20)


steam <- steam %>%
  select(appid, name, price, owners_estimate, primary_genre, review_ratio, 
         achievements, average_playtime, windows_only, free_to_play)
glimpse(steam)

summary(steam$price)
boxplot(steam$price, main = "Checking Outliers: Price")


summary(steam$owners_estimate)
boxplot(steam$owners_estimate, main = "Checking Outliers: Owners Estimate")


summary(steam$achievements)
boxplot(steam$achievements, main = "Checking Outliers: Achievements")


summary(steam$average_playtime)
boxplot(steam$average_playtime, main = "Checking Outliers: Average Playtime")


summary(steam$review_ratio)
boxplot(steam$review_ratio, main = "Checking Outliers: Review Ratio")


describe(steam)
table(steam$windows_only)
table(steam$free_to_play)


ggplot(steam, aes(x = price)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "blue") +
  labs(x = "Price", y = "Count")


ggplot(steam, aes(x = owners_estimate)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "blue") +
  labs(x = "Owners_estimate", y = "Count")


ggplot(steam, aes(x = achievements)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "blue") +
  labs(x = "Achievements", y = "Count")


ggplot(steam, aes(x = average_playtime)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "blue") +
  labs(x = "Average_playtime", y = "Count")


ggplot(steam, aes(x = review_ratio)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "blue") +
  labs(x = "Review_ratio", y = "Count")


ggplot(steam, aes(x = primary_genre)) +
  geom_bar(fill = "lightblue", color = "blue") +
  labs(x = "Primary_genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

sort(table(steam$primary_genre), decreasing = TRUE)

genre_counts <- steam %>% count(primary_genre)
genres_to_keep <- genre_counts %>% filter(n >= 67) %>% pull(primary_genre)
steam <- steam %>% filter(primary_genre %in% genres_to_keep)
sort(table(steam$primary_genre), decreasing = TRUE)

nrow(steam)


ggplot(steam, aes(x = primary_genre)) +
  geom_bar(fill = "lightblue", color = "blue") +
  labs(x = "Primary_genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))




ggplot(steam, aes(x = price, y = owners_estimate)) +
  geom_point(alpha = 0.3, color = "blue") +
  scale_y_log10() +
  labs(x = "Price", y = "Owners_estimate (log scale)")


ggplot(steam, aes(x = primary_genre, y = review_ratio)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(x = "Primary_genre", y = "Review_ratio") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


ggplot(steam, aes(x = free_to_play, y = review_ratio)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(x = "Free_to_play", y = "Review_ratio")


ggplot(steam, aes(x = achievements, y = average_playtime)) +
  geom_point(alpha = 0.3, color = "blue") +
  scale_y_log10() +
  labs(x = "Achievements", y = "Average_playtime")


ggplot(steam, aes(x = windows_only, y = owners_estimate)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  scale_y_log10() +
  labs(x = "Windows_only", y = "Owners_estimate (log scale)")



numeric_vars <- steam %>% select(price, owners_estimate, achievements, average_playtime, review_ratio)
cor(numeric_vars)

library(corrplot)
corrplot(cor(numeric_vars), method = "color", addCoef.col = "black", type = "upper", diag = FALSE)


#Hypothesis 1
h1_model <- lm(owners_estimate ~ price, data = steam)
summary(h1_model)

par(mfrow = c(2, 2))
plot(h1_model)
par(mfrow = c(1, 1))

set.seed(42)
shapiro.test(sample(resid(h1_model), 5000))

steam <- steam %>%
  mutate(log_owners_estimate = log(owners_estimate))

h1_model_log <- lm(log_owners_estimate ~ price, data = steam)
summary(h1_model_log)

par(mfrow = c(2, 2))
plot(h1_model_log)
par(mfrow = c(1, 1))

set.seed(42)
shapiro.test(sample(resid(h1_model_log), 5000))
summary(h1_model_log)



#Hypothesis 2
h2_model <- aov(review_ratio ~ primary_genre, data = steam)
summary(h2_model)

set.seed(42)
shapiro.test(sample(resid(h2_model), 5000))

leveneTest(review_ratio ~ primary_genre, data = steam)

par(mfrow = c(2, 2))
plot(h2_model)
par(mfrow = c(1, 1))

kruskal.test(review_ratio ~ primary_genre, data = steam)

pairwise.wilcox.test(steam$review_ratio, steam$primary_genre, p.adjust.method = "bonferroni")


#Hypothesis 3
h3_model <- t.test(review_ratio ~ free_to_play, data = steam)
summary(h3_model)

set.seed(42)
steam %>%
  group_by(free_to_play) %>%
  summarise(shapiro_p = shapiro.test(sample(review_ratio, min(5000, n())))$p.value)

leveneTest(review_ratio ~ free_to_play, data = steam)

wilcox.test(review_ratio ~ free_to_play, data = steam)


#Hypothesis 4
h4_model <- lm(average_playtime ~ achievements, data = steam)
summary(h4_model)

par(mfrow = c(2, 2))
plot(h4_model)
par(mfrow = c(1, 1))

set.seed(42)
shapiro.test(sample(resid(h4_model), 5000))

steam <- steam %>%
  mutate(log_avg_playtime = log1p(average_playtime))

h4_model_log <- lm(log_avg_playtime ~ achievements, data = steam)
summary(h4_model_log)

par(mfrow = c(2, 2))
plot(h4_model_log)
par(mfrow = c(1, 1))

set.seed(42)
shapiro.test(sample(resid(h4_model_log), 5000))

summary(h4_model_log)


#Hypothesis 5
h5_model <- t.test(owners_estimate ~ windows_only, data = steam)
summary(h5_model)

set.seed(42)
steam %>%
  group_by(windows_only) %>%
  summarise(shapiro_p = shapiro.test(sample(owners_estimate, min(5000, n())))$p.value)

leveneTest(owners_estimate ~ windows_only, data = steam)

wilcox.test(owners_estimate ~ windows_only, data = steam)

