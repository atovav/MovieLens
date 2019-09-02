#Complete code for MovieLens Dataset 10M 
#EDX and Valitadion data saved, load save data
load("~/MovieLens/test.RData")
#write_csv2(validation, "valid")  save data into csv
#write_csv2(edx, "train")

#load libraries
library(tidyverse)
library(caret)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(Matrix)
library(irlba)
library(ggthemes)

#Add RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#See structure of data, summary and see distinct 
str(edx)
summary(edx)
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#Data Exploration
edx$timestamp <- as_datetime(edx$timestamp)
edx %>% group_by(rating) %>%
  summarize(n = n()) %>%
  ggplot(aes(rating, n)) +
  geom_col()
p1 <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")
p2 <- edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")
grid.arrange(p1, p2, nrow = 1)

edx %>% count(title, sort = TRUE) %>%
  head(20) %>%
  mutate(title = reorder(title, n)) %>%
  ggplot(aes(x = title, y = n, fill = title)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_minimal()
edx %>% count(userId, sort = TRUE) %>%
  head(20) %>%
  mutate(User = reorder(userId, n)) %>%
  ggplot(aes(x = User, y = n, fill = User)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_color_brewer(palette = "Spectral")
edx %>% group_by(movieId) %>%
  summarize(n = n()) %>%
  filter(n < 10) %>%
  arrange(n) %>%
  head(10)
rm(p1,p2)
# Year movie effects plot
titles <- edx %>% group_by(movieId, title) %>%
  select(movieId, title, genres, rating)
titles$Year <- str_extract(titles$title, "\\((\\d{4})\\)")
titles$Year <- str_remove(titles$Year,"\\(")
titles$Year <- str_remove(titles$Year,"\\)")
titles$Year <- as.Date(titles$Year, format = '%Y')
titles %>% group_by(Year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(Year,rating)) +
  geom_point() +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90))
rm(titles)
#user effects plot
users <- edx[ ,!(names(edx) %in% c("genres", "title"))]
users$Year <- year(users$timestamp)
users %>% group_by(Year) %>%
  summarize(rating = mean(rating), n = n()) %>%
  ggplot(aes(Year,rating, size = sqrt(n))) +
  geom_point()+
  theme_clean()
rm(users)

#Spliting EDX into training and valid sets for model creation
set.seed(2019, sample.kind="Rounding")
#val_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-val_index,]
temp <- edx[val_index,]
# Make sure userId and movieId are also in train set
valid_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
# Add rows removed from valid set back into train set
removed <- anti_join(temp, valid_set)
train_set <- rbind(train_set, removed)
rm(edx, removed, temp, val_index)
#saving sets
write_csv2(train_set, "train_set")
write_csv2(valid_set, "valid_set")

#removing edx and validation until final model and predictions
rm(edx,validation)

#modifying train and valid set ym is the title year and yu is the year of user
train_set$ym <- str_extract(train_set$title, "\\((\\d{4})\\)")
train_set$ym <- str_remove(train_set$ym,"\\(")
train_set$ym <- str_remove(train_set$ym,"\\)")
train_set$timestamp <- as_datetime(train_set$timestamp)
train_set$yeus <- year(train_set$timestamp)
valid_set$timestamp <- as_datetime(valid_set$timestamp)
valid_set$yeus <- year(valid_set$timestamp)
valid_set$ym <- str_extract(valid_set$title, "\\((\\d{4})\\)")
valid_set$ym <- str_remove(valid_set$ym,"\\(")
valid_set$ym <- str_remove(valid_set$ym,"\\)")

# Regularization, findind optimal lambda given:
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
# movie mean
  mu <- mean(train_set$rating)
# movie effects  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
# user effects  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
# year of title effects
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(ym) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
# year user effects  
  b_yu <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "ym") %>%
    group_by(yeus) %>%
    summarize(b_yu = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  
  predicted_ratings <- 
    valid_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "ym") %>%
    left_join(b_yu, by = "yeus") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_yu) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, valid_set$rating))
})
qplot(lambdas, rmses) 
lambda <- lambdas[which.min(rmses)]
lambda # optimal lambda 5

# SVD
# changing from data frame to sparse matrix using matrix library
x <- train_set[ , c("userId","movieId","rating")]
x$rating <- x$rating - mu   #removing the mean as it will be added later
rm(train_set)
mat <- sparseMatrix(i = x$userId,j = x$movieId, x = x$rating)
#image(mat, main = "Raw Movielense Data") #this will take a lot of time
rm(x)
gc()
#  Finding the optimal number of svd 
SVD_5=irlba(mat,nu=5,nv=5)
SVD_50=irlba(mat,nu=50,nv=50)
SVD_75=irlba(mat,nu=75,nv=75)
SVD_100=irlba(mat,nu=100,nv=100)
plot(SVD_5$d, main="Largest singular values (r=5)") #see latent factors
rm(mat) #keep memory in check
gc() #keep memory in check
# You need to separate and predict only on the values that you need, if you 
# dot product on the entire matix you will get a memory error
u_sigma5 <- SVD_5$u %*% diag(SVD_5$d)
vt5 <- t(SVD_5$v)
u_sigma50 <- SVD_50$u %*% diag(SVD_50$d)
vt50 <- t(SVD_50$v)
u_sigma75 <- SVD_75$u %*% diag(SVD_75$d)
vt75 <- t(SVD_75$v)
u_sigma100 <- SVD_100$u %*% diag(SVD_100$d)
vt100 <- t(SVD_100$v)
rm(SVD_5,SVD_50,SVD_100,SVD_75)
gc()
#testing on valid set
valid_set <- read.csv2("~/MovieLens/valid_set", stringsAsFactors=FALSE)
v <- valid_set[ ,c(1,2)]
id <- seq(1,length(valid_set[,1]),1)
# for 5 latent
calc <- function(i){
  user <- v[i,1]
  movie <- v[i,2]
  pred <- u_sigma5[user, ] %*% vt5[ ,movie]
} 
o5 <- lapply(id,calc)
df5 <- data.frame(matrix(unlist(o5), nrow=length(o5), byrow=T))
df5$rat <- df5$matrix.unlist.o5...nrow...length.o5...byrow...T. + mu
rm(o5,u_sigma5,vt5)
#for 50 latent
calc <- function(i){
  user <- v[i,1]
  movie <- v[i,2]
  pred <- u_sigma50[user, ] %*% vt50[ ,movie]
} 
o50 <- lapply(id,calc)
df50 <- data.frame(matrix(unlist(o50), nrow=length(o50), byrow=T))
df50$rat <- df50$matrix.unlist.o50...nrow...length.o50...byrow...T. + mu
rm(o50,u_sigma50,vt50)
#for 75 latent
calc <- function(i){
  user <- v[i,1]
  movie <- v[i,2]
  pred <- u_sigma75[user, ] %*% vt75[ ,movie]
} 
o75 <- lapply(id,calc)
df75 <- data.frame(matrix(unlist(o75), nrow=length(o75), byrow=T))
df75$rat <- df75$matrix.unlist.o75...nrow...length.o75...byrow...T. + mu
rm(o75,u_sigma75,vt75)
gc()
#for 100 latent
calc <- function(i){
  user <- v[i,1]
  movie <- v[i,2]
  pred <- u_sigma100[user, ] %*% vt100[ ,movie]
} 
o100 <- lapply(id,calc)
df100 <- data.frame(matrix(unlist(o100), nrow=length(o100), byrow=T))
df100$rat <- df100$matrix.unlist.o100...nrow...length.o100...byrow...T. + mu
rm(o100,u_sigma100,vt100)
#calculate rmse
rmse5 <- RMSE(valid_set$rating,df5$rat)
rmse50 <- RMSE(valid_set$rating,df50$rat)
rmse75 <- RMSE(valid_set$rating,df75$rat)
rmse100 <- RMSE(valid_set$rating,df100$rat)
rm(df5,df50,df100,df75)
kable(RMSE <- data.frame('r'=c(5,50,75,100),'RMSE'=c(rmse5,rmse50,rmse75 ,rmse100)))
ggplot(RMSE,aes(r,RMSE))+
  geom_line(col='slateblue2',size=1)+geom_point(col='slateblue2',size=2)+labs(x="Number of Singular Values",y="RMSE")
#Best is 50



