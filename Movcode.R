library(tidyverse)
library(Matrix)
library(irlba)

edx$ym <- str_extract(edx$title, "\\((\\d{4})\\)")
edx$ym <- str_remove(edx$ym,"\\(")
edx$ym <- str_remove(edx$ym,"\\)")
edx$timestamp <- as_datetime(edx$timestamp)
edx$yeus <- year(edx$timestamp)
validation$timestamp <- as_datetime(validation$timestamp)
validation$yeus <- year(validation$timestamp)
validation$ym <- str_extract(validation$title, "\\((\\d{4})\\)")
validation$ym <- str_remove(validation$ym,"\\(")
validation$ym <- str_remove(validation$ym,"\\)")
mu <- mean(edx$rating)
lambda <- 5

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_y <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(ym) %>%
  summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_yu <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "ym") %>%
  group_by(yeus) %>%
  summarize(b_yu = sum(rating - b_i - b_u - b_y - mu)/(n()+lambda))

x <- edx[ , c("userId","movieId","rating")]
x$rating <- x$rating - mu
rm(edx)
gc()
mat <- sparseMatrix(i = x$userId,j = x$movieId, x = x$rating)
rm(x)
gc()
SVD_50=irlba(mat,nu=50,nv=50) #better
rm(mat)
gc()
u_sigma <- SVD_50$u %*% diag(SVD_50$d)
vt <- t(SVD_50$v)
rm(SVD_50)
gc()

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "ym") %>%
  left_join(b_yu, by = "yeus")

v <- validation[ ,c(1,2)]
id <- seq(1,length(validation[,1]),1)

calc <- function(i){
  user <- v[i,1]
  movie <- v[i,2]
  pred <- u_sigma[user, ] %*% vt[ ,movie]
}
o <- lapply(id,calc)
df <- data.frame(matrix(unlist(o), nrow=length(o), byrow=T))
rm(o,u_sigma,v,vt,id,calc)
gc()

predicted_ratings$svd <- df$matrix.unlist.o...nrow...length.o...byrow...T.

preds <- predicted_ratings %>%
  mutate(pred = mu + b_i + b_u + b_y + b_yu + svd) %>%
  pull(pred)

RMSE(validation$rating,preds)
