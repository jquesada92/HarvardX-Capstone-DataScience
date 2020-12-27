## ---- warning=FALSE, echo=FALSE, message=FALSE,   scrolled=FALSE--------------------------------------------------
## Enviroment
options(warn=-1)
options(tidyverse.quiet = TRUE)


if(!require(tidyverse, warn.conflicts = FALSE)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(devtools)) install.packages("devtools")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(kableExtra)) install.packages("kableExtra") #Kable table Styles
if(!require(purrr))install.packages("purrr")
if(!require(arules))install.packages("arules")
if(!require(arulesViz))install.packages("arulesViz")
library(arules)
library(arulesViz)

options(repr.plot.width=16, repr.plot.height=8)
devtools::install_github('yihui/tinytex')
options(tinytex.verbose = TRUE)
update_geom_defaults("text", list(size = 16))
knitr::opts_chunk$set(fig.pos = '!h')
knitr::purl()
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Set plot size for my report
fig <- function(width, heigth){
 options(repr.plot.width = width, repr.plot.height = heigth)
}



## ---- warning=FALSE, echo=FALSE, message=FALSE,   scrolled=FALSE, cache=TRUE--------------------------------------
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------
edx  <- edx %>% mutate(date = as_datetime(edx$timestamp),
                      title = str_replace(title,"(?=\\(\\d+).*(?<=\\))",""),#Replace (\\d+)
                       released_year = str_extract(edx$title,"(?=\\(\\d+).*(?<=\\))") )%>%#Extract Released Year from Title
                mutate(year= year(date)) #Extracting Year from timestamp

n_users <- n_distinct(edx$userId)
n_movies <- n_distinct(edx$movieId)
total_obs <- nrow(edx)
first_year <- min(edx$released_year)
last_year <- max(edx$released_year)
mu<-mean(edx$rating)


users_resume <- edx %>% group_by(userId) %>% 
                summarise(n_movies=n_distinct(movieId),avg_rating =mean(rating)) %>%
                distinct()

 

movies_avgs <- edx %>% group_by(movieId,released_year) %>% summarise(
               n_reviews = n()
              ,title = title[1]
              ,avg_rating = mean(rating)
              ,years = max(year) - as.numeric(str_extract(released_year,"(\\d+)"))
             )%>%
             mutate(rate = n_reviews/years)%>%
distinct()


cat(sprintf("
We are using the moviesId, userId, rating to detect patterns to reduce dimension and reduce the rmse between average and real rating.

New columns:
- date: Then timestamp columns is a int, we need to transform to datetime format.
- title: remove the relase year from title.
- relased_yrear: extracted by using (\\W{1}\\d+\\W{1}) as pattern
- year: year extracted from date column

Total Distinct Users: %s\nTotal of Distinct Movies: %s\nTotal Observations: %s\n",n_users,n_movies,total_obs))


## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------
 kable(t(edx[1,]))  %>%
  kable_minimal() %>%
  kable_material(c("striped")) %>% 
  add_header_above(c("EDX Example"=2))


## ---- warning=FALSE, echo=FALSE, message=FALSE,   scrolled=FALSE--------------------------------------------------
movies_avgs %>% ungroup() %>%  select(title, avg_rating, n_reviews) %>% arrange(desc(n_reviews)) %>% 
     slice(1:10)  %>% kable() %>%
  kable_minimal() %>%
  kable_material(c("striped"))%>% 
  add_header_above(c("Top 10 Movies by Views"=3))


## ---- warning=FALSE, echo=FALSE, message=FALSE,   scrolled=FALSE--------------------------------------------------

movies_avgs %>% ungroup() %>%  select(title, avg_rating, n_reviews) %>% arrange(desc(avg_rating)) %>% 
     slice(1:10) %>%  kable() %>%
  kable_minimal() %>%
  kable_material(c("striped"))%>% 
  add_header_above(c("Top 10 Movies by Avg. Rating"=3))



## ---- warning=FALSE, echo=FALSE, message=FALSE,   scrolled=FALSE--------------------------------------------------
movies_avgs %>% ungroup() %>% select(n_reviews,avg_rating) %>% summary() %>% kable() %>%
  kable_minimal() %>%
  kable_material(c("striped"))%>% 
  add_header_above(c("Movies Views & Rating Summary"=3))


## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------

mixfig <- arrangeGrob(
      users_resume %>% 
                ggplot(aes(x=avg_rating))+
                geom_histogram() + scale_x_log10() +
                labs(title="Avg.  Rating By USer")+
                theme(text = element_text(size=15)),
             
                users_resume %>%
            ggplot(aes(x =n_movies))+
                geom_histogram()  +
            scale_x_log10() +
            labs(title="Movies by UserID")+
            theme(text = element_text(size=15)),
      ncol = 2)
ggsave(mixfig,filename = "figs/users_eda.png",height=6, width=10, units="in")


## ---- warning=FALSE,  message=FALSE-------------------------------------------------------------------------------
set_class <-  function(col){
    if (col<=30){
        x = "1. Below 1th quantile"
    }
    else if( between(col,30,122)){
        x = "2. between 1th Q and median"
    }
    else if (between(col,122,565)){
        x = "3. Median to 3rd Q"
    }
    else{
        x = "4. BlockBuster"}
    x
}

movies_avgs<- movies_avgs %>% mutate(class=sapply(n_reviews,FUN=set_class))


## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------

top_plot = movies_avgs %>% 
            ggplot(aes(x=n_reviews,y=avg_rating,col=class)) + 
            geom_point(alpha=0.12) + scale_x_log10() + geom_smooth(method = 'gam')



right_botton<- movies_avgs %>%
                ggplot(aes(x =n_reviews))+
                    geom_histogram()  +
                scale_x_log10() +
                labs(title="UserID by Movies")+
                theme(text = element_text(size=15))


            
left_botton <-  movies_avgs %>% 
                ggplot(aes(x=class,y=avg_rating))+
                geom_boxplot()+labs(title="Classes")
#arrangeGrob let us join mutiples plots in one figure.
botton_plots<- arrangeGrob(left_botton,right_botton,ncol=2)

fig_ <-  arrangeGrob(top_plot,botton_plots
          ,nrow=2
                )
  
#Saving mixfig as png   
ggsave(fig_,filename='figs/eda_movies.png',height=8, width=12, units="in")
  #joinin plot in same figure
   



## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------
users_resume  %>% ungroup() %>% select(n_movies,avg_rating) %>% summary() %>% kable() %>%
  kable_minimal() %>%
  kable_material(c("striped"))%>% 
  add_header_above(c("Users Views & Rating Summary"=3))



## ---- warning=FALSE, echo=FALSE, message=FALSE--------------------------------------------------------------------
#Splitting genres column by "|" and then separeted by using unnest function in distinct rows
movie_genres <- edx %>% 
  select(movieId,userId, genres) %>%
  distinct() %>%
    mutate(genre = 
    strsplit(genres,split="\\|")) %>% 
    unnest(genre) %>% 
    select(movieId,genre,userId)%>% 
    distinct() %>% left_join(edx,by=c('movieId','userId'))%>% 
    group_by(movieId,genre) %>%
    summarise(count=n(),avg_rating=mean(rating))

genres_boxplot <- movie_genres %>% 
ggplot(aes(x=reorder(genre,desc(count)),y=avg_rating)) + 
geom_boxplot()+ 
geom_jitter(alpha=.035,color='blue')+
labs(title="Rating by Genre") 
bar_plot <- movie_genres %>% group_by(genre)%>%summarise(views= sum(count)) %>% ggplot(aes(x=reorder(genre,-views),y=views)) + geom_bar(stat='identity') +
labs(title="Views By Genre ") 





## ---- echo=FALSE--------------------------------------------------------------------------------------------------
figure <- arrangeGrob(bar_plot,genres_boxplot,
                    ncol = 1, nrow = 2)
ggsave(figure,file='figs/genressummary.png',height=8, width=12, units="in",dpi=300)


## ----echo=FALSE,messae=FALSE--------------------------------------------------------------------------------------
#parameters
set.seed(1500)
k <- 5
lambdas <- seq(0, 5, 0.1)
cv <- createFolds(edx$rating,k,returnTrain = TRUE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



## ---- warning=FALSE, echo=TRUE, message=FALSE, cache=FALSE--------------------------------------------------------


# define a empty matrix, k * length(lambda)
rmses <- matrix(nrow=k,ncol=length(lambdas))


# perform 5-fold cross validation to determine the optimal lambda
for(n in 1:k) {
  train_set <- edx[cv[[n]],]
  test_set <- edx[-cv[[n]],]
  
  # Make sure userId and movieId in test set are also in the train set
  test_final <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(test_set, test_final)
  train_final <- rbind(train_set, removed)
  
  mu <- mean(train_final$rating)
  
  rmses[n,] <- sapply(lambdas, function(l){
    #print(l,n)
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    rmse <- RMSE(predicted_ratings, test_final$rating)
    #printing results 
    #print(rmse)
    return(rmse)
  })
}

rmses_cv <- colMeans(rmses)
lambda <- lambdas[which.min(rmses_cv)]
qplot(lambdas,rmses_cv)



## ---- warning=FALSE, echo=TRUE, message=FALSE, cache=FALSE--------------------------------------------------------
#Model Validation and Result
mu <- mean(edx$rating)
reg_movies <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
reg_users  <- edx %>% 
    left_join(reg_movies, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings <- 
    validation %>% 
    left_join(reg_movies, by = "movieId") %>% #Movie Effect
    left_join(reg_users, by = "userId") %>% #User Effect
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
model<- RMSE(predicted_ratings, validation$rating)   # 0.8648185
cat(sprintf("Lambda after %s-Kfold: %s\n RMSE: %s",k, lambda,model))


## ---- echo =FALSE-------------------------------------------------------------------------------------------------
top_movies<-reg_movies %>%
    left_join(movies_avgs,by='movieId')  %>%  select(title,avg_rating,n_reviews,b_i) %>% arrange(desc(b_i)) %>% 
     slice(1:10) 
top_movies %>%  kable() %>%
  kable_minimal() %>%
  kable_material(c("striped"))%>% 
  add_header_above(c("Top Best Movies After Reg" =4))


## ---- echo=FALSE--------------------------------------------------------------------------------------------------
reg_movies %>%
    left_join(movies_avgs,by='movieId')  %>%  select(title,avg_rating,n_reviews,b_i) %>% arrange(b_i) %>% 
     slice(1:10) %>%  kable() %>%
  kable_minimal() %>%
  kable_material(c("striped")) %>% 
  add_header_above(c("Top Worst Movies After Reg"=4))

## -----------------------------------------------------------------------------------------------------------------
reg_movies %>% ggplot(aes(x=b_i)) + geom_histogram() + labs(title="Movie Effect Dist")


## ----echo=TRUE,message=FALSE,warning=FALSE,include=FALSE----------------------------------------------------------
good_movies<- reg_movies %>% filter(b_i>=0.5 ) %>% inner_join(edx,by='movieId')
# Split dataset into movies and users
data_list = split(good_movies$title,
                  good_movies$userId)

# Transform data into a transactional dataset
movie_trx = as(data_list, "transactions")


## ----echo=TRUE,message=FALSE,warning=FALSE,include=FALSE----------------------------------------------------------
# Set of confidence levels
confidenceLevels = seq(from=0.95, to=0.5, by=-0.05)

# Create empty vector
supports = NULL
confidences = NULL
rules_sup = NULL
n<-0

for (supp in seq(0.2,0.5,0.1)){
  
  for (i in 1:length(confidenceLevels)) {
      n = n+1
      supports[n]<- supp
      confidences[n]=confidenceLevels[i]
      rules_sup[n] = 
      length(apriori(movie_trx,
                     parameter=list(support=supp,
                                    minlen=2,
                                    conf=confidenceLevels[i],
                                    target="rules")))
  }
}


## ----echo=TRUE,message=FALSE,warning=FALSE------------------------------------------------------------------------
parameters_aprio <- data.table(supports,confidences,rules_sup) %>% mutate(supports=as.factor(supports))
parameters_aprio%>%
  ggplot(aes(x=confidences,y=rules_sup))+
  geom_point(aes(color=supports,linetype=supports))+
  geom_line(aes(color=supports,linetype=supports)) +
  labs(title="Parameters for Apriori Algorithm")



## ----echo=FALSE,message=FALSE,warning=FALSE-----------------------------------------------------------------------
parameters_aprio[(parameters_aprio$rules_sup!=0)&(which.max(parameters_aprio$confidences))]


## ---- include=FALSE-----------------------------------------------------------------------------------------------
rules<- apriori(movie_trx,
                     parameter=list(support=0.2, 
                                    conf=0.6,
                                    minlen=2,
                                    target="rules"),
                  )
             
top_lift<-head(sort(rules,by="lift"),15)   



## ----warning=FALSE------------------------------------------------------------------------------------------------
inspectDT(top_lift[1:10])


## -----------------------------------------------------------------------------------------------------------------
plot(top_lift,method="Graph")

