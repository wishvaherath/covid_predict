library(tidyverse)
library(e1071)
library(nnet)
library(nmp)
library(caret)

#modifiers
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#this is the oririnal file
#data_file = "../csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# get the latest file from github
data_file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
search_location = "_US"
threshold = 100 #cutoof for the trimming of the time series
days_past = 5 #days to go back for the past projection
days_future = 5 #days to go forward for the future projection

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#load data csv
df = read.csv(data_file)

#combine province and country
df = df %>% unite("location", Province.State, Country.Region, sep="_", remove = F)

#filter for a given country
data_row = df %>% filter(location == search_location)
#remove all but the time series
data_vector = as_vector(data_row %>% select(starts_with("X")))

#finding the index of the first value greater than "Threshold"
threshold_index = 0
counter = 1
for (i in data_vector){
  if (i >= threshold){
    threshold_index = counter 
    break
  }
  counter = counter + 1
}

total_length = length(data_vector)
length_after_trim = total_length - threshold_index

#data vector that contains the original dataset used for training
usable_data_vector = data_vector[threshold_index:total_length]

#input_df contains the original data used for testing
#input_df = tibble(y=usable_data_vector, x=1:length(usable_data_vector))

#past_df cotains the input_df data minus the selected days in days_past
#past_df = tibble(y=usable_data_vector[past_day_list], x=past_day_list)

#future_df is input_df x values plus days_future
future_df = tibble(x=future_day_list)


#past_df cotains the input_df data minus the selected days in days_past
past_df = tibble(y=usable_data_vector[1:(length(usable_data_vector)-days_past)], x=1:length(usable_data_vector))


#input_df contains the original data used for testing
input_df = tibble(y=usable_data_vector, x=1:length(usable_data_vector)) %>% add_column(class="CSSE_data")



#SVM based regression
svm_model <- svm(y ~ x, input_df)
#predicted vals for all X
#input_df = add_column(input_df, p=predict(svm_model, input_df))
df_svm <- future_df
df_svm <- add_column(df_svm, y=predict(svm_model, future_df)) %>% add_column(class="predicted_svm")


#NEURAL NETWORK
nnet_model <- nnet(y/max(input_df$y) ~ x , data=input_df, size=2)
df_nnet <- future_df
df_nnet<- add_column(df_nnet, y=(predict(nnet_model, future_df)*max(input_df$y))) %>% add_column(class="predicted_neuralnet")


#nonlinear least squares
nls_model <- nls(y ~ (x ^ b), input_df,  start=c(b=2), trace=T)
df_nls <- future_df
df_nls<- add_column(df_nls, y=(predict(nls_model, future_df))) %>% add_column(class="predicted_nonlinear_ls")

#creating main table
df_main = bind_rows(input_df, df_svm, df_nnet, df_nls)


#g = ggplot(data=input_df, aes(x=x, y=y))  + geom_point(size=.5)

ggplot(data=df_main, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line() + geom_point(size=.5) 


#using the caret package

tc = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")

postResample(df_nnet$x, df_nnet$y)
#estimating goodness of fit


#---------------------------------------------------


caret_methods = c( "krlsPoly", "nnet", "avNNet", "brnn", "rvmPoly", "qrf","rf", "enet", "bridge", "lars", "leapSep", "pcr", "relaxo", "ridge")
#caret_methods = c( "krlsPoly", "nnet", "avNNet", "brnn", "rvmPoly", "qrf","rf", "enet", "bridge", "lars", "leapSep", "pcr", "relaxo", "ridge")

#caret_methods = c("qrf","rf")


combined_df = c()
caret_model_list = c()
r2_list = c()
for (method in caret_methods){
  print(method)
  #set repeated cross validation
  tc = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
  
  #tc = trainControl(method = "repeatedcv", number = 3, repeats = 5)
  #list of available methods https://topepo.github.io/caret/available-models.html
  try({caret_model = caret::train(y ~ x, data=input_df,method=method,trainControl=tc , preProcess = c("scale", "center"))})
  temp <- future_df
  predicted_y = predict(caret_model, future_df)
  r2 = postResample(pred=predicted_y, obs = future_df$x)['Rsquared']
  r2_list = c(r2_list, r2)
  print(paste(method, r2, sep="__"))
  
  temp <- add_column(temp, y=predicted_y) %>% add_column(class=method)
  caret_model_list = c(caret_model_list, caret_model)
  
  #combine with combined_df
  combined_df = bind_rows(combined_df, temp)
 
}

names(r2_list) = caret_methods

#remove NA
r2_list =r2_list[!is.na(r2_list)]

#filtering for r2 greater than 0.8
r2_list = r2_list[r2_list > 0.7]
r2_list = r2_list[r2_list < 0.95]


#plotting
#combined_df = combined_df %>% filter(class != "nnet")


#top_five = sort(r2_list, decreasing = T)[1:5] %>% names() %>% as.matrix()
combined_df = combined_df %>% filter(class %in% names(r2_list))
combined_df = bind_rows(combined_df, input_df)
#combined_df = bind_rows(combined_df, df_main)
ggplot(data=combined_df, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line()  + geom_point( size=0.5) 

ggplot(data=df_main, aes(x=x, y=y, color=class, group=class, linetype=class)) + geom_line() + geom_point(size=.5) 



#---------------------------------------------------
xx = c()
for (i in c(1,2,3,4)){
  xx = c(xx, c(i))
}





#CARET

tc = trainControl(method = "repeatedcv", number = 10, repeats = 10, search = "random")
#list of available methods 2
caret_brnn = caret::train(y ~ x, data=input_df,method="rvmPoly", trControl= tc, preProcess = c("scale", "center"))
df_caret_brnn <- future_df
df_caret_brnn<- add_column(df_caret_brnn, y=(predict(caret_brnn, future_df))) %>% add_column(class="brnn")
plot(df_caret_brnn$x, df_caret_brnn$y)


plot(input_df$x, input_df$y)

plot(log(input_df$x), log(input_df$y))

tuneResult1 <- tune(svm, y ~ x,  data = input_df,
                    ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5)))
)

plot(tuneResult1)


tuneResult <- tune(svm, y ~ x,  data = input_df,
                   ranges = list(epsilon = seq(tuneResult1$best.model$epsilon-.15,
                                               tuneResult1$best.model$epsilon+.15,
                                               0.01), 
                                 cost = seq(2^(log2(tuneResult1$best.model$cost)-1),
                                            2^(log2(tuneResult1$best.model$cost)+1),
                                            length=6))
)

# viz comparison
plot(x,y)
title("original data + linear regression + svr")
abline(linregress_model, col="red")
points(Data$x, predictYsvm, col = "blue", pch=4)
points(Data$x, predictYsvm, col = "blue", type="l")