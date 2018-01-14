rm(list = ls())
library("dplyr")
library("bsts")
library("reshape2")
library("lubridate")
library("ggplot2")
library("igraph")

#Load data
bitcoin_data <- read.csv("D:\\Mis\\ALL_CRYPTO.csv" )

#Top 10
bitcoin_data %>% group_by(symbol) %>% 
  summarise("V" = mean(market)) %>% 
  arrange(desc(V)) %>% 
  dplyr::select(1) %>% 
  head(10) %>% t() %>% as.vector() -> top

#Filter only top 10
bitcoin_data <- dplyr::filter(bitcoin_data , symbol %in% top)
crypto_size <- dplyr::select(bitcoin_data , symbol , market)
crypto_size %>% group_by(symbol) %>% 
  summarise("Market_Size" = mean(market)) %>% 
  data.frame() -> crypto_size

bitcoin_data <- dplyr::select(bitcoin_data , symbol , date , close , close_ratio)

group_by(bitcoin_data , symbol) %>% 
  mutate("percent" = 100* (close/lag(close,1) - 1)) -> bitcoin_data

#bitcoin_data$percent <-  (bitcoin_data$close/lag(bitcoin_data$close,1)) -1
#bitcoin_data$close <- log(bitcoin_data$close) 
# Profile sd 
#group_by(bitcoin_data , symbol) %>% summarise("SD_Close" = sd(close , na.rm = TRUE) , 
#3                                              "SD_Close_Ratio" = sd(close_ratio , na.rm = TRUE))
#group_by(bitcoin_data , sym)

bitcoin_data <- data.frame(bitcoin_data)
bitcoin_data_t <- dplyr::select(bitcoin_data , -close , -close_ratio)
bitcoin_II <- reshape2::dcast(bitcoin_data_t , date ~ symbol , mean) 
#ss <- AddAr(ss, lags = 3, sigma.prior = SdPrior(3,1 ))
# model1 <- bsts(bitcoin_II$BTC ,
#                state.specification = ss,
#                niter = 1000)

#Convert to time serie format
test <- filter(bitcoin_II , complete.cases(bitcoin_II))
test$date <- ymd(test$date)
test <- zoo(test[,-1], order.by = test$date)

#Test with one crytocurrency
#State Space Model using 
#The local linear trend model assumes that both the mean 
#and the slope of the trend follow random walks
ss <- AddLocalLinearTrend(list() , test$ADA  )
#Also using other "regressors" as other cryptocurrency
model2 <- bsts(test$ADA ~. ,
               state.specification = ss,
               niter = 2500 ,
               data = test[,-1])
#Burn in Period from MCMC
burn_in <- bsts::SuggestBurn(0.3 , model2)
# Get Inclusion Probability
temp <- melt(colMeans(apply(model2$coefficients[-(1:burn_in),], 2 , function(x){ ifelse(x > 0 , 1 , 0)})))
temp <- tail(temp , nrow(temp) - 1)
temp$crypto <- row.names(temp)
plot(model2 , "coef" , labels)
temp <- arrange(temp , desc(value))
temp$crypto <- factor(temp$crypto, levels = temp$crypto[order(temp$value)])

#Plot 
ggplot(temp , aes(x = crypto , y = value)) + geom_bar(stat = "identity") + 
  ggtitle("Variable Selection for ADA") + 
  labs(x = "Cryptocurrency" , y = "Inclusion Probability") +
  coord_flip() + theme_grey() 

#k <- predict(model2 , horizon =  24)
#plot(k , plot.orginal = 26)

#Do the same for every bitcoin price 
#all_m <- list()

relation <- data.frame()

for(i in 1:ncol(test)){
  # Run Bayesian Structural Model with 3000 chains
  
  model <- bsts(test[,i] ~. ,
                 state.specification = ss,
                 niter = 3000 ,
                 data = test[,-i])
  
  burn_in <- bsts::SuggestBurn(0.3 , model)
  # Get 
  prop <- melt(colMeans(apply(model$coefficients[-(1:burn_in),], 2 , function(x){ ifelse(x > 0 , 1 , 0)})))
  #print(prop)
  #Format for relation table
  kk <- cbind(expand.grid(names(test)[i] , row.names(prop)[-1]) , "Prob" = prop[-1,])
  relation <- rbind(relation , kk)
  #name <- names(test)[i]
  #all_m[[name]] <- model
}

#names(test)[names(test) %in% row.names(prop)]

relation <- dplyr::select(relation ,Var2 , Var1 , Prob)
names(relation) <- c("from" , "to" , "Prob")

# plot(all_m[[1]] , "coef")
# par(mfrow(c(4,1)))
# plot(all_m[[1]] , "coef")
# plot(all_m[[2]] , "coef")
# plot(all_m[[3]] , "coef")
# plot(all_m[[4]] , "coef")
#install.packages("igraph")

main <- melt(names(test) ,value.name = "Coin")
main$Coin <- as.character(main$Coin)
crypto_size$symbol <- as.character(crypto_size$symbol)

main <- inner_join(main , crypto_size , by = c("Coin" = "symbol"))
#get rid off small edge
relation_all <- relation
relation <- filter(relation , Prob >= 0.025)
g <- graph_from_data_frame(relation , directed = TRUE , vertices = main)

#c_scale <- colorRamp(c('black','grey'))
#E(g)$color = apply(c_scale(E(g)$weight), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
dev.off()
plot.igraph(g , edge.arrow.size= 0.2 , vertex.label.cex = 0.9 , 
            label.cex = 0.1 , layout=layout.fruchterman.reingold ,
            vertex.color = "gray" , edge.color = "black" 
            ,vertex.label.color="black" , edge.width=E(g)$Prob*8 
            ,edge.curved= TRUE , main = "Network of inclusion probability for each cryptocurrency (P > 0.025)" 
             )


#ALL
g2 <- graph_from_data_frame(relation_all , directed = TRUE , vertices = main)
plot.igraph(g2 , edge.arrow.size= 0.2 , vertex.label.cex = 0.9 , 
            label.cex = 0.1 , layout=layout.fruchterman.reingold ,
            vertex.color = "gray" , edge.color = "black" 
            ,vertex.label.color="black" , edge.width=E(g)$Prob*8 
            ,edge.curved= TRUE , main = "Network of inclusion probability for each cryptocurrency" 
)

