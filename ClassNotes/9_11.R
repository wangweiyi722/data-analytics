car<-read.csv("329E/carPrices.csv")
car_data <- car[19:26]
head(car_data)
cor(car_data)
pairs(car_data)

