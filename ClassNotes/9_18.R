library(tidyverse)
library(modelr)
library(data.world)

options(na.action = na.warn)

project <- "https://data.world/cannata/f-17-eda-project-1"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM diamonds"),
  dataset = project
)
summary(df)
attach(df)
ggplot(df, aes(x=cut, y=price)) + geom_boxplot()
ggplot(df, aes(x=color, y=price)) + geom_boxplot()
ggplot(df, aes(x=clarity, y=price)) + geom_boxplot()

ggplot(df, aes(x=carat, y=price)) + geom_point()

# Exponential Growth:
# y = c ^ (ax + b)
# logc(y) = mx + b
# ln(y)  = mx + b

ggplot(df, aes(x=carat, y=price)) + geom_point()
ggplot(df, aes(x=carat, y=price)) + geom_hex(bins = 50)
# Take the df dataframe, send it to the dplyr function and that that value and send it to the mutate function
# mutate() adds columns to the data
diamonds2 <- df %>% dplyr::filter(carat <= 2.5) %>% dplyr::mutate(lprice = log2(price)) 
ggplot(diamonds2, aes(x=carat, y=lprice)) + geom_point()
ggplot(diamonds2, aes(carat, lprice)) + geom_hex(bins = 50)


# R^2 is high and p value is less than 5%
# R^2 is percentage of variation that can be account for by the model
# t- value is greater than 2
# ==> Good predictor
mod_diamond <- lm(lprice ~ carat, data = diamonds2)
summary(mod_diamond)

# seq_range takes the max and min of carat and take 20 evently spaced points between them
grid <- diamonds2 %>% data_grid(carat = seq_range(carat, 20)) %>% 
  add_predictions(mod_diamond, "lprice") %>% mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, lprice)) + 
  geom_point() + 
  geom_line(data = grid, colour = "red", size = 1)

ggplot(diamonds2, aes(x=carat, y=price)) + geom_point() + geom_line(data = grid, colour = "red", size = 1)

# Power law:
# ln(y) = m*ln(x) + b
# e ^ ln(y) = e ^ (m*ln(x) + b)
# y = e ^ (m*ln(x)) * e ^ b = e ^ (ln(x) ^ m) * e ^ b = (x ^ m) * (e ^ b) = c * (x ^ m)

# Take the log of both price and carat and you get a linear model
diamonds2 <- df %>% dplyr::filter(carat <= 2.5) %>% dplyr::mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(x=lcarat, y=lprice)) + geom_point()

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
summary(mod_diamond)

grid <- diamonds2 %>% data_grid(lcarat = seq_range(lcarat, 20)) %>% 
  add_predictions(mod_diamond, "lprice") %>% mutate(price = 2 ^ lprice, carat = 2 ^ lcarat)

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_point() + 
  geom_line(data = grid, colour = "red", size = 1)

ggplot(diamonds2, aes(x=carat, y=price)) + geom_point() + 
  geom_line(data = grid, colour = "red", size = 1)

# Get rid of everything but residuals
diamonds2 <- diamonds2 %>% add_residuals(mod_diamond, "lresid") %>% mutate(resid = 2 ^ lresid)

ggplot(diamonds2, aes(lcarat, lresid)) + geom_point()
ggplot(diamonds2, aes(carat, resid)) + geom_point()
ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, resid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, resid)) + geom_boxplot()

mod_diamond <- lm(lresid ~ carat, data = diamonds2)
grid <- diamonds2 %>% data_grid(carat = seq_range(carat, 20)) %>% 
  add_predictions(mod_diamond, "resid")
ggplot(diamonds2, aes(x=carat, y=resid)) + geom_point() + 
  geom_line(data = grid, colour = "red", size = 1)

mod_diamond <- lm(resid ~ carat, data = diamonds2)
grid <- diamonds2 %>% data_grid(carat = seq_range(carat, 20)) %>% 
  add_predictions(mod_diamond, "resid")
ggplot(diamonds2, aes(x=carat, y=resid)) + geom_point() + 
  geom_line(data = grid, colour = "red", size = 1)
ggplot(diamonds2, aes(x=cut, y=resid)) + geom_point()
ggplot(diamonds2, aes(x=cut, y=resid)) + geom_hex(bin=50)
ggplot(diamonds2, aes(x=color, y=resid)) + geom_hex(bin=50)
ggplot(diamonds2, aes(x=clarity, y=resid)) + geom_hex(bin=50)
