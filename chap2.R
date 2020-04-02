
# summary -----------------------------------------------------------------
# 1.  aging (CNN?)

# 2-1 estimate the number at age
# 2-1.2  make the age-length key
# 2-1.2.1 remove the data that age is 10+, 10++, and ?
# 2-1.2.2 fit the von Bertalanffy growth curve and estimate parameters; k and t0.
# 
# 2-1.3.1 make the dataframe including length, age, number, and freq. within age class
# 2-1.3.2 make the tables of number at age and age composition


# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(investr)

# set working directory -----------------------------------------------------------
setwd("/Users/yk/Dropbox/キチジ/森川さん由来/R01d_キチジ資源評価")


# 2.1 estimate the number at age ------------------------------------------
# 2.1.1 make the age-length key -------------------------------------------

# load the data
df = read.xlsx("1_2018年キチジ年齢分解.xlsx", 1)
df = df[, c(1,3)]
colnames(df) = c("length_cm", "age")
summary(df)

# make dataframe with length at age and number at age (not necessary for stock assessment)



# 2.1.1.1 remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = length_cm*10, age_num = as.numeric(age)) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)


# 2.1.1.2 fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*(age_num - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)


