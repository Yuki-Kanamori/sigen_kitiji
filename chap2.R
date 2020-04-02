
# summary -----------------------------------------------------------------
# 2-1 estimate the number at age
#
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
require(stringr)

# set working directory -----------------------------------------------------------
setwd("/Users/yk/Dropbox/キチジ/森川さん由来/R01d_キチジ資源評価")


# 2=1 estimate the number at age ------------------------------------------
# 2-1.1 make the age-length key -------------------------------------------

# load the data
df = read.xlsx("1_2018年キチジ年齢分解.xlsx", 1)
df = df[, c(1,3)]
colnames(df) = c("length_cm", "age")
summary(df)

# make dataframe with length at age and number at age (not necessary for stock assessment)



# 2-1.1.1 remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = length_cm*10, age_num = as.numeric(age)) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)


# 2-1.1.2 fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)


# 2-1.3.1 make the dataframe including length, age, number, and freq. within age class
# use 10+ and 10++
head(df)
df3 = df %>% select(length_mm, age)
df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, df3$age),
                     age2 = ifelse(df3$age == "10+", 10, ifelse(df3$age == "10++", 10, df3$age))) %>% filter(fumei != 100) %>% select(-fumei) %>% mutate(count = 1)
df3$age2 = as.numeric(df3$age2)
df3$age3 = ifelse(df3$age2 > 10, 10, df3$age2)
summary(df3)
naa = ddply(df3, .(length_mm, age3), summarize, number = sum(count))

length_mm = rep(seq(min(df3$length_mm), max(df3$length_mm)), length(unique(df3$age3))+1) #2893rows
age_num = rep(0:max(df3$age3), each = length(unique(length_mm)))
tag = data_frame(length_mm = length_mm, age_num = age_num)
tag = tag %>% mutate(length_cate = str_sub(tag$length_mm, 1,1))

head(naa)
head(tag)

naa = naa %>% dplyr::rename(age = age3)
tag = tag %>% dplyr::rename(age = age_num)

naa2 = merge(naa, tag, by = c("length_mm", "age"), all = T)
naa2$number = ifelse(is.na(naa2$number), 0, naa2$number)
summary(naa2)
mode(naa2$length_cate)




NAA = naa2 %>% group_by(age, length_cate) %>% summarize(count = n())



naa2$length_cate = as.factor(naa2$length_cate)
mode(naa2$length_cate)

NAA = ddply(naa2, .(age, length_cate), summarize, number = sum(number))



# naa = df3 %>% group_by(length_mm, age) %>% summarize(number = n())
naa = count(df3, "length_mm", "age")

naa = ddply(df3, .(length_mm, age), summarize, number = sum)
