
# SUMMARY -----------------------------------------------------------------
# 2-1  成長曲線の作成，ALKの作成，および年齢別資源微数の算出 
#      (引き継ぎ資料の2-1部分)
# step 1 成長曲線の前処理（成長曲線に不必要な10+, 10++, and ?のデータを除去する）
# step 2 von Bertalanffy growth curveにfittingし，パラメータ（k and t0）の推定を行う
# step 3 ALKの作成 (number at age)    ※表がcsvで出てきます
# step 4 ALKの作成 (age composition)  ※表がcsvで出てきます
# step 5 年齢別資源微数の算出
# 
# 
# 2-2  年齢別資源体重の算出
#      (引き継ぎ資料の2-2部分) 
# step 1 
# 
# 
# 2-3  漁獲量まとめ
#      (引き継ぎ資料の2-3部分)
# step 1 魚種別集計
# step 2 県から提出されたデータの集計
# 
# 
# 2-4  資源量計算とABCの算定


# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(investr)
require(stringr)

# please change here -----------------------------------------------------------
# set working directory
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")

# how many years ago
# e.g. wanna analyze the data of 2018 and now is 2020, then n = 2
# n = 2


# 2-1 estimate the number at age ------------------------------------------
# 2-1.1 make the age-length key -------------------------------------------

# load the data
# year = as.numeric(str_sub(Sys.Date(), 1, 4))-n
# filename = paste0("1_", year, "年キチジ年齢分解.xlsx")
# df = read.xlsx(filename, 1)
# df = df[, c(1,3)]
# colnames(df) = c("length_cm", "age")
# summary(df)
df = read.csv("ALdata.csv") %>% filter(pick == "TRUE") %>% select(label, SL, age)
summary(df)
mode(df$age)
# make dataframe with length at age and number at age (not necessary for stock assessment)



# 2-1.1.1 remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)
summary(df2)

# 2-1.1.2 fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)


# 2-1.3.1 make the tables of number at age
# use 10+ and 10++
head(df)
df3 = df %>% select(length_mm, age)
summary(df3)
head(df3)
df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)))


df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)),
                     age2 = ifelse(df3$age == "10+", 10, ifelse(df3$age == "10++", 10, as.character(df3$age)))) %>% filter(fumei != 100) %>% select(-fumei) %>% mutate(count = 1)
df3$age2 = as.numeric(df3$age2)
df3$age3 = ifelse(df3$age2 > 10, 10, df3$age2)
df3 = na.omit(df3)
summary(df3)
naa = ddply(df3, .(length_mm, age3), summarize, number = sum(count))

length_mm = rep(seq(min(df3$length_mm), max(df3$length_mm)), length(unique(df3$age3))+1) #2893rows
age_num = rep(0:max(df3$age3), each = length(unique(length_mm)))
tag = data_frame(length_mm = length_mm, age_num = age_num)
tag = tag %>% mutate(length_cate = ifelse(length_mm < 100, str_sub(tag$length_mm, 1, 1), str_sub(tag$length_mm, 1, 2)))

head(naa)
head(tag)

naa = naa %>% dplyr::rename(age = age3)
tag = tag %>% dplyr::rename(age = age_num)

naa2 = merge(naa, tag, by = c("length_mm", "age"), all = T)
naa2$number = ifelse(is.na(naa2$number), 0, naa2$number)
summary(naa2)
NAA = ddply(naa2, .(age, length_cate), summarize, number = sum(number))
NAA$length_cate = as.numeric(NAA$length_cate)
summary(NAA)

# add the data that NAA does not have
add = NAA %>% filter(length_cate < min(NAA$length_cate)*2-1)
add = add %>% mutate(length_cate = rep(1:(min(add$length_cate)-1)), number = 0)
NAA = rbind(add, NAA)
NAA = NAA %>% arrange(length_cate, age) 
sum = ddply(NAA, .(length_cate), summarize, sum = sum(number))

NAA2 = NAA %>% tidyr::spread(key = length_cate, value = number)
sum2 = sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
number_at_age = rbind(NAA2, sum2)
write.csv(number_at_age, "number_at_age.csv", fileEncoding = "CP932")

# 2-1.3.2 make the tables of age composition (AC)
AC = left_join(NAA, sum, by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
AC = AC %>% select(length_cate, age, freq)
a_sum = ddply(AC, .(length_cate), summarize, sum = sum(freq))

age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
age_composition = rbind(age_composition, a_sum2)
write.csv(age_composition, "age_composition.csv", fileEncoding = "CP932")


# step 5 年齢別資源尾数の算出 ---------------------------------------------
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価")
len_num = read.csv("length_number.csv")
head(len_num)
colnames(len_num) = c("length_cate", "number")
summary(len_num)
AC2 = left_join(AC, len_num, by = "length_cate") %>% mutate(bisu = freq*number)
num_ac2 = ddply(AC2, .(length_cate), summarize, total = sum(number)) #多分計算間違い．あとカテ4に値はいらん

number_at_age2 = AC2 %>% select(length_cate, age, bisu) %>% tidyr::spread(key = length_cate, value = bisu)
num_ac2 = num_ac2 %>% tidyr::spread(key = length_cate, value = total) %>% mutate(age = "total")

number_at_age2 = rbind(number_at_age2, num_ac2)

# get survey data and make dataframe
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")
len_num = read.csv("survey_N_at_length.csv", fileEncoding = "CP932")
len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit()
summary(len_num)
# len_num2 = ddply(NatL, .(age_j), summarize, number = sum(number))
len_num2 = len_num %>% dplyr::group_by(age_j) %>% dplyr::summarize(number = sum(number)) %>% mutate(length_cate = as.numeric(str_sub(age_j, 3, 4))) %>% select(-age_j)

summary(len_num2)
AC2 = left_join(AC, len_num2, by = "length_cate") %>% mutate(bisu = freq*number)
num_ac2 = ddply(AC2, .(length_cate), summarize, total = mean(number))

number_at_age2 = AC2 %>% select(length_cate, age, bisu) %>% tidyr::spread(key = length_cate, value = bisu)
num_ac2 = num_ac2 %>% tidyr::spread(key = length_cate, value = total) %>% mutate(age = "total")

number_at_age2 = rbind(number_at_age2, num_ac2)
# x = number_at_age2[1:(nrow(number_at_age2)-1), 2:ncol(number_at_age2)]
# apply(x, 2, sum) - number_at_age2[nrow(number_at_age2), 2:ncol(number_at_age2)]

