
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
require(abind)

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
write.csv(number_at_age, "number_at_age_freq.csv", fileEncoding = "CP932")

# 2-1.3.2 make the tables of age composition (AC)
AC = left_join(NAA, sum, by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
AC = AC %>% select(length_cate, age, freq)
a_sum = ddply(AC, .(length_cate), summarize, sum = sum(freq))

age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
age_composition = rbind(age_composition, a_sum2)
write.csv(age_composition, "age_composition.csv", fileEncoding = "CP932")


# step 5 年齢別資源尾数の算出 ---------------------------------------------
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

number_at_age2[2,5] = number_at_age2[nrow(number_at_age2), 5]
write.csv(number_at_age2, "number_at_age.csv")



# 2-2 -----------------------------------------------------------
number_at_age3 = number_at_age2[-nrow(number_at_age2), ] %>% gather(key = length, value = number, 2:ncol(number_at_age2))
number_at_age3 = number_at_age2 %>% gather(key = length, value = number, 2:ncol(number_at_age2)) %>% filter(age != "total")
summary(number_at_age3)
mode(number_at_age3$age)
length = number_at_age3 %>% mutate(sum_length = (as.numeric(as.character(as.factor(length))) + 0.5)*number)

s_length_age = ddply(length, .(age), summarize, sum_l = sum(sum_length))
s_number_age = ddply(length, .(age), summarize, sum_n = sum(number))

mean_length_weight_at_age = left_join(s_length_age, s_number_age, by = "age") %>% mutate(mean_cm = sum_l/sum_n) %>% select(age, mean_cm) %>% mutate(mean_mm = mean_cm*10) %>% mutate(weight = (1.86739*10^(-5))*(mean_mm^3.06825547)) 
write.csv(mean_length_weight_at_age, "mean_length_weight_at_age.csv")



# 2-3 -----------------------------------------------------------
okisoko = read.csv("okisoko.csv")
summary(okisoko$魚種名)
colnames(okisoko)
summary(okisoko)

okisoko = okisoko %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
  mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort)

catch_t1 = ddply(okisoko, .(pref, method, area), summarize, sum = sum(catch)) %>% tidyr::spread(key = area, value = sum)
catch_t1[is.na(catch_t1)] = 0

catch_t2 = ddply(okisoko, .(area), summarize, sum = sum(catch))
catch_t2[is.na(catch_t2)] = 0

catch_t3 = ddply(okisoko, .(method, area), summarize, sum = sum(catch)) %>% tidyr::spread(key = method, value = sum)
catch_t3[is.na(catch_t3)] = 0

effort_t1 = ddply(okisoko, .(method, area), summarize, sum = sum(effort)) %>% tidyr::spread(key = method, value = sum)
effort_t1[is.na(effort_t1)] = 0

write.csv(catch_t1, "catch_t1.csv", fileEncoding = "CP932")
write.csv(catch_t2, "catch_t2.csv", fileEncoding = "CP932")
write.csv(catch_t3, "catch_t3.csv", fileEncoding = "CP932")
write.csv(effort_t1, "effort_t1.csv", fileEncoding = "CP932")


### data from each prefecture
# aomori
ao = read.xlsx("catch_pref.xlsx", sheet = "ao") %>% select(年, 漁法名, 漁法, 月間数量) %>% dplyr::rename(year = 年, method_name = 漁法名, method = 漁法, catch_kg = 月間数量)
summary(ao)
unique(ao$method)
unique(ao$method_name)
ao_sum = ddply(ao, .(method), summarize, sum_temp = sum(catch_kg))
ao_sum$method
ao_sum$method2 = c("その他", "その他", "沖底", "刺網", "小底")
ao_sum = ao_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))

### iwate
iwa = read.xlsx("catch_pref.xlsx", sheet = "iwa") %>% select(漁業種名, 合計) %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa
iwa_sum$method
iwa_sum$method2 = c("延縄", "沖底", "延縄", "刺網", "延縄")
iwa_sum = iwa_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### miyagi
miya = read.xlsx("catch_pref.xlsx", sheet = "miya", startRow = 4)
miya = miya[, c(1,2,ncol(miya))]
miya_l = miya %>% filter(魚種コード == "きちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)
miya_s = miya %>% filter(魚種コード == "こきちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)

miya2 = left_join(miya_l, miya_s, by = "method")
miya2[is.na(miya2)] = 0
miya2 = miya2 %>% mutate(sum_temp = sum.x+sum.y) %>% select(method, sum_temp)
miya_sum = miya2 %>% filter(method != "その他漁業種") %>% filter(method != "その他漁業種・全漁法2")
miya_sum$method
miya_sum$method2 = c("沖底", "刺網", "沿岸小漁?", "延縄")
miya_sum = miya_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))


### fukusima
fuku = read.xlsx("catch_pref.xlsx", sheet = "fuku", startRow = 2) %>% 
  select(沖合底びき網) %>% 
  mutate(method = paste0("沖合底びき網")) %>% 
  dplyr::rename(catch_kg = 沖合底びき網) %>% 
  na.omit %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarize(sum = sum(catch_kg))
fuku_sum = fuku
fuku_sum$method
fuku_sum$method2 = c("沖底")
fuku_sum = fuku_sum %>% select(-method)

### ibaraki
iba = read.xlsx("catch_pref.xlsx", sheet = "iba", startRow = 3)
iba = iba[, c("漁法", "年計")] 
iba = iba %>% dplyr::rename(method = 漁法) %>% mutate(num = as.numeric(as.character(as.factor(iba$年計))))
iba = iba %>% filter(method != "小計") %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(num))
iba_sum = iba
iba_sum$method
iba_sum$method2 = c("その他", "延縄", "沖底", "小底", "小底")
iba_sum = iba_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))

# 
# require(abind)
# tag = abind(ao_sum$method, iwa_sum$method, miya_sum$method, fuku_sum$method, iba_sum$method) %>% data.frame() %>% distinct() 
# %>% dplyr::rename(method = .)


merge = ao_sum %>% dplyr::full_join(iwa_sum, by = "method2") %>% dplyr::full_join(miya_sum, by = "method2") %>% dplyr::full_join(fuku_sum, by = "method2") %>% dplyr::full_join(iba_sum, by = "method2")
colnames(merge) = c("漁業種", "青森", "岩手", "宮城", "福島", "茨城")
merge[is.na(merge)] = 0
write.csv(merge, "merge.csv")



# 2-4 -----------------------------------------------------------

# catch trend ---------------------------------------------------
catch_old = read.csv("catchdata_old.csv", fileEncoding = "CP932") %>% na.omit()
catch_old = catch_old[, c(1, 3:5)]
catch_old = catch_old %>% tidyr::gather(key = method, value = sum, 2:4) %>% dplyr::rename(year = 年)
catch_old = catch_old %>% mutate(method2 = ifelse(str_detect(catch_old$method, pattern = "以外"), "沖底・小底以外", catch_old$method)) %>% select(-method) %>% dplyr::rename(method = method2)
summary(catch_old)

catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = 2019)
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000)
summary(catch_new)
write.csv(catch_new, "catch2019.csv", fileEncoding = "CP932")

colnames(catch_new)
colnames(catch_old)
catch = rbind(catch_old, catch_new %>% select(-catch_kg))
summary(catch)
catch = catch %>% dplyr::group_by(method, year) %>% dplyr::summarize(catch_t = sum(sum))

unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底"))

require(ggplot2)
g = ggplot(catch, aes(x = year, y = catch_t, fill = method))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "漁業種")
col_catch = c("grey50", "white", "grey0")
c = scale_fill_manual(values = col_catch)
fig5 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")
ggsave(file = "fig5.png", plot = fig5, units = "in", width = 11.69, height = 8.27)



# effort trend --------------------------------------------------
eff_old = read.csv("effortdata_old.csv", fileEncoding = "CP932")
eff_old = ddply(eff_old, .(method, year), summarize, sum = sum(effort))

eff = ddply(okisoko, .(method), summarize, sum = sum(effort))
eff$year = 2019

eff = rbind(eff_old, eff)
eff = eff %>% mutate(label = ifelse(eff$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(eff$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き")))

unique(eff$label)
levels(eff$label)
eff$label = factor(eff$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))

g = ggplot(eff, aes(x = year, y = sum/1000, shape = label))
p = geom_point()
l = geom_line()
lab = labs(x = "年", y = "有漁網数 (千)", shape = "漁業種")
# col_catch = c("grey50", "white", "grey0")
# c = scale_fill_manual(values = col_catch)
fig6 = g+p+l+lab+theme_bw(base_family = "HiraKakuPro-W3")
ggsave(file = "fig6.png", plot = fig6, units = "in", width = 11.69, height = 8.27)



# weighted cpue ----------------------------------------------------------
gyo_old = read.csv("gyoseki_old.csv", fileEncoding = "CP932")
unique(gyo_old$method)

okisoko = read.csv("okisoko.csv")
summary(okisoko$魚種名)
colnames(okisoko)
summary(okisoko)
okisoko2 = okisoko %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
  mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% filter(漁区名 != "襟裳西")
summary(okisoko2$漁区名)
summary(okisoko2)
cpue = ddply(okisoko2, .(method), summarize, effort = sum(網数の合計), catch = sum(漁獲量の合計))
cpue$year = 2019

cpue2 = rbind(gyo_old, cpue) %>% mutate(cpue = catch/effort)
mean_cpue = ddply(cpue2, .(method), summarize, m_cpue = mean(cpue))
cpue2 = left_join(cpue2, mean_cpue, by = "method") %>% mutate(cpue2 = cpue/m_cpue) %>% mutate(bunsi = catch*cpue2)

y_cpue = ddply(cpue2, .(year), summarize, bunsi = sum(bunsi))
y_catch = ddply(cpue2, .(year), summarize, total_catch = sum(catch))
w_cpue = left_join(y_cpue, y_catch, by = "year") %>% mutate(weighted_cpue = bunsi/total_catch)

# cpue2 = cpue2 %>% mutate(label = ifelse(cpue2$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(cpue2$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き")))
cpue2$label = ifelse(cpue2$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(cpue2$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き"))
unique(cpue2$label)
levels(cpue2$label)
cpue2$label = factor(cpue2$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))


### かけ廻し
g = ggplot(cpue2 %>% filter(method == "かけ廻し"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 15, size = 3)
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13),
           strip.text.x = element_text(size = rel(1.5)))
kake = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2019, by = 3), limits=c(1972, 2019))

### 2そう
g = ggplot(cpue2 %>% filter(method == "2そう曳き"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 17, size = 3)
l = geom_line(linetype = "solid", size = 1)
lab = labs(x = "年", y = "CPUE", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13),
           strip.text.x = element_text(size = rel(1.5)))
niso = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2019, by = 3), limits=c(1972, 2019))

### トロール
g = ggplot(cpue2 %>% filter(method == "トロール"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 18, size = 4)
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_blank(),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13),
           strip.text.x = element_text(size = rel(1.5)))
tra = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2019, by = 3), limits=c(1972, 2019))

### 重み付きCPUE
w_cpue$label = "太平洋北部"
g = ggplot(w_cpue, aes(x = year, y = weighted_cpue))
p = geom_point(shape = 20, size = 4)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "重み付CPUE \n（相対値）", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13),
           strip.text.x = element_text(size = rel(1.5)))
w = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2019, by = 3), limits=c(1972, 2019))

require(gridExtra)
fig8 = grid.arrange(kake, niso, tra, w, ncol = 1)
ggsave(file = "fig8.png", plot = fig8, units = "in", width = 11.69, height = 8.27)



# stock abundance (number & biomass) ---------------------------------------------------------
olddata = read.csv("olddata_trawl_length.csv") 
old_trawl = olddata %>% filter(data == 'trawl') %>% gather(key = year_tag, value = number, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
old_length = olddata %>% filter(data == 'length') %>% gather(key = year_tag, value = mean_mm, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_trawl)

naa = read.csv("number_at_age.csv")
naa = naa[1:(nrow(naa)-1), 3:ncol(naa)]
naa = apply(naa, 1, sum)
naa = naa %>% data.frame() %>% mutate(age = 0:10) %>% filter(age != 0)
colnames(naa) = c('number', 'age')
naa$year = 2019

trawl = rbind(old_trawl, naa)

length = mean_length_weight_at_age %>% select(age, mean_mm) %>% mutate(age = as.numeric(age), year = 2019) %>% filter(age > 1)
length = rbind(old_length, length)
summary(length)

### survival rate at age
survival = NULL
for(i in min(trawl$year):(max(trawl$year)-1)){
  # i = min(trawl$year)
  data_lastyr = trawl %>% filter(year == i)
  data_thisyr = trawl %>% filter(year == (i+1))
  data = left_join(data_lastyr, data_thisyr, by = 'age') %>% arrange(age)
  surv = matrix(NA, ncol = 1, nrow = 9)
  
  if(i < 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
  }
  }
  
  if(i == 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = (data$number.y[(j)]+data$number.y[(j+1)]+data$number.y[(j+2)]+data$number.y[(j+3)]+data$number.y[(j+4)]+data$number.y[(j+5)])/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i > 2006){
    for(j in 2:10){
      if(j < 10){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  survival = rbind(survival, surv)
}
survival = data.frame(surv = survival, year = rep(1996:2019, each = 9), age = rep(2:10))

# naa = read.csv("number_at_age.csv")
# naa = naa[1:(nrow(naa)-1), 3:ncol(naa)]
# naa = apply(naa, 1, sum)
# naa = naa %>% data.frame() %>% mutate(age = 0:10)
# colnames(naa) = c('catch', 'age')
# 
# naa = naa %>% mutate(sel03 = naa$catch/0.3, catch_pre = c(0, 87995, 131464, 343926, 699914, 1134037, 1515615, 1690661, 1741119, 2661833, 41257681)) %>% mutate(sel03_pre = catch_pre/0.3)
# 
# ### survival
# naa$sur = NA
# for(i in 1:nrow(naa)){
#   if(i < (nrow(naa)-1)){
#     # naa[i, "sur"] = naa[(i+1), naa$sel03]/naa[i, naa$sel03_pre]
#     naa[i, "sur"] = naa$sel03[(i+1)]/naa$sel03_pre[i]
#   }else{
#     naa[i, "sur"] = naa$sel03[(i+1)]/(naa$sel03_pre[i]+naa$sel03_pre[i+1])
#   }
# }
# summary(naa)

# logis = data.frame(length_mm = seq(15, 315, 10)) %>% mutate(selectivity = 0.738/(1+1525*exp(-0.0824*length_mm)))
# param = nls(selectivity ~ a/(1+b*exp(c*length_mm)), data = logis, start = c(a = 1, b = 0.1, c = 0.1))
# summary(param)

### selectivity at age
a = 1524.581
b = 0.082366
c = 0.738107

q = NULL
for(i in min(length$year):max(length$year)){
  # i = max(length$year)-1
  data = length %>% filter(year == i) %>% arrange(age)
  temp_q = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_q[j, 1] = c/{1+a*exp(-b*data$mean_mm[j])}
  }
  temp_q2 = data.frame(q = temp_q[,1], year = mean(data$year), age = 2:10)
  q = rbind(q, temp_q2)
}  
summary(q)

# length = mean_length_weight_at_age %>% select(age, mean_mm) %>% mutate(age = as.numeric(age))
# naa = left_join(naa, length, by = "age")
# 
# naa$selectivity = NA
# for(i in 1:nrow(naa)){
#   naa[i, "selectivity"] = c/{1+a*exp(-b*naa$mean_mm[i])}
# }

# naa$weight = NA
# for(i in 1:nrow(naa)){
#   naa[i, "weight"] = (1.86739*10^(-5))*naa$mean_mm[i]^(3.06725547)
# }

### weight at age
weight = NULL
for(i in min(length$year):max(length$year)){
  # i = min(length$year)
  data = length %>% filter(year == i) %>% arrange(age)
  temp_w = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_w[j, 1] = (1.86739*10^(-5))*data$mean_mm[j]^(3.06825547)
  }
  temp_w2 = data.frame(weight = temp_w[,1], year = mean(data$year), age = 2:10)
  weight = rbind(weight, temp_w2)
}
summary(weight)

### number at age when selectivity changes at age
abund_oct = NULL
for(i in min(trawl$year):max(trawl$year)){
  i = min(trawl$year)
  data_trawl = trawl %>% filter(year == i)
  data_q = q %>% filter(year == i)
  data_weight = weight %>% filter(year == i)
  data = left_join(data_trawl, data_q, by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  data = left_join(data, data_weight,  by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  
  temp_naa_sel = matrix(NA, ncol = 1, nrow = 9)
  temp_baa_sel = matrix(NA, ncol = 1, nrow = 9)

  for(j in 1:9){
    #j = 9
    temp_naa_sel[j, 1] = data$number[j]/data$q[j]
  }
  
  for(j in 1:9){
    temp_baa_sel[j, 1] = temp_naa_sel[j, 1]*data$weight[j]*(0.001)^2
  }

  temp_abund_oct = data.frame(number_sel = temp_naa_sel[, 1], biomass_sel = temp_baa_sel[, 1], year = mean(data$year), age = 2:10)
  abund_oct = rbind(abund_oct, temp_abund_oct)
}


### number in January
M = 2.5/20
catch_this_yr = sum(okisoko$漁獲量の合計)/1000 # metric tons
biomass_jan_this_yr = 9897*exp(-2/12*0.125)-460/6*exp(-2/12*0.125)
fishing_rate = catch_this_yr/biomass_jan_this_yr
terminal_F = -log(1-(fishing_rate/exp(-M/2)))
Z = terminal_F + M
surv_2month = exp(-Z/6)

naa$number_2019j = NA
for(i in 1:nrow(naa)){
  naa[i, "number_2019j"] = surv_2month*(naa$catch_pre[i]/naa$selectivity[i])
}
# naa$catch_pre/naa$selectivity

naa$biomass_2019j = naa$number_2019j*naa$weight*(0.001)^2
naa$number_2020j = (naa$catch_pre[i]/naa$selectivity[i])*surv_2month
naa$biomass_j_next = (naa$catch_pre[i]/naa$selectivity[i])*surv_2month
