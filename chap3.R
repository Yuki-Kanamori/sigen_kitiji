
# SUMMARY -----------------------------------------------------------------
# 3-1  図4; 漁場の空間分布 
#      (引き継ぎ資料の3-1部分)
# step 1 成長曲線の前処理（成長曲線に不必要な10+, 10++, and ?のデータを除去する）
# step 2 von Bertalanffy growth curveにfittingし，パラメータ（k and t0）の推定を行う
# step 3 ALKの作成 (number at age)    ※表がcsvで出てきます
# step 4 ALKの作成 (age composition)  ※表がcsvで出てきます
# step 5 年齢別資源微数の算出
# 
# 
# 3-2  図9; 漁獲物の体長組成 
#      (引き継ぎ資料の3-2部分)
#      
#      
# 3-3  図17; F-YPRとF-%SPRの関係 
#      (引き継ぎ資料の3-3部分)
#      
#      
# 3-5  補足図3-1; 調査地点，密度分布，及び体長組成 
#      (引き継ぎ資料の3-5部分)     
# 
# 
# 3-6  補足図3-1; 年齢別体長組成
#      (引き継ぎ資料の3-6部分)
# 
# 
# 3-7  補足表3-1; 調査から得られた資源量と資源微数の経年変化
#      (引き継ぎ資料の3-7部分)



# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(readxl)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(maps)
require(mapdata)
require(investr)
require(stringr)

# please change here -----------------------------------------------------------
# set working directory
setwd("/Users/yk/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価")

# how many years ago
# e.g. wanna analyze the data of 2018 and now is 2020, then n = 2
n = 2


# 3-1 図4: 漁場の空間分布 --------------------------------------------------------------
# とりあえず『4_キチジ漁績〜.xlsx』からデータを読むようなコードにしておき，後から2-3-2から直接求められるように修正を加える
df_gj = read.xlsx("4_キチジ漁績2006-2018.xlsx", 21) %>% filter(魚種コード == 408) %>% select(漁区, 漁獲量の合計) #日本語入力のためか，renameが使えない
colnames(df_gj) = c("AREA", "abundance")
head(df_gj)
summary(df_gj)

# 緯度経度データ
lonlat = read.csv("area_lonlat.csv")
summary(lonlat)
lonlat = lonlat %>% mutate(lon = LON%/%1+LON%%1/60, lat = LAT%/%1+LAT%%1/60) #10進法を60進法に変換

df_gj = left_join(df_gj, lonlat, by = "AREA") %>% na.omit()
df_gj2 = ddply(df_gj, .(lon, lat), summarize, total = sum(abundance)*0.001) #ここが違う．そもそもnが違うので，平均をとる必要があるのでは？
summary(df_gj2)

g = ggplot(df_gj2, aes(x = lon, y = lat))
p = geom_point()
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
g+p+c

# mapping
unique(map_data("world")$region)
map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == "Japan")
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(xlim = c(min(df_gj2$lon)-1), max(df_gj2$lon)+5), ylim = c(min(df_gj2$lat)-1, max(df_gj2$lat)+1))
th = theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_blank(),
               axis.title.x = element_text(size = rel(1.5)),
               axis.title.y = element_text(size = rel(1.5)),
               strip.text = element_text(size = rel(1.3)),
               legend.title = element_text(size = 13))
p = geom_point(data = df_gj2, aes(x = lon, y = lat, colour = total), shape = 15, size = 2)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
fig = local_map+theme_bw()+th+p+c+labs(title = "", x = "Longitude", y = "Latitude", colour = "")



# # 3-2  図9; 漁獲物の体長組成  ------------------------------------------
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価/R01d_キチジVPA")


# (1-B) ---------------------------------------------------------
g_miya = read.csv("漁獲量_宮城.csv", fileEncoding = "CP932")
summary(g_miya)
g_miya = g_miya %>% mutate(ymd = as.Date(g_miya$年月日, format = "%Y/%m/%d")) %>% 
  dplyr::rename(mizuage = 日別水揚量, size = 魚種コード) %>% select(ymd, size, mizuage) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), day = as.numeric(str_sub(ymd, 9, 10))) %>%
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
g_miya2 = ddply(g_miya, .(size, year, month, season), summarize, total = sum(mizuage))


# (1-C) ---------------------------------------------------------
tai_miya = read.xlsx("02_キチジ宮城体長組成明細2018.xlsx", 1)
tai_miya = tai_miya[, 1:26] 
summary(tai_miya)
tai_miya = tai_miya %>% dplyr::rename(ymd = 漁獲年月日, start = 開始の階級値, do = 度数) %>% select(ymd, start, do) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd,5, 6)), day = as.numeric(str_sub(ymd, 7, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))

rand = runif(nrow(tai_miya)*10) %>% matrix(ncol = 10)

loop = matrix(NA, ncol = 11, nrow = nrow(tai_miya))
loop[, 1] = tai_miya$start
for(i in 1:10){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = tai_miya$year, season = tai_miya$season, month = tai_miya$month, do = tai_miya$do) %>% gather(key = times, value = taityo, 1:10)
tai_miya2 = loop %>% group_by(year, season, month, taityo) %>% dplyr::summarize(number = round(mean(do)))

weight = data_frame(taityo = rep(5:19), weight = c(3.0,5.2,8.4,12.7,18.3,25.5,34.4,45.3,58.5,74.0,92.3,113.5,137.8,165.6,197.1))
tai_miya2 = left_join(tai_miya2, weight, by = "taityo") %>% mutate(total_weight = number*weight)
summary(tai_miya2)

# (1-D) ---------------------------------------------------------
yatyo = read.csv("宮城_野帳.csv", fileEncoding = "CP932")
head(yatyo)
summary(yatyo)
# 28*171 = 4788
yatyo = yatyo %>% dplyr::rename(ymd = 調査年月日, meigara = 銘柄, n_hako = 箱数) %>% tidyr::gather(key = no, value = zentyo, 4:31) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), gyokaku_kg = n_hako*7, gyokaku_n = meigara*n_hako) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12")) %>% na.omit()

rand = runif(nrow(yatyo)*100) %>% matrix(ncol = 100)
loop = matrix(NA, ncol = 101, nrow = nrow(yatyo))
loop[, 1] = yatyo$zentyo
for(i in 1:100){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = yatyo$year, season = yatyo$season, month = yatyo$month) %>% gather(key = times, value = taityo, 1:100)
loop2 = loop %>% group_by(year, season, month, times, taityo) %>% dplyr::summarize(count = n())
m_sosei = loop2 %>% group_by(year, season, taityo) %>% dplyr::summarize(mean = mean(count))

sokutei_n = nrow(yatyo)
total_gyokaku_n = sum(yatyo$gyokaku_n)
rate = total_gyokaku_n/sokutei_n

total_sosei = m_sosei %>% mutate(rate = rate) %>% mutate(total_n = mean*rate)

# figures
g = ggplot(total_sosei, aes(x = taityo, y = total_n), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ season, ncol = 1)
labs = labs(x = "Length", y = "Numbers", title = "Kichiji")
g+b+f+labs+theme_bw()


# (1-E) ---------------------------------------------------------
kiti = total_sosei %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527) %>% mutate(total_weight_kg = (total_n*weight)/1000)

sum_kiti = kiti %>% group_by(season) %>% dplyr::summarize(sum = sum(total_weight_kg))
total_g_miyagi = g_miya2 %>% group_by(size, season) %>% dplyr::summarize(sum = sum(total))
rate = left_join(sum_kiti, total_g_miyagi %>% filter(size == "きちじ"), by = "season") %>% mutate(rate = sum.y/sum.x)

kiti = left_join(kiti, rate %>% select(season, rate), by = "season")
kiti = kiti %>% mutate(weight2 = total_n*rate.y)


# 3-6 補足図3-1 --------------------------------------------------------------





