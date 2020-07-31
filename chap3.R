
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
# 3-6  補足図3-2; 年齢別体長組成
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
require(gridExtra)
require(ggrepel)

# please change here -----------------------------------------------------------
# set working directory
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価")

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


# (1-B) きちじとこきちじの漁獲量---------------------------------------------------------
g_miya = read.csv("漁獲量_宮城.csv", fileEncoding = "CP932")
summary(g_miya)
g_miya = g_miya %>% mutate(ymd = as.Date(g_miya$年月日, format = "%Y/%m/%d")) %>% 
  dplyr::rename(mizuage = 日別水揚量, size = 魚種コード) %>% select(ymd, size, mizuage) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), day = as.numeric(str_sub(ymd, 9, 10))) %>%
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
g_miya2 = ddply(g_miya, .(size, year, month, season), summarize, total = sum(mizuage))


# (1-C) こきちじの体長組成---------------------------------------------------------
tai_miya = read.xlsx("02_キチジ宮城体長組成明細2018.xlsx", 1)
tai_miya = tai_miya[, 1:26] 
summary(tai_miya)
tai_miya = tai_miya %>% filter(銘柄コード == 91) # 別の種や体長組成の算出に不必要なデータが入っている場合があるため，ここで念のためフィルターをかける
tai_miya = tai_miya %>% dplyr::rename(ymd = 漁獲年月日, start = 開始の階級値, do = 度数) %>% select(ymd, start, do) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd,5, 6)), day = as.numeric(str_sub(ymd, 7, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
# 度数 = 尾数．つまり，「開始の階級値」サイズの個体が度数分だけあったってこと．

set.seed(1)
rand = runif(nrow(tai_miya)*100) %>% matrix(ncol = 100)

loop = matrix(NA, ncol = 101, nrow = nrow(tai_miya))
loop[, 1] = tai_miya$start
for(i in 1:100){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = tai_miya$year, season = tai_miya$season, month = tai_miya$month, do = tai_miya$do) %>% tidyr::gather(key = times, value = taityo, 1:100) %>% dplyr::rename(number = do)
loop2 = loop %>% group_by(year, season, times, taityo) %>% dplyr::summarize(count = sum(number))
summary(loop2)
tai_miya2 = loop2 %>% group_by(year, season, taityo) %>% dplyr::summarize(mean = mean(count))

weight = data.frame(taityo = rep(5:19)) %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527)
tai_miya2 = left_join(tai_miya2, weight, by = "taityo") %>% mutate(total_weight_kg = (mean*weight)/1000)
summary(tai_miya2)

# figures
g = ggplot(tai_miya2, aes(x = taityo, y = mean), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ season, ncol = 1, scales = 'free')
labs = labs(x = "Length", y = "Numbers", title = "Kokichiji")
g+b+f+labs+theme_bw()


# (1-D) きちじの体長組成---------------------------------------------------------
yatyo = read.csv("宮城_野帳.csv", fileEncoding = "CP932")
head(yatyo)
summary(yatyo)
# 28*171 = 4788
# na.omit -> 2409
yatyo = yatyo %>% dplyr::rename(ymd = 調査年月日, meigara = 銘柄, n_hako = 箱数) %>% 
  tidyr::gather(key = no, value = zentyo, 4:31) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), gyokaku_kg = n_hako*7, gyokaku_n = meigara*n_hako) %>% 
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(ymd, meigara, sep = '_')) %>% na.omit()
yatyo = yatyo %>% mutate(day = ifelse(yatyo$month/1 < 10, as.numeric(str_sub(yatyo$ymd, 8, 9)), as.numeric(str_sub(yatyo$ymd, 9, 10))))

sokutei_n = yatyo %>% group_by(ymd, meigara) %>% dplyr::summarize(sokutei_n = n())
yatyo = left_join(yatyo, sokutei_n, by = c("ymd", "meigara")) %>% mutate(yatyo, rate = gyokaku_n/sokutei_n)

tag_rate = yatyo %>% select(tag, rate) %>% distinct(.keep_all = T)

set.seed(1)
rand = runif(nrow(yatyo)*100) %>% matrix(ncol = 100)
loop = matrix(NA, ncol = 101, nrow = nrow(yatyo))
loop[, 1] = yatyo$zentyo
for(i in 1:100){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
ncol(loop)
nrow(loop)
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = yatyo$year, tag = yatyo$tag) %>% gather(key = times, value = taityo, 1:100)
loop2 = loop %>% group_by(year, tag, times, taityo) %>% dplyr::summarize(count = n())
m_sosei = loop2 %>% group_by(year, taityo, tag) %>% dplyr::summarize(mean = mean(count))
summary(m_sosei)
m_sosei2 = left_join(m_sosei, tag_rate, by = 'tag')
total_sosei = m_sosei2 %>% mutate(total_n = mean*rate, month = as.numeric(str_sub(tag, 6, 7))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))

# figures
g = ggplot(total_sosei, aes(x = taityo, y = total_n), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ season, ncol = 1, scales = 'free')
labs = labs(x = "Length", y = "Numbers", title = "Kichiji")
g+b+f+labs+theme_bw()


# (1-E) ---------------------------------------------------------
kiti = total_sosei %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527) %>% mutate(total_weight_kg = (total_n*weight)/1000, species = 'kiti') %>% select(year, season, taityo, total_n, weight, total_weight_kg, species) %>% dplyr::rename(mean = total_n)
head(kiti)
kokiti = tai_miya2 %>% mutate(species = 'kokiti')
head(kokiti)
miyagi = rbind(kiti, kokiti)
summary(miyagi)

sum_miya = miyagi %>% group_by(year, season, species) %>% dplyr::summarize(sum = sum(total_weight_kg))
total_g_miyagi = g_miya2 %>% mutate(species = ifelse(g_miya2$size == "きちじ", 'kiti', 'kokiti')) %>% group_by(year, season, species) %>% dplyr::summarize(sum = sum(total))
head(sum_miya)
head(total_g_miyagi)
rate = left_join(sum_miya, total_g_miyagi, by = c('year', 'season', 'species')) %>% mutate(rate = sum.y/sum.x)

miyagi = left_join(miyagi, rate, by = c('year', 'season', 'species')) 
miyagi = miyagi %>% mutate(weight2 = mean*rate) %>% mutate(pref = 'Miyagi')

total = miyagi %>% group_by(year, season) %>% dplyr::summarize(total = sum(weight2)) %>% mutate(pref = "miyagi") %>% as.data.frame()
head(total)
fukuiba_mae = 49117　#ここ変更の余地あり
fukuiba_usiro = 1212 #ここ変更の余地あり
rate_fukuiba_mae = (total %>% filter(season == '1-6') %>% select(total) + fukuiba_mae)/total %>% filter(season == '1-6') %>% select(total)
rate_fukuiba_usiro = (total %>% filter(season == '7-12') %>% select(total) + fukuiba_usiro)/total %>% filter(season == '7-12') %>% select(total)

head(miyagi)
fukuiba = miyagi %>% group_by(year, season, taityo) %>% dplyr::summarize(sum = sum(weight2))
fukuiba = fukuiba %>% mutate(rate = ifelse(fukuiba$season == '1-6', as.numeric(rate_fukuiba_mae), as.numeric(rate_fukuiba_usiro))) %>% mutate(weight2 = sum*rate, pref = 'South of Miyagi')
head(fukuiba)
head(miyagi)

fig = rbind(miyagi %>% select(year, season, taityo, weight2, pref), fukuiba %>% select(year, season, taityo, weight2, pref))

# figures
g = ggplot(fig, aes(x = taityo, y = weight2), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ pref, ncol = 1, scales = 'free')
labs = labs(x = "Length", y = "Weight", title = "Length composition in 2018")
g+b+f+labs+theme_bw()



# (3) 八戸 ------------------------------------------------------------------
tai_hati = read.xlsx("04_キチジ八戸漁連2004-2018.xlsx", "みなと2018")
head(tai_hati)
summary(tai_hati)
tai_hati = tai_hati[, c(1,2,3,5,8,9)]
colnames(tai_hati) = c('year', 'month', 'fisheries', 'kikaku', 'irisu', 'kg')

unique(tai_hati$kikaku)

tai_hati = tai_hati %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), iri_bisu = ifelse(kikaku == 13, 'P', ifelse(kikaku == 7, 'S', as.numeric(str_sub(irisu, -2, -1)))))

unique(tai_hati$iri_bisu)
summary(tai_hati)


a = 473.69
b = -0.2583
cv = 0.0448

kg = tai_hati %>% group_by(year, season, iri_bisu) %>% dplyr::summarize(sum = sum(kg)) %>% filter(iri_bisu != "S") %>% filter(iri_bisu != "P") %>% mutate(n_iri_bisu = as.numeric(iri_bisu)) %>% na.omit() %>% mutate(hako = sum/7, gyokaku_bisu = hako*n_iri_bisu) %>% filter(n_iri_bisu > 1) %>% mutate(meanBL = a*n_iri_bisu^b) %>% mutate(SD = meanBL*cv)
unique(kg$iri_bisu)
summary(kg$n_iri_bisu)

# pn = NULL
# length = c(seq(50, 350, 10), 1000)
# for(i in 1:length(length)){
#   for(j in 1:length(kg$n_iri_bisu)){
#     temp = pnorm(length[i], kg$meanBL[j], kg$SD[j])
#     pn = rbind(pn, temp)
#   }
# }
# length(length)*length(kg$n_iri_bisu)-length(pn)
# 
# pn1 = pn[-((length(pn)-length(kg$n_iri_bisu)+1):length(pn))]
# pn2 = pn[-(1:length(kg$n_iri_bisu))]
# pn3 = cbind(pn1, pn2) %>% data.frame() 
# pn3 = pn3 %>% mutate(sa = pn2-pn1, BL = rep(c(seq(50, 350, 10), 1000), each = length(kg$n_iri_bisu)))

pn = NULL
length = c(seq(50, 350, 10), 1000)
for(i in 1:length(length)){
  i = 21
  temp = matrix(NA, ncol = 2, nrow = length(kg$n_iri_bisu))
  for(j in 1:length(kg$n_iri_bisu)){
    temp[j, 1] = pnorm(length[i], kg$meanBL[j], kg$SD[j])
    temp[j, 2] = pnorm(length[i+1], kg$meanBL[j], kg$SD[j])
  }
  temp2 = (temp[,2]-temp[,1]) %>% data.frame %>% mutate(iri_bisu = kg$n_iri_bisu, gyokaku_bisu = kg$gyokaku_bisu, season = kg$season, BL = paste0(length[i+1]))
  pn = rbind(pn, temp2)
}
pn$number = pn$.*pn$gyokaku_bisu
pn2 = ddply(pn, .(season, BL), summarize, total_number = sum(number))










# ---------------------------------------------------------------
# 3-5  補足図3-1; 調査地点，密度分布，及び体長組成 ---------------------------------
# ---------------------------------------------------------------
trawl_length = read.csv("trawl_ns_length2.csv", fileEncoding = "CP932")
trawl_length1 = trawl_length[, c(6,10,11,15)]

colnames(trawl_length1)
colnames(trawl_length1) = c("NS", "station_code", "depth", "total_number")
summary(trawl_length1)
number_at_depth = ddply(trawl_length1, .(station_code, depth), summarize, total = sum(total_number))
# number_at_depth$depth2 = as.factor(number_at_depth$depth)
unique(number_at_depth$depth)
number_at_depth$depth2 = factor(number_at_depth$depth, levels = c("150", "250", "350", "450", "550", "650", "750", "900"))

g = ggplot(number_at_depth, aes(x = depth2, y = total/1000))
b = geom_bar(stat = "identity", width = 1, colour = "grey50")
lab = labs(x = "水深（m）", y = "資源尾数 (千尾/km)", title = "(B)")
f = facet_wrap(~ station_code, ncol = 2)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(2)),
           strip.text.x = element_text(size = rel(2)),
           plot.title = element_text(size = rel(2)))
figa31b = g+b+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(expand = c(0,0),limits = c(0, 25))
# ggsave(file = "figa31b.png", plot = figa31b, units = "in", width = 11.69, height = 8.27)
# bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')


trawl_length2 = trawl_length[, c(6,16:ncol(trawl_length))]
colnames(trawl_length2)
trawl_length2 = trawl_length2 %>% tidyr::gather(key = temp, value = extention_number, 2:ncol(trawl_length2)) 
trawl_length2 = trawl_length2 %>% mutate(size_class = as.numeric(str_sub(trawl_length2$temp, 2, 4)))
summary(trawl_length2)
colnames(trawl_length2)[1] = "NS"
trawl_length2$NS2 = ifelse(trawl_length2$NS == "N", "北部", "南部")
levels(trawl_length2$NS2)
trawl_length2$NS2 = factor(trawl_length2$NS2, levels = c("北部", "南部"))
trawl_length2 = ddply(trawl_length2, .(size_class, NS2), summarize, total = sum(extention_number))

g = ggplot(trawl_length2, aes(x = size_class, y = total/1000, fill = NS2))
b = geom_bar(stat = "identity", width = 0.8, colour = "black", position = "dodge")
lab = labs(x = "体長（cm）", y = "資源尾数 (千尾)", title = "(C)")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(2)),
           strip.text.x = element_text(size = rel(2)),
           plot.title = element_text(size = rel(2)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
figa31c = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(0, 30, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 25))

figa31 = grid.arrange(figa31b, figa31c, ncol = 1)
ggsave(file = "figa31.png", plot = figa31, units = "in", width = 11.69, height = 8.27)





lonlat = read.csv("lonlat2019.csv", fileEncoding = "CP932")
lonlat = lonlat[, c(2,3,4,9:12)]
colnames(lonlat)
colnames(lonlat) = c("station_code", "depth", "ami", "lat1", "lat2", "lon1", "lon2")
summary(lonlat)
lonlat = lonlat %>% mutate(lat = lat1+round(lat2)/100, lon = lon1+round(lon2)/100, tag = paste0(station_code, depth)) %>% filter(ami == 1)
# lonlat = ddply(lonlat, .(tag), summarize, m_lon = mean(lon), m_lat = mean(lat))


trawl_length = read.csv("trawl_ns_length2.csv", fileEncoding = "CP932")
trawl_length3 = trawl_length[, c(10,11)]

colnames(trawl_length3)
colnames(trawl_length3) = c("station_code", "depth")
summary(trawl_length3)
trawl_length3 = trawl_length3 %>% mutate(tag = paste0(station_code, depth))
length()

trawl_length3 = left_join(trawl_length3, lonlat %>% select(-station_code, -depth), by = "tag") %>% distinct(tag, .keep_all = TRUE)
levels(trawl_length3$station_code)


### map
tohoku <- data.frame(read.csv("marmap_coord.csv"))
colnames(tohoku) <- c("long","lat","depth")
check = tohoku[tohoku$depth<0 & tohoku$depth>=-1300,]
summary(check)

japan <- map_data("japan")
japan2 <- japan
japan2$long <- japan$long-0.01

g <- ggplot(subset(tohoku[tohoku$depth<0 & tohoku$depth>=-1300,]),aes(long, lat))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_blank(),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text = element_text(size = rel(1.3)),
           legend.title = element_text(size = 13))
g + geom_polygon(data = japan2, group = japan$group, fill= "gray50", colour= "gray50")  + coord_map(xlim = c(140, 143), ylim = c(36, 42)) + stat_contour(aes(z=depth),binwidth=200,colour="black")+theme_bw()+th+geom_point(data = trawl_length3, aes(x = lon, y = lat, shape = station_code), size = 3)+scale_shape_manual(values = c(16, 4, 17, 15, 18, 8, 1, 2))
summary(trawl_length3)
length(unique(trawl_length3$tag))
