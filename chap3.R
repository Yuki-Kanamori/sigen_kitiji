
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
# setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/森川さん由来/R01d_キチジ資源評価")

# # how many years ago
# # e.g. wanna analyze the data of 2018 and now is 2020, then n = 2
# n = 2


# 3-1 図4: 漁場の空間分布 --------------------------------------------------------------
# load the packages -------------------------------------------------------
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(maps)
require(mapdata)
require(stringr)
require(cowplot)


# please change here -----------------------------------------------------------
# set working directory
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")

# lonlat = read.delim("Map.txt", header=T)
# lonlat2 = read.table("キアンコウ緯度経度.txt", header=T, fileEncoding = "CP932")
# 
# gyoseki_2years_ago = read.table("kitiji_gyoseki2018.txt", header=T)

###10進法で、LONGとLATのみ。列名もLONGとLATにしてください###
x <- read.delim("Map.txt", header=T)


###地図の入ったテキストファイル###
z <- read.table("キアンコウ緯度経度.txt", header=T, fileEncoding = "CP932")
z2 <- read.table("kitiji_gyoseki2018.txt", header=T)

head(z2)
head(z)

for(i in 1 : nrow(z2)) {
  z2[i, 10] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 5])
  z2[i, 11] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 6])
}
y <- z2 
colnames(y) <- c(colnames(y)[1:9], "緯度", "経度")

y[, 12] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$緯度, 1, 2)))+as.numeric(as.character(substr(y$緯度, 4, 5)))/60)))
y[, 13] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$経度, 1, 3)))+as.numeric(as.character(substr(y$経度, 5, 6)))/60)))
y <- data.frame(AREA = tapply(y$漁区, y$漁区, mean), 
                LAT = tapply(y[, 12], y$漁区, mean), 
                LONG = tapply(y[, 13], y$漁区, mean), 
                catch = tapply(y$漁獲量の合計, y$漁区, sum))


quartzFonts(HiraKaku = quartzFont(rep("HiraginoSans-W3", 4)))
theme_set(theme_cowplot(font_family = "HiraKaku")) 

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == "Japan")
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(140.5, 145.5), ylim = c(35, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(1.5)),
           plot.title = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.2)),
           legend.text = element_text(size = rel(1.2)))
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.2), angle = 90),
#            axis.text.y = element_text(size = rel(1.5)),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_blank(),
#            legend.text = element_text(size = rel(1.2)),
#            strip.text.x = element_text(size = rel(1.5)),
#            legend.position = c(0.85, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
p = geom_point(data = y, aes(x = LONG, y = LAT, colour = catch/1000), shape = 15, size = 4)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "経度", y = "緯度", colour = "漁獲量（トン）")
fig4 = local_map+theme_bw(base_family = "HiraKakuPro-W3")+th+p+c+labs+
  geom_hline(yintercept = 40.5, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 39, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 38, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 36.5, colour="black", linetype = "dashed")+
  annotate("text",label="尻屋崎", x=144.5, y=41, family="HiraKaku", size = 5)+
  annotate("text",label="岩手", x=144.3, y=39.5, family="HiraKaku", size = 5)+
  annotate("text",label="金華山", x=144.4, y=38.5, family="HiraKaku", size = 5)+
  annotate("text",label="房総", x=144.3, y=37, family="HiraKaku", size = 5) 
ggsave(file = "fig4.png", plot = fig4, units = "in", width = 8.27, height = 11.69)




# # 3-2  図9; 漁獲物の体長組成  ------------------------------------------
# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")

# (1-B) きちじとこきちじの漁獲量---------------------------------------------------------
g_miya = read.csv("catch_miyagi.csv", fileEncoding = "CP932")
summary(g_miya)
g_miya = g_miya %>% mutate(ymd = as.Date(g_miya$年月日, format = "%Y/%m/%d")) %>% 
  dplyr::rename(mizuage = 日別水揚量, size = 魚種コード) %>% select(ymd, size, mizuage) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), day = as.numeric(str_sub(ymd, 9, 10))) %>%
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
g_miya2 = ddply(g_miya, .(size, year, month, season), summarize, total = sum(mizuage))

# (1-C) こきちじの体長組成---------------------------------------------------------
tai_miya = read.csv("taityo_miyagi.csv", fileEncoding = "CP932")
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
round2 = function(x, d=0) {
  p = 10^d
  return((x * p * 2 + 1) %/% 2 / p)
}
tai_miya2 = loop2 %>% group_by(year, season, taityo) %>% dplyr::summarize(mean = round2(mean(count), 0))
# round2(7.45, 0)

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
yatyo = read.csv("yatyo_miyagi.csv", fileEncoding = "CP932")
# yatyo = read.xlsx("箱入れキチジ体長.xlsx", sheet = "2019")
head(yatyo)
summary(yatyo)

yatyo = yatyo %>% dplyr::rename(ymd = 調査年月日, meigara = 銘柄, n_hako = 箱数)
yatyo = yatyo %>% tidyr::gather(key = no, value = zentyo, 11:ncol(yatyo)) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), gyokaku_kg = n_hako*7, gyokaku_n = meigara*n_hako) %>% 
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(ymd, meigara, sep = '_')) %>% na.omit()
yatyo = yatyo %>% mutate(day = ifelse(yatyo$month/1 < 10, as.numeric(str_sub(yatyo$ymd, 8, 9)), as.numeric(str_sub(yatyo$ymd, 9, 10))))

summary(yatyo)

sokutei_n = yatyo %>% group_by(ymd, meigara) %>% dplyr::summarize(sokutei_n = n())
summary(sokutei_n)

yatyo = left_join(yatyo, sokutei_n, by = c("ymd", "meigara")) %>% mutate(yatyo, rate = gyokaku_n/sokutei_n)
summary(yatyo)

tag_rate = yatyo %>% select(tag, rate) %>% distinct(.keep_all = T) # tag = paste(ymd, meigara, sep = '_')

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
summary(loop2)

loop2 = left_join(loop2, tag_rate, by = "tag") %>% mutate(number = count*rate, month = as.numeric(str_sub(tag, 6, 7))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
summary(loop2)

m_sosei = loop2 %>% group_by(year, times, taityo, season) %>% dplyr::summarize(sum = sum(number))
summary(m_sosei)

total_sosei = m_sosei %>% group_by(year, taityo, season) %>% dplyr::summarize(total_n = round2(mean(sum), 0))
summary(total_sosei)

# figures
g = ggplot(total_sosei %>% filter(taityo < 80), aes(x = taityo, y = total_n), stat = "identity")
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
fukuiba_mae = 4248.8+1667.1　#fuku+iba
fukuiba_usiro = 948.5+16591.9 
rate_fukuiba_mae = (total %>% filter(season == '1-6') %>% select(total) + fukuiba_mae)/total %>% filter(season == '1-6') %>% select(total)
rate_fukuiba_usiro = (total %>% filter(season == '7-12') %>% select(total) + fukuiba_usiro)/total %>% filter(season == '7-12') %>% select(total)

head(miyagi)
fukuiba = miyagi %>% group_by(year, season, taityo) %>% dplyr::summarize(sum = sum(weight2))
fukuiba = fukuiba %>% mutate(rate = ifelse(fukuiba$season == '1-6', as.numeric(rate_fukuiba_mae), as.numeric(rate_fukuiba_usiro))) %>% mutate(weight2 = sum*rate, pref = 'South of Miyagi')
head(fukuiba)
head(miyagi)

fig = rbind(miyagi %>% select(year, season, taityo, weight2, pref), fukuiba %>% select(year, season, taityo, weight2, pref))

# figures
g = ggplot(fig %>% filter(taityo < 50), aes(x = taityo, y = weight2), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ pref, ncol = 1, scales = 'free')
labs = labs(x = "Length", y = "Numbers", title = "Length composition in 2019")
g+b+f+labs+theme_bw()


# (3) 八戸 ------------------------------------------------------------------
tai_hati = read.csv("hati_sokutei.csv", fileEncoding = "CP932")
# ファイルの規格コードが変なので，修正
(code = data.frame(規格名 = unique(tai_hati$規格名)))
code$CD = c(NA, 13, 27, 28, 7, 8, 7, 7, 8, 31, 68, 31, 8, NA, 7, 8, NA, 27)
tai_hati = full_join(tai_hati, code, by = "規格名")
summary(tai_hati)
tai_hati[is.na(tai_hati)] = 0
colnames(tai_hati)

tai_hati = tai_hati %>% select(年, 月, 漁法名, CD, 入数, 月間数量.Ｋｇ.)
colnames(tai_hati) = c("year", "month", "fisheries", "kikaku", "irisu", "kg")
tai_hati$irisu = tai_hati$irisu%/%10

tai_hati = tai_hati %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), iri_bisu = ifelse(kikaku == 13, 'P', ifelse(kikaku == 7, 'S', as.numeric(str_sub(irisu, -2, -1)))))

unique(tai_hati$iri_bisu)
summary(tai_hati)

a = 473.69
b = -0.2583
cv = 0.0448

# PとS以外
kg = tai_hati %>% group_by(year, season, iri_bisu) %>% dplyr::summarize(sum = sum(kg)) %>% filter(iri_bisu != "S") %>% filter(iri_bisu != "P") %>% mutate(n_iri_bisu = as.numeric(iri_bisu))
summary(kg)

kg = kg %>% filter(n_iri_bisu > 1) %>% na.omit() %>% mutate(hako = sum/7, gyokaku_bisu = hako*n_iri_bisu)
kg = kg %>% mutate(meanBL = a*n_iri_bisu^b) %>% mutate(SD = meanBL*cv)
unique(kg$iri_bisu)
summary(kg$n_iri_bisu)
summary(kg)

pn = NULL
length = c(seq(50, 350, 10), 1000)
for(i in 1:length(length)){
  #i = 21
  temp = matrix(NA, ncol = 2, nrow = length(kg$n_iri_bisu))
  for(j in 1:length(kg$n_iri_bisu)){
    temp[j, 1] = pnorm(length[i], kg$meanBL[j], kg$SD[j])
    temp[j, 2] = pnorm(length[i+1], kg$meanBL[j], kg$SD[j])
  }
  temp2 = (temp[,2]-temp[,1]) %>% data.frame %>% mutate(iri_bisu = kg$n_iri_bisu, gyokaku_bisu = kg$gyokaku_bisu, season = kg$season, BL = paste0(length[i+1]))
  pn = rbind(pn, temp2)
}
summary(pn)

colnames(pn)
colnames(pn)[1] = "prob"
pn$number = pn$prob*pn$gyokaku_bisu
summary(pn)

pn2 = ddply(pn, .(season, BL), summarize, total_number = sum(number))
summary(pn2)
pn2$taityo = as.numeric(pn2$BL)/10 # with message "about "get NA"



# pとs
ps = read.csv("seimitu.csv", fileEncoding = "CP932") %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(taityo, meigara, month, total_number_in_box, sep = "_"), tag_box = paste(month, total_number_in_box,meigara,  sep = "_"), taityo2 = taityo%/%10)

unique(ps$tag_box)

comp_ps = ps %>% group_by(season, taityo2, meigara) %>% dplyr::summarize(count = n())
(n_total = ddply(comp_ps, .(season, meigara), summarize, n_total = sum(count)))
n_total = n_total %>% mutate(n_box = c(3, 2)) %>% mutate(n_iri_bisu = n_total/n_box)

(stat_ps = ddply(ps, .(season, meigara), summarize, meanBL = mean(taityo), SD = sd(taityo)))

comp_ps = left_join(comp_ps, n_total, by = c("season", "meigara"))
comp_ps = comp_ps %>% mutate(freq = count/n_total) %>% arrange(taityo2)

# freq = comp_ps$freq
# length = c(seq(50, 350, 10), 1000)
# data = comp_ps %>% select(season, taityo2, freq) %>% mutate(taityo = taityo2*10) 
# data = full_join(data, data_frame(taityo = c(seq(50, 350, 10), 1000))) %>% arrange()


# 
# temp = matrix(0, ncol = 1, nrow = (length(unique(comp_ps$taityo2)))*2*2+1)
# temp = NULL
# for(i in 1:length(unique(comp_ps$season))){
#   i = 1
#   s = unique(comp_ps$season)[i]
#   data = comp_ps %>% filter(season == s)
#   for(j in 1:length(unique(data$meigara))){
#     j = 1
#     d = unique(data$meigara)[j]
#     data2 = data %>% filter(meigara == d)
#     for(k in 1:nrow(data)){
#       temp[k] = data2$freq[k] + temp[k-1]
#     }
#   }
# }

data_p = comp_ps %>% filter(meigara == "P") %>% arrange(taityo2)
temp_p = matrix(0, ncol = 1, nrow = nrow(data_p))
for(k in 2:nrow(data_p)){
  # k = 2
  temp_p[k] = data_p$freq[k] + temp_p[k-1]
}

data_s = comp_ps %>% filter(meigara == "S") %>% arrange(taityo2)
temp_s = matrix(0, ncol = 1, nrow = nrow(data_s))
for(k in 2:nrow(data_s)){
  # k = 2
  temp_s[k] = data_s$freq[k] + temp_s[k-1]
}

(stat_ps = ddply(ps, .(season, meigara), summarize, meanBL = mean(taityo), SD = sd(taityo)))
unique(comp_ps$n_iribisu)
temp1 = data_frame(freq2 = temp_p, taityo2 = data_p$taityo2, meigara = "P", season = "1-6")
temp2 = data_frame(freq2 = temp_s, taityo2 = data_s$taityo2, meigara = "S", season = "1-6")
temp = rbind(temp1, temp2)
comp_ps = full_join(comp_ps, temp, by = c("taityo2", "season", "meigara")) %>% mutate(n_catch = n_iri_bisu*n_box) %>% mutate(total_number = freq2*n_catch)

head(comp_ps)
head(pn2)

sokutei = data_frame(season = comp_ps$season, taityo = comp_ps$taityo2*10, total_number = comp_ps$total_number) %>% mutate(BL = taityo*10)

all_ao = rbind(pn2, sokutei)

# figures
g = ggplot(all_ao %>% na.omit() %>% filter(taityo < 50), aes(x = taityo, y = total_number), stat = "identity")
b = geom_bar(stat = "identity")
f = facet_wrap(~ season, ncol = 1, scales = 'free')
labs = labs(x = "Length", y = "Number", title = "Hachinohe")
g+b+f+labs+theme_bw()


# 引き延ばし
ao = all_ao %>% filter(taityo < 100) %>% mutate(weight = 0.00001867*(taityo*10)^3.068) %>% mutate(biomass = weight*total_number)
total_ao = ddply(ao, .(season), summarize, total = sum(biomass)/1000)

(ddply(tai_hati, .(season), summarize, sum = sum(kg)))

catch_mae = 58551+108677 #aomori+iwate?
catch_usiro = 4105.3+44273.1

total_ao = total_ao %>% mutate(catch = ifelse(total_ao$season == "1-6", catch_mae, catch_usiro)) %>% mutate(rate = catch/total)

ao = left_join(ao, total_ao, by = "season") %>% mutate(number = rate*total_number)
summary(ao)


# Tohoku area ---------------------------------------------------
# 宮城以南 = 福島茨城
# 岩手以北 = 青森岩手（ただし，岩手は漁獲量を使っているだけで，体長データはない）
head(fukuiba)
head(ao)
summary(ao)
summary(fukuiba)

# miya1 = ddply(tai_miya2, .(taityo), summarize, total_number = sum(mean))
# miya2 = ddply(total_sosei, .(taityo), summarize, total_number = sum(total_n))
# aomori = ddply(pn2 %>% filter(taityo < 100), .(taityo), summarize, total_number = sum(total_number))
# tohoku = rbind(miya1, miya2) %>% mutate(area = "宮城県以南")
# tohoku = rbind(tohoku, aomori %>% mutate(area = "岩手県以北"))

fukuiba2 = ddply(fukuiba, .(taityo), summarize, total_number = sum(sum))
ao2 = ddply(ao, .(taityo), summarize, total_number = sum(total_number))
tohoku = rbind(fukuiba2 %>% mutate(area = "宮城県以南"), ao2 %>% mutate(area = "岩手県以北"))

tohoku = ddply(tohoku, .(area, taityo), summarize, total_number = sum(total_number))

unique(tohoku$area)
levels(tohoku$area) 
tohoku$area = factor(tohoku$area, levels = c("岩手県以北", "宮城県以南"))
summary(tohoku)

g = ggplot(tohoku %>% filter(taityo < 50), aes(x = taityo, y = total_number/10000, fill = area))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "体長（cm）", y = "漁獲尾数 (万尾)", fill = "")
col_catch = c("white", "grey0")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.2)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.2)),
           strip.text.x = element_text(size = rel(1.5)),
           legend.position = c(0.85, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig9 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(2, 36, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 4))
ggsave(file = "fig9.png", plot = fig9, units = "in", width = 11.69, height = 8.27)



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
lab = labs(x = "水深（m）", y = "資源密度", title = "(B)")
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
ggsave(file = "figA31B.png", plot = figa31b, units = "in", width = 8.27, height = 11.69)
ggsave(file = "figA31B_2.png", plot = figa31b, units = "in", width = 11.69, height = 8.27)





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

ggsave(file = "figA31C.png", plot = figa31c, units = "in", width = 11.69, height = 8.27)





lonlat = read.csv("lonlat2019.csv", fileEncoding = "CP932")
lonlat = lonlat[, c(2,3,4,9:12)]
colnames(lonlat)
colnames(lonlat) = c("station_code", "depth", "ami", "lat1", "lat2", "lon1", "lon2")
summary(lonlat)
lonlat = lonlat %>% mutate(lat = lat1+(round(lat2)/60), lon = lon1+(round(lon2)/60), tag = paste0(station_code, depth, "_", ami))
# %>% filter(ami == 1)
# lonlat = ddply(lonlat, .(tag), summarize, m_lon = mean(lon), m_lat = mean(lat))


trawl_length = read.csv("trawl_ns_length2.csv", fileEncoding = "CP932")
trawl_length3 = trawl_length[, c(10,11,12)]

colnames(trawl_length3)
colnames(trawl_length3) = c("station_code", "depth", "ami")
summary(trawl_length3)
trawl_length3 = trawl_length3 %>% mutate(tag = paste0(station_code, depth, "_", ami))

length(unique(lonlat$tag))
length(unique(trawl_length3$tag))

trawl_length3 = left_join(trawl_length3, lonlat %>% select(-station_code, -depth), by = "tag") %>% distinct(tag, .keep_all = TRUE)
levels(trawl_length3$station_code)
summary(trawl_length3)


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
           axis.text.x = element_text(size = rel(1), angle = 90),
           axis.text.y = element_text(size = rel(1)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(2)),
           plot.title = element_text(size = rel(1.5)))
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_blank(),
#            axis.text.y = element_blank(),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            strip.text = element_text(size = rel(1.3)),
#            legend.title = element_text(size = 13))
figa31a = g + geom_polygon(data = japan2, group = japan$group, fill= "gray50", colour= "gray50")  + coord_map(xlim = c(140.5, 143), ylim = c(36, 42)) + stat_contour(aes(z=depth),binwidth=200,colour="black")+theme_bw(base_family = "HiraKakuPro-W3")+th+geom_point(data = trawl_length3, aes(x = lon, y = lat, shape = station_code), size = 3)+scale_shape_manual(values = c(15, 4, 17, 8, 18, 16, 1, 2))+labs(title = "(A)", x = "経度", y = "緯度", shape = "調査ライン")
ggsave(file = "figA31A.png", plot = figa31a, units = "in", width = 8.27, height = 11.69)


layout1 <- rbind(c(1, 2),
                 c(3, 3))

figa31 = grid.arrange(figa31a,figa31b, figa31c, layout_matrix = layout1)
ggsave(file = "figa31.png", plot = figa31, units = "in", width = 11.69, height = 8.27)


# +scale_shape_manual(values = c(22, 4, 24, 8, 23, 22, 1, 2)) + scale_fill_manual(values = c("gray70", NA, "gray70", NA, "gray70", "gray70", NA, NA))




# ---------------------------------------------------------------
# 3-56  補足図3-2; 年齢べつ隊長組成（調査） ---------------------------------
# ---------------------------------------------------------------
age_comp = read.csv("age_composition.csv")
age_comp = age_comp[-nrow(age_comp), -1]
age_comp = age_comp %>% gather(key = l, value = freq, 2:ncol(age_comp))
age_comp = age_comp %>% mutate(size_class = as.numeric(str_sub(age_comp$l, 2,3))) %>% select(-l)

len_num = read.csv("survey_N_at_length.csv", fileEncoding = "CP932") # with warning because of so long column
len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit() %>% mutate(size_class = as.numeric(str_sub(age_j, 3, 4)))
surv_n_total = ddply(len_num, .(size_class), summarize, n_total = sum(number))

head(age_comp)
head(surv_n_total)
age_comp = full_join(age_comp, surv_n_total, by = "size_class")
age_comp = age_comp %>% mutate(number = freq*n_total) %>% filter(age > 0)


old = read.csv("survey_N_at_age.csv")
old = old %>% gather(key = l, value = number, 3:ncol(old))
old = old %>% mutate(size_class = as.numeric(str_sub(old$l, 2, 4))) %>% select(-l)

head(old)
head(age_comp)
now = age_comp %>% mutate(Age = ifelse(age_comp == 10, "10+", age_comp$age), Year = 2019) %>% select(Age, Year, size_class, number) 

all = rbind(old, now)


#figure
levels(all$Age)
unique(all$Age)
all$age = factor(all$Age, levels = c("1", "2", "3", "4", "5", "5+", "6", "7", "8", "9", "10+"))
mode(all$size_class)
summary(all)

g = ggplot(all, aes(x = size_class, y = number/1000000, fill = age))
b = geom_bar(stat = "identity", width = 0.5, colour = "black", size = 0.5)
lab = labs(x = "体長 (cm)", y = "資源尾数 (百万尾)", fill = "年齢")
col_catch = c("red1", "red1", "darkorange", "goldenrod1", "goldenrod4", "grey60", "palegreen3", "palegreen4", "steelblue3", "steelblue4", "grey60")
c = scale_fill_manual(values = col_catch)
f = facet_wrap(~ Year, ncol = 5)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.2)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.2)),
           strip.text.x = element_text(size = rel(1.5)))
figa32 = g+b+lab+c+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(0, 36, by = 5))+scale_y_continuous(expand = c(0,0))
ggsave(file = "figa32.png", plot = figa32, units = "in", width = 11.69, height = 8.27)





g = ggplot(catch, aes(x = year, y = catch_t, fill = method))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "漁業種")
col_catch = c("grey50", "white", "grey0")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.2), angle = 90),
           axis.text.y = element_text(size = rel(1.5)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.2)),
           strip.text.x = element_text(size = rel(1.5)),
           legend.position = c(0.85, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig5 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(1975, 2019, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 4000))
ggsave(file = "fig5.png", plot = fig5, units = "in", width = 11.69, height = 8.27)
