
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
lonlat = lonlat %>% mutate(lon = LON%/%1+LON%%1/60, lat = LAT%/%1+LAT%%1/60) #10進法を60進法に変換

df_gj = left_join(df_gj, lonlat, by = "AREA") %>% na.omit()
df_gj2 = ddply(df_gj, .(lon, lat), summarize, total = sum(abundance)*0.001) #ここが違う．そもそもnが違うので，平均をとる必要があるのでは？
summary(df_gj2)

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



# 3-6 補足図3-1 --------------------------------------------------------------


