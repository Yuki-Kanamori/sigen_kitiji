# load the packages -------------------------------------------------------
require(tidyr)
require(dplyr)
require(ggplot2)
require(stringr)

# set working directory -----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")

# load the data -------------------------------------------------
catch = read.csv("catch_at_area.csv", fileEncoding = "CP932")
catch = catch %>% gather(key = year, value = catch, 2:ncol(catch)) %>% 
  mutate(year = as.numeric(str_sub(year, 2, 5)))
# unique(catch$area)
# levels(catch$area)
catch$area = factor(catch$area, levels = c("尻屋崎", "岩　手", "金華山", "常　磐", "房　総"))
mode(catch$catch)
summary(catch)

# make a figure -------------------------------------------------
g = ggplot(catch, aes(x = year, y = catch, fill = area))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "")
col_catch = c("grey50", "white", "grey0", "gold2", "dodgerblue")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.85, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(1975, 2019, by = 3))+scale_y_continuous(expand = c(0,0),limits = c(0, 2510))
ggsave(file = "fig.png", plot = fig, units = "in", width = 11.69, height = 8.27)
