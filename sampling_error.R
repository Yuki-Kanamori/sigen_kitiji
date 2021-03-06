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
require(gridExtra)
require(ggrepel)

# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2020")

old = read.csv("survey_N_at_age.csv", fileEncoding = "CP932")
old = old %>% gather(key = c, value = number, 3:ncol(old))
old2 = ddply(old, .(c, Year), summarise, sum = sum(number))
tag = ddply(old, .(Year), summarise, total = sum(number))
old2 = left_join(old2, tag, by = "Year") %>% mutate(freq = sum/total, size = as.numeric(str_sub(c, 2,4))) %>% arrange(size)


new = read.csv("survey_N_at_length.csv", fileEncoding = "CP932")
new = new[, 16:ncol(new)]
new = new %>% gather(key = c, value = number, 1:ncol(new)) %>% mutate(Year = 2019)
new2 = ddply(new, .(c, Year), summarise, sum = sum(number))
tag = ddply(new %>% na.omit(), .(Year), summarise, total = sum(number))
new2 = left_join(new2, tag, by = "Year") %>% mutate(freq = sum/total, size = as.numeric(str_sub(c, 3,5))) %>% arrange(size) %>% filter(size < 32)
new2[is.na(new2)] = 0


freq = rbind(old2, new2) %>% filter(size > 15)

g = ggplot(freq, aes(x = size, y = freq))
p = geom_point()
l = geom_line()
f = facet_wrap(~ Year)
labs = c(x = "Year", y = "Freq")
g+p+l+g+theme_bw()


g = ggplot(freq, aes(x = Year, y = sum/10000))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "資源尾数（万尾）", shape = "")
f = facet_wrap(~ size, scales = "free", ncol = 4)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), angle = 90),
           axis.text.y = element_text(size = rel(2)),
           axis.title.x = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(2)),
           legend.title = element_text(size = 13),
           strip.text.x = element_text(size = rel(2)))
sampling_bias = g+p+l+f+lab+theme_bw(base_family = "HiraKakuPro-W3")
ggsave(file = "sampling_bias.png", plot = sampling_bias, units = "in", width = 11.69, height = 8.27)




# re-check ------------------------------------------------------
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

