require(tidyverse)
require(openxlsx)

dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021"

setwd(dir = dir)
df = read.xlsx("202010 1-3leg耳石キチジ選定済.xlsx", 1)
colnames(df)
unique(df$STATION)

# tag = data.frame(STATION = c("A", "B", "C", "D", "E", "F", "G", "H"), NS = c("N", "N", "N", "N", "S", "S", "S", "S"))
# df = left_join(df, tag, by = "STATION")

# df = df %>% mutate(NS = ifelse(df$STATION == "A", "N", ifelse(df$STATION == "B", "N", ifelse(df$STATION == "C", "N", ifelse(df$STATION == "D", "N", ifelse(df$STATION == "E", "S", ifelse(df$STATION == "F", "S", ifelse(df$STATION == "G", "S", "S"))))))), length_class = df$体長（ｍｍ）%/%10)

df$NS = ifelse(df$STATION == "A", "N", ifelse(df$STATION == "B", "N", ifelse(df$STATION == "C", "N", ifelse(df$STATION == "D", "N", ifelse(df$STATION == "E", "S", ifelse(df$STATION == "F", "S", ifelse(df$STATION == "G", "S", "S")))))))
df$length_class = df$`体長（ｍｍ）`%/%10
df$serial_number = rep(1:nrow(df))
summary(df)

df1 = df %>% select(-備考) %>% na.omit()
summary(df1)

check = df %>% select(length_class, NS, serial_number) %>% dplyr::group_by(NS, length_class) %>% dplyr::summarize(count = n())

NS = c("N", "S")
df2 = df1 %>% select(length_class, NS, serial_number) %>% na.omit()
pick = NULL
for(i in NS){
  data = df2 %>% filter(NS == i)
    
  # for(j in min(data$length_class):max(data$length_class)){
  for(j in unique(data$length_class)){
    data2 = data %>% filter(length_class == j)
    
    if(nrow(data2) < 11){
      if(nrow(data2) > 0){
        data2 = data2 %>% mutate(pickup = 1)
        pick = rbind(pick, data2)
      }
    }else{
      data3 = data2 %>% sample_n(size = 10) %>% mutate(pickup = 1)
      pick = rbind(pick, data3)
    }
  }
}
pick = pick %>% arrange(NS, length_class)

check2 = pick %>% dplyr::group_by(NS, length_class) %>% dplyr::summarize(count = n())
check3 = pick %>% dplyr::group_by(length_class) %>% dplyr::summarize(count = n())
sum(check3$count) #433
large = check3 %>% filter(length_class > 16)
sum(large$count) #213
large2 = large %>% filter(length_class > 22)
sum(large2$count) #93


pick2 = NULL
for(i in NS){
  data = pick %>% filter(NS == i, length_class > 17)
  
  for(j in unique(data$length_class)){
    data2 = data %>% filter(length_class == j)
    
    if(nrow(data2) > 5){
      data3 = data2 %>% sample_n(size = 5) %>% mutate(pickup2 = 1)
      pick2 = rbind(pick2, data3)
    }else{
      data2$pickup2 = 1
      pick2 = rbind(pick2, data2)
    }
  }
}
pick2 = pick2 %>% arrange(NS, length_class)

temp = pick %>% filter(length_class < 18) %>% mutate(pickup2 = 1)
rcheck2 = pick2 %>% dplyr::group_by(NS, length_class) %>% dplyr::summarize(count = n())
rcheck3 = pick2 %>% dplyr::group_by(length_class) %>% dplyr::summarize(count = n())

pick3 = rbind(temp, pick2)
rcheck4 = pick3 %>% dplyr::group_by(length_class) %>% dplyr::summarize(count = n())
sum(pick3$pickup2) #344

colnames(df)
# df = df[, -12]
df2 = left_join(df, pick %>% select(serial_number, pickup), by = "serial_number")
df2 = left_join(df2, pick3 %>% select(serial_number, pickup2), by = "serial_number")

df2 = df2 %>% dplyr::rename(priority = pickup2)
write.csv(df2, "df2.csv", fileEncoding = "CP932")

sum(df2 %>% filter(pickup == 1) %>% select(pickup))
sum(df2 %>% filter(priority == 1) %>% select(priority))

write.csv(rcheck4, "rcheck4.csv")