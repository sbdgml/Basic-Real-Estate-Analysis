install.packages("magrittr")
install.packages("ggplot2")
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
Sys.setlocale("LC_ALL","C")
Sys.setlocale("LC_ALL","Korean")

data<- read.csv("C:/Users/jinny/Desktop/REE.csv", header=T,sep=",")
data
#데이터 전처리
data$시군구 <- as.character(data$시군구)

word_fun <- function(word) {
  data_split  <-   strsplit(word, " ")
  split  <- data.frame (분리 = unlist(data_split))
  
  시.도 <- split$분리[1]
  군.구  <- split$분리[2]
  split_df <- data.frame(시.도 = 시.도,
                          구.군 = 군.구)
  return(split_df)
}

for (i in 1:nrow(data)) {
  word <- data$시군구[i]
  word_temp <- word_fun(word)
  if (i == 1) {
    word_df <- word_temp
  } else {
    word_df <- rbind(word_df, word_temp)
  }
}
data <- cbind(data, word_df)
data
write.csv(data,'real_estate_final.txt')
Sys.setlocale("LC_ALL", "korean")

data$거래금액.만원. <- as.numeric(gsub(",","",data$거래금액.만원.))

#그래프 생성
library(dplyr)

df <- data %>%
  group_by(시.도) %>%
  summarise(mean_거래금액 = mean(거래금액.만원.))
df


ggplot(data = df , aes(x=시.도 , y=mean_거래금액)) + 
  geom_col()+
  labs(x = "시/도",
       y = "평균 매매가 (만원)")+
  ggtitle ( "2020년도 5월 지역별 아파트 평균 거래가")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))



df1 <- subset(data, 시.도=="서울특별시")
df1

df2 <- df1 %>%
  group_by(구.군)%>%
  summarise(mean_서울거래금액 = mean(거래금액.만원.))
df2
ggplot(data = df2 , aes(x=구.군 , y=mean_서울거래금액))+ 
  geom_col()+
  labs(x = "",
       y = "평균 거래가 (만원)",
       title = "2020년도 5월 서울특별시 아파트 평균 거래가")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))


df3 <- data %>%
  group_by(시.도)

df3=count(df3)
df3
ggplot(data = df3 , aes(x=시.도 , y=n))+ 
  geom_col()+
  labs(x = "",
       y = "거래건수",
       title = "2020년도 5월 지역별 아파트 거래 건수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))

df4 <- df1 %>%
  group_by(구.군)
df4=count(df4)
df4
ggplot(data = df4 , aes(x=구.군 , y=n))+ 
  geom_col()+
  labs(x = "",
       y = "거래건수",
       title = "2020년도 5월 서울특별시 아파트 거래 건수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))






data1<- read.csv("C:/Users/jinny/Desktop/서울시아파트거래.csv", header=T)
data1$시군구 <- as.character(data1$시군구)

word_fun <- function(word) {
  data_split  <-   strsplit(word, " ")
  split  <- data.frame (분리 = unlist(data_split))

  구  <- split$분리[2]
  split_df <- data.frame(구 = 구)
  return(split_df)
}

for (i in 1:nrow(data1)) {
  word <- data1$시군구[i]
  word_temp <- word_fun(word)
  if (i == 1) {
    word_df <- word_temp
  } else {
    word_df <- rbind(word_df, word_temp)
  }
}
data1 <- cbind(data1, word_df)
data1

df5 <- data1 %>%
  group_by(구)
df5<-count(df5)
df5

ggplot(data = df5 , aes(x=구, y=n))+ 
  geom_col()+
  labs(x = "",
       y = "거래건수",
       title = "2019년도 서울특별시 지역별 아파트 거래 건수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))

data1 %>% 
  mutate_at(
    vars(`계약년월`), 
    as_date,
    format = "%Y%m"
  )

ggplot(data = df6 , aes(x=계약년월, y=n))+ 
  geom_line()+
  labs(x = "계약년월",
       y = "거래건수",
       title = "2019년도 서울특별시 월별 아파트 거래 건수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))+
  theme(axis.text.x=element_text(angle=90)) + stat_smooth(method='lm')+
  scale_x_continuous(breaks=c(201901:201912))


df7 <- data1 %>%
  group_by(구)%>%
  count(계약년월)
df7

ggplot(df7 , aes(계약년월, y=n, group=구))+ 
  geom_line()+ facet_wrap(~구,scale='free_y', ncol=3) + stat_smooth(method = 'lm')+
  theme(axis.text.x = element_blank())+
  labs(x = "계약년월",
       y = "거래건수",
       title = "2019년도 서울시 구별 아파트 거래 건수")+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 17, color = "black"))

  