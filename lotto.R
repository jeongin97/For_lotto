install.packages('ggbreak')
library(ggplot2)
library(RColorBrewer)
library(ggbreak)
library(showtext)
library(dplyr)
library(RSelenium)
library(rvest)
library(httr)
library(KoNLP)
library(wordcloud2)
library(fmsb)
library(scales)
# 복권 판매금액 크롤링
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445, browserName = "chrome")
remDr$open()
remDr$navigate("https://dhlottery.co.kr/gameResult.do?method=byWin")

total = NULL

for (i in 1:980){
  nextpage <- paste0("#dwrNoList > option:nth-child(",i,")")
  Sys.sleep(0.5)
  nextpagenum <- remDr$findElement(using='css selector', nextpage)
  nextpagenum$clickElement()
  Sys.sleep(0.5)
  nextlistlink <- remDr$findElement(using='css selector', '#searchBtn')
  nextlistlink$clickElement()
  Sys.sleep(0.5)
  
  weektotle_node<- remDr$findElements(using='css selector', '#article > div:nth-child(2) > div > ul > li:nth-child(2) > strong')
  weektotle <- sapply(weektotle_node,function(x){x$getElementText()})
  total <- c(total, unlist(weektotle))
}

total <- gsub("[원]","",total)
total <- gsub("[,]","",total)
total_lotto <- data.frame("최종 판매 금액" = total)  

write.csv(total_lotto, 'data/total.csv', row.names = F)


#덧글 크롤링
remDr <- remoteDriver(remoteServerAddr = "localhost" , 
                      port = 4445, browserName = "chrome")
remDr$open()
remDr$navigate("https://dhlottery.co.kr/gameResult.do?method=winTalkList&boardType=T")
lotto_com = NULL
j = 2
k = 10

for (page in 1:5){
  comm <-remDr$findElements(using ="css", "td.ta_left")
  text <-sapply(comm,function(x){x$getElementText()})
  lotto_com <- c(lotto_com, unlist(text))
  for (i in j:k) {
    nextCss <- paste0("#page_box > a:nth-child(",i,")")
    Sys.sleep(0.5)
    nextListlink <- remDr$findElement(using = 'css selector', nextCss)
    nextListlink$clickElement()
    Sys.sleep(0.5)
    
    comm <-remDr$findElements(using ="css", "td.ta_left")
    text <-sapply(comm,function(x){x$getElementText()})
    lotto_com <- c(lotto_com, unlist(text))
  }
  nextPageLink <- remDr$findElement(using='css selector', '#page_box > a.go.next')
  nextPageLink$clickElement()
  Sys.sleep(0.5)
  j = 4
  k = 12
}

comm <-remDr$findElements(using ="css", "td.ta_left")
text <-sapply(comm,function(x){x$getElementText()})
lotto_com <- c(lotto_com, unlist(text))

write.csv(lotto_com,'data/lottoco.csv',row.names = F)

#덧글 이미지화
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

useSejongDic()
lotto <- readLines("data/lottoco222.csv")
lotto <- gsub('[^가-힇,0-9]'," ",lotto)
lotto <- gsub(' +'," ",lotto)
lotto <- sapply(lotto, extractNoun, USE.NAMES = F)
lotto <- extractNoun(lotto)
lotto <- unlist(lotto)
lotto <- table(lotto)
lotto <- Filter(function(x) {nchar(x)>=1 && nchar(x)<=1.5}, lotto)
wordcloud2(data = lotto, shape = 'diamond')




# 1등, 2등, 3등 당첨 금액 평균

lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_m <- lotto %>% 
  select("X1등.당첨금액","X2등.당첨금액","X3등.당첨금액") %>% 
  colSums()/980
names(lotto_m) <- c("1등 당첨 금액 평균","2등 당첨 금액 평균","3등 당첨 금액 평균")
lotto_m
# 당첨 금액 평균 그래프
options(scipen=100)
m <- round((lotto_m/10000),digits = 1)
mm <- c(1,2,3)
aa<- data.frame(mm,m)

gg_m <- ggplot(aa,aes(x=mm,y=m)) +
  geom_bar(stat='identity',width=0.5, fill=c("Light Cyan 3","Pale Turquoise 3","Light Blue 3")) +
  labs(title="등수별 당첨 금액 평균", x="등수", y="당첨금액") +
  geom_text(aes(label = m),vjust = 2) +
  scale_x_continuous(breaks=c(1,2,3), labels = paste0(c(1,2,3),"등"), " ") +
  scale_y_break(c(300,6000), scale = 1.5) +
  scale_y_break(c(7000,270000), scale = 1.5) +
  ylab("당첨금액\n(단위: 만 원)") + xlab("등수") +
  theme_classic() +
  theme(plot.title=element_text(size=25, face="bold",color = "Royal Blue 3"),
        axis.line.x = element_line(color ="Royal Blue 3"),
        axis.line.y = element_line(color ="Royal Blue 3"),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(angle=0, face="bold", size=16, vjust = 6),
        axis.title.y=element_text(angle=0, face="italic", size=16))
gg_m


# 당첨금액 boxplot
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_m2 <- lotto %>% 
  select("X1등.당첨금액","X2등.당첨금액","X3등.당첨금액")
names(lotto_m2) <- c("1등 당첨 금액","2등 당첨 금액","3등 당첨 금액")
lotto_m2 <- lotto_m2[-which(lotto_m2$`1등 당첨 금액` == 0),]
lotto_m2 <- lotto_m2[-which(lotto_m2$`2등 당첨 금액` == 0),]
lotto_m2 <- round((lotto_m2/10000),digits = 1)

boxplot(lotto_m2, col=c('Plum 2','Light Pink 2','Medium Purple 2'),
        main = "당첨금액분포", cex.main=2, col.main="purple",
        xlab="등수", ylab="당첨금액 / (단위 : 만원)")

min(lotto_m2$`1등 당첨 금액`) ; max(lotto_m2$`1등 당첨 금액`)
min(lotto_m2$`2등 당첨 금액`) ; max(lotto_m2$`2등 당첨 금액`)
min(lotto_m2$`3등 당첨 금액`) ; max(lotto_m2$`3등 당첨 금액`)




#나라별 복권판매 금액
gdp_lotto <- read.csv('data/세계 복권 판매.csv')
gdp_lotto <- gdp_lotto[order(gdp_lotto$총판액),]

ggplot(gdp_lotto, aes(x=총판액,y=reorder(국가,총판액)))+
  geom_bar(stat='identity',fill = rainbow(36))+
  geom_text(aes(label = 총판액),hjust = 0)+
  labs(title="나라별 복권판매금액", x="판매액\n(단위: 백만달러)", y="국가별")


# 홀짝 비율
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto <- lotto %>% select("X1", "X2", "X3", "X4", "X5", "X6")
split_num <- (unlist(lotto)) 
even = 0
odd = 0
for (i in split_num) {
  if (i %% 2 == 0){
    even = even + 1
  } else{
    odd = odd + 1
  }
}


#홀짝 그래프
total <- c(even,odd)
홀짝 <- c("짝수","홀수")
even2 <- even/(even+odd)*100
odd2 <- odd/(even+odd)*100
per <- c(even2,odd2)
final <- data.frame(홀짝,total,per)
View(final)

ggplot(final,aes(x='',y=total,fill=홀짝))+
  geom_bar(width = 1, stat = "identity", color="white")+
  theme_void()+
  coord_polar("y", start = 0)+
  geom_text(aes(label=paste0(round(per,1),'%')),
            position =position_stack(vjust=0.5))


# 10단위 확률
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_n <- lotto %>% select("X1", "X2", "X3", "X4", "X5", "X6", "보너스")
lotto_n <- unlist(lotto_n)
num_9 = 0 ;num_19 = 0 ;num_29 = 0 ;num_39 = 0 ;num_45 = 0

for(i in lotto_n) {
  if(i>0 && i<10){
    num_9 = num_9+1
  }else if(i>=10 && i<20){
    num_19 = num_19+1
  }else if(i>=20 && i<30){
    num_29 = num_29+1
  }else if(i>=30 && i<40){
    num_39 = num_39+1
  }else {num_45 = num_45+1}
}
num_9 ;num_19 ;num_29 ;num_39 ;num_45

sumsum <- num_9+num_19+num_29+num_39+num_45

num10 <- round(num_9/sumsum*100,digits = 5)
num20 <- round(num_19/sumsum*100,digits = 5)
num30 <- round(num_29/sumsum*100,digits = 5)
num40 <- round(num_39/sumsum*100,digits = 5)
num45_2 <- round(num_45/sumsum*100,digits = 5)

allnum <- c(num10,num20,num30,num40,num45_2)
max.score <- rep(25,5)
min.score <- rep(0,5)
final <- data.frame(rbind(max.score,min.score,allnum))
colnames(final) <- c('0-9','10-19','20-29','30-39','40-45')

radarchart(final,
           axistype = 1,
           axislabcol = 'red',
           seg = 5,
           caxislabels = seq(0,25,5),
           pfcol = rgb(0.5,0.5,1,0.5),
           pcol = rgb(0.8,0.5,1,1),
           title=c("10단위 번호별 뽑힐 확률"))

#로또 당첨 확률 (각 회차별)
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
loser2 <- sum(lotto$최종.판매.금액/1000)
lotto <- lotto %>% select('X1등.당첨자수','X2등.당첨자수','X3등.당첨자수','X4등.당첨자수','X5등.당첨자수','최종.판매.금액')
winner <- lotto$X1등.당첨자수+lotto$X2등.당첨자수+lotto$X3등.당첨자수+lotto$X4등.당첨자수+lotto$X5등.당첨자수
winner2 <- sum(winner)
challenger <- c(winner2,loser2)
winner <- round(winner/(lotto$최종.판매.금액/1000)*100,digits=5)
loser <- 100-winner
winner <- round(winner,digits = 5)
loser <- round(loser,digits = 5)
total <- data.frame(winner,loser)
totalmean <- data.frame(colMeans(total))
win_los <- c('당첨','꽝')
totalFinal <- data.frame(win_los,challenger,totalmean)
colnames(totalFinal) <-c("결과","게임한수","확률")
View(totalFinal)

ggplot(totalFinal,aes(x='',y=게임한수,fill=결과))+
  geom_bar(width = 1, stat = "identity", color="white")+
  theme_void()+
  coord_polar("y", start = 0)+
  geom_text(aes(label = paste0(round(확률,1),'%')),
            position = position_stack(vjust=0.5))


# 가장 많이 당첨된 번호 뽑기 (보너스 번호 포함)
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_n <- lotto %>% select("X1", "X2", "X3", "X4", "X5", "X6", "보너스")
table(unlist(lotto_n))
data.frame(round((table(unlist(lotto_n))/980)*100, digits = 1))


# 보너스 번호 미포함(1~6)
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_nx <- lotto %>% select("X1", "X2", "X3", "X4", "X5", "X6")
table(unlist(lotto_nx))
data.frame(round((table(unlist(lotto_nx))/980)*100, digits = 1))

# 번호 뽑기 그래프(미포함)
xx <- data.frame(round((table(unlist(lotto_nx))/980)*100, digits = 1))
gg_nx <- ggplot(xx,aes(x=Var1,y=Freq)) +
  geom_bar(stat='identity',width=0.5, fill=terrain.colors(45)) +
  labs(title="번호별 당첨 확률(보너스 미포함)", x="번호", y="확률") +
  geom_text(aes(label = paste0(Freq,"%")),vjust = -1) +
  ylab("확률\n(%)") + xlab("번호") +
  theme_classic() +
  theme(plot.title=element_text(size=25, face="bold",color = "orange"),
        axis.line.x = element_line(color ="orange"),
        axis.line.y = element_line(color ="orange"),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(angle=0, face="bold", size=16),
        axis.title.y=element_text(angle=0, face="italic", size=16)) 
gg_nx

# 보너스 번호
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_no <- lotto %>% select("보너스")
table(unlist(lotto_no))
data.frame(round((table(unlist(lotto_no))/980)*100, digits = 1))

# 보너스번호 그래프
oo <- data.frame(round((table(unlist(lotto_no))/980)*100, digits = 1))
gg_no <- ggplot(oo,aes(x=Var1,y=Freq)) +
  geom_bar(stat='identity',width=0.5, fill=terrain.colors(45)) +
  labs(title="보너스 번호", x="번호", y="확률") +
  geom_text(aes(label = paste0(Freq,"%")),vjust = -1, check_overlap=TRUE) +
  ylab("확률\n(%)") + xlab("번호") +
  theme_classic() +
  theme(plot.title=element_text(size=25, face="bold",color = "red"),
        axis.line.x = element_line(color ="red"),
        axis.line.y = element_line(color ="red"),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(angle=0, face="bold", size=16),
        axis.title.y=element_text(angle=0, face="italic", size=16))
gg_no



#로또 당첨 확률 (등수별 각 회차)
options(scipen=100)
lotto <- read.csv("data/로또통파일.csv")
lotto <- data.frame(lotto)
lotto_winner <- lotto %>% 
  select('X1등.당첨자수','X2등.당첨자수','X3등.당첨자수','X4등.당첨자수','X5등.당첨자수')
winner <- round(lotto_winner/(lotto$최종.판매.금액/1000)*100,digits=6)
total <- data.frame(winner)
#totalmean <- data.frame(colMeans(total))
totalsum <- rowSums(total)
totalsum1 <- (total/totalsum)*100
num <- c(980:1)
final <- data.frame(num,totalsum1,totalsum)
colnames(final) <-c("회차","1등","2등",'3등','4등','5등','승리확률')
write.csv(final, 'data/등수별당첨.csv', row.names = F)

#당첨확률 그래프
final <- read.csv("data/등수별당첨2.csv")
colnames(final) <-c("회차","등수","확률")
ggplot(data=final, aes(x=회차, y=확률, colour=등수)) + 
  geom_point(shape=19, size=1)



#-------------------------------------------------------------------------------------------------

ch <- round(xx$Freq/45,digits = 4)*10
ch
hello<- NULL
f <- NULL
for(i in 1:100){
  hi <- cbind(sample(1:45, 6, replace = F, prob = ch))
  hello <- cbind(hi, hello)
}
a <- unlist(table(hello))
rownames(a) <- NULL


for (i in 1:10) {
  h <- cbind(sample(1:45, 6, replace = F, prob = a))
  f <- cbind(h, f)
}

t(f)

ff = sorted(f)
