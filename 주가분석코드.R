rm(list=ls())
stock<-read.csv("stock_data.csv",header=T,fileEncoding = "euc-kr")
stock %>% str
stock

for(i in 8:13){
  stock[,i]<-as.factor(stock[,i])
}

#데이터의 히스토그램 확인
par(mfrow=c(2,3),family="AppleGothic") #맥 한글깨짐 방지
stock[,3] %>% hist(main="로그수익률")
stock[,4] %>% hist(main="일중변동성")
stock[,5] %>% hist(main="역사적변동성")
stock[,6] %>% hist(main="유동성")
stock[,7] %>% hist(main="거래량")

#이상치 제거 + 히스토그램 확인
stock2<-filter(stock,x4<20 & x3<100 & x1>-3 & x1<3 & x5<3500000)

par(mfrow=c(2,3),family="AppleGothic") #맥 한글깨짐 방지
stock2[,3] %>% hist(main="로그수익률")
stock2[,4] %>% hist(main="일중변동성")
stock2[,5] %>% hist(main="역사적변동성")
stock2[,6] %>% hist(main="유동성")
stock2[,7] %>% hist(main="거래량")

#파워변환
stock2<-mutate(stock2,x3=x3^0.8,x4=x4^0.2,x5=x5^0.3)

par(mfrow=c(2,3),family="AppleGothic") #맥 한글깨짐 방지
stock2[,3] %>% hist(main="로그수익률")  #x1 음수포함 변환x
stock2[,4] %>% hist(main="일중변동성")
stock2[,5] %>% hist(main="역사적변동성")
stock2[,6] %>% hist(main="유동성")
stock2[,7] %>% hist(main="거래량")

#x6 시가총액
mst1<-manova(as.matrix(stock2[,3:7])~x6,data=stock2)
mst1 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst1$residuals)
B=(N-1)*cov(mst1$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x6,data=stock2),"x6",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x6,data=stock2),"x6",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x6,data=stock2),"x6",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x6,data=stock2),"x6",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x6,data=stock2),"x6",p.adj = 'none',group=T))

#x7 주가
mst2<-manova(as.matrix(stock2[,3:7])~x7,data=stock2)
mst2 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst2$residuals)
B=(N-1)*cov(mst2$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x7,data=stock2),"x7",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x7,data=stock2),"x7",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x7,data=stock2),"x7",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x7,data=stock2),"x7",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x7,data=stock2),"x7",p.adj = 'none',group=T))

#x8 배당수익률
mst3<-manova(as.matrix(stock2[,3:7])~x8,data=stock2)
mst3 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst3$residuals)
B=(N-1)*cov(mst3$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x8,data=stock2),"x8",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x8,data=stock2),"x8",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x8,data=stock2),"x8",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x8,data=stock2),"x8",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x8,data=stock2),"x8",p.adj = 'none',group=T))

#x9 per
mst4<-manova(as.matrix(stock2[,3:7])~x9,data=stock2)
mst4 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst4$residuals)
B=(N-1)*cov(mst4$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x9,data=stock2),"x9",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x9,data=stock2),"x9",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x9,data=stock2),"x9",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x9,data=stock2),"x9",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x9,data=stock2),"x9",p.adj = 'none',group=T))

#x10 pbr
mst5<-manova(as.matrix(stock2[,3:7])~x10,data=stock2)
mst5 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst5$residuals)
B=(N-1)*cov(mst5$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x10,data=stock2),"x10",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x10,data=stock2),"x10",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x10,data=stock2),"x10",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x10,data=stock2),"x10",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x10,data=stock2),"x10",p.adj = 'none',group=T))

#x11 투자시기
mst6<-manova(as.matrix(stock2[,3:7])~x11,data=stock2)
mst6 %>% summary(test='Wilks')
N=dim(stock2)[1]
W=(N-1)*cov(mst6$residuals)
B=(N-1)*cov(mst6$fitted.values)      
solve(W)%*%B %>% eigen

(x1<-LSD.test(aov(x1~x11,data=stock2),"x11",p.adj = 'none',group=T))
(x2<-LSD.test(aov(x2~x11,data=stock2),"x11",p.adj = 'none',group=T))
(x3<-LSD.test(aov(x3~x11,data=stock2),"x11",p.adj = 'none',group=T))
(x4<-LSD.test(aov(x4~x11,data=stock2),"x11",p.adj = 'none',group=T))
(x5<-LSD.test(aov(x5~x11,data=stock2),"x11",p.adj = 'none',group=T))

#주성분 분석

#상관행렬 플랏
stock[,3:7] %>% cor %>% ggcorrplot(lab = T)

#분산 비율
prcomp(stock[,3:7],scale=T) %>% summary

#screeplot
screeplot(a,type='l',main="")
abline(h=1)
screeplot(a,main = "")

#eigenvalue와 주성분점수
a<-prcomp(stock[,3:7],scale=T)
a$x

#biplot
biplot(a,c(1,2))

#주성분점수
b<-cbind(stock[,1:7],a$x[,1:2])
arrange(b,PC1)
arrange(b,PC2)
