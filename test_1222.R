# header 옵션: 첫 번째 줄을 열이름으로 인식여부(default:F)
# header가 없으면 첫 번째 줄도 data로 인식
data <- read.csv(file.choose(), header = T)
head(data)
# data를 attach하면 data 안의 모든 변수들이 memory상에 상주한다.
attach(data)
a1
table(a1)
table(Gender)
#"a17",a18","a19","a20", "a21","a22","a23","a24","a25","a26","a27","a28","a29","a30","a31","a32","a33","a34"
test1 <-subset(data, select =c("a18","a20", 
                               "a21","a22",
                               "a23","a24","a25","a27","a28","a29","a30"
))
head(test1)       
e_value <- eigen(cor(test1))
e_value
fit <- factanal(test1, factors = 2, rotation = 'varimax')
print(fit, cutoff=0.4, digits = 3)

#"a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14", "a15","a16"
test2 <-  subset(data, select = c("a6","a7","a8","a12","a13","a14" ))
e_value <- eigen(cor(test2))
e_value

fit2 <- factanal(test2, factors = 1, rotation="varimax")
print(fit2, cutoff=0.4, digit=3)
# 6, 7, 13
# "a35","a36", "a37","a38","a39","a40", "a41","a42","a43","a44","a45","a46","a47","a48","a49","a50","a51","a52","a53","a54","a55"
test3 <- subset(data,, select = c("a35","a36", 
                                  "a37","a38",
                                  "a41","a42","a43","a44","a45"))
e_value <- eigen(cor(test3))
e_value

fit3 <- factanal(test3, factors = 3, rotation="varimax", score = "regression")
print(fit3, cutoff=0.4, digit=3)

test4 <- subset(data, select = c("a6","a7","a8","a12","a13","a14","a18","a20","a21","a22",
                                 "a23","a24","a25","a27","a28","a29","a30"))
e_value <- eigen(cor(test4))
print(e_value, digit=3)

fit4 <- factanal(test4, factors = 3, rotation="varimax", score = "regression" )
print(fit4, cutoff=0.4, digit=3)
fit4$score

# 독립변수
#1 - 도전(6 7 8 12 13 14)
#2 - 개인(18 23 24 25 27 29)
#3 - 사회(20 21 22 28 30)
# 종속변수
#1 - 레저(35 36)
#2 - 운동(37 38)
#3 - 취미(41 42 43 44 45)

IND1 <- (a6+a7+a8+a12+a13+a14)/5 
IND2 <- (a18+a23+a24+a25+a27+a29)/6
IND3 <- (a20+a21+a22+a28+a30)/5
DP1 <- (a35+a36)/2
DP2 <- (a37+a38)/2
DP3 <- (a41+a42+a43+a44+a45)/5

head(data)
DT <- cbind(data,IND1,IND2,IND3,DP1,DP2,DP3)
head(DT)

# score저장 
INSC1 <- fit4$scores[,1]
INSC2 <- fit4$scores[,2]
INSC3 <- fit4$scores[,3]

DPSC1 <- fit3$scores[,1]
DPSC2 <- fit3$scores[,2]
DPSC3 <- fit3$scores[,3]

DT <- cbind(DT, INSC1,INSC2,INSC3,DPSC1,DPSC2,DPSC3)
head(DT)
sen <- (a1+a2+a3+a4+a5)/5
DT <- cbind(DT,sen)
attach(DT)
summary(sen)

DT$sen_G[sen<2.800] <- 1
DT$sen_G[sen>=2.800&sen<3.200] <- 2
DT$sen_G[sen>=3.200&sen<3.600] <- 3
DT$sen_G[sen>=3.600] <- 4
head(DT)

attach(DT)
table(sen_G)

# crosstab

Cros_T <- table(Gender, sen_G)
margin.table(Cros_T, 1) # row sum
margin.table(Cros_T, 2) # column sum 
margin.table(Cros_T) # total
# 하나하나는 관측빈도
chisq.test(Cros_T)

install.packages("gmodels")
library(gmodels)
CrossTable(Cros_T, expected=TRUE, prop.c = FALSE, prop.r=FALSE, prop.t=FALSE, )
head(DT)  

# independent sample t test (독립표본 t검정)
var.test(DP1~Gender)
var.test(DP2~Gender)
var.test(DP3~Gender)

t.test(DP1~Gender, alt="two.sided", var.eq=F)
t.test(DP2~Gender, alt="two.sided", var.eq=F)
t.test(DP3~Gender, alt="two.sided", var.eq=F)

var.test(IND1~Gender)
var.test(IND2~Gender)
var.test(IND3~Gender)

t.test(IND1~Gender, alt="two.sided", var.eq=T)
t.test(IND2~Gender, alt="two.sided", var.eq=T)
t.test(IND3~Gender, alt="two.sided", var.eq=T)

# 종속표본 paired t test 사후-사후검정

t.test(a1, a2, alt="two.sided")

# 
install.packages("psych")
library(psych)
describeBy(IND1, Gender)
describeBy(IND2, Gender)
describeBy(IND3, Gender)
describeBy(DP1, Gender)
describeBy(DP2, Gender)
describeBy(DP3, Gender)

describeBy(IND1, sen_G)
describeBy(IND2, sen_G)
describeBy(IND3, sen_G)
describeBy(DP1, sen_G)
describeBy(DP2, sen_G)
describeBy(DP3, sen_G)

# one-way ANOVA

FV1 <- aov(DP1~sen_G, data=DT)
summary(FV1)
FV2 <- aov(DP2~sen_G, data=DT)
summary(FV2)
FV3 <- aov(DP3~sen_G, data=DT)
summary(FV3)

# Scheffe's Multiple Comparison
install.packages("doBy")
install.packages("agricolae")
library(doBy)
library(agricolae)
# "" 따옴표 안에 넣어줘야 한다. 
scheffe.test(FV2, "sen_G", alpha=0.05, console=TRUE)
scheffe.test(FV3, "sen_G", alpha=0.05, console=TRUE)

duncan.test(FV2, "sen_G", alpha=0.05, console=TRUE)
duncan.test(FV3, "sen_G", alpha=0.05, console=TRUE)

# Two-way ANOVA 이원변량분석 / 주효과, 상호작용효과
# aov(DP1~sen_G+Gender+ ) or sen_G*Gender or sen_G:Gender
FFV1 <- aov(DP1~Gender+sen_G+Gender:sen_G)
summary(FFV1)

FFV2 <- aov(DP2~Gender+sen_G+Gender*sen_G)
summary(FFV2)

FFV3 <- aov(DP3~Gender+sen_G+Gender:sen_G)
summary(FFV3)

# correlation Analysis

cor(DP1, DP2)
COR_T <- subset(DT, select=c(IND1,IND2,IND3,DP1,DP2,DP3))
cor(COR_T, method="pearson")

# scatter plot 
plot(DP2, DP3, pch=34)

## Multiple Regression
# 표준화된 회귀 계수
install.packages("lm.beta")
library(lm.beta)

reg1 <- lm(DP1~IND1+IND2+IND3, data=DT)
summary(reg1)
# coefficient 비표준화된 계수 값
coef(reg1)
fitted(reg1)
lm.beta(reg1)
# 잔차 값이 큰 것들을 찾아서 확인해봐야 한다. 
residuals(reg1)
# 신뢰구간 0.34126535 ~ 0.6532407 안에 95% 의 데이터가 들어있다
confint(reg1)
# 예측값 
predict(reg1)
# 오차를 고려 안 한 것이다. confidence는
head(predict(reg1, interval="confidence"))
predict(reg1, interval="prediction")
anova(reg1)
# 더미변수 주의사항 Gender라는 변수가 남자를 1 주고 여자를 2주고, 평균을 1.7+- 1.1 (M+-SD(Standard Deviation))
# Gender는 명목변수이다. 빈도 -> 최빈수 평균을 저렇게 계산하는 건 무리가 있다.  
# 더미는 평균으로 계산 하지 않는다. 약간의 방법을 추가 한다. 남자를 1 주고 여자를 0을 준다. +(남자) -(여자) 
# 독립변수 성별(남) 종교(유) 결혼(기혼) /더미를 할 때

reg2 <- lm(DP2~IND1+IND2+IND3, data=DT)
summary(reg2)

regSC1 <- lm(DPSC1~INSC1+INSC2+INSC3, data=DT)
summary(regSC1)
lm.beta(regSC1)

reg3 <- lm(DP3~IND1+IND2+IND3, data=DT)
summary(reg3)
# Normal Q-Q 45도 선을 잘 따라갈수록 정제를 잘 했다. Residuals vs Fitted가 패턴이 보이면 안 좋다
par(mfrow=c(2,2))
plot(reg1)

resid <- residuals(reg1)
# 정규성을 따르는지 안 따르는지 
shapiro.test(resid)

install.packages("car")
library(car)
# 10 넘어가면 안 된다 
vif(reg2)
# 확인해야 할 것 잔차가 등분산 되는지 /Normal Q-Q/잔차 정규성 검증 /VIP 다중공산성/ 이상치 측 
# 정규화 하는 이유는? 집단 A, B, C가 있다고 하자. 분포가 달라서 비교를 할 수 없다. 여러 집단을 비교하기 위해서 

attach(DT)
DT$G_DM[Gender==1] <- 1
DT$G_DM[Gender==2] <- 0
head(DT)
table(G_DM)

reg3 <- lm(DP3~IND1+IND2+IND3+G_DM, data=DT)
summary(reg3)
coef(reg3)
fitted(reg3)
lm.beta(reg3)

DT$sen_DM1 = ifelse(sen_G==1,1,0)
DT$sen_DM2 = ifelse(sen_G==2,1,0)
DT$sen_DM3 = ifelse(sen_G==3,1,0)
table(DT$sen_G)
table(DT$sen_DM1)
table(DT$sen_DM2)
table(DT$sen_DM3)

reg1 <- lm(DP1~IND1+IND2+IND3+G_DM+sen_DM1+sen_DM2+sen_DM3, data=DT)
summary(reg1)
