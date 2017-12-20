data <- read.csv(file.choose(), header = T)
head(data)

attach(data)
a1
table(a1)
table(Gender)
test1 <-subset(data, select =c("a18","a20",
                               "a21","a22",
                               "a23","a24","a25","a27","a28","a29","a30"
                               ))
head(test1)               
fit <- factanal(test1, factors = 2, rotation = 'varimax')
print(fit, cutoff=0.4, digits = 3)

# 내가 알고 싶은 것 종속 변수 
# 독립변수 종속 변수에 영향을 주는 것 