x <- 10*c(138,312,352,113,103,172,392,149,186,343,200,366,250,122,139)
y <- c(76,216,238,69,50,119,282,81,132,228,145,251,170,71,29)

house <- data.frame(x,y)

plot(house, pch = 16, cex = 2)
reg1 <- lm(y ~ x, data = house) # 계수 도출

abline(reg1, col = "red", lwd = 3) # 회귀선 출력

w0_hat <- reg1$coefficients[0]
w0_hat <- reg1$coefficients[1]

abline(v=2227, lty = 2)

predict(reg1, newdata = data.frame(x=2227)) # 집크기가 2227일때 가격 예측
predict(reg1, newdata = data.frame(x=c(2227,3000))) # 집크기가 2227,3000일때 가격 예측 (벡터형식으로 입력)

house$잔차 <- reg1$residuals # 잔차 계산
sum(house$잔차)
#잔차를 전부더하면 0이 나오지만 여기서는 5.329071e-15 이 계산됨

house$예측 <- reg1$fitted.values # 예측값 계산

View(house)

#---------

x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(3,3,3,6,6,9,9,9,10,11)

test2 <- data.frame(x,y)

plot(test2, pch = 16, cex = 2)
reg2 <- lm(y ~ x, data = test2) # 계수 도출

abline(reg2, col = "red", lwd = 3) # 회귀선 출력

predict(reg2, newdata = data.frame(x=11)) # x가 11일때 y값 예측

#---------

y <- c(30.5,28.0,42.9,52.0,51.5,53.8,25.4,37.2,50.9,29.2)
x <- c(14,9,15,20,21,25,9,13,20,10)

test3 <- data.frame(x,y)

plot(test3, pch = 16, cex = 1)
reg3 <- lm(y ~ x, data = test3) # 계수 도출

abline(reg3, col = "red", lwd = 3) # 회귀선 출력

predict(reg3, newdata = data.frame(x=17)) # 신규고객 1700명을 확보하기 위해 필요한 광고비 : 42.8443억원

#------------- 다중 선형회귀 분석

