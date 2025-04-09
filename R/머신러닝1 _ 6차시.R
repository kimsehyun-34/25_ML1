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

#---------

x <- 10*c(1,2,3,4,5,6,7,8,9,10)
y <- c(3,3,3,6,6,9,9,9,10,11)

test2 <- data.frame(x,y)

plot(test2, pch = 16, cex = 2)
reg2 <- lm(y ~ x, data = test2) # 계수 도출

abline(reg2, col = "red", lwd = 3) # 회귀선 출력

w0_hat <- reg1$coefficients[0]
w0_hat <- reg1$coefficients[1]

predict(reg2, newdata = data.frame(x=11)) # x가 11일때 y값 예측
