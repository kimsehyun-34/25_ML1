x1 <- 1:10
x2 <- 3:12
x2[c(5,6)] <- 11

y <- c(4,5,8,10,11,14,16,18,20,20)
df <- data.frame(x1,x2,y)
df

x <- cbind(1,x1,x2) # X메트릭스 생성

w_hat <- solve(t(x) %*% x) %*% t(x) %*% y # w값 도출
w_hat

reg1 <- lm(y ~ ., data=df) # 종속변수 ~ .(나머지 전부)
reg1 # x1 = w1값, x2 = w2값

#=============

state <- as.data.frame(state.x77) # 범죄율 데이터 수집
state <- state[,-c(6,8)] # 불필요한 변수 제거
state

reg2 <- lm(Murder ~ ., data=state)
round(reg2$coefficients,4) # 소수점 4째 자리 반올림

#============ 


