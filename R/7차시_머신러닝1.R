# install.packages("Deriv")
library(Deriv)

#========== 변수가 1개 일때

f <- function(x) x^2-10*x+26
curve(f,1,9,lwd=2)
x <- 7

alpha <- 0.01 # 학습률

points(x,f(x),col="red", cex=2, pch=16)

f_prime <- Deriv(f, "x")

for(i in 1:1000) {
  x <- x - alpha*f_prime(x)
  points(x, f(x), col="red", cex=2, pch=16)
}

#======== pdf 변수가 2개 일때

f <- function(x_1, x_2) x1^2+x_2^2-2*x_1+x_1*x_2+1
#curve(f,1,9,lwd=2)
x <- c(5,5)

alpha <- 0.1 # 학습률

f_prime_x1 <- Deriv(f,"x_1")
f_prime_x2 <- Deriv(f,"x_2")

for(i in 1:100) {
  grad <- c(f_prime_x1(x[1],x[2]),
            f_prime_x2(x[1],x[2])
            )
  x <- x - alpha*grad
  cat(x,f(x[1],x[2]),"\n")
}

#======================================================================= 1과제!! 변수가 3개 일때

f <- function(x_1, x_2, x_3) (x_1-4)^2+x_3^2*x_1+(x_2+1)^2+6
x <- c(5,5,5)

alpha <- 0.1 # 학습률

f_prime_x1 <- Deriv(f,"x_1")
f_prime_x2 <- Deriv(f,"x_2")
f_prime_x3 <- Deriv(f,"x_3")

for(i in 1:100) {
  grad <- c(f_prime_x1(x[1],x[2],x[3]),
            f_prime_x2(x[1],x[2],x[3]),
            f_prime_x3(x[1],x[2],x[3])
  )
  x <- x - alpha*grad
  cat(x,f(x[1],x[2],x[3]),"\n")
}


#========== df로 계산

df <- data.frame(x = c(1,2.5,3), y = c(1,1.5,3))
plot(df, col="red", pch=16, cex=2)

reg1 <- lm(y ~ x, data = df)
abline(reg1, col="green", lwd=3)

alpha <- 0.07

w <- matrix(c(0,0))
X <- cbind(1, df$x)
y <- matrix(df$y)

for (i in 1:1000) { # 경사하강법으로 결과 도출 과정 (초록색 종국엔 초록색 선과 같아)
  w <- w - alpha*t(X)%*%(X%*%w-y)/length(y)
  abline(a=w[1],b=w[2], col="blue", lwd=2)
}
w # w값 확인

# =========================================================== 2과제!! 전에 했던 state 를 경사하강법으로 추정하기



