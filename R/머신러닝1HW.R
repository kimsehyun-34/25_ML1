library(Deriv)

f <- function(x_1, x_2, x_3) (x_1-4)^2+x_3^2*x_1+(x_2+1)^2+6
x <- c(2,2,2)

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


#===========


library(Deriv)

state <- as.data.frame(state.x77)
state_scaled <- as.data.frame(scale(state))

X <- as.matrix(cbind(1, state_scaled[, -which(names(state_scaled) == "Murder")])) # 독립 변수
y <- state_scaled$Murder # 종속 변수

beta <- rep(0, ncol(X)) 
alpha <- 0.001 # 학습률

# 함수 정의
f <- function(beta, X) {
  return(X %*% beta)
}

# 계산
f_prime <- function(beta, X, y) {
  y_pred <- f(beta, X)
  error <- y_pred - y
  grad <- t(X) %*% error / length(y)
  return(grad)
}

for (i in 1:10000) {
  grad <- f_prime(beta, X, y)
  beta <- beta - alpha * grad
  
  if (i %% 500 == 0 || i == 1) { # 500의 배수만 보이도록
    y_pred <- f(beta, X)
    loss <- mean((y_pred - y)^2)
    cat(sprintf("Epoch %d: Loss = %.6f | Beta = %s\n", 
                i, loss, paste(round(beta, 4), collapse = ", ")))
  }
}
