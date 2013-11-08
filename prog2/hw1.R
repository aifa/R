simulateAR1 <- function(e_t, phi, x_0){

	sums <- array(length(e_t))

	for (i in seq_along(e_t)){
		if (i>1){
			sums[i] = phi * sums[i-1] + e_t[i]
		}else{
			sums[i] = 0
		}
	}
	sums
}

simulateR <- function(d_t, x_val){

	sums <- array(length(d_t))

	for (i in seq_along(d_t)){
		if (i>1){
			sums[i] = x_val[i-1] + d_t[i]
		}else{
			sums[i] = 0
		}
	}
	sums
}

simulateSum <- function(e_t, phi, x_0){

	sums <- array(length(d_t))

	for (i in seq_along(d_t)){
		if (i>1){
			sums[i] = (phi+phi^2+phi^3+phi^4+phi^50)*x_val[i-1] + (1+phi^2+phi^3+phi^4)*e_t[i] + d_t[i]
		}else{
			sums[i] = 0
		}
	}
	sums
}

cov_matrix <- matrix(nrow=2, ncol=2)
cov_matrix[1, 1]<-0.018
cov_matrix[1, 2]<-0
cov_matrix[2, 2]<-0.18*sqrt(1-(-0.80756^2))
cov_matrix[2, 1]<- (-0.80756)*0.18

set.seed(10)

w_t = rnorm(1000000, sd=0.18^2)
z_t = rnorm(1000000, sd=0.018^2)
value_mtrx = rbind(z_t, w_t)
data<-cov_matrix%*%value_mtrx
e_t<-z_t
d_t<-w_t
x_val<-simulateAR1(e_t, 0.94, 0)
y_val<-simulateR(d_t, x_val)
fit <- lm(y_val~ x_val)
summary(fit)
