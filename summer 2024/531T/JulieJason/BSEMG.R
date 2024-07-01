data = read.csv("/home/cern/Desktop/EMGdata_full.csv",header = T)
head(data)

# Baseline deltoid activity <E1>
E1_start = as.numeric(rownames(data[data$Event == "baseline deltoids",]))
E1_end = as.numeric(rownames(data[data$Event == "end of baseline deltoids",]))
Set1 = data[E1_start: E1_end,]

# Deltoid with pectoral activity <E2>
E2_start = as.numeric(rownames(data[data$Event == "internal rotation start pecs",]))
E2_end = as.numeric(rownames(data[data$Event == "internal rotation end pecs",]))
Set2 =data[E2_start: E2_end,]

# Deltoid with revisited pectoral activity <E3>
E3_start = as.numeric(rownames(data[data$Event == "int rot w resistance pecs",]))
E3_end = as.numeric(rownames(data[data$Event == "end of int rotation pecs w resi",]))
Set3 = data[E3_start: E3_end,]

# Shoulder external rotation <E4>
E4_start = as.numeric(rownames(data[data$Event == "ext rot ",]))
E4_end = as.numeric(rownames(data[data$Event == "ext rot end",]))
Set4 =data[E4_start: E4_end,]

# Testing on/off detection
library(biosignalEMG)
newdata = rbind(Set2,Set3)



data <- emg(newdata[,2], samplingrate = 2048, units = "mV")
julie <- data$values   # data
ts.plot(julie)


JasonVersion <- function(datav, ome, lam, eps, maxit = 10000){
	n = length(datav)
	sigmas_data = var(datav)
	sigmas_a_0 = sigmas_data
	sigmas_s_0 = sigmas_data*0.1
	phi_lite<- function(sigs, x){
		-0.5*log(2*pi)-log(sigs)-(x^2)/(2*sigs)
	}
	tb_0 = phi_lite(sigs= sigmas_s_0, datav[1:n])/(phi_lite(sigs= sigmas_a_0, datav[1:n])+phi_lite(sigs= sigmas_s_0, datav[1:n]))
	for(m in 1:maxit){
		sigmas_a_new = sum(tb_0*tb_0*datav*datav)/sum(tb_0*tb_0)
		sigmas_s_new = sum((1-tb_0)*(1-tb_0)*datav*datav)/sum((1-tb_0)*(1-tb_0))
		tb_new = rep(0, n)
		# Following for tb
		for(j in 1:n){
			if(j == 1){
				UP = 2*phi_lite(sigs= sigmas_s_new, datav[j]) - 2*lam* tb_0[j+1] + ome
				DOWN = 2*(phi_lite(sigs= sigmas_a_new, datav[j])+phi_lite(sigs= sigmas_s_new, datav[j])) -2*lam +2*ome
				tb_new[j] = UP/DOWN
			}else if(j == n){
				UP = 2*phi_lite(sigs= sigmas_s_new, datav[j]) - 2*lam* tb_new[j-1] + ome
				DOWN = 2*(phi_lite(sigs= sigmas_a_new, datav[j])+phi_lite(sigs= sigmas_s_new, datav[j]))-2*lam +2*ome
				tb_new[j] = UP/DOWN
			}else{
				UP = 2*phi_lite(sigs= sigmas_s_new, datav[j]) - 2*lam* (tb_0[j+1]-tb_new[j-1]) + ome
				DOWN = 2*(phi_lite(sigs= sigmas_a_new, datav[j])+phi_lite(sigs= sigmas_s_new, datav[j]))-4*lam +2*ome
				tb_new[j] = UP/DOWN
			}
		}
		# Equation 20
		for(j in 1:n){
			if(tb_new[j] < 0){
				tb_new[j] = 0
			}
			if(tb_new[j] > 1){
				tb_new[j] = 1
			}
		}
		print(norm(tb_new-tb_0,type="2"))
		if(norm(tb_new-tb_0,type="2") < eps){
			print("Break by small norm value.")
			break
		}
		tb_0 = tb_new
	}
	print("Finished")
	list(sigmas_a_new, sigmas_s_new, tb_new)
}
result = JasonVersion(julie, ome=5, lam=5, eps=0.00001)
sigmas_a_hat = result[[1]]
sigmas_s_hat = result[[2]]
b_hat = round(result[[3]])

ts.plot(julie, main = "Sample EMG Data", ylab = "mV")

ts.plot(julie, main = "Results with Preprocessing", ylab = "mV")
for (i in 2:length(b_hat)) {
  if (b_hat[i-1] == 0 && b_hat[i] == 1) {
    abline(v = i, col = 'red')
  } else if (b_hat[i-1] == 1 && b_hat[i] == 0) {
    abline(v = i, col = 'green')
  }
}

# A posteriori processing
DeltaEpsilon <- function(datav, k = 2, del = T){
	n = length(datav)
	result = rep(0, n)
	for(i in 1:n){
		if(del == T){ # Here is delta
			lower = max(i-k, 0)
			upper = min(i+k,n)
			result[i] = max(datav[lower:upper])
		}else{ # Here is epsilon
			lower = max(i-k, 0)
			upper = min(i+k,n)
			result[i] = min(datav[lower:upper])
		}
	}
	result
}
k2 = 2
k1 = 1
Step1 = DeltaEpsilon(b_hat, k = k2, del= F)
Step2 = DeltaEpsilon(Step1, k = k2, del= T)
Step3 = DeltaEpsilon(Step2, k = k1, del= T)
Step4 = DeltaEpsilon(Step3, k = k1, del= F)
sum(Step4)

# Only using the delta function
Step_d2 = DeltaEpsilon(b_hat, k = 1200, del= T)

ts.plot(julie, main = "Results with Postprocessing", ylab = "mV")
for (i in 2:length(Step_d2)) {
  if (Step_d2[i-1] == 0 && Step_d2[i] == 1) {
    abline(v = i, col = 'red')
    print(i)
  } else if (Step_d2[i-1] == 1 && Step_d2[i] == 0) {
    abline(v = i, col = 'green')
    print(i)
  }
}


# ma
ts.plot(julie, main = "Results after Postprocessing", ylab = "mV", col = "grey")
overlay <- rep(sqrt(result[[2]]),length(Step_d2))
overlay[Step_d2 == 1] <- sqrt(result[[1]]) 
lines(overlay, col = "black", lwd = 1.5)
lines(-overlay, col = "black", lwd = 1.5)


ts.plot(julie, main = "Results before Postprocessing", ylab = "mV", col = "grey")
overlay.bhat <- rep(sqrt(result[[2]]),length(Step_d2))
overlay.bhat[b_hat == 1] = sqrt(result[[1]])
lines(overlay.bhat, col = "chocolate", lwd = 1.5)
lines(-overlay.bhat, col = "chocolate", lwd = 1.5)


# parse data abs
series.on.abs = abs(julie)
series.off.abs = abs(julie)
series.on[Step_d2 == 0] = 0
series.off[Step_d2 == 1] = 0
plot.ts(series.on, col = "blue", main = "Rectified Data", ylab = "mV")
lines(series.off, col = "grey")

# parse data 
series.on = julie
series.off = julie
series.on[Step_d2 == 0] = 0
series.off[Step_d2 == 1] = 0
plot.ts(series.on, col = "blue", main = "Delineated By Phase", ylab = "mV")
lines(series.off, col = "grey")

qqnorm(julie[27000:37000],main = "active phase")
qqline(julie[27000:37000], col = "red", lty = 2)

qqnorm(julie[10000:20000],main = "silent phase")
qqline(julie[10000:20000], col = "red", lty = 2)

hist(julie[27000:37000],main = "active phase", prob = T, breaks = 100, xlab ="mV")

hist(julie[10000:20000],main = "silent phase", prob = T, breaks = 100, xlab ="mV")

aov(julie[27000:37000] ~ julie[10000:20000]) -> Fstat
summary(Fstat)


julie[Step_d2 == 1] -> active
 julie[Step_d2 == 0] -> silent
 hist(active,breaks =50, freq =  F,
      xlab = "mV",
      main = "Histogram of Results Active Phase")
 hist(silent,breaks =50, freq =  F,
      xlab = "mV",
      main = "Histogram of Results Silent Phase")
 qqnorm(active, main = "Active Phase Data")
 qqline(active, col = "red", lty = 2)
 qqnorm(silent, main = "Silent Phase Data")
 qqline(silent, col = "red", lty = 2)