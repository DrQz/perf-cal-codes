# Apply USL model to Varnish scalability 
# These data are so linear that we only model the Amdahl subset
# Created by NJG on Wednesday, January 9, 2013

df.varnish <- read.table(text="N,X_N
1,1.4
2,2.7
5,6.4
10,12.8
25,32
50,64
75,98
100,131
150,197
250,320
300,392
400,518",
header=TRUE,sep=",")

print(df.varnish)
plot(df.varnish$N, df.varnish$X_N, type="b",main="Raw data")


# Cpalize them datas ...
df.varnish$Cp <- df.varnish$X_N/df.varnish$X_N[1]
# Take a gander at it ...
print("Cpalized data")
print(df.varnish)

#*****************************************
df.varnish$Ep <- df.varnish$Cp/df.varnish$N
if(any(df.varnish$Ep > 1)) { 
	print(df.varnish)
	print("Over achievers: Some efficiencies > 100%")
	stop()
}
#*****************************************
print(df.varnish)

# Standard non-linear least squares (NLS) fit using USL model
#usl <- nls(Cp ~ N / (1 + alpha * (N-1) + beta * N * (N-1)), df.varnish, start=c(alpha=0.01, beta=0.001))
usl <- nls(Cp ~ N / (1 + alpha * (N-1)), df.varnish, start=c(alpha=0.001))

# Look at the fitted USL parameters (alpha,beta)
coef(usl)

# Check statistical parameters
summary(usl)

# Get alpha & beta parameters for use in plot legend
x.coef <- coef(usl)

# Determine sum-of-squares for R-squared coeff from NLS fit
sse <- sum((df.varnish$Cp - predict(usl))^2)
sst <- sum((df.varnish$Cp - mean(df.varnish$Cp))^2)

# Calculate Nmax and X(Nmax) do not apply
#Nmax<-sqrt((1-x.coef['alpha'])/x.coef['beta'])
#Xmax<-df.varnish$X_N[1]* Nmax/(1 + x.coef['alpha'] * (Nmax-1) + x.coef['beta'] * Nmax * (Nmax-1))

# Plot all the results
A <- x.coef['alpha']
B <- 0
Nmax <- NaN
Xmax <- NaN
Xroof <- df.varnish$X_N[1]/A

plot(x <- 0:max(df.varnish$N), x*df.varnish$X_N[1] / (1 + A*(x-1)), 
	type="l",lty="solid",lwd=1,ylab="Throughput X(N)", xlab="Load generators (N)")
title("USL Fit to Varnish Data")
points(df.varnish$N, df.varnish$X_N)
legend("bottom", legend=eval(parse(text=sprintf(
   "expression(alpha == %.4f, beta == %.6f, R^2 == %.4f, Nmax==%.2f, Xmax==%.2f,Xroof==%.2f,Z(sec)==%.2f,TS==%15s)",
   A, B, 1-sse/sst, Nmax, Xmax, Xroof, NaN, format(Sys.time(),"%d%m%y%H%M") ))), ncol=2)
   
# Add linear regression
lm.fit <- lm(X_N ~ N, data=df.varnish)
lines(df.varnish$N, predict(lm.fit),lty="dashed")

# Projection
plot(x <- 0:5000, x*df.varnish$X_N[1] / (1 + A*(x-1)),
     type="l", col="blue", ylim=c(0, Xroof+100), 
     ylab="Throughput X(N)", 
     xlab="Load generators (N)")
points(df.varnish$N, df.varnish$X_N)
lines(x, coef(lm.fit)[2]*x + coef(lm.fit)[1],lty="dashed",col="red")
abline(h=Xroof,lty="dashed",col="red")
title(main="USL Projections for Varnish")




