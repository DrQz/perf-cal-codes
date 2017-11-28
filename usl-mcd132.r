# Performance Calendar 2017
# USL3 model for Memcached example
# Created by NJG on Mon Nov 27, 2017

# Get mcd-132 performance data from a file
df.mcd132 <- 
  read.table("~/GitHub/perf-cal-codes/mcd132-data.txt", header=TRUE)

# Do the USL nonlinear regression analysis
usl.mcd132.fit <- nls(X_N ~ A * N / (1 + B * (N - 1) + C * N * (N - 1)), 
               data=df.mcd132, 
               start=list(A=10, B=1e-2, C=1e-2),
               algorithm="port", 
               lower=c(10,0,0), upper=c(20,10e-2,10e-2)
               )

print(summary(usl.mcd132.fit))

# Find the peak USL parameters
Nmax <- sqrt((1 - coef(usl.mcd132.fit)['B']) / coef(usl.mcd132.fit)['C'])
Xmax <- coef(usl.mcd132.fit)['A'] * Nmax / 
  (1 + coef(usl.mcd132.fit)['B'] * (Nmax - 1) + 
     coef(usl.mcd132.fit)['C'] * Nmax * (Nmax - 1))

# Create a plot
plot(df.mcd132$N, df.mcd132$X_N, 
     xlim=c(0, 50), ylim=c(0, 400), 
     col="darkgray",
     xlab="Threads (N)", ylab="OPS X(N)")
abline(v=Nmax, col="red")
abline(h=Xmax, col="red")
curve(coef(usl.mcd132.fit)['A'] * x / 
        (1 + coef(usl.mcd132.fit)['B'] * (x - 1) + 
           coef(usl.mcd132.fit)['C'] * x * (x - 1)), 
      from=0, to=50, add=TRUE, col="blue", lwd=2)
title("USL Memcached v132 Analysis")

# Add a legend
legend("bottom",
       legend=eval(parse(text=sprintf("expression(A == %.4f, B == %.4f, C == %.6f,  Nmax==%.2f, Xmax==%.2f)", coef(usl.mcd132.fit)['A'], coef(usl.mcd132.fit)['B'], coef(usl.mcd132.fit)['C'],  Nmax, Xmax))), ncol=2, inset=0.05, cex=0.75)





