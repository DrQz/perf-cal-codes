# Performance Calendar 2017
# USL3 model for Tomcat example
# Created by NJG on Mon Nov 27, 2017

# Get the performance data from a file
df.tomcat <- 
  read.table("~/GitHub/perf-cal-codes/tc-data.txt", header=TRUE)

# Do the USL nonlinear regression analysis
usl.tc.fit <- nls(X_N ~ A * N / (1 + B * (N - 1) + C * N * (N - 1)), 
               data=df.tomcat, 
               start=list(A=10, B=1e-3, C=1e-6),
               algorithm="port", 
               lower=c(1,0,0), upper=c(200,10e-4,10e-6)
               )

print(summary(usl.tc.fit))

# Find the peak USL parameters
Nmax <- sqrt((1 - coef(usl.tc.fit)['B']) / coef(usl.tc.fit)['C'])
Xmax <- coef(usl.tc.fit)['A'] * Nmax / 
  (1 + coef(usl.tc.fit)['B'] * (Nmax - 1) + 
     coef(usl.tc.fit)['C'] * Nmax * (Nmax - 1))

# Create a plot
plot(df.tomcat$N, df.tomcat$X_N, 
     xlim=c(0, 800), ylim=c(0, 900), 
     col="gray",
     xlab="Threads (N)", ylab="RPS X(N)")
abline(v=Nmax, col="red")
abline(h=Xmax, col="red")
curve(coef(usl.tc.fit)['A'] * x / 
        (1 + coef(usl.tc.fit)['B'] * (x - 1) + 
           coef(usl.tc.fit)['C'] * x * (x - 1)), 
      from=0, to=800, add=TRUE, col="blue", lwd=2)
title("USL Tomcat Analysis")

# Add a legend
legend("bottom",
       legend=eval(parse(text=sprintf("expression(A == %.4f, B == %.4f, C == %.6f,  Nmax==%.2f, Xmax==%.2f)", coef(usl.tc.fit)['A'], coef(usl.tc.fit)['B'], coef(usl.tc.fit)['C'],  Nmax, Xmax))), ncol=2, inset=0.05, cex=0.75)





