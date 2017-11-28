# Performance Calendar 2017
# USL3 model for Varnish example
# Created by NJG on Mon Nov 27, 2017

# Scaling performance data
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
header=TRUE, 
sep=","
)


# Do the USL nonlinear regression analysis
usl.varn.fit <- nls(X_N ~ A * N / (1 + B * (N - 1) + C * N * (N - 1)), 
                  data=df.varnish, 
                  start=list(A=10, B=1e-3, C=1e-6),
                  algorithm="port", 
                  lower=c(1,0,0), upper=c(200,10e-4,10e-6)
)

print(summary(usl.varn.fit))

# Find the peak USL parameters
Nmax <- sqrt((1 - coef(usl.varn.fit)['B']) / coef(usl.varn.fit)['C'])
Xmax <- coef(usl.varn.fit)['A'] * Nmax / 
  (1 + coef(usl.varn.fit)['B'] * (Nmax - 1) + 
     coef(usl.varn.fit)['C'] * Nmax * (Nmax - 1))

# Create a plot
plot(df.varnish$N, df.varnish$X_N, 
     xlim=c(0, 800), ylim=c(0, 900), 
     col="darkgray",
     xlab="Clients (N)", ylab="RPS X(N)")
curve(coef(usl.varn.fit)['A'] * x / 
        (1 + coef(usl.varn.fit)['B'] * (x - 1) + 
           coef(usl.varn.fit)['C'] * x * (x - 1)), 
      from=0, to=800, add=TRUE, col="blue", lwd=2)
title("USL Varnish Analysis")

# Add a legend
legend("bottom",
       legend=eval(parse(text=sprintf("expression(A == %.4f, B == %.6f, C == %.6f,  Nmax==%.2f, Xmax==%.2f)", coef(usl.varn.fit)['A'], coef(usl.varn.fit)['B'], coef(usl.varn.fit)['C'],  Nmax, Xmax))), ncol=2, inset=0.05, cex=0.75)





