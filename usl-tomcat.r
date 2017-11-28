# 3 parameter USL model for smaato tomcat data
# Created by NJG on Tue Oct  3 16:30:19 2017

# SGI Origin data 
df.tomcat <- 
  read.table("~/Class PDQW 2017/USL Tomcat Models/mohit-data-complete.txt",
                    header=TRUE
)


# ULS 2=param interpolation produced:
# X(1)  = 3.3
# alpha = 2e-4
# beta  = 4e-6
usl.fit <- nls(X_N ~ gamma * N / (1 + alpha * (N - 1) + beta * N * (N - 1)), 
               data=df.tomcat, start=list(gamma=10, alpha=1e-3, beta=1e-6),
               algorithm="port", 
               lower=c(1,0,0), upper=c(200,10e-4,10e-6))

summary(usl.fit)


Nmax <- sqrt((1 - coef(usl.fit)['alpha']) / coef(usl.fit)['beta'])
Xmax <- coef(usl.fit)['gamma'] * Nmax / 
  (1 + coef(usl.fit)['alpha'] * (Nmax - 1) + 
     coef(usl.fit)['beta'] * Nmax * (Nmax - 1))
#Nopt <- abs(1 / coef(usl.fit)['alpha'])
#Xroof <- coef(usl.fit)['gamma'] * Nopt 


plot(df.tomcat$N, df.tomcat$X_N, 
     xlim=c(0,800), ylim=c(0,800), 
     col="gray",
     xlab="Threads (N)", ylab="RPS X(N)")
#abline(v=Nopt, col="gray")
#abline(h=Xroof, col="gray")
#abline(a=0, b=Xroof/Nopt, col="gray")
abline(v=Nmax, col="red")
abline(h=Xmax, col="red")
curve(coef(usl.fit)['gamma'] * x / (1 + coef(usl.fit)['alpha'] * (x - 1) + 
                               coef(usl.fit)['beta'] * x * (x - 1)), 
      from=0, to=800, add=TRUE, col="blue", lwd=2)
title("USL Tomcat Analysis for PDQ Workshop")
text(100, 200, "Tue Oct  3 16:42:52 2017", col="lightgray",cex=0.75)

# legend("bottom", 
#        legend=eval(parse(text=sprintf("expression(alpha == %.4f, beta == %.6f, gamma == %.4f, Nmax==%.2f, Xmax==%.2f,Xroof==%.2f)", coef(usl.fit)['alpha'], coef(usl.fit)['beta'], coef(usl.fit)['gamma'], Nmax, Xmax, Xroof))), ncol=2, inset=0.05, cex=0.75)


legend("bottom",
       legend=eval(parse(text=sprintf("expression(alpha == %.4f, beta == %.6f, gamma == %.4f, Nmax==%.2f, Xmax==%.2f)", coef(usl.fit)['alpha'], coef(usl.fit)['beta'], coef(usl.fit)['gamma'], Nmax, Xmax))), ncol=2, inset=0.05, cex=0.75)





