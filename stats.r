data=data.frame(
x<-c(10,15,20,25,30),
y<-c(10,20,30,40,50) )

varx=var(data$x)
vary=var(data$y)

cat("the variance of x is:",varx,"\n")
cat("the variance of y is:",vary,"\n")

plot(data$x,data$y)

rows=apply(data,1,sum)
cat("row sum is:",rows,"\n")
cols=apply(data,2,sum)
cat("column sum is:",cols,"\n")
totals=sum(rows)

expv=outer(rows,cols)/totals

cor_value<-cor(data$x,data$y)
cov_value<-cov(data$x,data$y)

cat("correlation between x & y",cor_value,"\n")
cat("correlation between x & y",cov_value,"\n")
cat("expected value is:",expv,"\n")

chisq.test(data$x)
chisq.test(data$y)

