
#Question1
#Newton Raphson Algorithm

p=2/3 ; #We took initial guess as a 2/3 because 2/3 is estimated value of phat=xhat/n=x/n=2/3
gy=function(p)
{
  return(3*p^4-4*p^3+1/2)
}
g_y=function(p)
{
  return(12*p^3-12*p^2)
}
k=0
for(i in 2:1000)
{
  p[i]=p[i-1]-(gy(p[i-1])/g_y(p[i-1]))
  k=k+1
  a=abs(p[i]-p[i-1])
  if(a<0.000001)
  {
    print(p[i])
    k 
    return(0)
  }    
}
k
print(paste("No. of iterations used",k))
print(paste("Posterior median of p",p[i]))

#EM Algorithm:
#Example 1
n=5
L=1
c=10
k=3
x01=c(3.1,8.2,6.9)
x0t=sum(x01)
d=0.000001
for(i in 2:100)
{
  L[i]=(n/(x0t+(n-k)*(c+(1/L[i-1]))))
  if(abs(L[i]-L[i-1])<d)
  {
    print(paste("No of iteration",i))
    print(paste("Posterior mode ",L[i]))
    break;
  }
}
#Example 2
n=1000
n1=5
k=3
x0=c(3.1,8.2,6.9)
x0t=mean(x0)
c=10
sig=1
mu=3
d=0.000001
e=function(p)
{
  p+sig*(dnorm((c-p)/sig))/(1-pnorm((c-p)/sig))
}
for(i in 2:n)
{
  mu[i]=(k*x0t+(n1-k)*e(mu)[i-1])/n1
  if(abs(mu[i]-mu[i-1])<d)
  {
    print(paste("No of iteration",i))
    print(paste("Posterior mode ",mu[i]))
    break;
  }
}

#Metropolis algorithm
#Question1
A=function(a,b)
{
  (a/b)^5
}
c=0.1
#Sample size
n=500
x = c()         	 # to store values coming from target  
y = c()               # to store values coming from proposal 
x[1]=0.5 

# initial value

for (i in 2:n)
{
  y[i]=runif(1,x[i-1]-c,x[i-1]+c)
  ap=A(y[i],x[i-1])
  ap=ifelse(ap>=1,1,ap)
  ap=ifelse(y[i]<0,0,ap)
  ap=ifelse(y[i]>1,0,ap)
  u=runif(1,0,1)
  
  x[i]=ifelse(u<=ap,y[i],x[i-1])
}
x
x=x[100:n];x
hist(x,freq=FALSE)
hist(rbeta(400,6,1),freq=FALSE,add=T,col = "red")


#Question2
c=4
#Sample size
n=10000
x = c()         	 # to store values coming from target  
y = c()               # to store values coming from proposal 
x[1]=0.5 
r=4   #tunning parameter 
# initial value

for (i in 2:n)
{
  
  y[i]=runif(1,x[i-1]-c,x[i-1]+c)
  ap=dnorm(y[i])/dnorm(x[i-1])
  ap=ifelse(ap>=1,1,ap)
  
  u=runif(1,0,1)
  
  x[i]=ifelse(u<=ap,y[i],x[i-1])
}
x
x=x[100:n];x
x=x[seq(1,9900,by=25)];x
n1=length(x)
sm=mean(x);sm    #sample mean
sv=sqrt(((n1-1)/n1)*var(x));sv   #sample sd
#95% confidence interval
lb=sm-(-qt(0.025,n1-1))*(sqrt(n1-1)*sv)/n1;lb
ub=sm+(-qt(0.025,n1-1))*(sqrt(n1-1)*sv)/n1;ub
print(paste('95% confidence interval for sample mean is lb and ub',lb,ub))
#Plot Probability Histogram
hist(x,freq=FALSE)    
hist(rnorm(n1,0,1),freq=FALSE,add=T,col = "red")