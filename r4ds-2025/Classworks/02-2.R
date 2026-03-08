x=choose(4,2)
y=choose(10,3)
print(y)

x=factorial(15)
print(x)
x=sample(1:5,3) #pick3 random distinct
print(x)
seed(111)
x=sample(1:5,3,replace=TRUE)
print(x)
x=sample(0:1,30,nrow=5)
y=matrix(x,nrow=5)
print(y)



n_people <- 164
n_days <-365
n_iter<-1000
res<-rep(0,n_iter)
for (i in 1:n_iter){
  days<-sample(1:365,n_people,replace=TRUE)
  res[i]<-length(unique(days))-n_people==0
}
mean(res)
sum(res)/length(res)
