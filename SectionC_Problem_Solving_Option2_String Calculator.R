library(stringr)

calculator1 <- function(x) {
	
num<-as.numeric(str_extract_all(x,"\\(?[0-9,.]+\\)?")[[1]])
char<-substr(x, start = 1, stop = 2)


if(char=="fa") { y<-factorial(num)} 
else if(char=="pr"){
	
	primest <- function(n){
    p <- 2:n
    i <- 1
    while (p[i] <= sqrt(n)) {
        p <-  p[p %% p[i] != 0 | p==p[i]]
        i <- i+1
    }
    p
}
     y<-primest(num)[length(primest(num))-1]} 


else if(char=="fi"){ 
	
	fib <- function(n){
  	v=NULL
  	v[1]<-1
  	v[2]<-1 
  	i<-2
  	while(v[i]<=n)
  	{
    	i<-i+1
    	v[i]<-as.numeric(v[i-1])+as.numeric(v[i-2])
  	}
  	p<-v[1:length(v)-1] }

	y<-max(fib(num))
} 

else if(char=="+ ") { y<-sum(num)}
else if(char=="- ") { y<-num[1]-num[2]}
else if(char=="* ") { y<-num[1]*num[2]}	
else if(char=="/ ") { y<-num[1]/num[2]}	
else if(char=="\\t") { y<-num}
else if(as.numeric(substr(x, start = 1, stop = 2))==as.numeric(substr(num, start = 1, stop = 2)) ) { y<-num}
else { y<-c("Try Again")}

return(as.character(y))		
} 

#Example To try :

calculator1("factorial 5")
calculator1("+ 12.5 12.5")


