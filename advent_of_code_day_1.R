
//the fuel calculation
fuel_calc <- function(mass){
fuel<-(floor(mass/3))-2
if(fuel<0)fuel<-0
return(fuel)
}

//the recursive rocket equation part
rocket_eq <- function(mass){
fuel.next<-fuel_calc(mass)
fuel.total<-fuel.next
	while (fuel.next > 0) {
	fuel.next <- fuel_calc(fuel.next)
	fuel.total <- c(fuel.total,fuel.next)
	}
return(sum(fuel.total))
}

//A is a vector of the module masses
sum(sapply(A,rocket_eq))