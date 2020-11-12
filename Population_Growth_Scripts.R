#Drug Absent: No positive or negative effects on growth rate of the subpop
#Drug Present: Subpop grows at 50% of its growth rate, Non-Mutant pop declines rapidly

#Initial Rates & Carrying Capacity
rn=0.1
rm=0.1
K=1000000

#Initialize Time Vector
t=1:400

#Initialize Population Vectors
N=rep(0, length(t))
M=rep(0, length(t))
N[1]=20

#Growth to EQ without drug
for (i in 1:200){
  if(N[i]< 100){
  N[i+1]=N[i]+(rn*N[i])*(1-(N[i]/K))
  M[i+1]=0
  if(N[i+1]>=100){
   M[i+1]=1 
  }
  } else if(N[i]>=100){
    N[i+1]=N[i]+((rn*N[i])*(1-(N[i]+M[i])/K))
    M[i+1]=M[i]+((rm*M[i])*(1-(N[i]+M[i])/K))
  }
}

#Drug Treatment Administered

#Redefine Growth Rates
rn=-0.1
rm=0.05

for(i in 201:(length(t)-1)){
  N[i+1]=N[i]+((rn*N[i])*(1-(N[i]+M[i])/K))
  M[i+1]=M[i]+((rm*M[i])*(1-(N[i]+M[i])/K))
}

pop <- data.frame("Time" = t, "Non-Mutant Pop" = N, "Mutant Pop"=M)

library(ggplot2)
ggplot(data=pop, aes(x=Time))+
  geom_line(aes(y = Non.Mutant.Pop), color = "darkred") + 
  geom_line(aes(y = Mutant.Pop), color="steelblue", linetype="twodash")+
  scale_y_continuous(breaks = seq(0, 1000000, by = 100000))+
  xlab("Time (0-400)")+
  ylab("Population")+
  ggtitle("Growth of Non-Mutant and Mutant Populations (Drug at t=200)")+
  theme_bw()
