#Atmospheric Sound attenuation model.

#alpha attenuation

#frequency (Hz)
f = 10000

#ambient pressure in kPa
Pa = 101.325

#reference pressure in kPa
Pr = 101.325

Prat = Pa/Pr
P_rel = Prat

#Temperature K
Temp = 293
T_kel = Temp

#reference Temp K
Temp0 <- 293.15

#temperature ratio
Trat <- Temp/Temp0
T_rel <- Trat

#triple-point isotherm temperature in °K
T01 = 273.16 

#Equations from Rickey et al, Noise Control Eng. J. 55 (6), 2007 Nov-Dec
#Note mistake in Eq 3 - parenthesis not aligned. 
#relative humidity
hrel = 70

V = 10.79586*(1-T01/Temp) - 5.02808*log10(Temp/T01) + 
  1.50474e-4*(1-10^(-8.29692*(Temp/T01-1))) + 
  4.2873e-4*(-1+10^(-4.76955*(1-T01/Temp))) - 
  2.2195983

psat = Pr*10^V

h = hrel*(psat/Pr)*(Pa/Pr)^-1

#Taken from http://resource.npl.co.uk/acoustics/techguides/absorption/
#psat_pref = 10^(-6.8346*(T01/Temp)^1.261 + 4.6151)
#h = hrel*((psat_pref)/(Pa/Pr))
#still don't get exactly the same answer.

#Oxygen relaxation frequency
FrO <- Prat*(24 + ((4.04e+4*h)*(0.02+h)/(0.391+h)))
#from online calculator
Fro = Prat * (24 + 40400 * h * (0.02 + h) / (0.391 + h))

#Nitrogen relaxation frequency
FrN <- Prat*Trat^(-.5) * (9 + 280*h * exp(-4.17*(Trat^(-1/3)-1)))
#from online calculator
#Frn = P_rel / Math.sqrt(T_rel) * (9 + 280 * H * Math.pow(e,(-4.17 * (Math.pow(T_rel,(-1/3)) - 1))));
Frn =  Prat  / sqrt(Trat)       * (9 + 280 * h * exp(-4.17 * (Trat^(-1/3) -1)))

#from Yang Atmospheric acoustics
#FrN <- Prat*Trat^(-.5) * (9 + 350*h*exp(-6.142*(Trat^(-1/3)-1)))
#or
#FrN <- Prat*(9+200*h)

alpha <- 8.686*f^2*(1.84e-11*Prat^-1*Trat^0.5) +
         Trat^(-5/2)*(0.01275*(exp(-2239.1/Temp))*(FrO/(FrO^2+f^2))) +
         Trat^(-5/2)*(0.1068*(exp(-3352/Temp))*(FrN/(FrN^2+f^2)))

#from online calculator
Math.sqrt <- function(n){
  return(sqrt(n))
}

Math.pow <- function(n,p){
  return(n^p)
}

sqr <- function(n){
  return(n^2)
}

e = exp(1)

Xc = 0.0000000000184 / P_rel * Math.sqrt(T_rel)
Xc = 0.0000000000184 / Prat * sqrt(Trat)
Xo = 0.01275 * Math.pow(e,(-2239.1 / T_kel)) * Math.pow((Fro + (sqr(f) / Fro)), -1)
Xo = 0.01275 * exp(-2239.1 / Temp) * (FrO + (f^2 / FrO))^-1
Xn = 0.1068 * Math.pow(e,(-3352 / T_kel)) * Math.pow((Frn + (sqr(f) / Frn)), -1)
Xn = 0.1068 * exp(-3352 / Temp) * (FrN + (f^2 / FrN))^-1

Alpha = 20 * log10(e) * sqr(f) * (Xc + Math.pow(T_rel,(-5/2)) * (Xo + Xn))

Alpha = 20 * log10(exp(1)) * f^2 * (Xc + Trat^(-5/2) * (Xo + Xn))
  

              −08 􏰉 log10􏰁T/T01􏰂
+ 1.50474 􏰉 10−4􏰅1 − 10−8.29692􏰃􏰁T/T01􏰂−1􏰄􏰆 + 0.42873 􏰉 10−3􏰅− 1 + 104.76955􏰃1−􏰁T01/T􏰂􏰄􏰆
− 2.2195983 􏰁6􏰂
where: T01=273.16 °K, triple-point isotherm tempera- ture.




#molar concentration of watr vapor (%)
h = 1.63

#Oxygen relaxation frequency
# paxp(-4.17*((Temp/Temp0)^(-1/3)*-1)))

#alpha <- 869*(f^2)*(((1.84e-11)*((Temp/Temp0)^(0.5))) + ((Temp/Temp0)^(-5/2)) * ( 0.01275*(exp(-2239.1/Temp)/((FrO+f^2)/FrO)) + 0.1068*(exp(-3352/Temp)/((FrN+f^2)/FrN)) ) ) 

alf <- function(f, h, FrO, FrN, Temp, Temp0) {
  alpha <- 8.69*(f^2)*(((1.84e-11)*((Temp/Temp0)^(0.5))) + ((Temp/Temp0)^(-5/2)) * ( 0.01275*(exp(-2239.1/Temp)/((FrO^2+f^2)/FrO)) + 0.1068*(exp(-3352/Temp)/((FrN^2+f^2)/FrN)) ) ) 
  return(alpha)
}

classical <- function(f, h, FrO, FrN, Temp, Temp0) {
  cla <- 869*(f^2)*(((1.84e-11)*((Temp/Temp0)^(0.5))) )
  return(cla)
}

oxy <- function(f, h, FrO, FrN, Temp, Temp0) {
  ox <-(f^2)* 100*(Temp/Temp0)^(-5/2) *  0.01275*exp(-2239.1/Temp)*(FrO/(FrO^2+f^2)) 
  return(ox)
}

nit <- function(f, h, FrO, FrN, Temp, Temp0) {
  ni <- (f^2)* 100*(Temp/Temp0)^(-5/2) *  0.1068*exp(-3352/Temp)*(FrN/(FrN^2+f^2))  
  return(ni)
}


df = data.frame(freq=10^(seq(1.5,6,.1)))
df$alpha <- alf(df$freq, h, FrO, FrN, Temp, Temp0) *100
df$classical <- classical(df$freq, h, FrO, FrN, Temp, Temp0)
df$oxygen <- oxy(df$freq, h, FrO, FrN, Temp, Temp0) 
df$nitrogen <- nit(df$freq, h, FrO, FrN, Temp, Temp0)
df$alpha2 <- df$classical+df$oxygen+df$nitrogen

plot(df$freq, df$alpha2, log="xy", ylim = c(10^-2.2, 10^4))
points(df$freq, df$classical, col = "green" )
points(df$freq, df$oxygen, col = "blue")
points(df$freq, df$nitrogen,  col = "red")

