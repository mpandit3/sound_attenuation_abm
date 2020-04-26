#Atmospheric Sound attenuation function.

att_atm <- function(f, d, Pa, T_kel){}


#alpha attenuation

#frequency (Hz)
f = 10000

#ambient pressure in kPa
Pa = 101.325

#reference pressure in kPa
Pr = 101.325

#relative pressure
P_rel = Pa/Pr

Kelvin = 273.15

#Temperature K
T_kel = 293

#reference Temp K
T_ref <- 293.15

#temperature ratio or relative temperature
T_rel <- T_kel/T_ref

#triple-point isotherm temperature in °K
T_01 = Kelvin + 0.01


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

H = 1.63


P_sat_over_P_ref = Math.pow(10,((-6.8346 * Math.pow((T_01 / T_kel), 1.261)) + 4.6151))


Fro = P_rel * (24 + 40400 * H * (0.02 + H) / (0.391 + H));


Frn = P_rel / Math.sqrt(T_rel) * (9 + 280 * H * Math.pow(e,(-4.17 * (Math.pow(T_rel,(-1/3)) - 1))));

Xc = 0.0000000000184 / P_rel * Math.sqrt(T_rel);
Xo = 0.01275 * Math.pow(e,(-2239.1 / T_kel)) * Math.pow((Fro + (sqr(f) / Fro)), -1);
Xn = 0.1068 * Math.pow(e,(-3352 / T_kel)) * Math.pow((Frn + (sqr(f) / Frn)), -1);

Alpha = 20 * log10(e) * sqr(f) * (Xc + Math.pow(T_rel,(-5/2)) * (Xo + Xn));

#molar concentration of watr vapor (%)
nfrequency
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

