#Atmospheric Sound attenuation function.

att_atm <- function(f, d, T_cel, h_rel Pa=101.325){}
#f=frequency, d = distance, T_cel = temperature in celcius
#h_rel = Relative humidity (percent - "70" = 70% humidity)
#Pa = ambient pressure (101.325 is standard)

#alpha attenuation

#frequency (Hz)
f = 10000

#relative humidity
h_rel = 70

#ambient pressure in kPa
Pa = 101.325

#reference pressure in kPa
Pr = 101.325

#relative pressure
P_rel = Pa/Pr

#standard temp in Kelvin
Kelvin = 273.15

T_cel = 20

#Temperature K
T_kel = T_cel + Kelvin

#reference Temp K
T_ref <- 293.15

#temperature ratio or relative temperature
T_rel <- T_kel/T_ref

#triple-point isotherm temperature in °K
T_01 = Kelvin + 0.01

P_sat_over_P_ref = 10^((-6.8346 * (T_01 / T_kel)^1.261) + 4.6151)
#0.02284741
H = h_rel * (P_sat_over_P_ref/P_rel)

Fro = P_rel * (24 + 40400 * H * (0.02 + H) / (0.391 + H))

Frn = P_rel / sqrt(T_rel) * (9 + 280 * H * exp(-4.17 * (T_rel^(-1/3) - 1)))

Xc = 0.0000000000184 / P_rel * sqrt(T_rel)
Xo = 0.01275 * exp(-2239.1 / T_kel) * (Fro + (f^2 / Fro))^-1
Xn = 0.1068 * exp(-3352 / T_kel) * (Frn + (f^2 / Frn))^-1
Alpha = 8.68589 * f^2 * (Xc + T_rel^(-5/2) * (Xo + Xn))


