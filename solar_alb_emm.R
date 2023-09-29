# Define the Stefan-Bolzmann constant
sigma=5.67e-8
#
# the time span to explore
N = 46
# Define variables to be used
year = numeric(N)
solar = numeric(N)
alpha = numeric(N)
emm = numeric(N)
Tbb = numeric(N)
Talb = numeric(N)
Talbemm = numeric(N)
#
# Define the initial values
year[1] = -4.5
solar[1] = 996
alpha[1] = 0.3
emm[1] = 0.62
#
# loop
for (i in 1:N) {
# only do the lines within these curly brackets if i is greater than 1:
    if (i>1) {
      year[i] = year[i-1] + 0.1
      solar[i] = solar[i-1] + 8.2
      alpha[i] = alpha[i-1] + 0.001
      emm[i] = emm[i-1]
    }
# but calculate Tbb for every timestep:
    Tbb[i] = (solar[i]/(4*sigma))^(1/4)
    Talb[i] = ((1-alpha[i])*solar[i]/(4*sigma))^(1/4)
    Talbemm[i] = ((1-alpha[i])*solar[i]/(4*sigma*emm[i]))^(1/4)
}
Tbb=Tbb-273.15
Talb=Talb-273.15
Talbemm=Talbemm-273.15
#
# Now do some plots
mydata = data.frame(year, solar, Tbb, Talb, Talbemm)
plot(year, Tbb, type='l', ylab='Equilibrium temperature, degreeC', xlab='Billions
     of years since formation of the Earth', main='Temperature over the last 4.5
     billion years')
plot(year, Talb, type='l')
plot(year, Talbemm, type='l')
#
print('finished!!')
