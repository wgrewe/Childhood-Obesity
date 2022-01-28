# Load Packages----------------------------------------------------------------
library(sf)
library(rgdal)
library(smerc)
library(spdep)
library(rgeos)
library(RColorBrewer)
library(plotrix)

setwd("~/Desktop/Spatial Data/D2P")
data <- get(load(file = "obesity_data_processed.RData"))


#------------------------------------------------------------------------------
# CEPP
#------------------------------------------------------------------------------

# N = 2000---------------------------------------------------------------------

cepp2000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 2000,
                     alpha = 0.05)
summary(cepp2000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp2000), main="Significant Clusters for n* = 2000")

# N = 5000--------------------------------------------------------------

cepp5000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 5000,
                     alpha = 0.05)
summary(cepp5000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp5000), main="Significant Clusters for n* = 5000")


# N = 8000--------------------------------------------------------------

cepp8000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 8000,
                     alpha = 0.05)
summary(cepp8000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp8000), main="Significant Clusters for n* = 8000")


# N = 11000--------------------------------------------------------------

cepp11000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 11000,
                     alpha = 0.05)
summary(cepp11000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp11000), main="Significant Clusters for n* = 11000")


# N = 15000--------------------------------------------------------------

cepp15000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 15000,
                     alpha = 0.05)
summary(cepp15000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp15000), main="Significant Clusters for n* = 15000")


# N = 20000--------------------------------------------------------------

cepp20000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 20000,
                     alpha = 0.05)
summary(cepp20000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp20000), main="Significant Clusters for n* = 20000")

# N = 25000--------------------------------------------------------------

cepp25000 = cepp.test(coords = cbind(data$x, data$y),
                     cases = data$NUMBER_OBESE,
                     pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                     nstar = 25000,
                     alpha = 0.05)
summary(cepp25000)
plot(data$geometry, border = "grey60", axes = FALSE,
     col = color.clusters(cepp25000), main="Significant Clusters for n* = 25000")


#------------------------------------------------------------------------------
# Besag-Newell
#------------------------------------------------------------------------------

# 500------------------------------------------------

bn500 = bn.test(coords = cbind(data$x, data$y),
                 cases = data$NUMBER_OBESE,
                 pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                 cstar = 500,
                 alpha = 0.1)


display.brewer.all(type = "qual", colorblindFriendly = TRUE)
mycol = brewer.pal(length(bn500$clusters), "Dark2")
mycol

data_colors500 = rep("white", length(data$east))


# Plot for bn500
for (i in 1:length(bn500$clusters)){
  data_colors500[bn500$clusters[[i]]$locids] = mycol[i]
}


plot(data$geometry, border="grey60", axes = FALSE, col = color.clusters(bn500),
     main = "Plot of Besag-Newell with c* = 500")


summary(bn500)
data_colors500

data$NBHD_ID

# 1000------------------------------------------------

bn1000 = bn.test(coords = cbind(data$x, data$y),
                 cases = data$NUMBER_OBESE,
                 pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                 cstar = 1000,
                 alpha = 0.1)


display.brewer.all(type = "qual", colorblindFriendly = TRUE)
mycol = brewer.pal(length(bn1000$clusters), "Dark2")

data_colors1000 = rep("white", length(data$east))



# Plot for bn1000
for (i in 1:length(bn1000$clusters)){
  data_colors1000[bn1000$clusters[[i]]$locids] = mycol[i]
}

bn1000$clusters[[1]]$locids
data_colors1000

plot(data$geometry, border="grey60", axes = FALSE, col = color.clusters(bn1000),
     main = "Plot of Besag-Newell with c* = 1000")

summary(bn1000)

# 2000------------------------------------------------

bn2000 = bn.test(coords = cbind(data$x, data$y),
                 cases = data$NUMBER_OBESE,
                 pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                 cstar = 2000,
                 alpha = 0.1)


summary(bn2000)
display.brewer.all(type = "qual", colorblindFriendly = TRUE)
mycol = brewer.pal(length(bn2000$clusters), "Dark2")

data_colors2000 = rep("white", length(data$east))


# Plot for bn2000
for (i in 1:length(bn2000$clusters)){
  data_colors2000[bn2000$clusters[[i]]$locids] = mycol[i]
}

plot(data$geometry, border="grey60", axes = FALSE, col = color.clusters(bn2000),
     main = "Plot of Besag-Newell with c* = 2000")

# 3000------------------------------------------------

bn3000 = bn.test(coords = cbind(data$x, data$y),
                 cases = data$NUMBER_OBESE,
                 pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                 cstar = 3000,
                 alpha = 0.1)

summary(bn3000)
display.brewer.all(type = "qual", colorblindFriendly = TRUE)
mycol = brewer.pal(length(bn3000$clusters), "Dark2")

data_colors3000 = rep("white", length(data$east))


# Plot for bn3000
for (i in 1:length(bn3000$clusters)){
  data_colors3000[bn3000$clusters[[i]]$locids] = mycol[i]
}

plot(data$geometry, border="grey60", axes = FALSE, col = color.clusters(bn3000),
     main = "Plot of Besag-Newell with c* = 3000")

for (i in 1:length(cepp20000$clusters)){
  print(cepp20000$clusters[[i]]$locids)
}
bn500$clusters

plot(data$NUMBER_OBESE/data$COUNT_CHILDREN_INREGISTRYBMI, data$geometry)

#------------------------------------------------------------------------------
# Spatial Scan Method
#------------------------------------------------------------------------------

# Scale 05-------------------------------------------------------
scan05 = scan.test(coords = cbind(data$x, data$y),
                   cases = data$NUMBER_OBESE,
                   pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                   ex = sum(data$NUMBER_OBESE)/sum(data$COUNT_CHILDREN_INREGISTRYBMI) * data$COUNT_CHILDREN_INREGISTRYBMI,
                   nsim = 999,
                   alpha  = 0.10,
                   ubpop = .05)

summary(scan05)
# Plot of Scan with ub = .05
# need to color 3 clusters
mycol_scan = grDevices::hcl.colors(length(scan05$clusters))
# color.clusters(scan, col = mycol) colors the 3 clusters using the desired clusters
plot(data$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan05, col = mycol_scan),
     main = "Plot of Scan Clusters with ub = .05")
# legend("topleft", legend = c("Cluster A", "Cluster B", "Cluster C",
#                             "Cluster D", "Cluster E", "Cluster F",
#                             "Cluster G", "Cluster H"),
#       lwd = 10, col = mycol_scan)

# Scale 1--------------------------------------------------------

scan1 = scan.test(coords = cbind(data$x, data$y),
                  cases = data$NUMBER_OBESE,
                  pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                  ex = sum(data$NUMBER_OBESE)/sum(data$COUNT_CHILDREN_INREGISTRYBMI) * data$COUNT_CHILDREN_INREGISTRYBMI,
                  nsim = 999,
                  alpha  = 0.10,
                  ubpop = .1)
summary(scan1)

# Plot of Scan with ub = .1
mycol_scan = grDevices::hcl.colors(length(scan1$clusters))
plot(data$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan1, col = mycol_scan),
     main = "Plot of Scan Clusters with ub = .10")
# legend("topleft", legend = c("Cluster A", "Cluster B", "Cluster C",
#                             "Cluster D", "Cluster E", "Cluster F",
#                             "Cluster G", "Cluster H"),
#       lwd = 10, col = mycol_scan)

# Scale 15-------------------------------------------------------
scan15 = scan.test(coords = cbind(data$x, data$y),
                   cases = data$NUMBER_OBESE,
                   pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                   ex = sum(data$NUMBER_OBESE)/sum(data$COUNT_CHILDREN_INREGISTRYBMI) * data$COUNT_CHILDREN_INREGISTRYBMI,
                   nsim = 999,
                   alpha  = 0.10,
                   ubpop = .15)
summary(scan15)
# Plot of Scan with ub = .15
mycol_scan = grDevices::hcl.colors(length(scan15$clusters))
plot(data$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan15, col = mycol_scan),
     main = "Plot of Scan Clusters with ub = .15")
# legend("topleft", legend = c("Cluster A", "Cluster B", "Cluster C",
#                             "Cluster D", "Cluster E", "Cluster F",
#                             "Cluster G", "Cluster H"),
#       lwd = 10, col = mycol_scan)

# Scale 20-------------------------------------------------------

scan20 = scan.test(coords = cbind(data$x, data$y),
                   cases = data$NUMBER_OBESE,
                   pop = data$COUNT_CHILDREN_INREGISTRYBMI,
                   ex = sum(data$NUMBER_OBESE)/sum(data$COUNT_CHILDREN_INREGISTRYBMI) * data$COUNT_CHILDREN_INREGISTRYBMI,
                   nsim = 999,
                   alpha  = 0.10,
                   ubpop = .2)

summary(scan20)

# Plot of Scan with ub = .20
mycol_scan = grDevices::hcl.colors(length(scan20$clusters))
plot(data$geometry, border="grey60", axes=FALSE,
     col = color.clusters(scan20, col = mycol_scan),
     main = "Plot of Scan Clusters with ub = .20")


#------------------------------------------------------------------------------
# Moran's I
#------------------------------------------------------------------------------

# Filter out zeros from data set

data_nz <- data[data$NUMBER_OBESE > 0,]


# CR Moran's I

nb <- poly2nb(data_nz$geometry)
w = nb2mat(nb, style = "B")
nsim = 999

N = length(data_nz$NUMBER_OBESE) # number of regions
y = data_nz$NUMBER_OBESE # number of cases
n = data_nz$COUNT_CHILDREN_INREGISTRYBMI #population sizes
r <- sum(y)/sum(n) # estimated risk
rni <- r * n # expected per region

min(data$COUNT_CHILDREN_INREGISTRYBMI)

i_cr = function(y, rni, w) {
  y_std = matrix((y - rni)/sqrt(rni))
  return(sum(w * y_std %*% t(y_std))/sum(w))
}

tsimc = numeric(nsim)
t0c = i_cr(y, rni, w) # observed statistic
# statistics for data simualted under CRH
for (i in 1:nsim) tsimc[i] = i_cr(rpois(N, rni), rni = rni, w = w)
# p-value
(sum(tsimc >= t0c) + 1)/(nsim + 1)

#------------------------------------------------------------------------------
# Geary's C
#------------------------------------------------------------------------------

lw <- nb2listw(nb)
rates <- y/n

t0b = geary(rates, listw = lw, n = N, n1 = N - 1, S0 = Szero(lw))$C
# calculate geary's c for poisson data simulated under crh, after rate
tsimb = numeric(nsim)
# correction
for (i in 1:nsim) {
  tsimb[i] = geary(rpois(N, rni)/n, listw = lw, n = N, n1 = N - 1, S0 = Szero(lw))$C
}

# p-value for geary's c constant risk monte carlo test for incidence rate
(sum(tsimb <= t0b) + 1)/(nsim + 1)


#------------------------------------------------------------------------------
# Tango's Index
#------------------------------------------------------------------------------

coords = as.matrix(cbind(data$x, data$y))
cases = data$NUMBER_OBESE
pop = data$COUNT_CHILDREN_INREGISTRYBMI

# Find distance matrix
d = as.matrix(stats::dist(coords))

w1  <- dweights(coords, kappa = 1)
w7  <- dweights(coords, kappa = 7)

(tango_mc1 <-  tango.test(cases, pop, w1, nsim = 999))
psim1 <- (1 + sum(tango_mc1$tstat.sim >= tango_mc1$tstat))/(1 + 999)
psim1

(tango_mc7 <-  tango.test(cases, pop, w7, nsim = 999))
psim7 <- (1 + sum(tango_mc7$tstat.sim >= tango_mc1$tstat))/(1 + 999)
psim7

tango_mc1
tango_mc7

# Plotting GoF vs Spatial Autocorrelation

hist(tango_mc1$gof.sim, xlim = range(c(tango_mc1$gof.sim, tango_mc1$gof)))
abline(v = tango_mc1$gof)

hist(tango_mc1$sa.sim, xlim = range(c(tango_mc1$sa.sim, tango_mc1$sa)))
abline(v = tango_mc1$sa)

hist(tango_mc7$gof.sim, xlim = range(c(tango_mc7$gof.sim, tango_mc7$gof)))
abline(v = tango_mc7$gof)

hist(tango_mc7$sa.sim, xlim = range(c(tango_mc7$sa.sim, tango_mc7$sa)))
abline(v = tango_mc7$sa)

gof <- c(tango_1$gof,tango_7$gof,
         tango_10$gof,tango_15$gof,
         tango_20$gof)
sa <- c(tango_1$sa,tango_7$sa,
        tango_10$sa,tango_15$sa,
        tango_20$sa)
plot(gof, sa)

plot(tango_mc1, main="Goodness of Fit vs Spatial Autocorrelation for k = 0.1")
plot(tango_mc7, main="Goodness of Fit vs Spatial Autocorrelation for k = 7")
