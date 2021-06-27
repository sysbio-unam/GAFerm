# Title: Modelo with inhibition by substrate
# Author: Juan Manuel
# Date: 21/06/20
######################################################################

# establish model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # specific growth rate
                mu = mu_max*s/(ks + s + ki*s^2)
                
                # growth rate
                rx = mu*x
                
                # substrate consumption rate
                rs = (1/Yxs)*rx
                
                # product generation rate
                rp = Ypx*rx
                
                # mass balance for substrate
                dsdt = - rs
                
                # mass balance for biomass
                dxdt <- rx
                
                # mass balance for product
                dpdt = rp

                return(list(c(dxdt,dpdt,dsdt)))
        })
}

# set parameters
#p <- c(mu_max = 0.5, ks = 80, Yxs = 0.8, Ypx = 12, ki = 0.3)