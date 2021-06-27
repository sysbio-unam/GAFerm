# Title: Model with inhibition by product and product partially linked to growth 
# Author: Juan Manuel
# Date: 21/06/20
######################################################################

# establish model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # specific growth rate
                mu = mu_max*s/(ks + s)*kp/(kp + p)
                
                # growth rate
                rx = mu*x
                
                # substrate consumption rate
                rs = (1/Yxs)*rx
                
                # product generation rate
                rp = alpha*rx
                
                # mass balance for substrate
                dsdt = - rs
                
                # mass balance for biomass
                dxdt <- rx
                
                # mass balance for product
                dpdt = rp + beta*x
                
                return(list(c(dxdt,dpdt,dsdt)))
        })
}