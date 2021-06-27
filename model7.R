# Model1: Fed-Batch with a set operation volume
# Author: Juan Manuel Gutiérrez García
# Date: 17/07/20
# Note: This model considers a kinetic growth type Monod. The operational parameters are Fint, Fout, Sin and Vlim.
##########################################################################################################################

# Set model
model <- function(time, parms ,state) {
        
        with(as.list(c(parms, state)),{
                
                if(V >= Vlim || time >= tf) {
                        
                        Q <- 0
                        
                } 
                
                #  specific growth rate 
                mu = mu_max*s/(ks+ s + ki*s^2)
                
                # growth rate
                rx = mu*x
                
                # substrate consumption rate
                rs = (1/Yxs)*rx
                
                # product generation rate
                rp = Ypx*rx
                
                # general mass balance
                dVdt = q
                
                # mass balance for biomass
                dxdt = rx - (q/V)*x
                
                # mass balance for substrate
                dsdt = - rs + (q/V)*(sin - s)
                
                # mass balance for product
                dpdt = rp - (q/V)*p
                
                return(list(c(dxdt,dpdt,dsdt,dVdt), Q = Q))
        })
}

