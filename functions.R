####################################################################################### 
# Title: Functions for optimisation app
# Author: Juan Manuel Gutiérrez García
# Date: January 2021
#########################################################################################

# Simulation section ####################################################################
# Batch process  subsection #######################
# make_sim_rec_sim ####
make_sim_fun_sim <- function(s, p, step, interval) {
        
        times <- seq(0,interval,step)
        
        data <- ode(y = s,
                    times = times,
                    func = model,
                    parms = p,
                    method = "rk4") %>% as.data.frame() 
        
        return(data)
        
}

# plot_sim_fun_sim ####
plot_sim_fun_sim <- function(data) {
        
        ggplot(data) + 
                geom_line(mapping = aes(x = time, y = s, color = "s"), size = 1.5) + 
                geom_line(mapping = aes(x = time, y = x, color = "x"), size = 1.5) +
                geom_line(mapping = aes(x = time, y = p,color = "p"), size = 1.5) + 
                labs(x = "Time (h)", y = "Concentration (g/L)", color = NULL) +
                theme_bw() +
                theme(
                        axis.text = element_text(size = 10, face = "bold",
                                                 colour = "black",hjust = "0.5",vjust = "1"),
                        
                        axis.title.y = element_text(size = 15, angle = 90,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 1),
                        
                        axis.title.x = element_text(size = 15, angle = 0,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 0),
                        legend.position  = "top",
                        
                        legend.justification  = "top",
                        
                        legend.title = element_text(size = 15, face = "bold"),
                        
                        legend.text = element_text(size = 15, colour = "black",face = "bold")
                )
}

# end_conc_fun_sim ####
end_conc_fun_sim <- function(data) {
        
        n <- nrow(data)
        
        end_conc <- list(
                
                x = paste0(round(data$x[n], 3), " (g/L)"),
                p = paste0(round(data$p[n], 3), " (g/L)"),
                s = paste0(round(data$s[n], 3), " (g/L)")
                        
                        )
        
        return(end_conc)
        
}
# Fed-batch simulation subsection #################
# fed_batch_fun_fb_sim ####
fed_batch_fun_fb_sim <- function(s, p, step, end_time) {
        
        out <- ode(y = s, times = seq(0, end_time, step), func = model_fb_fun_fb_sim, parms = p, method = "rk4") %>%
                
                as.data.frame()
        
}

# model_fb_fun_fb_sim #####
model_fb_fun_fb_sim <-function(time, parms ,state) {
        
        with(as.list(c(parms, state)),{
                
                if(V >= Vlim || time >= tf) {
                        
                        q <- 0
                        
                } 
                
                #  specific growth rate 
                mu = mu_max*s/(ks + s + ki*s^2)
                
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
                dsdt = - rs + (q/V)*(sf - s)
                
                # mass balance for product
                dpdt = rp - (q/V)*p
                
                return(list(c(dxdt,dpdt,dsdt,dVdt), q = q))
        })
}

# plot_var_fun_fb_sim #####
plot_var_fun_fb_sim <- function(data, var, label) {
        
        ls <- max(data[,var], na.rm = TRUE)
        
        ggplot(data = data, aes(x = time)) +
                
                geom_line(aes(y = .data[[var]]), size = 1, alpha = 1/2) +
                
                ylab(label) +
                
                xlab("Time (h)") +
                
                theme_light() +
                
                theme(
                        axis.text = element_text(size = 10, face = "bold",
                                                 colour = "black",hjust = "0.5",vjust = "1"),
                        
                        axis.title.y = element_text(size = 10, angle = 90,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 1),
                        
                        axis.title.x = element_text(size = 10, angle = 0,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 0)
                )
}

# Optimization section ####################################################################
# Batch process subsection #################################
# plot_data_fun_opt ####
plot_data_fun_opt <- function(data) {
        ggplot(data) + 
                geom_point(mapping = aes(x = time, y = s, color = "s"), size = 2) + 
                geom_point(mapping = aes(x = time, y = x, color = "x"), size = 2) +
                geom_point(mapping = aes(x = time, y = p,color = "p"), size = 2) + 
                labs(x = "Time (h)", y = "Concentration (g/L)", color = NULL) +
                theme_bw() +
                theme(
                        axis.text = element_text(size = 10, face = "bold",
                                                 colour = "black",hjust = "0.5",vjust = "1"),
                        
                        axis.title.y = element_text(size = 15, angle = 90,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 1),
                        
                        axis.title.x = element_text(size = 15, angle = 0,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 0),
                        legend.position  = "top",
                        
                        legend.justification  = "top",
                        
                        legend.title = element_text(size = 15, face = "bold"),
                        
                        legend.text = element_text(size = 15, colour = "black",face = "bold")
                )
}

# get_parms_fun_opt #####
get_parms_fun_opt <- function(data, p_val, p_name, p_range, pop_size, 
                              generations, run, cross_prob, mut_prob, max_fit) {
        
        # Select parameters to optimize ####
        opt_parms <- p_val[p_name]

        # No optimized parameters
        no_opt_parms <- p_val[!(names(p_val) %in% names(opt_parms))]

        # Select interval of parameters to optimize
        opt_p_range <- p_range[p_name]

        lower <- sapply(opt_p_range, min)

        upper <- sapply(opt_p_range, max)

        # Using GA to get parameters #####
        GA <- ga(type = 'real-valued',
                        fitness = fitness_fun_opt,
                        data,
                        opt_parms,
                        no_opt_parms,
                        lower = lower,
                        upper = upper,
                        crossover = gareal_blxCrossover, # To improve the optimization
                        popSize = pop_size, #
                        pcrossover = cross_prob, #
                        pmutation = mut_prob, #
                        names = names(opt_parms),
                        maxiter = generations, #10
                        maxFitness = -max_fit,
                        run =  run)

        # Optimized parameters
        opt_parms <- GA@solution %>% as.data.frame() %>% unlist()

        opt_parm_names <- paste0(names(opt_parms),"*")

        # Fitness value
        fit_val <- GA@fitnessValue

        # Values of parameters to make simulation
        values <- c(opt_parms, no_opt_parms)

        # Names of parameters to make simulation
        parameter_names_simulation <- c(names(opt_parms), names(no_opt_parms))
        names(values) <- parameter_names_simulation

        # Values of parameters and fitness
        values_result <- c(values, -fit_val)

        # Names of parameters to show result
        parameter_names_result <- c(opt_parm_names, names(no_opt_parms), "error")

        results_table <- data.frame("parameter" = parameter_names_result, "value" = values_result)
        
        ga_out <- data.frame(
                
                names = c("Iterations", parameter_names_result),
                
                values = c(GA@iter, round(values_result,3))
        )

        results <- list(values = values, ga_out = ga_out, GA = GA)


        return(results)
        ######
}

# fitness_fun_opt ####
fitness_fun_opt <- function(x, data, opt_parms, no_opt_parms) {

        # This is required to define what parms is.
        names(x) <- names(opt_parms)

        n <- nrow(data)
        

        # Simulate the model
        sim_out <- ode(y =  c(x = data$x[1], p = data$p[1], s = data$s[1]),
                       times = seq(data$time[1], data$time[n], data$time[2] - data$time[1]),
                       func = model,
                       parms = c(x, no_opt_parms),
                       method = "rk4") %>% as.data.frame()


        return(-modCost(sim_out, data)$model)
}


# comp_fun_opt ####
comp_fun_opt <- function(p, data) {
        
        n <- nrow(data)
        # Simulate the model with the optimized parameters
        fitness <- ode(y = c(x = data$x[1], p = data$p[1], s = data$s[1]),
                       times = seq(data$time[1], data$time[n], data$time[2] - data$time[1]),
                       func = model,
                       parms = p,
                       method = "rk4")
        fitness <- as.data.frame(fitness)

        # Plot the data and the simulation with the optimized parameters
        ggplot(data = fitness) +
                geom_line(mapping = aes(x = time, y = s, color = "s"), size = 1) +
                geom_line(mapping = aes(x = time, y = x, color = "x"), size = 1) +
                geom_line(mapping = aes(x = time, y = p,color = "p"), size = 1) +
                geom_point(data = data, mapping = aes(x = time, y = s, color = "s"), size = 2) +
                geom_point(data = data, mapping = aes(x = time, y = x, color = "x"), size = 2) +
                geom_point(data = data, mapping = aes(x = time, y = p,color = "p"), size = 2) +
                
                labs(x = "Time (h)", y = "Concentration (g/L)", color = NULL) +
                theme_bw() +
                theme(
                        axis.text = element_text(size = 10, face = "bold",
                                                 colour = "black",hjust = "0.5",vjust = "1"),
                        
                        axis.title.y = element_text(size = 15, angle = 90,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 1),
                        
                        axis.title.x = element_text(size = 15, angle = 0,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 0),
                        legend.position  = "top",
                        
                        legend.justification  = "top",
                        
                        legend.title = element_text(size = 15, face = "bold"),
                        
                        legend.text = element_text(size = 15, colour = "black",face = "bold")
                )
}

# plot_ga_fun_opt ####
plot_ga_fun_opt <- function(ga) {

        plot(ga)
}

# Fed Batch process process subsection #####################
# plot_fitness_fun_fb_opt ####
plot_fitness_fun_fb_opt <- function(s, p, time, interval) { 
        
        q <- seq(interval[1], interval[2], length.out = 60)
        
        n <- length(q)
        
        mx <- numeric()
        
        for (i in 1:n) {
                
                out_fit <- fitness_fun_fb_opt(x = q[i], s, p, time)
                
                mx <- c(mx, out_fit)
                
        }
        
        df <- data.frame(
                
                q = q,
                
                mx = mx
        )
        
        ggplot(data = df, aes(x = q)) +
                
                geom_line(aes(y = mx), color = "blue",  size = 1, alpha = 1/2) +
                
                theme_light() +
                
                ylab("mx (g)") +
                
                xlab("q (L/h)") +
                
                theme(
                        axis.text = element_text(size = 20, face = "bold",
                                                 colour = "black",hjust = "0.5",vjust = "1"),
                        
                        axis.title.y = element_text(size = 20, angle = 90,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 1),
                        
                        axis.title.x = element_text(size = 20, angle = 0,
                                                    face = "bold",colour = "black",
                                                    hjust = 0.5, vjust = 0)
                )
}

# fitness_fun_fb_opt  ####
fitness_fun_fb_opt <- function(x , s, p, time) {
        
        # set q value for the simulation
        p["q"] <- x
        
        # simulate the model 
        out <- ode(y = s, 
                   times = seq(0, time, 1),
                   func = model_fb_fun_fb_sim,
                   parms = p,
                   method = "rk4") %>% as.data.frame()
        
        # get final biomass produced
        n <- nrow(out)
        ind <- which(out$q == 0)[1]
        ind <- ifelse(is.na(ind), n, ind)
        tf <- out$time[ind]
        mx <- out$x[n]*out$V[n] 
        
        return(mx)
}

# get_q_fun_fb_opt ####
get_q_fun_fb_opt <- function(pop_size, generations, cross_prob, mutation_prob, run, s, p, interval, time) {
        
        GA <- ga(type = "real-valued",
                 fitness = fitness_fun_fb_opt,
                 s = s,
                 p = p,
                 time,
                 lower = interval[1], 
                 upper = interval[2], 
                 popSize = pop_size, 
                 maxiter = generations,
                 pcrossover = cross_prob, 
                 pmutation = mutation_prob,
                 run = run)
        
        ga_out <- data.frame(
                
                names = c( "Iterations", "Fitness function value", "q"),
                
                values = c(GA@iter, round(c(GA@fitnessValue, GA@solution),3)),
                
                units = c("", "(g)", "(L/h)")
        )
        
        results <- list(ga_out = ga_out, GA = GA)
        
        return(results)
        
}
