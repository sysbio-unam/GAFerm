
# Install required packages #########################################
list_of_packages <- c("shiny","shinydashboard","markdown","writexl","shinybusy","shinythemes","deSolve","GA","FME","tidyverse","shinyWidgets")
new_packages <- list_of_packages[!(list_of_packages %in%  installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries ############################################
library(shiny)
library(shinydashboard)
library(markdown)
library(writexl)
library(readxl)
library(shinybusy)
library(shinythemes)
library(deSolve)
library(GA)
library(FME)
library(tidyverse)
library( shinyWidgets)
# Load function.R script ######################################
source("functions.R")

# ui #################################################################################################
ui = navbarPage("GAFerm", theme = shinytheme("spacelab"),
                
                # Simulation section #### 
                navbarMenu("Simulation",
                           
                           # Batch process section ###########################################                   
                           tabPanel("Batch process",
                                    
                                    # For the LaTex code
                                    withMathJax(), 
                                    
                                    # For the wait
                                    add_busy_spinner(spin = "fading-circle"),
                                    
                                    sidebarLayout(
                                            
                                            # Sidebar Panel simulation ####
                                            sidebarPanel(
                                                    
                                                    # Select model ####
                                                    selectInput(inputId = "mod_int_sim",label = "Select a model",
                                                                choices = list("Model 1 (Monod)" = "model_1",
                                                                               "Model 2 (Inhibition by product)" = "model_2",
                                                                               "Model 3 (Product partially linked to growth)" = "model_3",
                                                                               "Model 4 (Monod with cell death)" = "model_4",
                                                                               "Model 5 (Inhibition by substrate)" = "model_5",
                                                                               "Model 6 (Inhibition by product and product partially linked to growth)" = "model_6"),
                                                                selected = "model_1"),
                                                    
                                                    # Initial conditions ####
                                                    
                                                    fluidRow(style = "text-align:center",
                                                             h5("Initial Conditions"),
                                                             
                                                             column(4,
                                                                    numericInput("x_int_sim", helpText('$$x_{0} \\ (g/L)$$'), 0.2)
                                                             ),
                                                             
                                                             column(4,
                                                                    numericInput("s_int_sim", helpText('$$s_{0} \\ (g/L)$$'), 40)
                                                             ),
                                                             
                                                             column(4,
                                                                    numericInput("p_int_sim", helpText('$$p_{0} \\ (g/L)$$'), 0)
                                                             )
                                                             
                                                    ),
                                                    
                                                    hr(),
                                                    
                                                    # Conditional panel model 1 ####
                                                    conditionalPanel(condition = "output.parms_mod1_ui_sim",
                                                                     
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod1_int_sim", label = helpText('\\(\\mu_{max}\\)'), 
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = " 1/h"),
                                                                                     sliderInput(inputId = "ks_mod1_int_sim",label = helpText('$$k_{s}$$'), 
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = " g/L")
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "yxs_mod1_int_sim",label = helpText('$$Y_{xs}$$'), 
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01),
                                                                                     sliderInput(inputId = "ypx_mod1_int_sim",label = helpText('$$Y_{px}$$'), min = 1, max = 20, value = 4, step = 0.1)
                                                                              )
                                                                     )
                                                    ),
                                                    
                                                    # Conditional panel model 2 #######################################################
                                                    conditionalPanel(condition = "output.parms_mod2_ui_sim",
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod2_int_sim", label = helpText('\\(\\mu_{max}\\)'),
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = " 1/h"),
                                                                                     sliderInput(inputId = "ks_mod2_int_sim", label = helpText('$$k_{s}$$'),
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = " g/L"),
                                                                                     sliderInput(inputId = "yxs_mod2_int_sim", label = helpText('$$Y_{xs}$$'),
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01)
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "ypx_mod2_int_sim", label = helpText('$$Y_{px}$$'), 
                                                                                                 min = 1, max = 20, value = 4, step = 0.1),
                                                                                     sliderInput(inputId = "kp_mod2_int_sim", label = helpText('$$k_{p}$$'), 
                                                                                                 min = 50, max = 200, value = 120, step = 1, post = " g/L")
                                                                              )
                                                                     )
                                                    ),
                                                    # Conditional panel model 3 #######################################################
                                                    conditionalPanel(condition = "output.parms_mod3_ui_sim",
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod3_int_sim", label = helpText('\\(\\mu_{max}\\)'), 
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = " 1/h"),
                                                                                     sliderInput(inputId = "ks_mod3_int_sim", label = helpText('$$k_{s}$$'), 
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = " g/L"),
                                                                                     sliderInput(inputId = "yxs_mod3_int_sim", label = helpText('$$Y_{xs}$$'),
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01)
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "alpha_mod3_int_sim", label = helpText('\\(\\alpha\\)'), 
                                                                                                 min = 1, max = 20, value = 4, step = 0.1),
                                                                                     sliderInput(inputId = "beta_mod3_int_sim", label = helpText('\\(\\beta\\)'), 
                                                                                                 min = 0, max = 0.1, value = 0.01, step = 0.01, post = " 1/h")
                                                                              )
                                                                     )
                                                    ),
                                                    # Conditional panel model 4 #######################################################
                                                    conditionalPanel(condition = "output.parms_mod4_ui_sim",
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod4_int_sim", label = helpText('\\(\\mu_{max}\\)'), 
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = " 1/h"),
                                                                                     sliderInput(inputId = "ks_mod4_int_sim", label = helpText('$$k_{s}$$'), 
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = " g/L"),
                                                                                     sliderInput(inputId = "yxs_mod4_int_sim", label = helpText('$$Y_{xs}$$'), 
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01)
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "ypx_mod4_int_sim", label = helpText('$$Y_{px}$$'), 
                                                                                                 min = 1, max = 20, value = 4, step = 1),
                                                                                     sliderInput(inputId = "kd_mod4_int_sim", label = helpText('$$k_{d}$$'), 
                                                                                                 min = 0, max = 0.1, value = 0.01, step = 0.01, post = " g/L")
                                                                              )
                                                                     )
                                                    ),
                                                    
                                                    # Conditional panel model 5 #######################################################
                                                    conditionalPanel(condition = "output.parms_mod5_ui_sim",
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod5_int_sim", label = helpText('\\(\\mu_{max}\\)'),
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = "1/h"),
                                                                                     sliderInput(inputId = "ks_mod5_int_sim", label = helpText('$$k_{s}$$'),
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = "g/L"),
                                                                                     sliderInput(inputId = "yxs_mod5_int_sim", label = helpText('$$Y_{xs}$$'),
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01)
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "ypx_mod5_int_sim", label = helpText('$$Y_{px}$$'), 
                                                                                                 min = 1, max = 20, value = 4, step = 1),
                                                                                     sliderInput(inputId = "ki_mod5_int_sim", label = helpText('$$k_{i}$$'),
                                                                                                 min = 0, max = 1, value = 0.3, step = 0.01, post = "L/g")
                                                                              )
                                                                     )
                                                    ),
                                                    
                                                    # Conditional panel model 6 #######################################################
                                                    conditionalPanel(condition = "output.parms_mod6_ui_sim",
                                                                     
                                                                     fluidRow(style = "text-align:center",
                                                                              h5("Kinetic parameters"),
                                                                              
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "vmax_mod6_int_sim", label = helpText('\\(\\mu_{max}\\)'), 
                                                                                                 min = 0.5, max = 3, value = 1.2, step = 0.1, post = "1/h"),
                                                                                     sliderInput(inputId = "ks_mod6_int_sim", label = helpText('$$k_{s}$$'), 
                                                                                                 min = 100, max = 400, value = 280, step = 1, post = "g/L"),
                                                                                     sliderInput(inputId = "yxs_mod6_int_sim", label = helpText('$$Y_{xs}$$'), 
                                                                                                 min = 0.01, max = 1, value = 0.2, step = 0.01)
                                                                              ),
                                                                              column(width = 6,
                                                                                     sliderInput(inputId = "kp_mod6_int_sim", label = helpText('$$k_{p}$$'), 
                                                                                                 min = 50, max = 200, value = 120, step = 1, post = "g/L"),
                                                                                     sliderInput(inputId = "alpha_mod6_int_sim", label = helpText('\\(\\alpha\\)'),
                                                                                                 min = 1, max = 20, value = 4, step = 0.1),
                                                                                     sliderInput(inputId = "beta_mod6_int_sim", label = helpText('\\(\\beta\\)'),
                                                                                                 min = 0, max = 0.1, value = 0.01, step = 0.01, post = "1/h")
                                                                              )
                                                                     )
                                                    ),
                                                    hr(),
                                                    # More widgets ####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             column(5,
                                                                    selectInput(inputId = "step_int_sim",label = "Step (h)",
                                                                                choices = list("0.1","0.5","1","4","6"), selected = "1")
                                                             ),
                                                             
                                                             column(5, offset = 1,
                                                                    selectInput(inputId = "interval_int_sim",label = "Simulation time (h)",
                                                                                choices = list("12","24","48","60","100"), selected = "60")      
                                                             )
                                                    ),
                                                    hr(),
                                                    fluidRow(
                                                            column(4, offset = 1,
                                                                   downloadButton("down_plot_out_sim", "Simulation Plot")
                                                            ),
                                                            column(4, offset = 1,
                                                                   downloadButton("down_data_out_sim", "Simulation Data")
                                                            )
                                                    )
                                                    
                                            ),              
                                            
                                            # Main panel simulation ####
                                            mainPanel(
                                                    div(style = "margin-bottom: -5em; text-align:center", 
                                                        fluidRow(
                                                                
                                                                column(4,offset = 1,
                                                                       
                                                                       h3("Model"),
                                                                       hr(),
                                                                       uiOutput('ui_out_sim')
                                                                ),
                                                                
                                                                column(4,offset = 1,
                                                                       
                                                                       h3("Process Network"),
                                                                       hr(),
                                                                       imageOutput("network_out_sim",height = 300)
                                                                )
                                                        )),
                                                    
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             column(12,
                                                                    
                                                                    h3("Batch Process Simulation"),
                                                                    hr(),
                                                                    plotOutput("plot_sim_out_sim")
                                                             )
                                                             
                                                    ),
                                                    
                                                    fluidRow(
                                                            
                                                            column(8,
                                                                   h3("Final concentration values"),
                                                                   hr(),
                                                                   tableOutput("end_conc_out_sim")       
                                                            )
                                                    )
                                            ) 
                                            #####
                                    )
                                    
                           ),
                           # Fed-batch process section ###########################################
                           tabPanel("Fed-batch process",
                                    
                                    ####
                                    
                                    # For the LaTex code
                                    withMathJax(), 
                                    
                                    # For the wait
                                    add_busy_spinner(spin = "fading-circle"),
                                    
                                    sidebarLayout(
                                            
                                            # sidebar panel ####
                                            sidebarPanel(
                                                    
                                                    # Initial conditions #####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             p("Initial conditions"),
                                                             
                                                             column(4,
                                                                    numericInput("x_int_fb_sim", helpText('$$x_{0} \\ (g/L)$$'), 0.2)
                                                             ),
                                                             
                                                             column(4,
                                                                    numericInput("s_int_fb_sim", helpText('$$s_{0} \\ (g/L)$$'), 40)
                                                             ),
                                                             
                                                             column(4,
                                                                    numericInput("p_int_fb_sim", helpText('$$p_{0} \\ (g/L)$$'), 0)
                                                             )
                                                    ),       
                                                    
                                                    hr(),
                                                    
                                                    # Kinetic parameters ####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             p("Kinetic parameters"),
                                                             
                                                             column(4, offset = 1, 
                                                                    
                                                                    
                                                                    numericInput(inputId = "vmax_int_fb_sim",label =  helpText('\\(\\mu_{max}\\)'),value = 1.2),
                                                                    numericInput(inputId = "ks_int_fb_sim",label = helpText('$$k_{s} \\ (g/L)$$'),value = 280),
                                                                    numericInput(inputId = "ki_int_fb_sim",label = helpText('$$k_{i} \\ (g/L)$$'),value = 0.3)
                                                             ),
                                                             
                                                             column(4, offset = 1,
                                                                    
                                                                    
                                                                    numericInput(inputId = "yxs_int_fb_sim",label = helpText('$$Y_{xs}$$'),value = 0.2),
                                                                    numericInput(inputId = "ypx_int_fb_sim",label = helpText('$$Y_{px}$$'),value = 4)
                                                             )
                                                    ),       
                                                    
                                                    hr(),
                                                    
                                                    # Operation parameters ####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             p("Operation parameters"),
                                                             
                                                             column(4, offset = 1,
                                                                    
                                                                    
                                                                    sliderInput(inputId = "q_int_fb_sim",label = helpText('$$q$$'),
                                                                                min = 0, max = 5, value = 0, step = 0.1, post = "L/h"),
                                                                    
                                                                    sliderInput(inputId = "sf_int_fb_sim", label = helpText('$$s_{f}$$'),
                                                                                min = 0, max = 50, value = 40, step = 5, post = "g/L"),
                                                                    
                                                                    numericInput(inputId = "tf_int_fb_sim",label = "Stop (h)", value = 100)
                                                                    
                                                                    
                                                             ),
                                                             
                                                             column(4, offset = 1,
                                                                    
                                                                    
                                                                    numericInput(inputId = "V0_int_fb_sim",label = helpText('$$V_{0} (L)$$'),value = 10),
                                                                    
                                                                    br(),
                                                                    
                                                                    sliderInput(inputId = "Vlim_int_fb_sim",label = helpText('$$V_{lim}$$'),
                                                                                min = 5, max = 300 ,value = 300, step = 5, post = "L")
                                                             )
                                                             
                                                    ),
                                                    
                                                    hr(),
                                                    # More widgets ####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             column(5,
                                                                    selectInput(inputId = "step_int_fb_sim",label = "Step (h)",
                                                                                choices = list("0.1","0.5","1","4","6"), selected = "1")
                                                             ),
                                                             
                                                             column(5, offset = 1,
                                                                    selectInput(inputId = "end_time_int_fb_sim",label = "Simulation time (h)",
                                                                                choices = list("12","24","48","60","100"), selected = "100")      
                                                             )
                                                    ),
                                                    hr(),
                                                    fluidRow(
                                                            
                                                            column(4, offset = 1,
                                                                   downloadButton("down_data_out_fb_sim", "Simulation Data")
                                                            )
                                                    ),
                                                    # More space ####
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br()
                                                    
                                            ),
                                            
                                            # Main panel ####
                                            mainPanel(
                                                    div(style = "margin-bottom: 0em; text-align:center",
                                                        fluidRow(
                                                                
                                                                # Model ####
                                                                column(7,
                                                                       
                                                                       h3("Model"),
                                                                       hr(),
                                                                       withMathJax(
                                                                               helpText('Model 5 (Inhibition by substrate):  $$\\frac{dV}{dt} = q $$'),
                                                                               helpText('$$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s + k_i s^2} \\right) x - \\frac{q}{V} x$$'),
                                                                               helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s + k_i s^2} \\right) x + \\frac{q}{V} \\left( s_f - s \\right)$$'),
                                                                               helpText('$$\\frac{dp}{dt} = Y_{px} \\mu_{max} \\left( \\frac{s}{k_{s} + s + k_i s^2} \\right) x - \\frac{q}{V} p$$')
                                                                       )
                                                                ),
                                                                
                                                                # Process network
                                                                column(4,
                                                                       
                                                                       h3("Process Network"),
                                                                       hr(),
                                                                       imageOutput("network_out_fb_sim",height = 300)
                                                                )
                                                        )),
                                                    
                                                    # Simulation plots #####
                                                    
                                                    fluidRow(style = "margin-bottom: 0em; text-align:center",
                                                             h3("Fed Batch Process Simulation"),
                                                             hr(),
                                                             column(6,
                                                                    plotOutput("plot_x_out_fb_sim",height = "200pt")
                                                             ),
                                                             column(6,
                                                                    plotOutput("plot_s_out_fb_sim",height = "200pt")
                                                             )
                                                    ),
                                                    
                                                    fluidRow(
                                                            
                                                            column(6,
                                                                   plotOutput("plot_q_out_fb_sim",height = "200pt")
                                                            ),
                                                            column(6,
                                                                   plotOutput("plot_v_out_fb_sim",height = "200pt")
                                                            )
                                                    ),
                                                    
                                                    fluidRow(
                                                            column(6, 
                                                                   plotOutput("plot_p_out_fb_sim",height = "200pt") 
                                                            ),
                                                            column(6, 
                                                                   h3("Final mass values"),
                                                                   hr(),
                                                                   tableOutput("fin_mass_out_fb_sim") 
                                                            )
                                                    )
                                            )         
                                            #####
                                    )
                                    
                                    #####
                                    
                           )
                           
                ),
                
                # Optimization section ########################################################################## 
                navbarMenu("Optimization",
                           
                           # Kinetic parameter section ####
                           tabPanel("Kinetic parameters",
                                    
                                    # For the LaTex code
                                    withMathJax(),
                                    
                                    add_busy_spinner(spin = "fading-circle"),
                                    
                                    sidebarLayout(
                                            
                                            # Sidebar Panel optimization ####
                                            sidebarPanel(
                                                    
                                                    # Enter data ####
                                                    
                                                    fileInput(inputId = "data_int_opt", label = "Enter the data", multiple = FALSE,
                                                              accept = c(".xlsx")),
                                                    
                                                    fluidRow(
                                                            
                                                            column(6,
                                                                   checkboxInput(inputId = "header_int_opt",label = "Head", value = TRUE)
                                                                   
                                                            )
                                                    ),
                                                    
                                                    # Select model #####
                                                    selectInput(inputId = "mod_int_opt",label = "Select a model",
                                                                choices = list("Model 1 (Monod)" = "model1.R",
                                                                               "Model 2 (Inhibition by product)" = "model2.R",
                                                                               "Model 3 (Product partially linked to growth)" = "model3.R",
                                                                               "Model 4 (Monod with cell death)" = "model4.R",
                                                                               "Model 5 (Inhibition by substrate)" = "model5.R",
                                                                               "Model 6 (Inhibition by product and product partially linked to growth)" = "model6.R"),
                                                                selected = "model1.R"),
                                                    
                                                    hr(),
                                                    # Model 1 conditional panel ####################################
                                                    conditionalPanel(condition = "output.parms_ui_mod1_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod1_name_int_opt", label = "", 
                                                                                                           choices =  "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod1_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod1_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                                 
                                                                                 
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod1_name_int_opt",label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod1_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod1_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod1_name_int_opt",label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "yxs_mod1_val_int_opt", label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod1_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ypx_mod1_name_int_opt", label = "", choices = "Ypx", selected = "Ypx")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ypx_mod1_val_int_opt", label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ypx_mod1_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         ))
                                                    ),
                                                    
                                                    # Model 2 conditional panel ####
                                                    conditionalPanel(condition = "output.parms_ui_mod2_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod2_name_int_opt",label = "", choices = "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod2_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod2_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                         )),
                                                                     
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod2_name_int_opt",label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod2_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod2_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod2_name_int_opt", label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "yxs_mod2_val_int_opt",label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod2_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ypx_mod2_name_int_opt", label = "", choices = "Ypx", selected = "Ypx")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ypx_mod2_val_int_opt",label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ypx_mod2_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "kp_mod2_name_int_opt", label = "", choices = "kp", selected = "kp")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "kp_mod2_val_int_opt", label = "", value = 80)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "kp_mod2_range_int_opt", label = "",
                                                                                                          value = c(0, 200))
                                                                                 )
                                                                         ))
                                                                     
                                                    ),
                                                    
                                                    # Model 3 conditional panel ####
                                                    conditionalPanel(condition = "output.parms_ui_mod3_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod3_name_int_opt", label = "", choices = "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod3_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod3_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod3_name_int_opt", label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod3_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod3_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod3_name_int_opt", label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "yxs_mod3_val_int_opt", label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod3_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "alpha_mod3_name_int_opt", label = "", choices = "alpha", selected = "alpha")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "alpha_mod3_val_int_opt", label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "alpha_mod3_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "beta_mod3_name_int_opt", label = "", choices = "beta", selected = "beta")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "beta_mod3_val_int_opt", label = "", value = 0.01)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "beta_mod3_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         ))
                                                                     
                                                    ),
                                                    
                                                    # Model 4 conditional panel ####
                                                    conditionalPanel(condition = "output.parms_ui_mod4_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod4_name_int_opt", label = "", choices = "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod4_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod4_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                                 
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod4_name_int_opt", label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod4_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod4_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod4_name_int_opt", label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "yxs_mod4_val_int_opt", label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod4_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ypx_mod4_name_int_opt", label = "", choices = "Ypx", selected = "Ypx")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ypx_mod4_val_int_opt", label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ypx_mod4_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "kd_mod4_name_int_opt", label = "", choices = "kd", selected = "kd")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "kd_mod4_val_int_opt", label = "", value = 0.01)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "kd_mod4_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         ))
                                                                     
                                                    ),
                                                    
                                                    # Model 5 conditional panel ####
                                                    
                                                    conditionalPanel(condition = "output.parms_ui_mod5_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod5_name_int_opt", label = "", choices = "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod5_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod5_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                         )),
                                                                     
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod5_name_int_opt", label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod5_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod5_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod5_name_int_opt", label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "yxs_mod5_val_int_opt", label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod5_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ypx_mod5_name_int_opt", label = "", choices = "Ypx", selected = "Ypx")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "ypx_mod5_val_int_opt", label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ypx_mod5_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ki_mod5_name_int_opt", label = "", choices = "ki", selected = "ki")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "ki_mod5_val_int_opt", label = "", value = 0.01)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ki_mod5_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         ))
                                                                     
                                                    ),
                                                    
                                                    # Model 6 conditional panel ####
                                                    conditionalPanel(condition = "output.parms_ui_mod6_opt",
                                                                     
                                                                     div(style = "text-align:center",
                                                                         h5("Kinetic parameters"),
                                                                         
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "vmax_mod6_name_int_opt", label = "", choices = "mu_max", selected = "mu_max")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "vmax_mod6_val_int_opt", label = "", value = 1.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "vmax_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 3))
                                                                                 )
                                                                         )),
                                                                     
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "ks_mod6_name_int_opt", label = "", choices = "ks", selected = "ks")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "ks_mod6_val_int_opt", label = "", value = 280)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "ks_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 400))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "yxs_mod6_name_int_opt", label = "", choices = "Yxs", selected = "Yxs")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "yxs_mod6_val_int_opt", label = "", value = 0.2)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "yxs_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "alpha_mod6_name_int_opt", label = "", choices = "alpha", selected = "alpha")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "alpha_mod6_val_int_opt", label = "", value = 4)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "alpha_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 20))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "kp_mod6_name_int_opt", label = "", choices = "kp", selected = "kp")  
                                                                                 ),
                                                                                 column(3, offset = 1,
                                                                                        numericInput(inputId = "kp_mod6_val_int_opt", label = "", value = 80)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "kp_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 200))
                                                                                 )
                                                                         )),
                                                                     
                                                                     div(style = "margin-top:-2em",
                                                                         fluidRow(
                                                                                 
                                                                                 column(2,
                                                                                        checkboxGroupInput(inputId = "beta_mod6_name_int_opt", label = "", choices = "beta", selected = "beta")  
                                                                                 ),
                                                                                 column(3, offset = 1, 
                                                                                        numericInput(inputId = "beta_mod6_val_int_opt", label = "", value = 0.01)
                                                                                 ),
                                                                                 column(6,
                                                                                        numericRangeInput(inputId = "beta_mod6_range_int_opt", label = "",
                                                                                                          value = c(0, 1))
                                                                                 )
                                                                         ))
                                                                     
                                                                     
                                                    ),
                                                    hr(),
                                                    # More widgets ####
                                                    actionButton("make_opt_int_opt","Make optimization"),
                                                    hr(),
                                                    fluidRow(
                                                            column(4, offset = 1, 
                                                                   downloadButton(outputId = "down_data_plot_out_opt",label = "Data plot"),
                                                                   hr(),
                                                                   downloadButton(outputId = "down_opt_parm_out_opt",label = "GA output")),
                                                            column(4, offset = 1, 
                                                                   downloadButton(outputId = "down_comp_plot_out_opt",label = "Comparison"),
                                                                   hr(),
                                                                   downloadButton(outputId = "down_ga_plot_out_opt",label = "GA plot"))
                                                    )
                                                    
                                            ),
                                            
                                            
                                            # Main panel optimization #####
                                            mainPanel(
                                                    
                                                    fluidRow(style = "margin-bottom: -3em; text-align:center",
                                                             
                                                             h3("Data"),
                                                             hr(),
                                                             column(3, offset = 1,
                                                                    tableOutput("table_data_out_opt")
                                                             ),
                                                             column(6,offset = 2,
                                                                    plotOutput("plot_data_out_opt",height = 300)
                                                                    
                                                             )
                                                    ),
                                                    
                                                    fluidRow(style = "margin-bottom: -3em; text-align:center",
                                                             # GA operators ####
                                                             column(6,
                                                                    h3("GA operators"),
                                                                    hr(),
                                                                    column(6,
                                                                           numericInput(inputId = "pop_size_int_opt", label = "Population size",
                                                                                        value = 50),
                                                                           
                                                                           numericInput(inputId = "num_gen_int_opt", label = "Number of generations",
                                                                                        value = 50),
                                                                           numericInput(inputId = "run_int_opt", label = "Run",
                                                                                        value = 100)
                                                                    ),
                                                                    column(6,
                                                                           numericInput(inputId = "cross_prob_int_opt", label = "Crossover prob",
                                                                                        value = 0.8),
                                                                           numericInput(inputId = "mut_prob_int_opt",label = "Mutation prob",
                                                                                        value = 0.1),
                                                                           numericInput(inputId = "max_fit_int_opt",label = "Tolerance",
                                                                                        value = 100)
                                                                           
                                                                    )
                                                                    
                                                             ),
                                                             
                                                             # GA results ####
                                                             column(6, 
                                                                    h3("Comparison"),
                                                                    hr(),
                                                                    plotOutput("plot_result_out_opt", height = 300)
                                                             ) 
                                                             
                                                    ),
                                                    
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             column(6,
                                                                    h3("GA output"),
                                                                    hr(),
                                                                    tableOutput("ga_out_out_opt")
                                                             ),
                                                             column(6, 
                                                                    h3("GA progress"),
                                                                    hr(),
                                                                    plotOutput("plot_ga_out_opt")
                                                             )
                                                    )
                                                    
                                            )
                                    )
                                    #####
                           ),
                           # Fed-Batch section #####
                           tabPanel("Fed-Batch",
                                    #####
                                    
                                    # For the LaTex code
                                    withMathJax(), 
                                    
                                    # For the wait
                                    add_busy_spinner(spin = "fading-circle"),
                                    
                                    sidebarLayout(
                                            
                                            sidebarPanel(
                                                    # Initial conditions #####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             p("Initial conditions"),
                                                             
                                                             column(4,
                                                                    numericInput(inputId = "x_int_fb_opt",label = helpText('$$x_{0} \\ (g/L)$$'),value = 0.2)),
                                                             
                                                             column(4, 
                                                                    numericInput(inputId = "s_int_fb_opt",label = helpText('$$s_{0} \\ (g/L)$$'),value = 40)),
                                                             
                                                             column(4, 
                                                                    numericInput(inputId = "p_int_fb_opt",label = helpText('$$p_{0} \\ (g/L)$$'),value = 0))
                                                             
                                                             
                                                    ),
                                                    
                                                    
                                                    hr(),
                                                    
                                                    # Kinetic parameters ####
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             p("Kinetic parameters"),
                                                             
                                                             column(4,  
                                                                    
                                                                    
                                                                    numericInput(inputId = "vmax_int_fb_opt",label = helpText('$$\\mu_{max} \\ (1/h)$$'),value = 1.2),
                                                                    numericInput(inputId = "ypx_int_fb_opt",label = helpText('$$Y_{px}$$'),value = 4)),
                                                                    
                                                             
                                                             column(4,
                                                                    
                                                                    numericInput(inputId = "ks_int_fb_opt",label = helpText('$$k_{s} \\ (g/L)$$'),value = 280),
                                                                    numericInput(inputId = "ki_int_fb_opt",label = helpText('$$k_{i} \\ (g/L)$$'),value = 0.3)),
                                                                    
                                                             
                                                             column(4, 
                                                                    
                                                                    numericInput(inputId = "yxs_int_fb_opt",label = helpText('$$Y_{xs}$$'),value = 0.2))
                                                                    
                                                    ),
                                                    
                                                    hr(),
                                                    
                                                    # GA operators ####
                                                    fluidRow(style = "text-align:center",
                                                             p("GA operators"),
                                                             column(6,
                                                                    numericInput(inputId = "pop_size_int_fb_opt", label = "Population size",
                                                                                 value = 10),
                                                                    
                                                                    numericInput(inputId = "num_gen_int_fb_opt", label = "Number of generations",
                                                                                 value = 10),
                                                                    numericInput(inputId = "run_int_fb_opt", label = "Run",
                                                                                 value = 10)
                                                             ),
                                                             column(6,
                                                                    numericInput(inputId = "cross_prob_int_fb_opt", label = "Crossover prob",
                                                                                 value = 0.8),
                                                                    numericInput(inputId = "mut_prob_int_fb_opt",label = "Mutation prob",
                                                                                 value = 0.1)
                                                                    
                                                             )
                                                    ),
                                                    
                                                    hr(),
                                                    # More widgets ####
                                                    fluidRow(
                                                            column(4, offset = 1,
                                                                   downloadButton("down_fit_plot_out_fb_opt", "Fitness Plot"),
                                                                   hr(),
                                                                   downloadButton("down_ga_plot_out_fb_opt", "GA Plot")
                                                            ),
                                                            column(4, offset = 1,
                                                                   downloadButton("down_ga_out_out_fb_opt", "GA out")
                                                            )
                                                            
                                                    )
                                            ),
                                            # main panel ####
                                            mainPanel(
                                                    
                                                    fluidRow(style = "text-align:center",
                                                             
                                                             column(6,
                                                                    h3("Biomass generated"),
                                                                    hr(),
                                                                    plotOutput("plot_fitness_out_fb_opt")     
                                                             ),
                                                             column(6,
                                                                    # Operation parameters ####
                                                                    p("Operation parameters"),
                                                                    
                                                                    column(5, offset = 1, 
                                                                           
                                                                           numericInput(inputId = "V0_int_fb_opt",label = helpText('$$V_{0} \\ (L)$$'),value = 10),
                                                                           
                                                                           selectInput(inputId = "end_time_int_fb_opt",label = "Simulation time (h)",
                                                                                       choices = list("12","24","48","60","100"), selected = "100"),
                                                                           
                                                                           numericInput(inputId = "tf_int_fb_opt",label = "Stop (h)",value = 100),
                                                                           
                                                                           br(),
                                                                           
                                                                           actionButton("make_opt_int_fb_opt", "Make optimization")
                                                                           
                                                                    ),
                                                                    
                                                                    column(6, 
                                                                           
                                                                           sliderInput(inputId = "sf_int_fb_opt", label = helpText('$$s_{f}$$'),
                                                                                       min = 0, max = 50, value = 40, step = 5, post = "g/L"),
                                                                           
                                                                           sliderInput(inputId = "Vlim_int_fb_opt",label = helpText('$$V_{lim}$$'),
                                                                                       min = 5, max = 300, value = 300, step = 5, post = "L"),
                                                                           
                                                                           numericRangeInput(inputId = "q_int_fb_opt", label = "q Interval (L/h)",
                                                                                             value = c(0, 5))
                                                                           
                                                                    )
                                                                    
                                                             )
                                                    ),
                                                    
                                                    fluidRow(style = "text-align:center",
                                                             # GA output ####
                                                             
                                                             column(4,  offset = 1,
                                                                    
                                                                    h3("GA output"),
                                                                    hr(),
                                                                    tableOutput("ga_out_out_fb_opt")  
                                                             ),
                                                             column(6, offset = 1,
                                                                    
                                                                    h3("GA progress"),
                                                                    hr(),
                                                                    plotOutput("plot_ga_out_fb_opt")
                                                             )
                                                             
                                                    )
                                                    #####
                                            )
                                    )
                                    
                                    #####
                           )
                           
                ) 
)


###########################################################################################################
###########################################################################################################
# server ##############################################################################################      
server = function(input, output, session) {
        
        # Simulation section ##########################################################
        # Batch process section #############################
        # Select the panel with the parameters of the model ####
        output$parms_mod1_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_1", TRUE, FALSE)
        })
        
        output$parms_mod2_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_2", TRUE, FALSE)
        })
        
        output$parms_mod3_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_3", TRUE, FALSE)
        })
        
        output$parms_mod4_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_4", TRUE, FALSE)
        })
        
        output$parms_mod5_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_5", TRUE, FALSE)
        })
        
        output$parms_mod6_ui_sim <- reactive({
                ifelse(input$mod_int_sim == "model_6", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_mod1_ui_sim", suspendWhenHidden = FALSE)
        outputOptions(output, "parms_mod2_ui_sim", suspendWhenHidden = FALSE) 
        outputOptions(output, "parms_mod3_ui_sim", suspendWhenHidden = FALSE) 
        outputOptions(output, "parms_mod4_ui_sim", suspendWhenHidden = FALSE) 
        outputOptions(output, "parms_mod5_ui_sim", suspendWhenHidden = FALSE) 
        outputOptions(output, "parms_mod6_ui_sim", suspendWhenHidden = FALSE) 
        
        # Reactive values ####
        s_rec_sim <- reactive({
                
                c(x = input$x_int_sim, p = input$p_int_sim, s = input$s_int_sim)
                
        })
        
        p_rec_val_sim <- reactiveValues(rec_val = vector(mode = "numeric"))
        
        # Select model and parameters ####
        observe({
                
                if (input$mod_int_sim == "model_1") { 
                        
                        source("model1.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod1_int_sim, ks = input$ks_mod1_int_sim, 
                                                   Yxs = input$yxs_mod1_int_sim, Ypx = input$ypx_mod1_int_sim)
                        
                        
                }
                
                else if (input$mod_int_sim == "model_2") { 
                        
                        source("model2.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod2_int_sim, ks = input$ks_mod2_int_sim,
                                                   Yxs = input$yxs_mod2_int_sim, Ypx = input$ypx_mod2_int_sim,
                                                   kp = input$kp_mod2_int_sim)
                }
                
                else if (input$mod_int_sim == "model_3") { 
                        
                        source("model3.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod3_int_sim, ks = input$ks_mod3_int_sim, 
                                                   Yxs = input$yxs_mod3_int_sim, alpha = input$alpha_mod3_int_sim,
                                                   beta = input$beta_mod3_int_sim)
                }
                
                else if (input$mod_int_sim == "model_4") {
                        
                        source("model4.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod4_int_sim, ks = input$ks_mod4_int_sim,
                                                   Yxs = input$yxs_mod4_int_sim, Ypx = input$ypx_mod4_int_sim,
                                                   kd = input$kd_mod4_int_sim)
                }
                
                else if (input$mod_int_sim == "model_5"){ 
                        
                        source("model5.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod5_int_sim, ks = input$ks_mod5_int_sim,
                                                   Yxs = input$yxs_mod5_int_sim, Ypx = input$ypx_mod5_int_sim,
                                                   ki = input$ki_mod5_int_sim)
                        
                }
                
                else {     
                        source("model6.R")
                        
                        p_rec_val_sim$rec_val <- c(mu_max = input$vmax_mod6_int_sim, ks = input$ks_mod6_int_sim, 
                                                   Yxs = input$yxs_mod6_int_sim, alpha = input$alpha_mod6_int_sim, 
                                                   kp = input$kp_mod6_int_sim, beta = input$beta_mod6_int_sim)
                        
                }        
                
                
        })
        
        # Make simulation ####
        make_sim_rec_sim <- reactive({
                
                make_sim_fun_sim(s_rec_sim(), 
                                 p_rec_val_sim$rec_val,
                                 as.numeric(input$step_int_sim), 
                                 as.numeric(input$interval_int_sim))
        })
        
        # Plot simulation ####
        plot_sim_rec_sim <- reactive({
                
                plot_sim_fun_sim(make_sim_rec_sim())      
        })
        
        output$plot_sim_out_sim <- renderPlot({
                
                plot_sim_rec_sim()
        })
        
        # Download simulation plot #####
        output$down_plot_out_sim <- downloadHandler(
                filename = function() {
                        paste("simulation_plot", ".png", sep = "")
                },
                content = function(file) {
                        
                        ggsave(file, plot_sim_rec_sim(),
                               width = 10, height = 8)
                }
        )
        
        # Download simulation data ####
        output$down_data_out_sim <- downloadHandler(
                filename = function() {
                        paste("simulation_data", ".xlsx", sep = "")
                },
                content = function(file) {
                        write_xlsx(make_sim_rec_sim(), file)
                }
        )
        
        # Show final concentrations ####
        end_conc_rec_sim <- reactive({
                
                end_conc_fun_sim(make_sim_rec_sim())
        })
        
        output$end_conc_out_sim <- renderTable({
                
                end_conc_rec_sim()
                
        }, striped = TRUE, spacing = "m", align = "c", digits = 3)
        
        # Show mathematical model ####
        output$ui_out_sim <- renderUI({
                if (input$mod_int_sim == "model_1") {
                        withMathJax(
                                helpText('Model 1 (Monod): $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = Y_{px} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$')
                        )
                }
                else if (input$mod_int_sim == "model_2") {
                        withMathJax(
                                helpText('Model 2 (Inhibition by product): $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = Y_{px} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x$$')
                        )
                }
                else if (input$mod_int_sim == "model_3") {
                        withMathJax(
                                helpText('Model 3 (Product partially linked to growth): $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = \\alpha \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x + \\beta x$$')
                        )
                }
                else if (input$mod_int_sim == "model_4") {
                        withMathJax(
                                helpText('Model 4 (Monod with cell death):  $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x - k_d x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = Y_{px} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) x$$')
                        )
                }
                else if (input$mod_int_sim == "model_5") {
                        withMathJax(
                                helpText('Model 5 (Inhibition by substrate):  $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s + k_i s^2} \\right) x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s + ki s^2} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = Y_{px} \\mu_{max} \\left( \\frac{s}{k_{s} + s + ki s^2} \\right) x$$')
                        )
                }
                
                else {
                        withMathJax(
                                helpText('Model 6 (Inhibition by product and product partially linked to growt): $$\\frac{dx}{dt} = \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x$$'),
                                helpText('$$\\frac{ds}{dt} = - \\frac{1}{Y_{xs}} \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x$$'),
                                helpText('$$\\frac{dp}{dt} = \\alpha \\mu_{max} \\left( \\frac{s}{k_{s} + s} \\right) \\left( \\frac{k_p}{k_p + p} \\right) x - \\beta x$$')
                        )
                }
        })
        
        # Show network ####
        output$network_out_sim <- renderImage({
                if (is.null(input$mod_int_sim))
                        return(NULL)
                
                if (input$mod_int_sim == "model_1") {
                        return(list(
                                src = "www/network1.png",
                                contentType = "image/png",
                                alt = "Network",height = 150,width = 250
                        ))
                } else if (input$mod_int_sim == "model_2") {
                        return(list(
                                src = "www/network2.png",
                                contentType = "image/png",
                                alt = "Network",height = 150,width = 250
                        ))
                } else if (input$mod_int_sim == "model_3") {
                        return(list(
                                src = "www/network3.png",
                                contentType = "image/png",
                                alt = "Network",height = 200,width = 300
                        ))
                } else if (input$mod_int_sim == "model_4") {
                        return(list(
                                src = "www/network4.png",
                                contentType = "image/png",
                                alt = "Network",height = 200,width = 250
                        ))
                } else if (input$mod_int_sim == "model_5") {
                        return(list(
                                src = "www/network5.png",
                                contentType = "image/png",
                                alt = "Network",height = 200,width = 250
                        ))
                } else {
                        return(list(
                                src = "www/network6.png",
                                contentType = "image/png",
                                alt = "Network",height = 200,width = 300
                        ))
                }
                
        }, deleteFile = FALSE)
        
        ######################################################
        # Fed-batch process section ##########################
        # Show network ####
        output$network_out_fb_sim <- renderImage({
                list(
                        src = "www/network7.png",
                        contentType = "image/png",
                        alt = "Network",height = 200,width = 250
                )
        }, deleteFile = FALSE)
        
        # Reactive state and parameters ####
        s_rec_val_fb_sim <- reactiveValues(rec_val = numeric())
        p_rec_val_fb_sim <- reactiveValues(rec_val = numeric())
        
        observe({
                
                # Change state and parameters
                s_rec_val_fb_sim$rec_val <- c(x = input$x_int_fb_sim, p = input$p_int_fb_sim,
                                              s = input$s_int_fb_sim, V = input$V0_int_fb_sim)
                
                p_rec_val_fb_sim$rec_val <- c(mu_max = input$vmax_int_fb_sim, ks = input$ks_int_fb_sim,
                                              ki = input$ki_int_fb_sim, Yxs = input$yxs_int_fb_sim, 
                                              Ypx = input$ypx_int_fb_sim, q = input$q_int_fb_sim,
                                              sf = input$sf_int_fb_sim, tf = input$tf_int_fb_sim, 
                                              Vlim = input$Vlim_int_fb_sim) 
        })
        
        # Make simulation ####
        out_rec_fb_sim <- reactive({
                
                fed_batch_fun_fb_sim(s_rec_val_fb_sim$rec_val, 
                                     p_rec_val_fb_sim$rec_val,
                                     as.numeric(input$step_int_fb_sim),
                                     as.numeric(input$end_time_int_fb_sim))
        })
        # Download simulation data ####
        output$down_data_out_fb_sim <- downloadHandler(
                filename = function() {
                        paste("simulation_data", ".xlsx", sep = "")
                },
                content = function(file) {
                        write_xlsx(out_rec_fb_sim(), file)
                }
        )
        ##
        # Show simulation ####
        output$plot_x_out_fb_sim <- renderPlot({
                
                plot_var_fun_fb_sim(data = out_rec_fb_sim(), var = "x", label = "Biomass concentration (g/L)")
        })
        
        output$plot_s_out_fb_sim <- renderPlot({
                
                plot_var_fun_fb_sim(data = out_rec_fb_sim(), var = "s", label = "Substrate concentration (g/L)")
        })
        
        output$plot_p_out_fb_sim <- renderPlot({
                
                plot_var_fun_fb_sim(data = out_rec_fb_sim(), var = "p", label = "Product concentration (g/L)")
        })
        
        output$plot_q_out_fb_sim <- renderPlot({
                
                plot_var_fun_fb_sim(data = out_rec_fb_sim(), var = "q", label = "Inflow velocity (L/h)")
        })
        
        output$plot_v_out_fb_sim <- renderPlot({
                
                plot_var_fun_fb_sim(data = out_rec_fb_sim(), var = "V", label = "Volume (L)")
        })
        
        # Final mass values ####
        fin_mass_rec_val_fb_sim <- reactiveValues(rec_val = data.frame())
        
        # Reactive values ####
        observe({
                
                
                # To calculate substrate consumption 
                n <- nrow(out_rec_fb_sim())
                ind <- which(out_rec_fb_sim()[,6] == 0)[1]
                ind <- ifelse(is.na(ind), n, ind)
                tf <- out_rec_fb_sim()[ind,1]
                
                                        # biomass            # product                    
                end_val_aux <- round(c(out_rec_fb_sim()[n,2], out_rec_fb_sim()[n,3],
                                       # substrate            # volume
                                       out_rec_fb_sim()[n,4], out_rec_fb_sim()[n,5]), 2)
                
                fin_mass_rec_val_fb_sim$rec_val <- list(
                        
                        mx = paste0(round(end_val_aux[1]*end_val_aux[4],2), " (g)"),
                        
                        mp = paste0(round(end_val_aux[2]*end_val_aux[4],2), " (g)"),
                        
                        ms = paste0(round(input$q_int_fb_sim*input$sf_int_fb_sim*(tf - 0),2), " (g)")
                )
                
                
                
        })
        
        
        output$fin_mass_out_fb_sim <- renderTable({
                
                fin_mass_rec_val_fb_sim$rec_val
                
        }, striped = TRUE, spacing = "m", align = "c", digits = 3)
        
        
        ################################################################################
        # Optimization section #########################################################
        # Batch process section #############################
        # Select the panel with the parameters of the model ####
        # Show model 1 panel 
        output$parms_ui_mod1_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model1.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod1_opt", suspendWhenHidden = FALSE)
        
        # Show model 2 panel
        output$parms_ui_mod2_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model2.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod2_opt", suspendWhenHidden = FALSE)
        
        # Show model 3 panel
        output$parms_ui_mod3_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model3.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod3_opt", suspendWhenHidden = FALSE)
        
        # Show model 4 panel
        output$parms_ui_mod4_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model4.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod4_opt", suspendWhenHidden = FALSE)
        
        # Show model 5 panel
        output$parms_ui_mod5_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model5.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod5_opt", suspendWhenHidden = FALSE)
        
        # Show model 6 panel
        output$parms_ui_mod6_opt <- reactive({
                
                ifelse(input$mod_int_opt == "model6.R", TRUE, FALSE)
        })
        
        outputOptions(output, "parms_ui_mod6_opt", suspendWhenHidden = FALSE)
        # Reactive values ####
        # Save parameters values 
        p_val_rec_opt <- reactiveValues(rec_val = numeric())
        
        # Save parameters names to be optimized
        p_name_rec_opt <- reactiveValues(rec_val = character())
        
        # Save parameters ranges to be optimized
        p_range_rec_opt <- reactiveValues(rec_val = list())
        
        model_rec_val_opt <- reactiveValues(rec_val = character())
        
        # Update parameters ####
        observeEvent(input$make_opt_int_opt, { # colcar make_opt_inp_opt
                
                if (input$mod_int_opt == "model1.R") {
                        
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod1_val_int_opt, ks =  input$ks_mod1_val_int_opt,
                                                   Yxs = input$yxs_mod1_val_int_opt, Ypx =  input$ypx_mod1_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod1_name_int_opt, input$ks_mod1_name_int_opt,
                                                    input$yxs_mod1_name_int_opt, input$ypx_mod1_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod1_range_int_opt, ks =  input$ks_mod1_range_int_opt,
                                                        Yxs = input$yxs_mod1_range_int_opt, Ypx =  input$ypx_mod1_range_int_opt)
                        
                }
                
                
                else if(input$mod_int_opt == "model2.R") {
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod2_val_int_opt, ks =  input$ks_mod2_val_int_opt,
                                                   Yxs = input$yxs_mod2_val_int_opt, Ypx =  input$ypx_mod2_val_int_opt,
                                                   kp =  input$kp_mod2_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod2_name_int_opt, input$ks_mod2_name_int_opt,
                                                    input$yxs_mod2_name_int_opt, input$ypx_mod2_name_int_opt,
                                                    input$kp_mod2_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod2_range_int_opt, ks =  input$ks_mod2_range_int_opt,
                                                        Yxs = input$yxs_mod2_range_int_opt, Ypx =  input$ypx_mod2_range_int_opt,
                                                        kp =  input$kp_mod2_range_int_opt)
                }
                
                
                else if(input$mod_int_opt == "model3.R") {
                        
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod3_val_int_opt, ks =  input$ks_mod3_val_int_opt,
                                                   Yxs = input$yxs_mod3_val_int_opt, alpha =  input$alpha_mod3_val_int_opt,
                                                   beta =  input$beta_mod3_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod3_name_int_opt, input$ks_mod3_name_int_opt,
                                                    input$yxs_mod3_name_int_opt, input$alpha_mod3_name_int_opt,
                                                    input$beta_mod3_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod3_range_int_opt, ks =  input$ks_mod3_range_int_opt,
                                                        Yxs = input$yxs_mod3_range_int_opt, alpha =  input$alpha_mod3_range_int_opt,
                                                        beta =  input$beta_mod3_range_int_opt)
                }
                
                
                else if(input$mod_int_opt == "model4.R") { 
                        
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod4_val_int_opt, ks =  input$ks_mod4_val_int_opt,
                                                   Yxs = input$yxs_mod4_val_int_opt, Ypx =  input$ypx_mod4_val_int_opt,
                                                   kd =  input$kd_mod4_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod4_name_int_opt, input$ks_mod4_name_int_opt,
                                                    input$yxs_mod4_name_int_opt, input$ypx_mod4_name_int_opt,
                                                    input$kd_mod4_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod4_range_int_opt, ks =  input$ks_mod4_range_int_opt,
                                                        Yxs = input$yxs_mod4_range_int_opt, Ypx =  input$ypx_mod4_range_int_opt,
                                                        kd =  input$kd_mod4_range_int_opt)
                }
                
               
                else if(input$mod_int_opt == "model5.R") {
                        
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod5_val_int_opt, ks =  input$ks_mod5_val_int_opt,
                                                   Yxs = input$yxs_mod5_val_int_opt, Ypx =  input$ypx_mod5_val_int_opt,
                                                   ki =  input$ki_mod5_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod5_name_int_opt, input$ks_mod5_name_int_opt,
                                                    input$yxs_mod5_name_int_opt, input$ypx_mod5_name_int_opt,
                                                    input$ki_mod5_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod5_range_int_opt, ks =  input$ks_mod5_range_int_opt,
                                                        Yxs = input$yxs_mod5_range_int_opt, Ypx =  input$ypx_mod5_range_int_opt,
                                                        ki =  input$ki_mod5_range_int_opt)
                }
                
                
                else  {
                        
                        
                        p_val_rec_opt$rec_val <- c(mu_max = input$vmax_mod6_val_int_opt, ks =  input$ks_mod6_val_int_opt,
                                                   Yxs = input$yxs_mod6_val_int_opt, alpha =  input$alpha_mod6_val_int_opt,
                                                   kp =  input$kp_mod6_val_int_opt, beta =  input$beta_mod6_val_int_opt)
                        
                        
                        p_name_rec_opt$rec_val <- c(input$vmax_mod6_name_int_opt, input$ks_mod6_name_int_opt,
                                                    input$yxs_mod6_name_int_opt, input$alpha_mod6_name_int_opt,
                                                    input$kp_mod6_name_int_opt, input$beta_mod6_name_int_opt)
                        
                        p_range_rec_opt$rec_val <- list(mu_max = input$vmax_mod6_range_int_opt, ks =  input$ks_mod6_range_int_opt,
                                                        Yxs = input$yxs_mod6_range_int_opt, alpha =  input$alpha_mod6_range_int_opt,
                                                        kp =  input$kp_mod6_range_int_opt, beta =  input$beta_mod6_range_int_opt)
                }
        })
        
        # Load the model ####
        
        observeEvent(input$make_opt_int_opt,{
                
                
                if (input$mod_int_opt == "model1.R") {
                        
                        source("model1.R")
                }
                else if (input$mod_int_opt == "model2.R") {
                        
                        source("model2.R")
                }
                else if (input$mod_int_opt == "model3.R") {
                        
                        source("model3.R")
                }
                else if (input$mod_int_opt == "model4.R") {
                        
                        source("model4.R")
                }
                else if (input$mod_int_opt == "model5.R") {
                        
                        source("model5.R")
                }
                else {
                        
                        source("model6.R")
                }
                
        })
        
        # Load the data ####
        df_rec_opt <- reactive({
                
                read_xlsx(input$data_int_opt$datapath, col_names = TRUE, 
                          sheet = 1) %>% filter(!is.na(time)) %>% select(time,x,p,s) %>% as.data.frame()
        })
        
        # Show data ####
        output$table_data_out_opt <- renderTable({
                
                req(input$data_int_opt)
                
                if(input$header_int_opt) {
                        
                        return(head(df_rec_opt()))
                }
                
                else {
                        return(df_rec_opt())
                }
                
        })
        
        # Plot data ####
        output$plot_data_out_opt <- renderPlot({
                
                req(input$data_int_opt)
                
                plot_data_fun_opt(df_rec_opt())
                
        })
        
        output$down_data_plot_out_opt <- downloadHandler(
                
                filename = function() {
                        
                        paste("data_plot", ".png", sep="")
                },
                
                content = function(file) {
                        
                        ggsave(file,plot_data_fun_opt(df_rec_opt()),
                               width = 10, height = 8)
                }
        )
        
        # Get optimized parameters ####
        opt_parms_rec_opt <- eventReactive(input$make_opt_int_opt, {
                
                
                req(input$data_int_opt)
                req(p_name_rec_opt$rec_val)
                
                get_parms_fun_opt(df_rec_opt(),
                              p_val_rec_opt$rec_val,
                              p_name_rec_opt$rec_val,
                              p_range_rec_opt$rec_val, 
                              input$pop_size_int_opt,
                              input$num_gen_int_opt,
                              input$run_int_opt,
                              input$cross_prob_int_opt,
                              input$mut_prob_int_opt, 
                              input$max_fit_int_opt)
                
        })
        
        # Show comparison plot ####
        comp_rec_opt <- eventReactive(input$make_opt_int_opt ,{
                
                req(input$data_int_opt)
                comp_fun_opt(opt_parms_rec_opt()[["values"]], df_rec_opt())
        })
        
        output$plot_result_out_opt <- renderPlot({
                
                comp_rec_opt()
                
        })
        
        # Download comparison plot #####
        output$down_comp_plot_out_opt <- downloadHandler(
                filename = function() {
                        paste("comparison_plot", ".png", sep = "")
                },
                content = function(file) {
                        
                        ggsave(file, comp_rec_opt(),
                               width = 10, height = 8)
                }
        )
        
        # GA output ####
        output$ga_out_out_opt <- renderTable({
                
                opt_parms_rec_opt()[["ga_out"]]
                
        }, striped = TRUE, colnames = FALSE, spacing = "m", align = "l", digits = 3)
        
        # Download parameters ####
        output$down_opt_parm_out_opt <- downloadHandler(
                filename = function() {
                        paste("kinetic_parameters", ".xlsx", sep = "")
                },
                content = function(file) {
                        write_xlsx(opt_parms_rec_opt()[["ga_out"]], file, col_names = TRUE)
                }
        )
        
        # Plot GA progress ####
        plot_ga_rec_opt <- eventReactive(input$make_opt_int_opt ,{
                
                req(input$data_int_opt)
                plot_ga_fun_opt(opt_parms_rec_opt()[["GA"]])
        })
        
        output$plot_ga_out_opt <- renderPlot({
                
                plot_ga_rec_opt()
                
        })
        
        # Download GA plot #####
        output$down_ga_plot_out_opt <- downloadHandler(
                filename = function() {
                        paste("ga_plot", ".png", sep = "")
                },
                content = function(file) {
                        
                        png(file)
                        plot_ga_fun_opt(opt_parms_rec_opt()[["GA"]])
                        dev.off()
                }
        )
        
        ######################################################
        # Fed-batch process section ##########################
        # Reactive state and parameters ####
        s_rec_val_fb_opt <- reactiveValues(rec_val = numeric())
        p_rec_val_fb_opt <- reactiveValues(rec_val = numeric())
        
        # Change initial state and parameter vector ####
        observe({
                
                s_rec_val_fb_opt$rec_val <- c(x = input$x_int_fb_opt, p = input$p_int_fb_opt,
                                              s = input$s_int_fb_opt, V = input$V0_int_fb_opt)
                
                p_rec_val_fb_opt$rec_val <- c(mu_max = input$vmax_int_fb_opt, ks = input$ks_int_fb_opt, 
                                              ki = input$ki_int_fb_opt, Yxs = input$yxs_int_fb_opt, 
                                              Ypx = input$ypx_int_fb_opt, q = 0, sf = input$sf_int_fb_opt,
                                              tf = input$tf_int_fb_opt, Vlim = input$Vlim_int_fb_opt) 
        })
        
        # Plot fitness function ####
        plot_fitness_rec_fb_opt <- reactive({
                
                plot_fitness_fun_fb_opt(s_rec_val_fb_opt$rec_val, 
                                        p_rec_val_fb_opt$rec_val, 
                                        input$end_time_int_fb_opt, 
                                        input$q_int_fb_opt)
                
        })
        
        
        output$plot_fitness_out_fb_opt <- renderPlot({
                
                plot_fitness_rec_fb_opt()
        })
        
        
        # Download fitness plot #####
        output$down_fit_plot_out_fb_opt <- downloadHandler(
                filename = function() {
                        paste("fitness_plot", ".png", sep = "")
                },
                content = function(file) {
                        
                        ggsave(file, plot_fitness_rec_fb_opt(),
                               width = 10, height = 8)
                }
        )
        # Make optimization ####
        
        get_q_rec_fb_opt <- eventReactive(input$make_opt_int_fb_opt, {
                
                get_q_fun_fb_opt(input$pop_size_int_fb_opt, 
                                 input$num_gen_int_fb_opt,
                                 input$cross_prob_int_fb_opt, 
                                 input$mut_prob_int_fb_opt,
                                 input$run_int_fb_opt,
                                 s_rec_val_fb_opt$rec_val, 
                                 p_rec_val_fb_opt$rec_val, 
                                 input$q_int_fb_opt,
                                 input$end_time_int_fb_opt)
                
        })
        
        # GA output ####
        output$ga_out_out_fb_opt <- renderTable({
                
                get_q_rec_fb_opt()[["ga_out"]]
                
        }, striped = TRUE, colnames = FALSE, spacing = "m", align = "l", digits = 3)
        
        # Download ga output ####
        output$down_ga_out_out_fb_opt <- downloadHandler(
                filename = function() {
                        paste("ga_output", ".xlsx", sep = "")
                },
                content = function(file) {
                        write_xlsx(get_q_rec_fb_opt()[["ga_out"]], file, col_names = FALSE)
                }
        )
        
        
        # Plot GA progress ####
        plot_ga_rec_fb_opt <- eventReactive(input$make_opt_int_fb_opt ,{
                
                plot_ga_fun_opt(get_q_rec_fb_opt()[["GA"]])
        })
        
        
        # Plot GA progress ####
        output$plot_ga_out_fb_opt <- renderPlot({
                
                plot_ga_rec_fb_opt()
        })
        
        
        # Download GA plot #####
        output$down_ga_plot_out_fb_opt <- downloadHandler(
                filename = function() {
                        paste("ga_plot", ".png", sep = "")
                },
                content = function(file) {
                        
                        png(file)
                        plot_ga_fun_opt(get_q_rec_fb_opt()[["GA"]])
                        dev.off()
                }
        )
}

shinyApp(ui, server)