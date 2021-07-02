# GAFerm
This GitHub page contains the code used to developed the GAFerm app. A Shiny web application that can be used for simulation and optimization of fermentation processes. This app allows you to use seven different growth kinetic models to simulate fermentation processes in different operation conditions. Also you can use Genetic Algorithms to carry out parametric optimization and to find the optimal inflow rate to a feed batch bioreactor.

### Dependencies 
The following R packages are used in this app 

| Package          	| Description |
|----------------------	|----------------------------------------------------------------------------------	|
| shiny |shiny makes it incredibly easy to build interactive web applications with R|
| shinydashboard | shinydashboard makes it easy to use Shiny to create dashboards|
| markdown | Markdown is a plain-text formatting syntax that can be converted to XHTML or other formats |
| writexl | writes a data frame to an xlsx file |
| readxl | import excel files into R|
| shinybusy | ddd indicators in your 'shiny' applications to show the user that the server is busy |
| shinythemes | this package contains Bootstrap themes from https://bootswatch.com/, which are packaged for use with Shiny applications |
| deSolve | functions that solve initial value problems of a system of first-order ordinary differential equations (ODE), of partial differential equations (PDE), of differential algebraic equations (DAE) and delay differential equations |
| GA | flexible general-purpose toolbox implementing genetic algorithms (GAs) for stochastic optimisation|
| FME | contains functions to run complex applications of models that produce output as a function of input parameters |
| tidyverse | a set of packages used for data manipulation and visualization |
| shinyWidgets | provides several custom widgets to extend those available in package shiny |


### Input variables for **GAFerm**
GAFerm works with standard xlsx-files (.xlsx). The variables used in GAFerm are:

| Variable             	| Description |
|----------------------	|----------------------------------------------------------------------------------	|
| time | time of each sample in the data|
| x | biomass of each sample in the data |
| p | product of each sample in the data |
| s | substrate of each sample in the data |
 

Each column must be named with the respective name of the variable that it contains.

![Alt text](readme_files/data_format.PNG)

## User interface  
You can access to the web application in the following [link](https://juanmanuelgutierrezg.shinyapps.io/GAFerm/) 

### Simulation section
The simulation section allows you to use seven different models and modify their kinetic and operation parameters. This is a useful tool when you are trying to understand the differences between the models and evaluate the effect of each parameter in the model.
![Alt text](readme_files/user_interface_sim.jpeg)


### Optimization section 
In the optimization section it's possible to enter you data, select a model and both the parameters and their search ranges. Genetic Algorithms are used to carry out the optimization.You have to select the value of the following genetic algorithm parameters: population size, number of generations, crossover probability and mutation probability. 
![Alt text](readme_files/user_interface_opt.jpeg)

### Work team 

This web application was developed as a thesis project. The members and institutions involved are: 

* D.C. Elisa Domínguez Hüttinger 
* D.C. Juan Carlos González Hernández
* Juan Manuel Gutiérrez García 

Instituto Tecnológico de Morelia 

![Alt text](readme_files/tec_logo.PNG)


Instituto de Investigaciones Biomédicas de la Universidad Nacional Autónoma de México

![Alt text](readme_files/unam_logo.PNG)
