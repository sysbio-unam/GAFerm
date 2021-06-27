# GAFerm
This GitHub page contains the code used to developed the GAFerm app. A Shiny web application that can be used for the simulation and optimization of fermentation processes. This app allow you to use six different growth kinetic models to simulate fermentation processes in different operation conditions. Also you can use Genetic Algorithms to carry out parametric optimization and to find the optimal inflow rate to a feed batch bioreactor.

## Format
The input data that must be introduced to the app in the "Optimization -> kinetic parameters" seccion should have the following format. 

Excel (extension .xlsx) file with four columns. The column names are: time, x, s and p. The order may be different  

![Alt text](readme_files/data_format.PNG)

## User interface  

### Simulation section
The simulation section allows you to use seven different models and modify their kinetic and operation parameters. This is a useful tool when you are trying tyo understand the differences among the models and evaluate the effect of each parameter in the model.
![Alt text](readme_files/user_interface_sim.jpeg)


### Optimization section 
In the optimization secction it's possible to enter you data, select a model and both the parameters and their search ranges. Genetic Algorithms are used to carry out the optimization, so you select the value of the following genetic algorithm parameters: population size, number of generations, crossover probability and mutation probability. 
![Alt text](readme_files/user_interface_opt.jpeg)
