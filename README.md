# americanRoulette - R Shiny Implementation
`americanRoulette` is a R Shiny implementation of the American Roulette, the popular casino game. 


## How to Use?
To download and launch `americanRoulette` you need to download and install `devtools` as well as the application itself. To do that use the R Studio console and type in the following code:

    install.packages("devtools")    # install devtools
    devtools::install_github("ignatkulinka/americanRoulette")    #install americanRoulette
    library(americanRoulette)    # import the americanRoulette app
    americanRoulette::launch()    # launch the app!

---
## App Mechanics
`americanRoulette` is divided into parts by tabs on the left and right sides of the screen.

### The left hand side tabs:

![left hand side tabs](images/lhs_tabs.png)

#### Betting


* **Manual Betting**: allows for a user to enter bets. Here the user can manually select a bet amount and then click on the roulette table to place a bet. To select the bet amount click the button with the corresponding dollar amount. The bets can be stacked by clicking the betting spot again to increase bet amount. 

<p align="center">
  <img src="images/250_black.png"> <img src="images/placing_ten.png">
  <br/>
  <i><b>^Placing $250 on Black &nbsp;&nbsp;&nbsp; ^Placing ten random bets</b></i>
</p>


* **Computer Assisted Betting**: another way to bet is to use the random bet generator. First, use the slider to set the number of bets you want to generate and then click the _**place random bet**_ button to automatically add the bets to the roulette table. 


#### Summary
Provides a list overview of the bets that a user has placed. The bets are divided into two tables: Manual and CPU bets. The Reset button allows for the user to clear the table of any bets. In addition, this tab has the _**spin the roulette**_ button. This button will generate a random winning slot and output it. Lastly, the Results table will show the user which of the bets won and lost.  

<p align="center">
   <img src="images/spin_reset.png"> 
   <br/>
   <i><b>^spin the roulette and reset buttons</b></i>
</p>

### The right hand side tabs:

![right hand side tabs](images/rhs_tabs.png)

#### Roulette Table
This tab outputs a simulation of the American roulette table. This plot can be clicked to place bets. The clickable spots follow the general official rules for betting. Bets can be stacked on top of each other by clicking them again.

#### Summary Plots
This tab allows the user to monitor their game session. The user can look at the running total of money lost or won from round to round. In addition, this tab has a bar plot which compares the number of bets won by manual vs CPU bets. Lastly, the third plot shows the frequency bar plot of the slots on the roulette wheel. 

#### Data Output
This tab shows the running list of all the possibly useful data that is being generated by the simulation. By clicking this tab, the user can take a quick look at the data or click the Download button to download a .csv copy of the table.

<p align="center">
  <img src="images/download_data.png"> 
  <br/>
  <i><b>^download the data button</b></i>
</p>

