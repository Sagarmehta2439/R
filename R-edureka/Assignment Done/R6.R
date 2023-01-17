library(R6)
# object generator
fruit <- R6Class("Fruit",private = list(name = "apple",cost = 100))
class(fruit)
apple1 <- fruit$new()
apple1

#object generator:Assigning function as R object
employee_generator <- R6Class("Employee",private = list(Name = NA,Designation = NA),
                              public = list(set_name = function(x) {private$Name = x},
                              set_designation = function(x){private$Designation = x}))
class(employee_generator)
employee1 <- employee_generator$new()
employee1
employee1$set_name("Sam")
employee1$set_designation("Ceo")
employee1
employee2 <- employee_generator$new()
employee2
employee2$set_name("Adam")
employee2$set_designation("Cto")
employee2

#fruit generator
fruit_generator <- R6Class("fruit",private = list(Name = NA,Cost = NA),
                           public = list(set_name = function(x){private$Name = x},
                                         set_cost = function(x){private$Cost = x}))
class(fruit_generator)
fruit1 <- fruit_generator$new()
fruit1
fruit1$set_name("Apple")
fruit1$set_cost("100")
fruit1
fruit2 <- fruit_generator$new()
fruit2$set_name("Banana")
fruit2$set_cost("50")
fruit2

# initialize function
food_generator <- R6Class("food",private = list(Name = NA, Cost = NA),
                          public = list(initialize = function(x,y){private$Name = x
                          private$Cost = y}))
pizza <- food_generator$new("Pizza",500)
pizza
salad <- food_generator$new("Salad",330)
salad

#initialize multiple values
student_marks <- R6Class("students",private = list(Name = NA,Branch = NA, CGPA = NA, Placement = NA),
                         public = list(initialize = function(w,x,y,z){
                           private$Name = w
                           private$Branch = x
                           private$CGPA = y
                           private$Placement = z
                         }
                         ))
students <- student_marks$new(c("Mason","Tammy","Declan","Ruben"),
                              c("Mechanical","Electrical","Civil","IT"),
                              c(9,9,6,8),
                              c(T,T,F,T))
students

#active bindings
player_generator <- R6Class("Player",private = list(..Name = NA,..Club = NA),
                            active = list(
                              Name = function(x){private$..Name = x},
                              Club = function(x){private$..Club = x}
                            )
                            )
player1 <- player_generator$new()
player1$Name <- "Mason"
player1$Club <- "ChelseaFC"
player1
player2 <- player_generator$new()
player2$Name <- "Jadon"
player2$Club <- "Borussia Dortmund"
player2
playerstats_generator <- R6Class("Stats",inherit = player_generator,
                                 private = list(Goals = NA,Assists = NA),
                                 public = list(set_goals = function(x){private$Goals = x},
                                               set_assists = function(x){private$Assists = x}))
p1 <- playerstats_generator$new()
p1$Name <- "Cristian"
p1$Club <- "Chelsea"
p1$set_assists(5)
p1$set_goals(10)
p1
playercountry_generator <- R6Class("Country",inherit = playerstats_generator,
                                   private = list(country = NA),
                                   public = list(set_country = function(x){private$country = x}))
p1 <- playercountry_generator$new()
p1
p1$Name <- "Cristian"
p1$Club <- "Chelsea"
p1$set_assists(5)
p1$set_goals(10)
p1$set_country("USA")

#assignment
#1.
library(R6)

#2
#2.Create a new class template/object generator with the name “Football_Generator”, it should comprise of these components:•Three private data members:”Player_Name”, “Player_Club” & “Player_Salary”•Three public functions: “set_name()”, “set_club()” and “set_salary()”
Football_Generator <- R6Class("Football",private = list(Player_Name = NA,Player_Club = NA,Player_Salary = NA),
                              public = list(set_name = function(x){private$Player_Name = x},
                                            set_club = function(x){private$Player_Club = x},
                                            set_salary = function(x){private$Player_Salary = x}))
player1 <- Football_Generator$new()
player1
player1$set_name("Messi")
player1$set_club("Barcelona")
player1$set_salary("£500,000")
player1

#3
Football_Generator <- R6Class("Football",private = list(Player_Name = NA,Player_Club = NA,Player_Salary = NA,Player_Age = NA,Player_Country = NA),
                              public = list(set_name = function(x){private$Player_Name = x},
                                            set_club = function(x){private$Player_Club = x},
                                            set_salary = function(x){private$Player_Salary = x},
                                            set_age = function(x){private$Player_Age = x},
                                            set_country = function(x){private$Player_Country= x}))
player2 <- Football_Generator$new()
player2$set_name("C.Ronaldo")
player2$set_club("Real Madrid")
player2$set_salary("£500,000")
player2$set_age(35)
player2$set_country("Portugal")
player2

#4

Movie_Generator <- R6Class("Movie",private = list(Movie_Name = NA, Protagonist_Name = NA, Movie_Budget = NA),
                           public = list(initialize = function(x,y,z){
                             private$Movie_Name = x
                             private$Protagonist_Name = y
                             private$Movie_Budget = z
                           }))

#5
Movie_details <- Movie_Generator$new(c("Paa","Piku","Pk"),
                                     c('Abhishek Bachan','Amitabh Bachan','Amir Khan'),
                                     c('10crores','20crores','50crores'))
Movie_details

#6
Vegetable_Generator <- R6Class("Vegetable",private = list(..Vegetable_Name = NA,..Vegetable_Cost = NA),
                               active = list(name = function(x){private$..Vegetable_Name = x},
                                             cost = function(x){private$..Vegetable_Cost = x}))

vegetable1 <- Vegetable_Generator$new()
vegetable1$name <- "Cabbage"
vegetable1$cost <- "Rs 150"
vegetable1

