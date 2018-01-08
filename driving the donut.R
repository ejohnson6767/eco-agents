################# Driving The Donut #################

#####################################################
# RUN THIS FIRST
library(rJava)
javaClassPath <- "/C:/Users/Evan/workspace/IBMGeneralEvolvable/"
.jinit()
.jaddClassPath(javaClassPath)
create2SpeciesCommunity <- function()
{
gridLengthEX = 100
dtEX = 1
n1InitAbundEX = 2000
n2InitAbundEX = 2000
r1EX = 0.6
r2EX = 0.6
d1EX = 0.2
d2EX = 0.2
disp1EX <- 3
disp2EX <- 3
listEX <- .jnew("java.util.ArrayList")
listEX$add(.jnew("Species",r1EX,d1EX,as.integer(disp1EX)))
listEX$add(.jnew("Species",r2EX,d2EX,as.integer(disp2EX)))
com <-.jnew("Community",as.integer(gridLengthEX),dtEX,listEX)
com
}
create2SpeciesCommunityAndInoculate <- function()
{
  com<-create2SpeciesCommunity()
  ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
  ac$addCrittersRandomly(as.integer(1000))
  ac <- .jnew("AddCritters", com, as.integer(2), FALSE)
  ac$addCrittersRandomly(as.integer(1000))
  com
}

################# Table of Contents #################

# 1. setup
## 1.1 Java Setup
## 1.2 notes on rJava and creating java objects
### 1.2.1 types
### 1.2.2 constructors, methods, and static methods
### 1.2.3 converting between java arrays and R arrays
## 1.3 a basic community

# 2. a quantitative cheater community
## 2.1 a two species community
## 2.2 a three species community - third is a double-producer

# 3 adding and removing individuals/species
## 3.1 inoculating/adding individuals
### 3.1.1 using the species constructor
### 3.1.1 using the addCritters class
#### 3.1.2.1  using the addCritters Class to randomly add individuals 
#### 3.1.2.2  using the addCritters Class to add individuals uniformly
#### 3.1.2.3  using the addCritters Class to add individuals in a tight cluster
#### 3.1.2.4  using the addCritters Class to add a single individual
#### 3.1.2.5  using the addCritters Class to add individuals around other individuals
#### 3.1.2.6  using the addCritters Class and avoiding overwriting a subset of heterospecifics
#### 3.1.2.7 using the addCritters Class to add multiple species at once
## 3.2 removing individuals using the removeCritters class

# 4 traversing time: step and scramble methods
## 4.1 a few notes on dt 
## 4.2 stepping 
## 4.3 scrambling
## 4.4 stepping until something happens, the ITerminateCondition interface

# 5 time series stuff
## 5.1 construcing the object
## 5.2 abundance time series
## 5.3 grid time series
## 5.4 flexible time series:
###    abundance, pair correlations, lambda tila, and beyond
## 5.5 time series until something happens: the return of ITerminateCondition

# 6 measuring invasion fitness
## 6.1 binary persistence 
## 6.2 indirect lambda
## 6.3 direct lambda

# 7 Visualizing the simulations
## 7.1 R plots and Sys.sleep
## 7.2 animation

# 8 Individual Based Evolution
## 8.1 the setup, from a software engineering perspective
## 8.2 the key ingredient: a mutation function
## 8.3 how to start evolution: an example with dispersal distance
## 8.4 getting individual-based trait data
## 8.5 evolving dispersal
## 8.6 evolving "excreted" effect magnitude: an example with a predator-prey system
## 8.7 evolving "excreted" effect diffusiveness: an example with a predator-prey system
## 8.8 evolving mutation proabilities
## 8.9 evolving mutation magnitudes
## 8.10 evolving q: an example with quantitative cheaters






################ 1) ################

####################################
# 1.1 Java Setup

# load R - Java interface. Install if not already installed
# isntall.packages("rJava")
library(rJava)

# sets us up to work with the individual based model package contents. This is taking us to my eclipse working directory.
javaClassPath <- "/C:/Users/Evan/workspace/IBMGeneralEvolvable/"

# starts the virtual machine and tells R where my java classes are. Use commented out option if you need to allocate more memory to java
#.jinit(parameters = getOption("-Xmx2048m"))
.jinit()
.jaddClassPath(javaClassPath)

####################################
# 1.2 notes on rJava and creating java objects

####################################
# 1.2.1 Types

# R interprets the type of object you want, whereas java demands that you specify the type. Most of Java's types have a one-to-one corollary in R.
## for example, logical in R is boolean in Java. However, all of R's numeric values are automatically double (double-precision numbers), so
## if java needs an integer, you have to convert to R's integer type with as.integer(my_value) 
## here's what I mean...
aNum <- 7
is.integer(aNum)
is.double(aNum)

aNum <- as.integer(aNum)
is.integer(aNum)

#####################################
# 1.2.2 constructors, methods, and static methods

# In RJava, you construct an object using .jnew("obj_name", params...)
# you use methods with .jcall(java_obj, "return_signature_in_JNI", "method_name", params...)...
## However, there is a simpler way. You can use the MONEY SIGN OPERATOR $ like you would use a period in Java.
## for example, if your object (implicit parameter) is a car, your method is drive, and your explicit parameter is integer miles...
## In Java: car.drive(10)
## In R: car$drive(as.integer(10))
## the ease of the dollar sign trades off with speed. For our purposes, it won't make much of a difference; almost all operations is done in Java.

# many class have multiple constructors. To see these, use...
.jconstructors("Species")
# unfortunatly, this function doesn't show the names of the parameters. Consult the java docs for that information
# similarly, you can find look at methods. 
.jmethods("Species")

# to call a Static method use the syntax...
#J("Species")$method(params...)



# in Java, arrays are immutable objects, so R doesn't know how to represent them without some post processing. 
# to make a java array...
jArr <- .jarray(c(1,2,3))
# to convert to an R vector...
.jevalArray(jArr)
# say that you have a two dimensional java array...
jArr2 <- .jarray(matrix(1:9, nrow = 3), dispatch = TRUE)
# using .jevalArray plainly will give a you a list of java arrays. To get your matrix back, use...
.jevalArray(jArr2, simplify = TRUE)
# this should work for higher dimensional arrays


####################################
# 1.3 a basic community

# parameters for creating species and the community.
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
r1 = 0.46
r2 = 0.46
d1 = 0.2
d2 = 0.2
disp1 <- 1
disp2 <- 2

# put species objects in a list
list <- .jnew("java.util.ArrayList")
  
# add the first species
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)

# add the second species
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)

# creates a community
com <-
  .jnew(
    "Community",
    as.integer(gridLength),
    dt,
    list
  )

################ 2) ################
# quantitative cheater

# 2.1 a two species community

# parameters for creating species and the community.
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
# these won't matter, they'll get overwritten by the quant cheater's birth process
r1 = 0.46
r2 = 0.46
d1 = 0.2
d2 = 0.2
disp1 <- 1
disp2 <- 2

# make baseline species, just like in 1.3
list <- .jnew("java.util.ArrayList")

# add the first species
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)

# add the second species
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)

# setup parameters that are specfic to quantitative cheaters
bmin1 <- 0.7
bmin2 <- 0.7
gain1 <- 0.1
gain2 <- 0.1
q1 <- 0.8
q2 <- 0.8
theta1 <- 5
theta2 <- 5

K <- 1.5

# there are two fundamental differences between quantitative cheaters and baseline species: 1) the resources they excrete 2) their birth process (realized b * resource limatation)
## first, let's create the different birth processes...
bpQCn1 <- .jnew("BirthQuantCheat", bmin1, gain1, K, q1, theta1)
bpQCn2 <- .jnew("BirthQuantCheat", bmin2, gain2, K, q2, theta2)
# ... then give them to our basleine species
## note that since indexing in java is zero-based, the species 1 is retrieved with get(0)
list$get(as.integer(0))$setBirthProcess(bpQCn1)
list$get(as.integer(1))$setBirthProcess(bpQCn2)

# next, we have to tell the species what they're excreting
# setup a few params first
n1DiffusionRadius <- 3
n2DiffusionRadius <- 3

# the last argument [1], is the type of resource the species is making. species 1 makes type 1. Species 2 makes type 2.
eff1 <- .jnew("Effect", q1, as.integer(n1DiffusionRadius), as.integer(1))
eff2 <- .jnew("Effect", q2, as.integer(n2DiffusionRadius), as.integer(2))

# now we have to give the species their resource excrement responsibilities, and make sure the other species can "see" the resources
# the following line reads... species 1 [speciesList.get(0)] affects species 1 [setLocalEffect(1...,)] with resource 1 (...eff1)
list$get(as.integer(0))$setLocalEffect(as.integer(1), eff1)
# it also affects species 2
list$get(as.integer(0))$setLocalEffect(as.integer(2), eff1)

list$get(as.integer(1))$setLocalEffect(as.integer(1), eff2)
list$get(as.integer(1))$setLocalEffect(as.integer(2), eff2)


###############################
# 2.2 a three species community - the third species is a double producer


# parameters for creating species and the community.
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
# these birth rates won't matter, they'll get overwritten by the quant cheater's birth process
r1 <- 0.46
r2 <- 0.46
r3 <- 0.46
d1 <- 0.2
d2 <- 0.2
d3 <- 0.2
disp1 <- 1
disp2 <- 2
disp3 <- 3

# make baseline species, just like in 1.3
list <- .jnew("java.util.ArrayList")

# add the first species
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)

# add the second species
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)

# add the third species
list$add(
  .jnew(
    "Species",
    r3,
    d3,
    as.integer(disp3)
  )
)



# setup parameters that are specfic to quantitative cheaters
bmin1 <- 0.7
bmin2 <- 0.7
gain1 <- 0.1
gain2 <- 0.1
q1 <- 0.8
q2 <- 0.8
theta1 <- 5
theta2 <- 5

K <- 1.5


# add the two syntrophs/cheaters

# there are two fundamental differences between quantitative cheaters and baseline species: 1) the resources they excrete 2) their birth process (realized b * resource limatation)
## first, let's create the different birth processes...
bpQCn1 <- .jnew("BirthQuantCheat", bmin1, gain1, K, q1, theta1)
bpQCn2 <- .jnew("BirthQuantCheat", bmin2, gain2, K, q2, theta2)



# add the the double producer/ double cheater
# setup parameters that are specfic to double producer
bmin3 <- 0.7
gain31 <- 0.1
gain32 <- 0.1
q31 <- 0.8
q32 <- 0.8
theta31 <- 5
theta32 <- 5

# just a note, the realized birth rate for this speces is bmin ( 1 + gain1(1-q1) + gain2(1-q2))
bpQCn3 <- .jnew("BirthQuantCheatGen", bmin3, .jarray(c(gain31, gain32)), K, .jarray(c(q31, q32)), .jarray(c(theta31, theta32)))
# ... then give them to our basleine species
## note that since indexing in java is zero-based, the species 1 is retrieved with get(0)
list$get(as.integer(0))$setBirthProcess(bpQCn1)
list$get(as.integer(1))$setBirthProcess(bpQCn2)
list$get(as.integer(2))$setBirthProcess(bpQCn2)

# next, we have to tell the species what they're excreting
# setup a few params first
n1DiffusionRadius <- 3 # how far does species 1 spit out resource 1
n2DiffusionRadius <- 3 # how far does species 2 spit out resource 2
n31DiffusionRadius <- 3 # how far does species 3 spit out resource 1
n32DiffusionRadius <- 3 # how far does species 3 spit out resource 2


# the last argument [1], is the type of resource the species is making. species 1 makes type 1. Species 2 makes type 2.
eff1 <- .jnew("Effect", q1, as.integer(n1DiffusionRadius), as.integer(1))
eff2 <- .jnew("Effect", q2, as.integer(n2DiffusionRadius), as.integer(2))
eff31 <- .jnew("Effect", q31, as.integer(n31DiffusionRadius), as.integer(1))
eff32 <- .jnew("Effect", q32, as.integer(n32DiffusionRadius), as.integer(2))


# now we have to give the species their resource excrement responsibilities, and make sure the other species can "see" the resources
# the following line reads... species 1 [speciesList.get(0)] affects species 1 [setLocalEffect(1...,)] with resource 1 (...eff1)
list$get(as.integer(0))$setLocalEffect(as.integer(1), eff1)
list$get(as.integer(0))$setLocalEffect(as.integer(2), eff1)
list$get(as.integer(0))$setLocalEffect(as.integer(3), eff1)

# species 2 gives it's effects
list$get(as.integer(1))$setLocalEffect(as.integer(1), eff2)
list$get(as.integer(1))$setLocalEffect(as.integer(2), eff2)
list$get(as.integer(1))$setLocalEffect(as.integer(3), eff2)

# species 3 gives it's effects
list$get(as.integer(2))$setLocalEffect(as.integer(1), eff31)
list$get(as.integer(2))$setLocalEffect(as.integer(2), eff31)
list$get(as.integer(2))$setLocalEffect(as.integer(3), eff31)
list$get(as.integer(2))$setLocalEffect(as.integer(1), eff32)
list$get(as.integer(2))$setLocalEffect(as.integer(2), eff32)
list$get(as.integer(2))$setLocalEffect(as.integer(3), eff32)

# you can see that this gets sloppy as the number of species grows. Of course, you can always loop-ify it.







################ 3) ################
# adding and removing individuals/species

####################################
#3.1 inoculating/adding individuals

####################################
#3.1.1 using the species constructor

# there are a few ways to inoculate species. If you want individuals to be randomly placed on the grid at a pre-determined time, 
##you can take care of inoculation in the construction of the species. Check out this example below

#check out this example below
# regular params
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
r1 = 0.46
r2 = 0.46
d1 = 0.2
d2 = 0.2
disp1 <- 1
disp2 <- 2

#inoculation params
n1InitAbund <- 1000 # number of individuals to be inoculate
n1InoculationTime <- 0 # what time does inoculation happen? automatically scaled my 
n1Overwrite <- TRUE # can you overwrite heterospecifics during inoculation. Meaningless if this is the first species to be inoculated
n2InitAbund <- 1000 
n2InoculationTime <- 10 
n2Overwrite <- TRUE

# put species objects in a list
list <- .jnew("java.util.ArrayList")

# add the first species
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1),
    as.integer(n1InitAbund),
    as.integer(n1InoculationTime),
    n1Overwrite
  )
)

# add the second species
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2),
    as.integer(n2InitAbund),
    as.integer(n2InoculationTime),
    n2Overwrite
  )
)

# creates a community
com <-
  .jnew(
    "Community",
    as.integer(gridLength),
    dt,
    list
  )

# at time zero, no one is around
com$getAbundances()
# do one step, n1 should have been inoculated
com$step()
com$getAbundances()
# note that the n1 abundance isn't exactly 1000. This is because n1 was inoculated and then a time step was executed.
# what about n2...
com$step(as.integer(10))
com$getAbundances()


######################################
# 3.1.2 using the addCritters Class

# I prefer addCritters, as it gives you more control
# use the shortcut function to make a basic community
com <- create2SpeciesCommunity()

######################################
# 3.1.2.1  using the addCritters Class to randomly add individuals 
com <- create2SpeciesCommunity()
# the params here are community object, species number, and should overwrite heterospecifics
ac <- .jnew("AddCritters", com, as.integer(1), n1Overwrite)
# to add critters randomly 
ac$addCrittersRandomly(as.integer(100))

ac <- .jnew("AddCritters", com, as.integer(2), n1Overwrite)
# to add critters randomly 
ac$addCrittersRandomly(as.integer(100))
# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
myColors <- c("Black", "Red", "Green" )
image(grid, xaxt = "n", yaxt = "n", col = myColors)


######################################
# 3.1.2.2  using the addCritters Class to add individuals uniformly

# you can also add critters uniformly (to maximize the distance between individuals)
com <- create2SpeciesCommunity()
ac$addCrittersUniformly(as.integer(100))
# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
image(grid)


######################################
# 3.1.2.3  using the addCritters Class to add individuals in a tight cluster

# say you want to inoculate a species in a tight cluster
# this will put species 1 in a 10x10 box, the bpttom-left corner of which is centered at 50,50
com <- create2SpeciesCommunity()
ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
ac$setReferenceLocation(as.integer(50), as.integer(50)) 
ac$setDispersalLength(as.integer(10)) 
ac$addCrittersRandomly(as.integer(100))
com$getAbundances()# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
image(grid)




######################################
# 3.1.2.4  using the addCritters Class to add a single individual

# similarly, If you want to add one Critter at one location
com <- create2SpeciesCommunity()
ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
ac$setReferenceLocation(as.integer(50), as.integer(50)) 
ac$setDispersalLength(as.integer(1)) 
ac$addCrittersRandomly(as.integer(1))
com$getAbundances()# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
image(grid)




######################################
# 3.1.2.5  using the addCritters Class to add individuals around other individuals

# you can also use [addCrittersNearby] add critters around where there are already critters (this is one way of adding critters and somewhat preserving spatial structure)
com <- create2SpeciesCommunity()
ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
ac$setReferenceLocation(as.integer(50), as.integer(50)) 
ac$setDispersalLength(as.integer(10)) 
ac$addCrittersRandomly(as.integer(100))
ac$addCrittersNearby(as.integer(100))
com$getAbundances()# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
image(grid)


######################################
# 3.1.2.6  using the addCritters Class and avoiding overwriting a subset of heterospecifics

# say that you want to be able to overwrite all but one type of heterospecific, use...
# ac$addCrittersRandomlyWithLimitations(as.integer(100), .jarray(c(as.integer(offLimitSpeciesValue))))
# ac$addCrittersNearbyWithLimitations(as.integer(100), .jarray(c(as.integer(offLimitSpeciesValue))))


################################################
# 3.1.2.7 using the addCrittersMultipleSpecies Class to add multiple species at once

# maybe you want to add two species at once. If you try adding species one after the other. If you add enough individuals, the first species will have a truly...
## ...poisson distribution, but the second will "fill in the cracks". To give both species the same-ish spatial distribution, a different algorithm must be used.
## Note that if you use the species constructor method to inoculate, and you specify two or more species to be inoculated at one time, this is the method that gets called, behind the scenes.
com <- create2SpeciesCommunity()
# params are community object, array of species values, array of overwrite booleans
acms <- .jnew("AddCrittersMultipleSpecies", com, .jarray(c(as.integer(1), as.integer(2))), .jarray(c(FALSE,FALSE)))
# note that you can only add individuals of multiple species in a random fashion
# param is an array of abundances
acms$addCrittersRandomlyMultipleSpecies(.jarray(c(as.integer(100), as.integer(100))))
com$getAbundances()# see?
# install.packages("Matrix")
library(Matrix)
grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
myColors <- c("Black", "Red", "Green" )
image(grid, col = myColors)

###########################################################
## 3.2 removing individuals using the removeCritters class
# removing individuals is straightforward. First, I'll add critters, and then take some away

# I prefer addCritters, as it gives you more control
#check out this example below
# regular params
com <- create2SpeciesCommunity()
ac <- .jnew("AddCritters", com, as.integer(1), TRUE)
ac$addCrittersRandomly(as.integer(1000))
com$getAbundances()

# ok, let's take some away
rc <- .jnew("RemoveCritters", com, as.integer(1))
rc$removeCritters(as.integer(9))
com$getAbundances()

# to kill off a species entirely, find the number of individuals in that population, and then remove that many
popSize <- com$getAbundances()
rc$removeCritters(as.integer(popSize[1]))
com$getAbundances()


################ 4) ################
# traversing time: step and scramble methods


####################################
# 4.1 a few notes on dt 

# In the IBM, you traverse time be calling the step method. dt is the length of the step method in time units. As dt goes to zero, the IBM more closely approximates a continuous time system
# Rates in the birth and death processes are in units births/time unit. The program automatically scales the birth/death (per step method call) by dt for consistency.
# If you use the species constructor for inoculation (see 3.1.1), the program automatically calculates number of time steps until inoculation, based on dt.
# If you're interested in exactly what's going on behind the scenes, please reference the scaleByDt method in the community class
## for example, if you tell the species to inoculate at time 10, and dt = 0.1, then the species will be inoculated after 100 calls to the step method. 

# if you want the system to run for a certain amount of time units, but you don't want to calculate the number of step calls you need to make (based on dt)...
## then use stepThroughTime. This method automatically does the math for you: num step calls : round(time/dt). Note the rounding.

# if you want to know how many step method calls have occured, use...
com$getStepCallCounter()

# if you want to know how much time has elapsed, use...
com$getTime()

# you can change dt mid-simulation, although I don't recommend it
com$setDt(0.1)


####################################
## 4.2 stepping
com <- create2SpeciesCommunityAndInoculate()
# execute a time step method call with 
com$step()

# make multiple calls to step
com$step(as.integer(1000))
# NOTE: why not just put com$step() in an R loop? Well, it just so happens that communication between R and Java is relatively expensive, not to mention that loops are slow in R.
## ... the lesson is simple: if performance is critical, minimize R code and rely on the underlying Java code

## what if you want to step to a particular time (approximately, see 4.1, a few  notes on dt)? Simple. Use stepThroughTime
com$stepThroughTime(as.integer(1000))


#####################################
## 4.3 scrambling

# in past explorations with IBMs, it has been useful to scatter/scramble up the entire community. All individuals are taken off the lattice and then re-inoculated randomly
## This is one way of breaking up spatial structure. 

# if you want to scramble up the entire community, make a scramble object...
com <- create2SpeciesCommunityAndInoculate()
s <- .jnew("Scramble", com)
# ... and scramble away
s$scramble()

# if you only want to scramble a subset of the species, make an array of the species Values you want to scramble, and use...
s$scrambleSomeSpecies(.jarray(as.integer(1)))

# if you want to step and then scramble right after...
com$stepAndScramble()
# if you want to do this for many step method calls...
com$stepAndScramble(as.integer(100))

# if you want to step through time (see 4.2) and then scramble right after...
com$stepAndScrambleThroughTime()
# if you want to do this for many step method calls...
com$stepAndScrambleThroughTime(as.integer(100))
# note that when you step through time, scramble gets called after every step method call, which is not necessarily once every time unit.
## you can always devise something more intricate, like...
dt <- com$setDt(0.1)
for(j in 1:100)
{
  com$step(as.integer(10))
  s <- .jnew("Scramble", com)
  s$scramble()
}


#####################################
## 4.4 stepping until something happens

# it's likely that you'll want to run a simulation until something happens. Once that "thing" happens, you no longer need to keep the simulation running.
## here's the way to do that, using the stepAndMaybeTerminate method in the commuity class. 
### This method takes the following parameters, max number of step call, check for termination every "this many" step calls, scramble (Y/N), and your termination condition.
#### your termination condition is actually some object that implements the ITerminateCondition interface (an interface is a sort of contract between many object)

# for example, you want to stop stepping when any species dies
com <- create2SpeciesCommunityAndInoculate()
com$step()
# set dispersal higher for species 1 - farther dispersers usually displace less disperseres
sp1 <- com$getSpeciesList()$get(as.integer(0))
newD <- .jnew("DispersalStrategyStandard", as.integer(10), .jcast(sp1,"ISpecies"))
newD$setupAfterCommunityIsCreated(com)
sp1$setDispersalStrategy(newD)

tc<- .jnew("TerminateIfAnyoneDies", com)
com$stepAndMaybeTerminate(as.integer(1000), as.integer(1), FALSE, tc)
# see? species 1 wins
com$getAbundances()
com$getStepCallCounter()

# there is a step through time variant (see 4.1 and 4.2), where you step through time and check the condition once every x time units 
com$stepAndMaybeTerminateThroughTime(as.integer(1000), as.integer(1), FALSE, tc)

# of course, you can always jerry-rig your own Terminate if anyone dies condition...
for(i in 1000)
{
  com$step()
  abunds <- com$getAbundances()
  if(any(abunds == 0))
  {
    break
  }
}
# as you can see, the above is a bit simpler, but it comes at the cost of performance. Making your own ITerminateCondition object in Java should only take minute or two. 

## the other ITerminateCondition objects are...

tc <- .jnew("TerminateIfSpeciesDiesOrGrowsTooLarge", com, as.integer(speciesValue), as.integer(abundUpperBound)) # stops of the specified species dies or grows more abundant (units: individuals) than abundUpperBound
# good for invasion fitness related things

# and this one
tc <- .jnew("TerminateIfTooFewOrTooMany", com, as.integer(speciesValue), as.integer(tooFew), as.integer(tooMany))




################ 5) ################
# time series stuff


####################################
# 5.1 making the time series object

# all time series start with a time series object...
# params
com <- create2SpeciesCommunityAndInoculate()
numSteps <- 1000 
measureHowOften <- 1
throughTime <- FALSE # if true, then you step until numSteps has been reached, except in time units. Additionally, measureHowOften no longer refers to step calls, but time units. See 4.1 and 4.2 for more information
scramble <- FALSE

ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften))
# there are alternate constructors with a throughtime and scramble option. If you just use the first constructor, you never scramble or use step "through time" 
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften), throughTime)
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften), throughTime, scramble)


####################################
# 5.2 abundance time series
# setup
com <- create2SpeciesCommunityAndInoculate()
numSteps <- 1000 
measureHowOften <- 1
throughTime <- FALSE
scramble <- FALSE
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften))


reportDensity <- FALSE # if true, gives time series in densities (num individuals/ total sites) instead of raw abundances
abundsOverTime <- .jevalArray(ts$timeSeries(reportDensity), simplify = TRUE)
maxim <- max(abundsOverTime)
plot(abundsOverTime[,1], col = "Green", ylim = c(1000, maxim))
points(abundsOverTime[,2], col = "Red")



####################################
# 5.3 grid time series=
# sometimes you want to run your own functions on the entire state of the system. If there's no individual-based evolution, then a picture of the lattice should give you the whole picture

# setup
com <- create2SpeciesCommunityAndInoculate()
numSteps <- 1000 
measureHowOften <- 1
throughTime <- FALSE
scramble <- FALSE
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften))

grids <- .jevalArray(ts$gridTimeSeries(), simplify = TRUE)
str(grids) # the first dimension is time, the second and thirdare space

####################################
# 5.4 flexible time series 
# you can make a time series of any object that implements the ITimeSeriesQuantity interface. The interface only forces you to add concretions for two methods - get and returnLength -  so making your own is easy
# setup
com <- create2SpeciesCommunityAndInoculate()
com$step(as.integer(10))
numSteps <- 1000 
measureHowOften <- 1
throughTime <- FALSE
scramble <- FALSE
speciesValue <- 1
reportDensity <- FALSE
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften), FALSE)

# here are some available objects

# lambdas correct for dt, so it's always n(t+1)/n(t)
lb <- .jnew("LambdaBar", com, as.integer(speciesValue))
lt <- .jnew("LambdaTilda", com, as.integer(speciesValue))
ldc <- .jnew("LambdaDensityCovariance", com, as.integer(speciesValue))
tsaas <- .jnew("TimeSeriesAbundAllSpecies", com, reportDensity)
tsass <- .jnew("TimeSeriesAbundSpeciesSubset", com, reportDensity, .jarray(c(as.integer(1))))

df<-as.matrix(data.frame(c(1,1,1,1), c(1,1,1,1), c(1,2,3,4)))
df <- apply(df, c(1,2), as.integer)
maxRadius <- 8
tspc <- .jnew("TimeSeriesPairCorrelation", com, as.integer(maxRadius), .jarray(df, dispatch = TRUE))

tsq <- .jnew("java.util.ArrayList")
tsq$add(lb)
tsq$add(lt)
tsq$add(ldc)
tsq$add(tsaas)
tsq$add(tsass)
tsq$add(tspc)

dat <- .jevalArray(ts$timeSeries(tsq), simplify = TRUE)

### you can use the get method to get any of these quantites. For example ... 
lb$get()



####################################
# 5.5 time series until something happens: the return of IterminateCondition
## any of the timeSeries methods we've seen up to this point can take one final argument, and ITerminateCondition object
### Here's an example using the code above

# setup
com <- create2SpeciesCommunityAndInoculate()
com$step(as.integer(40))
sp1 <- com$getSpeciesList()$get(as.integer(0))
newD <- .jnew("DispersalStrategyStandard", as.integer(10), .jcast(sp1,"ISpecies"))
newD$setupAfterCommunityIsCreated(com)
sp1$setDispersalStrategy(newD)
numSteps <- 1000 
measureHowOften <- 1
throughTime <- FALSE
scramble <- FALSE
speciesValue <- 1
reportDensity <- FALSE
ts <- .jnew("TimeSeries", com, as.integer(numSteps), as.integer(measureHowOften), FALSE)

# here are some available objects

# lambdas correct for dt, so it's always n(t+1)/n(t)
lb <- .jnew("LambdaBar", com, as.integer(speciesValue))
lt <- .jnew("LambdaTilda", com, as.integer(speciesValue))
ldc <- .jnew("LambdaDensityCovariance", com, as.integer(speciesValue))
tsaas <- .jnew("TimeSeriesAbundAllSpecies", com, reportDensity)
tsass <- .jnew("TimeSeriesAbundSpeciesSubset", com, reportDensity, .jarray(c(as.integer(1))))

df<-as.matrix(data.frame(c(1,1,1,1), c(1,1,1,1), c(1,2,3,4)))
df <- apply(df, c(1,2), as.integer)
maxRadius <- 8
tspc <- .jnew("TimeSeriesPairCorrelation", com, as.integer(maxRadius), .jarray(df, dispatch = TRUE))

tsq <- .jnew("java.util.ArrayList")
tsq$add(lb)
tsq$add(lt)
tsq$add(ldc)
tsq$add(tsaas)
tsq$add(tsass)
tsq$add(tspc)
tc<- .jnew("TerminateIfAnyoneDies", com)

dat <- .jevalArray(ts$timeSeries(tsq, tc), simplify = TRUE)
com$getAbundances()





################ 6) ################
# 6. measuring invasion fitness
# I'll demonstrate several ways to measure invasion fitness. In each case, the general approach is the same: let the resident equilibrate, inoculate the invader, and finally so some measuring
# in the following subsections, I'll use the same example, the invader has a higher dispersal rate than the resident, so it should be able to invade

####################################
# 6.1 binary persistence.
# this is the dumb approach. A while after inoculation check to see if the invader is still around. If it is, that's a 1. If not, 0. Average this over many runs to estimate the probability of persistence

# first, the setup
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
r1 = 0.7
r2 = 0.7
d1 = 0.2
d2 = 0.2
disp1 <- 10
disp2 <- 1
list <- .jnew("java.util.ArrayList")
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)
com <-
  .jnew(
    "Community",
    as.integer(gridLength),
    dt,
    list
  )

# now inoculate species 2, the resident...
ac <- .jnew("AddCritters", com, as.integer(2), FALSE)
ac$addCrittersRandomly(as.integer(5000))

# let it step to equilibrium. You have to make a judgment call here, though there are heuristic approaches
com$step(as.integer(3000))

# save this a reference point
gridState <- com$getEnvironment()$getGrid()

persistProb <- c()

for(i in 1:10)
{
  # revert back to the old grid state. This prevents us from having to let the residents equilibrate every invasion rep. 
  # I assume that this won't lead to any appreciable psuedosampling effects, since the invader's low-density inoculation should be sufficiently random
  com$getEnvironment()$setGrid(gridState)  
  
  # now inoculate the invader - 10 individuals
  ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
  ac$addCrittersRandomly(as.integer(10))
  
  # now call step a maximum of 1000 times. If the invader dies or has a population of over 500, then terminate
  tc <- .jnew("TerminateIfSpeciesDiesOrGrowsTooLarge", com, as.integer(1), as.integer(500)) 
  com$stepAndMaybeTerminate(as.integer(1000), as.integer(50), FALSE, tc )
  invAbund <- com$getAbundances()[1]
  if(invAbund == 0)
  {
    persistProb <- append(persistProb, 0)
  } else
  {
    persistProb <- append(persistProb, 1)
  }
}

mean(persistProb)
# notice that the persistence probability is very low, despite the fact that the invader should be superior. This points to strong stochastic effects and strong priority effects.
# when it comes to dispersal distances, I've found that very relatively high dispersal distances don't do as well as some lower dispersal distances...
## I'm guessing that this is because at lower dispersal distances, a lucky individual can grow to a cluster, and if the edge individuals die, then the center individuals have immediate access to that space.

####################################
# 6.2 indirect lambda - measure lambda using the formula lambda = (n(t+x) / n(t)) ^ (1/(x-1)) 

# first, the setup
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
r1 = 0.7
r2 = 0.7
d1 = 0.2
d2 = 0.2
disp1 <- 10
disp2 <- 1
list <- .jnew("java.util.ArrayList")
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)
com <-
  .jnew(
    "Community",
    as.integer(gridLength),
    dt,
    list
  )

# now inoculate species 2, the resident...
ac <- .jnew("AddCritters", com, as.integer(2), FALSE)
ac$addCrittersRandomly(as.integer(5000))

# let it step to equilibrium. You have to make a judgment call here, though there are heuristic approaches
com$step(as.integer(3000))

# save this a reference point
gridState <- com$getEnvironment()$getGrid()

lambdaVec <- c()

for(i in 1:10)
{
  # revert back to the old grid state. This prevents us from having to let the residents equilibrate every invasion rep. 
  # I assume that this won't lead to any appreciable psuedosampling effects, since the invader's low-density inoculation should be sufficiently random
  com$getEnvironment()$setGrid(gridState)  
  
  # now inoculate the invader - 10 individuals
  ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
  ac$addCrittersRandomly(as.integer(10))
  
  # try for a max of 100 step calls, measure every step
  ts <- .jnew("TimeSeries", com, as.integer(1000), as.integer(1))
  # terminate if the invader dies out or the population has grown to 100
  tc <-  .jnew("TerminateIfSpeciesDiesOrGrowsTooLarge", com, as.integer(1), as.integer(100))
  tsDat <- .jevalArray(ts$timeSeries(FALSE, tc), simplify = TRUE)[,1]
  lambda <- 0
  if(length(tsDat) > 1 )
  {
    lambda <- (tsDat[length(tsDat)] / tsDat[1]) ^ (1/(length(tsDat) -1)) 
  }
  lambdaVec <- append(lambdaVec, lambda)
}

mean(lambdaVec)


####################################
# 6.2 direct lambda - measure lambda tilda using fitness generating functions
# at every time step, the per capita fitness is measured using fitness generating functions. 
## The outcome n(t+1)/n(t) is not measured, but the average outcome ...
### ...(what would happen if to that individual if that time step was replayed 1 million times and averaged).

# you can also measure lambda bar and fitness density covariance, which I do here for demonstrative purposes

# first, the setup
gridLength = 100
dt = 1
n1InitAbund = 2000
n2InitAbund = 2000
r1 = 0.7
r2 = 0.7
d1 = 0.2
d2 = 0.2
disp1 <- 10
disp2 <- 1
list <- .jnew("java.util.ArrayList")
list$add(
  .jnew(
    "Species",
    r1, 
    d1,
    as.integer(disp1)
  )
)
list$add(
  .jnew(
    "Species",
    r2,
    d2,
    as.integer(disp2)
  )
)
com <-
  .jnew(
    "Community",
    as.integer(gridLength),
    dt,
    list
  )

# now inoculate species 2, the resident...
ac <- .jnew("AddCritters", com, as.integer(2), FALSE)
ac$addCrittersRandomly(as.integer(5000))

# let it step to equilibrium. You have to make a judgment call here, though there are heuristic approaches
com$step(as.integer(3000))

# save this a reference point
gridState <- com$getEnvironment()$getGrid()

lambdaVec = c()
lambdaBarVec = c()
lambdaDensityCovVec = c()

for(i in 1:10)
{
  # revert back to the old grid state. This prevents us from having to let the residents equilibrate every invasion rep. 
  # I assume that this won't lead to any appreciable psuedosampling effects, since the invader's low-density inoculation should be sufficiently random
  com$getEnvironment()$setGrid(gridState)  
  
  # now inoculate the invader - 10 individuals
  ac <- .jnew("AddCritters", com, as.integer(1), FALSE)
  ac$addCrittersRandomly(as.integer(10))
  
  # measure invasion fitness for 1000 steps, max
  ts <- .jnew("TimeSeries", com, as.integer(1000), as.integer(1))
  tsList <- .jnew("java.util.ArrayList")
  
  lambdaTilda <- .jnew("LambdaTilda", com, as.integer(1))
  lambdaBar <- .jnew("LambdaBar", com, as.integer(1))
  lambdaDensityCovariance <- .jnew("LambdaDensityCovariance", com, as.integer(1))
  
  tsList$add(lambdaTilda)
  tsList$add(lambdaBar)
  tsList$add(lambdaDensityCovariance)
  
  # terminate in the case of extinction or a population size over 50
  tc <-  .jnew("TerminateIfSpeciesDiesOrGrowsTooLarge", com, as.integer(1), as.integer(50));
  
  tsDat <- .jevalArray(ts$timeSeries(tsList, tc), simplify = TRUE)
  
  # average over all measurements
  lambdaVec <- append(lambdaVec, mean(tsDat[,1]))
  lambdaBarVec <- append( lambdaBarVec, mean(tsDat[,2]))
  lambdaDensityCovVec <- append( lambdaDensityCovVec, mean(tsDat[,3]))
}

mean(lambdaVec)
mean(lambdaBarVec)
mean(lambdaDensityCovVec)


################ 7) ################
# 7 Visualizing the simulations
## I totally don't recommend frequently doing this. You can use Mathematica or my OpenGL visualization in Java.
## If you go to mathematica, use a Do loop inside a Monitor functions. You can (and should) plot time series as the simulations unfold

## 7.1 R Plots and Sys.sleep
# Sys.Sleep pauses the loop for a second. This is really slow, but any faster and you won't see the plots (i.e. the R window won't be able to keep up)
com <- create2SpeciesCommunityAndInoculate()
library(Matrix)
for(i in 1:100)
{
  com$step()
  grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
  myColors <- c("Black", "Red", "Green" )
  image(grid, xaxt = "n", yaxt = "n", col = myColors)
  Sys.sleep(0.1)
}

# 7.2 animation 
## use the animation package to make gifs. Don't make them too long, or they'll take forever to compile, or they won't compile at all
library(animation)
ani.options(interval=0.0001)
saveGIF({
  for(j in seq(1,length(liDist),1))
  {
    com$step()
    grid <- .jevalArray(com$getEnvironment()$getGrid(), simplify = TRUE)[1,,]
    myColors <- c("Black", "Red", "Green" )
    image(grid, xaxt = "n", yaxt = "n", col = myColors)
  }})



################ 8) ################
# 8 Individual based evolution
# Here, each individual has some chance of mutating (more on that in 8.2) when it is born. 
## In this sense the birth process and the mutation process are coupled.

####################################
# 8.1 the setup, from a software engineering y

# First, some notes on how things are normally done.

# Normally, these individual based simulation models have IndividualCritter objects populating some lattice. 
## The boon of this approach is that those individual objects all have their own traits, and in turn, 
## intraspecific variation is easily accomodated. However, this approach has a few downsides. For starters, 
## it doesn't scale well in terms of CPU or memory (but mostly memory); we could be disposing of and creating 
## thousands of objects in each time step. Integers are more managable. The individual object approach also beckons us to use asynchronus updating -
## this is where each individual tries to give birth or death in sequence (and the sequence can be mixed up between step calls
## to obviate artifacts). As dt goes to zero, asynchronous updating is all fair and well. On a well populated lattice, however, 
## there are bound to be some individuals who would disperse to the same empty site if only it had not been filled during 
## the same step call. To circumvent this, I constrain birth and death rates such that the sum of the probabilities of all
## neighboring individuals dispersing into an empty site is less than one, so anything can happen. More importantly, the 
## empty-site-view of births allows you to use fitness generating functions to measure fitness and it's derivatives 
## (lambdabarm, fitness density covariance), which (in my experience) is the least noisy way to measure fitness. 
## We're still left with the challenge of incorporating intraspecific variation. How can we do this if the lattice is full of integers.

## the species god object approach

# each species object tells the integers on the lattice how to behave. It's directions are encoded in some necessary objects, such as
## BirthProcessStandard, DeathProcessStandard, and DispersalStrategy. These objects typically implement interfaces. For the previous examples, these
## are IBirthProcess, IDeathProcess, and IDispersalStrategy, respectively. Each species also has a Traitlist Object, which holds traits that implement
## an Evolvable interface. These traits are either subclasses of ContinuousTrait or DiscreteTrait(which itself is a subclass of ContinuousTrait).
## Most importantly, these traits implement an interface that the Species requires. For example, every species requires an IDispersalStrategy object, 
## and DispersalIB (IB for individual-based) implements IDispersalStrategy. The DispersalIB object (in the traitlist in the species, remember) has 
## access to the species as a member variable. So, when the time for evolution comes, the DispersalIB object can overwrite the IDispersalStrategy object
## that the species was previously using. Each trait has it's own lattice for tracking the spatial distribution of it's values. Deletions of trait values
## (with death) or additions (from births) are all managed by the TraitList object. 

# as you can imagine, simulations can slow down significantly when individual based evolution is occuring. The program has some crazy logic
## that keeps the simulation running as fast as possible. If you're interested, the logic is in the propagule rain grid class. 
## Here is high level view of what to expect. These special algorithms only really matter when dispersal is evolving to high values.
## Individual based dispersal is the speed killeer. When dispersal is the only individual based trait, a distinct algorithm is used. It's pretty fast,
## and shouldn't cause major slow downs even at high dispersal abilities. When dispersal is high (> 20) and there are other individual based traits, 
## but all of those traits are constrained to discrete values, a separate algorithm is used. This is going to be moderately slow, no matter what. 
## But it will be better than the alternative. If dispersal has evolved to high values, and there are other individual based traits, and one or some
## are continuous in nature, then the simulation is really going to drag. There's nothing you can do.  


##############################################
## 8.2 the key ingredient: a mutation function
# each and every Evovable Trait sontructor takes a mutation function as it's first argument. Each mutation function is forced to implement the IMutationFunction interface
# each mutation function takes two doubles as arguments. The first is the probability of mutation, the second is the standard deviation of the guassian from which new trait values are pulled.

# Hereare some mutation  functions...
# but first, some setup
mutationMagnitude <- 1
evolveProb <- 0.05

# this rounds mutations to non-negative integers. Good for evolving the diffusiveness of interactions. Not great for dispersal, since dispersal can't be zero  
mfDifn1 <- .jnew("MutationFunctionDiscreteBoundedByZero",evolveProb, mutationMagnitude)

# this is for continuous traits that can assume any real number. Good for evolving effect values of "excreted" resources
mfEffn1 <- .jnew("MutationFunctionContinuous",evolveProb, mutationMagnitude)

# this round mutations to non-negative, non-zero integers. Good for evolving dispersal
mfDispn1 <- .jnew("MutationFunctionDiscreteBoundedByOne",evolveProb, mutationMagnitude)

# this is for continuous traits that cannot be less than zero. Good for evolving birth rate, evolution probabilities, ect.
mfDispn1 <- .jnew("MutationFunctionDiscreteBoundedByOne",evolveProb, mutationMagnitude)

## TAKE NOTE: when mutation functions are given to evolvable traits, they must be cast as IMutationFunction

## all mutation functions operate by the mutate method
# to see the mutation function in action, we must first give a random number generator to the mutation function...
# normally, this is done behind the scenes when a evolvable trait is added to a species (more on that later)
mfEffn1$setGenerator(.jnew("java.util.Random"))
imax <- 1000
mutVec <- rep(NA, imax)
for(i in 1:imax)
{
 mutVec[i] <- mfEffn1$mutate(5)
}
# this the empirical probability of mutation
length(which(mutVec!=5))/length(mutVec)
##############################################
## 8.3 how to start evolution: an example with dispersal distance

# setup
com <- create2SpeciesCommunityAndInoculate()
com$step()
# first make the mutation function
mutationMagnitude <- 1
evolveProb <- 0.01
mfDispn1 <- .jcast(.jnew("MutationFunctionDiscreteBoundedByOne",evolveProb, mutationMagnitude), "IMutationFunction")

# then use it to construct a dispersalIB object (IB for individual based)
dispIB <- .jcast(.jnew("DispersalIB", mfDispn1), "Evolvable")


# give the trait to species 1
com$getSpeciesList()$get(as.integer(0))$addTrait(dispIB)


# let the system run for a bit...
ts <- .jnew("TimeSeries", com, as.integer(1000), as.integer(5))
abundsOverTime <- .jevalArray(ts$timeSeries(TRUE), simplify = TRUE)

# then start evolution 
dispIB$startEvolution()

com$step()
#run for longer and collect data
# let the system run for a bit...
ts <- .jnew("TimeSeries", com, as.integer(1000), as.integer(5))
abundsOverTime <- rbind(abundsOverTime, .jevalArray(ts$timeSeries(TRUE), simplify = TRUE))

maxim <- max(abundsOverTime)
plot(abundsOverTime[,1], col = "Green", ylim = c(0, maxim))
points(abundsOverTime[,2], col = "Red")

## 8.4 getting individual-based trait data
## 8.5 evolving dispersal
## 8.6 evolving "excreted" effect magnitude: an example with a predator-prey system
## 8.7 evolving "excreted" effect diffusiveness: an example with a predator-prey system
## 8.8 evolving mutation proabilities
## 8.9 evolving mutation magnitudes
## 8.10 evolving q: an example with quantitative cheaters
