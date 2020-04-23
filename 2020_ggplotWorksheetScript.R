# Follow along with this script or save a copy and annotate as we go.
# Modified version: Kate Dodds April 2020
# Original script by - Kyle Zawada 2018

#Intro ====

#This script will guide you through the basics of ggplot covering
#a variety of topics. Throughout will be a number of test exercises
#to help drive home some important ideas, as well as giving you an
#opportunity to play around a bit.


#1. Set up - loading data and packages ====

#The first thing to do is to set up a clean environment.
#that is, run the rm() function to delete any objects.

rm(list=ls())

#This is good practice to make sure that all the code works
#sequentially with all the necessary info contained in the script itself.

#Next, load the ggplot package

library(tidyverse)  #or
library(ggplot2)

#Next up is the data. We will be using a built in dataset called
#mtcars to show most of the functions of ggplot. It's already loaded
#as you can see here:

str(mtcars)

#But we'll copy it to our own object anyway

PlotDat <- mtcars

#If you have your own data you would like to play around with feel
#free to load it in now and call it MyDat

#MyDat <-

#Here's the first important bit about ggplot;
#Your life will go much easier if you make sure your data
#i) is a data frame, and
#ii) has the correct variable types

#If we run...

str(PlotDat)

#We can see at the top that PlotDat is a data.frame (that's good!)

#Just as a reminder, a data.frame is an R object that allows each column
#of data to have it's own type (numeric, factor, etc)

#All of the variables are numeric at the moment, but some of them might
#be more useful as factors.

#For example, we might want to treat cars with different numbers of
#gears (gear) or carburettors (carb) as separate groups. And the "am"
#variable is actually a binary variable telling us whether the car
#is an automatic (0) or manual (1) transmission.

#Lets add these variables as factors to our data.frame using the
#as.factor function - note that the original data in "non-factor" form is still 
# there, we have created NEW columns. 

PlotDat$amFac  <- as.factor(PlotDat$am)
PlotDat$gearFac  <- as.factor(PlotDat$gear)
PlotDat$carbFac  <- as.factor(PlotDat$carb)

#Lets check the data.frame sturcture again

str(PlotDat)

#Cool. So we now have the factor variables added to the dataframe.

#Although the levels for the "am" variable are "0" or "1", it might be
#more useful to have them as "automatic" and "manual" to make it easier to
#understand whats going on when plotting.

#we can change the level names using the levels function

levels(PlotDat$amFac) <- c("Automatic", "Manual")

#And check the structure one more time...

str(PlotDat)

#Ok! So we're all set up to go.

#It's a bit of boring housekeeping to start with but I've had issues in the past
#by not doing these checks, so it's probably a good idea to drill it home now!

#Right, so we have our data, it's a data.frame and the variables are the correct
#type. Now we can start to use ggplot!


#2. ggplot syntax - understanding aesthetics ====

#ggplot has a way of doing things that is a bit tricky to understand to begin
#with, but is pretty intuitive once you understand the thinking behind
#it

#ggplot builds plots sequentially in layers, with each line of code building on what
#was ran previously.

#The first step is to specify what data will be referred to when building plots,

#this is done using the "data" argument within the "ggplot()" function.

#The second step is to link variables to particular aspects of the plot.

#For example setting a variable to "x" at the start will map that variable to the
#x-axis, so  that any time ggplot needs to find something that needs an x axis variable
#(e.g a scatterplot) it will already know what to use.

#this is known as setting an aesthetic, and you can set a huge variety of these
#such as x and y axes, colour outline, colour fill, size, shape, etc.

#These variable aesthetics are set with the "aes()" call within "ggplot()"

#A quick example:

ggplot(data = PlotDat, aes(x = mpg, y = hp))

#so our data is the data.frame "PlotDat", and we set our aesthetics with the
#"aes" function. Our x variable is "mpg", and our y is "hp".

#You can see in the plot window that ggplot has generated a base layer with
#our x and y axes, tick marks and values, and axis labels, and is ready to start
#adding layers of data on top.

#ggplot detects the type of variable as well, so if we change our x aesthetic to
#the amFac factor variable, the base layer automatically changes.

ggplot(data = PlotDat, aes(x = amFac, y = hp))

#Also notice that we don't use quotation marks around the variable names.
#ggplot does this to cut down on the chances of being tripped up by missing
#punctuation. If you do use quotation marks ggplot takes them as a character value.

#for example;

ggplot(data = PlotDat, aes(x = "amFac", y = "hp"))

#notice that the axes are no longer correct!

#So we have our base layer, but how do we add data to the plot?

#3. geoms - ways of specifying what to plot ====

#Adding data layers to the base layer requires the use of "geoms", short for geometries.
#These geometries are standalone functions that take various aesthetics as inputs, and
#converts them to a specific output.

# for example, "geom_point" plots points, "geom_histogram" plots a histogram, etc

#thankfully the naming of most geoms is pretty intuitive.

#geoms are called after the intial ggplot() call, included via a "+" after the previous
#line.

#for example;

#our base layer with x and y aesthetics, folllowed by a "+" to tell ggplot to add another layer
# THis is similar to the pipe (%>%) symbol when using dplyr/tidyverse.

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +

  #geom (geometry), _ (underscore), point (specifying that we're adding points to our
  #base layer)

  geom_point()

#Bingo!

#if we try the same thing with the + sign on the new line, it doesn't work

ggplot(data = PlotDat, aes(x = mpg, y = hp))
+ geom_point()

#If there's no "+" after the call on the same line, R will run it without expecting any
#further input. The "+" is almost like telling R to think about doing something, but
#to hold off running it as there's something else coming that's important.


#To reiterate on basic plotting;

#we specify a base layer using a data frame for the data argument,
#we also specify aesthetics such as x, y, colour, etc in the intial call.
#we then add a data layer via a "geom_" function.

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point()

#notice that we didn't need to add anything to the geom_point function, as it
#automatically detected our x and y values from the aesthetics.

#This is really useful as it allows us to add multiple layers without needing to specify
#the data source and variables every time, and to change our geom with less typing.

#Lets add a factor as a colour!

#All we have to do is add a colour aesthetic to the ggplot() call.

ggplot(data = PlotDat, aes(x = mpg, y = hp, colour = amFac)) +
  geom_point()

#Note that ggplot has added a legend automatically to help interpret the plot.

#But what if our colour variable was continuous?

ggplot(data = PlotDat, aes(x = mpg, y = hp, colour = wt)) +
  geom_point()

#Again, the geom_point function has automatically detected that the colour
#aesthetic is continuous, set a colour gradient, and added the correct
#legend.

#We can add even more aesthetics if we wanted, and ggplot will oblige!

ggplot(data = PlotDat, aes(x = mpg,  #note that we can add a newline to break the
                           y = hp,   #into easier to see chunks.
                           colour = drat,
                           shape = amFac,
                           size = wt,
                           alpha = qsec)) +
  geom_point()

#note: alpha = transparency! You can set this to different levels. 

#Ok ok, the interpretability of what's going on is a bit confusing now. But, this
#just highlights how easy it is to modify and add to your plots by just changing
#very little of your code.

#This lets you concentrate on exploring the data rather than fiddling with code.

#Plus the plots look pretty decent right out of the box!

#However, you can customise your plots by setting aesthetics like colour,
#shape, size, etc without mapping them to a variable.

#For example. If you want to set the colour for the all points to red,
#instead of mapping them to a variable, you can set "colour" in the
#"geom_" as an argument.
# THat way all points associated with that geometry will be red, if you have another
# geometry added as another layer, this could be a different colour. 

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = red)

#hmm, that didn't work... and that's because red needs to be in quotation marks
#as it's not an object or variable.

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "red")

#Also note that if we put "colour = wt" (one of our variables) in the geom_
#call, it throws an error

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = wt)

#this is because all aesthetics that you want to map to a variable have to be set
#within the aes() argument, which is possible within a geom.

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(aes(colour = wt))

#specifying variable aesthetics within a "geom_" can be useful when you get to
#more advanced plotting or in particular circumstances, however to begin with
#it's probably best to set any variable aesthetics within the "ggplot()" call.


#you can specify colours in many different ways which I won't go into detail
#about here. But most english colour names work quite well as a rule.
# GGPLOT has a VERY large user base and apparently a very creative one too - 
# as a result there are loads of different colours themes and options that you 
# can use in your plots - look for these online and experiment with different 
# options. See: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/ 


ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "lightblue")

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "darkorange")

#you can also change the shape and size of the points as well.

#size

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(size = 5)

#notice that size takes a numeric argument

#shape

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(shape = 4)

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(shape = "4")

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(shape = "W")

#note: there are many ways to set the shape parameter!

#And you can mix and match to your hearts content.

ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "blue", shape = "W", size = 7)

#And remember that you can still choose to set a mix of variable
#aesthetics and set values.

ggplot(data = PlotDat, aes(x = mpg, y = hp, size = wt)) +
  geom_point(colour = "blue", shape = "W")

#But, if you set an aesthetic in the base layer and the geom at the same time,
#the value in the geom will cancel out the variable aesthetic

ggplot(data = PlotDat, aes(x = mpg, y = hp, size = wt)) +
  geom_point(colour = "blue", shape = "W", size = 3)

#Question break ... 

#Next we'll take a quick look at some of the other geometries available in ggplot and
#some of their nuances.

#There are a number of geometries available for use.

#A good way to see what???s around is to type "geom_"
#and look through the autocomplete options.

#press tab after the underscore (_) to have a look.

geom_

#There are lots of different geometries, enough in fact to
#offer all of the flexibility of base R plotting if needed!

#Lets look at some other common geometries and start building on our knowledge

#boxplot - good for categorical by continuous data.

#x = categorical variable, y = continuous

ggplot(data = PlotDat, aes(x = amFac, y = hp)) +
  geom_boxplot()

# If you'd like to still see the raw data points, try adding geom_jitter()
ggplot(data = PlotDat, aes(x = amFac, y = hp)) +
  geom_boxplot() +
  geom_jitter()

# you can adjust the jitter: 
ggplot(data = PlotDat, aes(x = amFac, y = hp)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2, color = "tomato")

#again we can map other variables to aesthetics such a the gearFac factor to
#the fill colour of the boxplot.
# And can add the jitter points as a geom_point - and have them associated with 
# the groupings from the FILL group. 

ggplot(data = PlotDat, aes(x = amFac, y = hp, fill = gearFac)) +
  geom_boxplot() +
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1,
                                                        jitter.height = 0.1))



#bar

#can be used for counts of data when
#x is a factor without a y aesthetic variable

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar()

#Here is a good time to show off the difference between "fill" and "colour"

#fill= fill everything with a colour

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar(fill = "lightblue")

#colour = colour of the outline

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar(colour = "red")

#both together = better looking plots!

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar(colour = "black", fill = "lightblue")

#geom_bar can also have a continuous variable mapped to y
#however, we need to add a few arguments to the "geom_bar()" call.

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean")

#we've added a "stat" and a "fun.y" argument, which has given us a plot of the mean
#values of wt for each level of gearFac.

#So what does this do?

#ggplot is able to apply statistical transformations (stat) to the data provided to it.

#This includes things like counts, means, log10 transforms, etc.

#This is useful as you don't have to calculate and store different variables or
#dataframes for different purposes.

#For example, you might want to plot the mean value of wt for each
#level of gearFac, but maybe also the mean for each level of amFac.

#Rather than have two dataframes with this information, we can specify these transforms
#in our ggplot call on the fly.

#Every geom has a default stat.

#In many cases no transformation is applied.

#geom_point for example plots the raw data as is. and therefore uses the "identity" stat.

#"identity" almost means identical, and is the term for when no transform is applied.

#some geoms apply some transformation or generate a summary statistic by default.

#The default "stat" for "geom_bar" is "count". i.e. by default, geom_bar will count
#the number of instances for each x value, whether x is a factor or numeric.

#If we specify a y variable aesthetic with the default stat for geom_bar,
#we get a telling error message

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(fill = "lightblue",colour = "black")

#Error: stat_count() must not be used with a y aesthetic.

#In this case, the default stat "count", creates a new y variable by summarising our
#x variable "gearFac", which can be seen on the y axis of this plot.

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar(colour = "black", fill = "lightblue")

#notice the y-axis is labelled "count".

#Setting a y variable aesthetic fills that spot, and so 'stat = "count"' can't be applied.

#if we set our stat to "identity", ggplot will no longer try to create a new count
#variable.

ggplot(data = PlotDat, aes(x = gearFac)) +
  geom_bar(colour = "black",
           fill = "lightblue",
           stat = "identity")

#Error in pmin(y, 0) : object 'y' not found

#freeing us up to add our own y variable, in this case wt

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(colour = "black",
           fill = "lightblue",
           stat = "identity")

#hmm... what's happened here then?

#Well, the geom_bar function has plotted the wt variable with no changes,
#and so by default stacks each value of wt on top of each other for each level
#of gearFac.

#the total height of the bar is equal to the sum of wt for that level of gearFac,
#which we can check

sum(PlotDat$wt[which(PlotDat$gearFac == "3")])
sum(PlotDat$wt[which(PlotDat$gearFac == "4")])
sum(PlotDat$wt[which(PlotDat$gearFac == "5")])

#Which is neat, I guess... but maybe we're more interested in the average value of
#wt for each level of gearFac. and that's where we get to 'stat = "summary"',
#and 'fun.y = "mean"'

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun.y = "mean")

#In this case we're saying that for geom_bar, we want to run a statistical summary
#on the data before plotting (hence stat = "summary").

#We then specify which data to transform with "fun.y" (apply a FUNCTION to variable Y)

#And what sort of transform ("mean")

#We can check our bar heights vs manually calculated means
#in a similar way that we did for counts.

mean(PlotDat$wt[which(PlotDat$gearFac == "3")])
mean(PlotDat$wt[which(PlotDat$gearFac == "4")])
mean(PlotDat$wt[which(PlotDat$gearFac == "5")])

#Which checks out.

#You can change fun.y to lots of different functions such as
#sd, mean, median, etc.

#even custom functions!

mean2 <- function(x) { sum(x)/length(x) } #a simple function that calculates the mean.

mean2(1:10)

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean2") #set our function to "mean2" outlined above.


#We can also add another geom on top of the bars that is typical of boxplots;
#error bars.

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  
  geom_errorbar(stat = "summary",
                fun.data = "mean_se")

#In this case we've added another geom layer on top using the "+" sign, so that we have

#our base layer: ggplot() +
#our bar geom: geom_bar() +
#our error bar geom: geom_errorbar()

#Again we specify that stat = "summary", but this time we use "fun.data" instead of fun.y
#as this gives us three values; y, ymax and ymin, calculated from the function "mean_se".

#"mean_se" calculates the mean and positive and negative standard errors used for our
#error
#bars.

#Also note that the axes ranges are automatically updated to allow the error bars to fit.

#However, we can specify these functions separately if we want other types of error bar

#mean of x plus the 2/standard deviation of x
positiveStandardDev <- function(x) {mean(x) + sd(x)/2}

#mean of x minus the 2/standard deviation of x
negativeStandardDev <- function(x) {mean(x) - sd(x)/2}


ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean2") + #our own mean function
  
  geom_errorbar(stat = "summary",
                fun = "mean2", #our own mean function
                fun.max = "positiveStandardDev", #our positive standard deviation value
                fun.min = "negativeStandardDev") #our negative standard deviation value


#We can tweak the width of the bars with the "width" aesthetic

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6)                               #change the width of the bars

#and now of course we can change our variable aesthetics to get plots for different data
#simply by changing the variable names

#change wt to mpg

ggplot(data = PlotDat, aes(x = gearFac, y = mpg)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun.y = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6)

#change gearFac to amFac

ggplot(data = PlotDat, aes(x = amFac, y = mpg)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6)


#we can also combine geom_bar, geom_errorbar and geom_point for even more information

ggplot(data = PlotDat, aes(x = amFac, y = mpg)) +
  geom_point(size = 3) +                               #don't forget the "+" sign!
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6)

#hmm, maybe having the points in from of the bar would be better. To do that, just put
#geom_point() at the end!

ggplot(data = PlotDat, aes(x = amFac, y = mpg)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6) +                           #don't forget the "+" sign!
  geom_point(size = 3)

#a few more tweaks...

ggplot(data = PlotDat, aes(x = amFac, y = mpg)) +
  geom_bar(fill = "lightblue",
           colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6) +                           #don't forget the "+" sign!
  geom_point(size = 3,
             shape = 21,
             fill = "white",
             alpha = 0.8)

#And cool, now we can see the summary of the data along with the actual raw data!



#4. Plot customisation - an outline of how to customise plots in ggplot. ====

#So we're able to create our plots, but now it's time to customise them a bit
#to make them our own.

#One of the downsides of base R plotting is the somewhat confusing and
#unintuitive "par()" function.

#ggplot however, makes customising plots a little more easy to remember.

#Here is a plot from earlier, tweaked to have a colour palette for the x axis
#by mapping gearFac to fill as well as the x aesthetic.

ggplot(data = PlotDat, aes(x = gearFac, y = mpg, fill = gearFac)) + #gearFac is mapped to the x and fill aesthetics.
  geom_bar(colour = "black",
           stat = "summary",
           fun = "mean2") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6) +
  geom_point(size = 3,
             shape = 21,
             alpha = 0.8)

#Now, before we get into customising plots here's a quick trick: you can save plots to
#R objects and copy/modify them as you go.

#We haven't done this until now because it's important to see and understand the syntax
#as much as possible. But to save space when doing the customisations, we'll save our
#plot as an object called "plot1"

plot1 <- ggplot(data = PlotDat, aes(x = gearFac, y = mpg, fill = gearFac)) +
  geom_bar(colour = "black",
           stat = "summary",
           fun = "mean") +
  geom_errorbar(stat = "summary",
                fun = "mean2",
                fun.max = "positiveStandardDev",
                fun.min = "negativeStandardDev",
                width = 0.6) +
  geom_point(size = 3,
             shape = 21,
             alpha = 0.8)

#Notice it hasn't actually plotted yet. to do that we need to call it.

plot1

#to make any adjustments we can treat the "plot1" object as if it was the full
#block of code

#for example;

plot1 + #use the plus sign to tell R to wait for another instruction before plotting
  labs(title = "Check out this cool graph")

#here we've added a title to the plot using the "labs()" function, short for labels.
#with the argument "title = " to tell ggplot that we're adding a title, followed by
#the title content in quotation marks.

#The names of the axes are easily changed by including "x = " and "y = " arguments

plot1 +
  labs(title = "Check out this cool graph",
       x = "Number of guinea pigs",
       y = "Miles per guinea pig")

#In fact, you can change the name for each aesthetic by writing the name of that
#aesthetic,
#followed by and =

plot1 +
  labs(title = "Check out this cool graph",
       x = "Number of guinea pigs",
       y = "Miles per guinea pig",
       fill = "Number of guinea pigs")

#Which is nice and intuitive!

#You can also add subtitles and captions

plot1 +
  labs(title = "Check out this cool graph",
       x = "Number of guinea pigs",
       y = "Miles per guinea pig",
       fill = "Number of guinea pigs",
       subtitle = "four guinea pigs is the optimal number for per guinea pig efficiency",
       caption = "in: Animal Engine Technology Review, Etal et al, 2018")

#Lets say we're settled on these changes for now. We can update our plot object
#Like we would any other R object

plot1 <- plot1 +
  labs(title = "Check out this cool graph",
       x = "Number of guinea pigs",
       y = "Miles per guinea pig",
       fill = "Number of guinea pigs",
       subtitle = "four guinea pigs is the optimal number for per guinea pig efficiency",
       caption = "in: Animal Engine Technology Review, Etal et al, 2018")

plot1

#What about the colours?

#Well we can change them using the "scale_fill_" function.

#This will change the colours for our fill aesthetic,
#but be aware that you can use "scale_colour_" to change the
#colour aesthetic

#Press tab next to the "_" to see the autocomplete options now

scale_fill_

#There are lots of options that cover slightly different things.
#Some such as "scale_fill_gradient" are useful for continuous
#variables mapped to the fill aesthetic.

#Others are better for discrete data like we have in this plot.

#We can use the "scale_fill_brewer" to use some ready made gradients that look
#pretty good

plot1 +
  scale_fill_brewer()

#If you run ?scale_fill_brewer you'll see some of the other options for palettes

?scale_fill_brewer

#Let's try "palette = "Set1"

plot1 +
  scale_fill_brewer(palette = "Set1")

#What about "RdBu"? (Red to Blue)

plot1 +
  scale_fill_brewer(palette = "RdBu")

#Now "Purples".

plot1 +
  scale_fill_brewer(palette = "Purples")

#There are a number of options available, but if you want to have more control
#overyour colours, look into "scale_fill_manual" and "scale_colour_manual"

#For continuous data, you can use "scale_fill_distiller" and "scale_colour_distiller"
#functions

#In this case we have a scatterplot with a colour aesthetic, which we will save as
#an object called plot2, in order to save space.

plot2 <- ggplot(data = PlotDat, aes(x = mpg, y = hp, colour = wt)) +
  geom_point(size = 3)

plot2

#So we will use "scale_colour_distiller" which defaults to a blue gradient.

plot2 +
  scale_color_distiller()

#One issue here however is that the colours white out as they approach 5.

#You can adjust the range of colours in the palette using the "values = "
#argument.

plot2 +
  scale_colour_distiller(values = c(0,1.5))

#The default range is from 0 to 1, by ranging from 0 to 1.5 we extend the
#gradient, pushing the lighter colours outside of the range of observed values.

#"scale_colour_distiller" uses the same discrete palettes as "scale_colour_brewer"
#and will interpolate the discrete colour palette into a continuous one.

#Typically you'll want to use either the sequential or diverging colour palettes
#for a smooth transition

#See "?scale_colour_distiller" again for the list of palettes.

plot2 +
  scale_colour_distiller(palette = "OrRd", values = c(0,1.5))


#The "scale_" functions also extend to the other aesthetics, with different options
#depending on the aesthetic.

#Have a quick browse by pressing tab after the "_" below

scale_

#The handy thing is that its easier to remember how to change the things you want to.

#If you're trying to change the SCALE of the SIZE aesthetic which is a CONTINUOUS
#variable, you add

scale_size_continuous()

#To your plot, and set the arguments.

#One typically used function for continuous data is "scale_x_log10" and "scale_y_log10"
#which change our axes to a log10 scale.

plot2 +
  scale_colour_distiller(palette = "OrRd", values = c(0,1.5)) +
  scale_x_log10() +
  scale_y_log10()

#Lets save these changes to plot2 and move onto themes

plot2 <- plot2 +
  scale_colour_distiller(palette = "OrRd", values = c(0,1.5)) +
  scale_x_log10() +
  scale_y_log10()


#5. Themes - quickly make your plots look presentation ready ====

#Another useful component of ggplot is the use of themes that apply a range of visual
#tweaks to your plots very quickly.

#You can define your own personal or project/paper orientated theme as well,
#although that is outside of the scope of this workshop.

#However! ggplot has a number of built in themes ready to use, and you can install
#the package ggthemes for a few extras.

#Now, to add a theme we need to just use a "+ theme_" call, in much the same way
#we add "geom_" calls. Here we'll use our plot1 bar chart.

plot1

#Now to add a theme

#theme_classic does away with the gridlines and grey background, giving a clean
#look.

plot1 +
  theme_classic()

#theme_minimal removes the axes lines and grey background but keeps the gridlines

plot1 +
  theme_minimal()

#theme_solid from the ggthemes package removes everything except the geoms and legend
#boxes

#note, you may need to install the ggthemes package!

plot1 +
  ggthemes::theme_solid()

#theme_few from the ggthemes package is another clean theme.

plot1 +
  ggthemes::theme_few()

#This highlights how themes can quickly alter the look of a plot
#without needing to do too much work.

#You can also change the font and base font size within "theme_"
#for even more control

#use base_size = for font size

plot1 +
  theme_minimal(base_size = 9)

#and use base_font = for font type.

#The default font is "sans" and will be specific to your OS (windows, iOS, linux)
#There other options available are "serif" and "mono"

plot1 +
  theme_minimal(base_family = "serif")

plot1 +
  theme_minimal(base_family = "mono")

#Although for much more control over fonts I suggest you look into the extrafont package
#which will let you load and use many more fonts in ggplot and R in general.

#Here's a quick example if you have extrafont installed

library(extrafont)

#run font_import() to import the fonts from your computer. It takes a few minutes but
#only has to be done once!

#font_import()

loadfonts(device="win") #load the fonts into R

plot1 +
  theme_minimal(base_family = "Comic Sans MS")

#Perfection.

#Lets save a theme to our plot1 object

plot1 <- plot1 +
  theme_classic(base_family = "sans", base_size = 14)

plot1

# What if you spend a lot of time developing a theme that you want to use multiple
# times? You can then save this theme as an object, and add it to your plot:

# saing a specifc theme
# THis one we'll call "grey_theme": 
grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, 
                                               angle = 45, hjust = 0.5, 
                                               vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16),
                    plot.title = element_text(hjust = 0.5))

# And we can now add it to any of our plots: 
plot1 + 
  theme_bw() +
  grey_theme



#Now lets save our graph!


#6. ggsave - an easy way to save your plots ====

#There are lots of ways to save plots in R, but for ggplot,
#one of the easiest and most consistent ways is to use "ggsave()"

#This function has a few arguments but the two that are nessecary are

#the filename to save the plot
#and the plot object

#for example

ggsave(filename = "MyPlot.png", plot = plot1)

#this will save the plot to your working directory as "MyPlot.png"

#Notice that we specify the filetype with the ".png" extension added to
#the end of the filename.

#We can also save it as a pdf, by changing ".png" to ".pdf"

ggsave(filename = "MyPlot.pdf", plot = plot1)

#The size of the saved plot will depend on the size of the plotting window
#But we can change this using the width and height arguments

ggsave(filename = "MyPlot2.pdf", plot = plot1, width = 3, height = 2)

#If we open the "MyPlot2.pdf" we see that the objects are all over the place.

#This is because we've changed the size of the plot area, but haven't scaled
#the plot objects to fit.

#We can do this by tweaking the scale option

ggsave(filename = "MyPlot3.pdf", plot = plot1, width = 3, height = 2, scale = 3)

#In this case a scale of 3 seems to work well.

#Unfortunately there's no easy way to get this right straight out of the box,
#so play around with the options and check the outputs until it looks about right.


# More Advanced Customistaion:  ##### 

#7. Faceting - a cool way to show off categorical data ====

#Another cool part of ggplot is the ability to create multiple copies of a plot
#subset by a factor.

#This is called facetting and can be done for up to two factors together.

#For example, lets plot a boxplot of a factor and continuous variable

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_boxplot()

#What if we wanted to see the datasplit by another factor?

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_boxplot() +
  facet_grid(.~amFac)

#here we can see two copies of the boxplot, the left is for the automatic data
#and the right is for the manual data

#To do this we use the "facet_grid()" function followed by ".~amFac"

#This tells ggplot  to facet our plot on the x axis by the factor "amFac"

#if we put "amFac" before the "~" it would split along the y axis

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_boxplot() +
  facet_grid(amFac~.)

#the "." in both cases is a placeholder. but we can include variables on both
#sides of the "~" to facet by two factors at the same time.

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_boxplot() +
  facet_grid(amFac~carbFac)

#Awesome!

#Faceting is a great way to tell more of the story in the data
#without using a single, somewhat confusing plot.

#faceting will work for numeric and other variable types,
#but it will create a single plot for each unique value
#of that variable.

ggplot(data = PlotDat, aes(x = gearFac, y = wt)) +
  geom_boxplot() +
  facet_grid(.~disp)

#Which may or may not work depending on your data.

#8. geom_smooth - quick regression models in ggplot ====

#Anther cool built in feature of ggplot is the ability to plot
#regression models onto plots directly via "geom_smooth"

#Here???s an example scatterplot

ggplot(data = PlotDat, aes(x = mpg, y = wt)) +
  geom_point()

#now with a linear regression

ggplot(data = PlotDat, aes(x = mpg, y = wt)) +
  geom_point() +
  geom_smooth(method = "lm")

#Bingo!

#Notice that we specify that we want to fit a linear model via the "method ="
#argument.

#Also notice that it automatically adds 95% confidence intervals to the regression
#line.

#If you don't specify a method, ggplot defaults to "method = loess" or
#local polynomial regression which basically fits a curved regression line

ggplot(data = PlotDat, aes(x = mpg, y = wt)) +
  geom_point() +
  geom_smooth()

#ggplot can plot most model types including generalised linear models and
#generalised additive models, with a few extra arguments.

#It can also fit multiple regressions based on other aesthetics.

#For example

ggplot(data = PlotDat, aes(x = mpg, y = wt, colour = gearFac)) +
  geom_point() +
  geom_smooth(method = "lm")

#here we've added gearFac as a colour aesthetic, and ggplot has fitted
#three regression lines for each level.

#If we mix this with facets, we can fit more lines!

ggplot(data = PlotDat, aes(x = mpg, y = wt, colour = gearFac)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(.~amFac)

#Of course, this way of modelling isn't too useful for inferential
#statistics or "proper" modelling, as we can't interrogate the models
#that ggplot builds.

#But for quick trend analyses and data exploration, the "geom_smooth"
#function is very useful.

#9. GridArrange - put multiple plots together easily. =====

#Sometimes it may be useful to plot different plots together as a single figure

#To do this in ggplot, you can use the gridExtra package.

library(gridExtra)

#First, build your plots!

plotA <- ggplot(data = PlotDat, aes(x = wt, y = mpg, colour = gearFac)) +
  geom_point(size = 3)

plotA

plotB <- ggplot(data = PlotDat, aes(x = gearFac, y = mpg, fill = gearFac)) +
  geom_bar(colour = "black",
           stat = "summary",
           fun.y = "mean") +
  geom_errorbar(width = 0.7,
                stat = "summary",
                fun.data = "mean_se")

plotB

#now we use the grid.arrange function to build a plot containing both of our
#plots


grid.arrange(plotA, plotB)

#now, maybe we want to plot them side-by-side instead.

#to do this we use the "ncol = " argument

grid.arrange(plotA, plotB, ncol = 2)

#This tells ggplot to set up a plotting space with two columns and to fill them
#with plotA first, followed by plot B.

#If we have three plots we're trying to plot we can customise the space even more

plotC <- ggplot(data = PlotDat, aes(x = gearFac, y = hp, fill = gearFac)) +
  geom_bar(colour = "black",
           stat = "summary",
           fun.y = "mean") +
  geom_errorbar(width = 0.7,
                stat = "summary",
                fun.data = "mean_se")

plotC

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 2)

#Here we've set up four spaces to plot on, and filled it with three plots.

#it also fills the plots from top left to bottom right order.

#But what if we wanted the scatterplot to take  up the left side and have
#the bar plots smaller to the right?

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 2,
             layout_matrix = rbind(c(1,2),
                                   c(1,3))
)

#here we've added a "layout_matrix = " argument, with the code;

rbind(c(1,2),
      c(1,3))

#remember that out plot space has two rows and two columns, specified by
#the "nrow =" and "ncol = " arguments.

#so we can represent our plotting space like this

# plot, plot
# plot, plot

#the layout matrix copies this structure

rbind(c("plot","plot"),
      c("plot","plot"))

#So what we do is we fill in the matrix with the number of the plot we want
#in each space

#so;

rbind(c(1,2),
      c(1,3))

#means that plot 1 will be in the top and bottom left areas, plot 2 will be top right
#and plot 3 will be bottom right.

#We can use this to make our scatterplot square again by adjusting the number of columns,
#rows and the layout matrix

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,3))
)

#Which is a pretty handy and sorta simple method for arranging plots!

#We can also add titles and subtitles to the final arranged plot

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,3)),
             top = "My title",
             bottom = "My caption"
)

#And even text on the right or left

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,3)),
             top = "My title",
             bottom = "My caption",
             left = "My x-axis",
             right = "My x-axis, again"
)


#be aware though that you can't adjust themes or plots once you've ran
#"grid.arrange"

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,3)),
             top = "My title",
             bottom = "My caption",
             left = "My x-axis",
             right = "My x-axis, again"
) +
  theme_minimal()

#So make your changes to the base plots first!

#One change you may like to make to this plot is to remove the redundant
#legends.

#to do this add a "guides()" function to your plot

#for example

plotB +
  guides(fill = FALSE)

#this tells ggplot that the guide (legend) for the fill aesthetic shouldn't be plotted.

#Lets make the changes and run our grid.arrange again.

plotB <- plotB +
  guides(fill = FALSE)

plotC <- plotC +
  guides(fill = FALSE)

grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(1,1,2),
                                   c(1,1,3)),
             top = "My title"
) +
  theme_minimal()

#cool, but maybe lets move the scatterplot to the right so the legend is in a
#better position


grid.arrange(plotA, plotB, plotC, nrow = 2, ncol = 3,  #2 x 3 plot spaces
             layout_matrix = rbind(c(2,1,1),  #plot 1 is on the right hand side now
                                   c(3,1,1)),
             top = "My title"
) +
  theme_minimal()

#And there you have it!





#7. A simple workflow template: from data to plot in 8 or so steps ====

#1. load your data frame

#MyDataFrame <-

#2. check your data frame and make changes if necessary

#str(MyDataFrame)

#3.Specify your data frame and aesthetics in ggplot()

#MyPlot <- ggplot(data = MyDataFrame, #specify a data frame
#aes(x = VAR, y = VAR, colour = VAR, etc)) + #specify your aesthetics

#4. add a geometry to plot the data and set other options within the "geom_" function

#  geom_SOMETHING() +

#5. Add labels and titles

#  labs(title = "EXAMPLE", x = "EXAMPLE) +

#6. Chenge colours and scales if needed

#  scale_AESTHETIC_OPTION() +

#7. Set the theme, font size, and font type

#  theme_OPTION(base_size = NUMBER, base_family = OPTION) #change the theme

#8. Save the plot using ggsave

#ggsave("MyPlotName.png", plot = MyPlot)


#That should be enough to get most basic plots working pretty well in a way
#that is easily modified to generate a large number of plots with very
#little code!


#8. concluding remarks and extra resources ====

#Hopefully this crash course has helped you get to grips with ggplot
#and provided you with enough knowledge of how it works to be able to
#use ggplot.

#There's a huge amount of depth to ggplot as well which allows you to build
#some fantastic plots of all types.

#But they are all built on the basic principles outlined in this tutorial.

#given ggplots popularity there are a growing number of add on packages
#and resources that I suggest you take a look at once you're comfortable
#in the ggplot workflow.

#I've listed a few big ones below;

#Online resources

#ggplot cheatsheet: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#A more in depth tutorial on ggplot: http://r-statistics.co/ggplot2-Tutorial-With-R.html
# Another great tutorial on the MQ Software carpentry page:  https://mq-software-carpentry.github.io/r-ggplot-extension/ 
# And more avaialable on the Carpentries Data carpentry pages: https://datacarpentry.org/lessons/ 
# Great help for beginners, in the R u with me programme from R ladies: https://rladiessydney.org/ 

#R packages

#ggpubr: an extension to ggPlot for tweaking plots to be publication ready.
#GGally: a range of pre-made functions for more advanced plots (see: http://ggobi.github.io/ggally/#ggally, for a showcase)
#ggthemes: provides a number of extra themes, scales and geoms.
# COWPLOT: Check out Theo's previous COWPLOT workshop for plotting multiple plot types and 
# themes at the same time - great for publication plot editing: https://github.com/mqRusers/2019_02_Cowplot 


#9. EXERCISES 
#....... 1. Exercises ====

#Something is wrong in each code or plot below, see if you can fix it!

ggplot(aes(x = mpg, y = hp)) +
  geom_point()


ggplot(data = PlotDat, aes(x = "mpg", y = "hp")) +
  geom_point()


ggplot(data = PlotDat, aes(x = mpg, y = hp))
+ geom_point()


ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point


ggplot(data = PlotDat, x = mpg, y = hp) +
  geom_point()


ggplot(data = PlotDat) +
  geom_point(x = mpg, y = hp)


ggplot(data = PlotDat, aes(x = mpg, y = hp, colour = amFac)) +
  geom_point(colour = "blue")


ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "blue", size = "4")


ggplot(data = PlotDat, aes(x = mpg, y = hp)) +
  geom_point(colour = "blue", size = 400)


#....... 2. Exercises ====

#Something is wrong or could do with improving in each code or plot below, see if you can
#fix it!

ggplot(data = PlotDat, aes(x = amFac, y = hp)) +
  geom_boxplot


ggplot(data = PlotDat, aes(x = amFac, y = hp, fill = gear)) +
  geom_boxplot()


ggplot(data = "PlotDat", aes(x = amFac, y = hp)) +
  geom_boxplot() +
  geom_point()


ggplot(data = PlotDat, aes(x = gearFac, y = hp)) +
  geom_bar()


ggplot(data = PlotDat, aes(x = gearFac, y = hp), stat = "summary") +
  geom_bar()


ggplot(data = PlotDat, aes(x = gearFac, y = hp)) +
  geom_bar(stat = "summary",
           fun.x = "sd")


ggplot(data = PlotDat, aes(x = gearFac, y = hp)) +
  geom_bar(stat = "summary") +
  geom_errorbar(stat = "identity",
                fun.data = "mean_se")





#....... 3. Exercises ====

#Below is a plot object.

plotTest <- ggplot(data = PlotDat, aes(x = mpg, y = hp, colour = wt)) +
  geom_point(size = 3)

plotTest

#Below are attempts at editing the plot, try to fix the errors!

plotTest +
  theme_minimal


plotTest
+ theme_classic


plotTest +
  theme_minimal(base_size = "14")


plotTest +
  scale_colour_distiller(palette = Greens)


plotTest +
  scale_x_log10() +
  scale_y_log10


plotTest +
  scale_colour_distiller(palette = "GnBu",
                         values = 0,1.5)


plotTest +
  title = "MyTitle"


plotTest +
  labs(title = "MyTitle"
       subtitle = "Subtitle")


ggsave(filename = "MyFile.png", plot = "plotTest")


ggsave(filename = "MyFile", plot = plotTest)


ggsave(filename = MyFile.png, plot = plotTest)


#....... 4. Exercise ====

#Use what you've learned so far to make the following plots using the
#built in iris data set. For multiple part exercises, make the initial
#plot, then add the changes afterwards.

str(iris)

#1. A scatter plot of Sepal.Length by Sepal.Width

#1.a #Change the labels of the plot using the "labs()" function


#2. A boxplot of Species and Petal.Width

#2.a change the theme of the plot

#2.b change the font size of the plot


#3. A count bar chart of Species

#3.a Change the fill colour of each level of Species using one of the "scale_" functions


#4. A scatterplot of Sepal.Length by Sepal.Width, coloured by Petal.Width

#4.a Change the colour scale of plot 4. using one of the "scale_" functions

#4.b Change the y axis to be on a log10 scale using one of the "scale_" functions


#5. A bar plot of Species by mean Petal.Length

#5.a Add standard error bars to the plot

#5.a change the width of the error bars.


#And that pretty much covers most of the main points of ggplot!








