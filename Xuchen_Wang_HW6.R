library(ggplot2)
library(grid)

data("diamonds")

# 2  
ggplot(diamonds,aes(x=carat,y=price,color=factor(color)))+ # x axis is carat, y axis is price, use color to classify
  geom_point()+                                            # scatter plot
  ggtitle("Diamonds-Weigth to Price by Color")+            # add title name
  xlab('Weight')+ylab('Price')+                            # add xlab and ylab
  theme(plot.title=element_text(color="blue"))             # change the color of the title

# 3
new_price <- log(diamonds$price)                           # do log transform to price
new_weight <- log(diamonds$carat)                          # do log transform to carat
new_diamonds <- data.frame(new_price,new_weight,diamonds)  # add new transformed columns to dataframe
ggplot(new_diamonds,aes(x=new_weight,y=new_price,color=factor(color)))+
  geom_point()+                                            # replot the dataframe as problem 2
  ggtitle("Diamonds-Weigth to Price(Linear)")+
  xlab('Weight')+ylab('Price')+
  theme(plot.title=element_text(color="blue"))

# 4 
resi <- summary(lm(new_price~new_weight,new_diamonds))$residuals   # do linear model and extract residuals
li_diamonds <- data.frame(resi,new_diamonds)                       # add residuals to dataframe
ggplot(li_diamonds,aes(x=new_weight,y=resi,color=factor(color)))+  # x axis is log(carat), y axis is residuals
  geom_point()+                                                    # scatter plot
  ggtitle("Diamonds-Weigth to Price by Color")+                    # add title
  xlab('weight')+ylab('Price Residuals')+                          # add xlab and ylab
  theme(plot.title=element_text(color="blue"),legend.position = "top") 
                                                                   # change color of title and change position of legend

# 5

p1 <- ggplot(li_diamonds,aes(x=new_weight,y=resi,color=factor(color)))+
  geom_point()+
  ggtitle("Diamonds-Weigth to Price by Color")+
  xlab("Weight")+ylab("Price Residuals")+
  theme(plot.title=element_text(color="blue"),legend.position = "top")+
  guides(col = guide_legend(nrow = 1))                            # show the legend in one col
p1  
# print the plot as problem 4

vie2 <- viewport(width=0.4,height=0.2,x=0.78,y=0.75)
# set up the relative position on the upper right
vie3 <- viewport(width=0.4,height=0.2,x=0.25,y=0.15)
# set up the relative position on the bottom left
p2 <- ggplot(diamonds,aes(x=carat,color=color,..density..))+     # x asix is carat and use color to classify
  geom_histogram(binwidth = 0.029)+ # plot the density histogram and set the proper binwidth
  theme(legend.position = "none",axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))                 # remove the legend and axis title and margin
p3 <- ggplot(diamonds,aes(x=price,colour=color,..density..))+    # use the same method to plot the density histogram as above
  geom_histogram(binwidth = 50)+
  theme(legend.position = "none",axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
print(p2,vp=vie2)  # print the plot of carat on the upper right
print(p3,vp=vie3)  # print the plot of price on the bottom left

# 6
grid.newpage()   # creat a new page

vie1 <- viewport(width=0.7,height=0.7,x=0.6,y=0.65) # set the relative position of the big plot
p1 <- ggplot(li_diamonds,aes(x=new_weight,y=resi,color=factor(color)))+  
  geom_point()+
  ggtitle("Diamonds-Weigth to Price by Color")+
  xlab("Weight")+ylab("Price Residuals")+
  theme(plot.title=element_text(color="blue",size=20),legend.position = "top")+
  guides(col = guide_legend(nrow = 1))
print(p1,vp=vie1) # plot the big plot in the middle 

vie2 <- viewport(width=0.6,height=0.23,x=0.63,y=0.15)
# set the relative position on the bottom
p2 <- ggplot(diamonds,aes(x=carat,color=color,..density..))+
  geom_histogram(binwidth = 0.029)+
  theme(legend.position = "none",axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
print(p2,vp=vie2) # print the density histogram of carat on the bottom

vie3 <- viewport(width=0.4,height=0.23,x=0.15,y=0.6,angle=90) 
# set the relative position on the left and rotate it 90 degree anticlockwise
p3 <- ggplot(diamonds,aes(x=price,colour=color,..density..))+
  geom_histogram(binwidth = 50)+
  theme(legend.position = "none",axis.title=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))
print(p3,vp=vie3) # print the density histogram of price on the left























