#Marianna Ohanyan info vis assigment 5  28.02.19

install.packages("RCurl")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("reshape2")
install.packages("wesanderson")

library(RCurl)
library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(wesanderson)
library(gridExtra)
library(grid)

#reading data
getData <- getURL("https://raw.githubusercontent.com/mmash98/infoVis/master/menu.csv")
df <- read.csv(text = getData)
df<- na.omit(df)

#data cleaning 
#taking only food data and according grams
summary(df$Category)
drinks <- c("Smoothies & Shakes", "Beverages", "Coffee & Tea")
drink = filter(df, df$Category %in% drinks)
food = filter(df, !df$Category %in% drinks)

clean1 = gsub("[\\()g]", "", food$Serving.Size)
clean2 = gsub(".*oz", "", clean1)
food$Serving.Size = as.integer(clean2)
food<- na.omit(food)
# only numeric data 
dim(df)
Qdata = df[,4:24]
Mdata = select(df,-c(2,3))

#BASIC ANALYSES
# we can see a lot of corralted items
dev.new()
corr <- round(cor(Qdata), 1)
corrplot(corr, method = "circle")

#1. First inside I want to plot food weight in grams and its categories 
#for people to understand which foods weight more in avarage
# just in case if person whats to fell light , which food to eat
df_agg <- aggregate(Serving.Size ~ Category , data=food,  FUN=mean, na.rm = TRUE)
attach(df_agg)

ggplot(df_agg, aes(x=Category, y=Serving.Size, color=Serving.Size)) +
  geom_point( size=3)+
  coord_polar()+
  theme_light()+
  labs(title="Serving size and product Category relation",
       y="",
       x="",
       caption="We can observe that Snack & Sides weight the less in gram")+
  guides(color = guide_legend(title = "Serving size in grams"))+
  scale_colour_gradientn(colours=rainbow(5))

#2. Lets see correlation between food size and calories 
#it will show uset which amout they should it for specific amount of calories
ggplot(food, aes(x=Serving.Size, y=Calories, color= Category)) +
  geom_point(size = 2)+
  labs(title="Serving size and Calories correlation",
       x="Serving Size",
       y="Calories",
       caption="The main point of this research is to figure out that if you choose food according  
       it grams, most probably it will have 2 time more calories")+
  theme_set(theme_bw()) 



#MAIN WORK
# 3.types of fats,
#As we know fast foods are known for its trans Fats which makes people get fat 
#very fast, by looking at this visualisation person can understood what type 
# of product to use in McDonalds not to get fat very fast.
# I have done a research and figured out that as Trans fat has very bad repitation 
#in USA , a lot of fast food places ban them from their foods.
#In McDonalds data we can observe that improvement.
#red color emphasize danger

#data prepearation 
df_agg3 <- aggregate(. ~ Category,data=Mdata, FUN=mean)

FATS = data.frame(
  Category = df_agg3$Category,
  Unsaturated = df_agg3$Total.Fat-df_agg3$Saturated.Fat-df_agg3$Trans.Fat,
  Saturated = df_agg3$Saturated.Fat,
  Trans = df_agg3$Trans.Fat
)

new_FATS = melt(FATS, id.vars=c("Category"))

#visualisation
ggplot(new_FATS, aes(x=reorder(Category, value), y=value, fill = variable))+
  geom_bar( stat="identity", width = 0.7, position = position_stack(reverse = TRUE))+
  coord_flip()+
  scale_fill_manual(values=c("#CFFBB5", "#FDD69E", "#E73C24"), name = "Types of fat")+
  theme_light()+
  labs(title="Fat types in McDonald's products",
       y="Quantity of fat in grams",
       x="",
       caption = "Data shoes that trans fats which are the most dangerous ones 
       are only in Beff & Pork in big quantities ")


# 4.Main components of food 
# for consumer to choose which type of food to consume, 
# very often a lot of people look at the components the buy something 

COMPONENTS = data.frame(
  Category = df_agg3$Category,
  Fat = df_agg3$Total.Fat,
  Carbohydrates  = df_agg3$Carbohydrates,
  Protein = df_agg3$Protein
)

new_COMPONENTS = melt(COMPONENTS, id.vars=c("Category"))

ggplot(new_COMPONENTS, aes(x=variable, y=value, fill = variable))+
  geom_bar( stat="identity", width = 0.7)+
  coord_flip()+
  facet_wrap(~Category)+
  scale_fill_manual(values=c("#CFFBB5", "#FDD69E", "#E73C24"), name = "Components")+
  theme_light()+
  labs(title="Main components of products",
       subtitle = "Quantity of Proteins, Fat, Carbs in grams inside products",
       y="Quantity in grams",
       x="")



