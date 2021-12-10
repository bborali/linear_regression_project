library(readxl)

# import datasets
amazon <- read_excel("~/Github/linear_regression_project/AMZN_Full_Data.xlsx")
amazon_pre <- read_excel("~/Github/linear_regression_project/AMZN_Pre_Covid.xlsx")
nvidia <- read_excel("~/Github/linear_regression_project/NVDA_Full_Data.xlsx")
nvidia_pre <- read_excel("~/Github/linear_regression_project/AMZN_Pre_Covid.xlsx")
sp <- read_excel("~/Github/linear_regression_project/SP_500_Full_Data.xlsx")

# visualize datasets
View(amazon)
View(amazon_pre)
View(nvidia)
View(nvidia_pre)
View(sp)


typeof(amazon$Date)
# Convert type to Date
amazon$Date<-as.Date(amazon$Date)
amazon_pre$Date<-as.Date(amazon_pre$Date)
nvidia$Date<-as.Date(nvidia$Date)
nvidia_pre$Date<-as.Date(nvidia_pre$Date)
sp$Date <- as.Date(sp$Date)

# line graph (separate)
plot(x=amazon$Date, y=amazon$Close,  type='l')
plot(nvidia$Date, nvidia$Close ,type='l')
plot(sp$Date, sp$`Close/Last` ,type='l')

#dates (all)
d=amazon$Date
#dates_pre_covid
d_pre=amazon_pre$Date


a1=amazon$Open
a2=amazon$High
a3=amazon$Low
y_a=amazon$Close
A=cbind(a1,a2,a3)

a1_p=amazon_pre$Open
a2_p=amazon_pre$High
a3_p=amazon_pre$Low
y_a_p=amazon_pre$Close
A_p=cbind(a1_p,a2_p,a3_p)

b1=nvidia$Open
b2=nvidia$High
b3=nvidia$Low
y_b=nvidia$Close
B=cbind(b1,b2,b3)

b1_p=nvidia_pre$Open
b2_p=nvidia_pre$High
b3_p=nvidia_pre$Low
y_b_p=nvidia_pre$Close
B_p=cbind(b1_p,b2_p,b3_p)

c1=sp$Open
c2=sp$High
c3=sp$Low
y_c=sp$`Close/Last`
C=cbind(c1,c2,c3)


# create full model for amazon
model_a=lm(y_a~a1+a2+a3)
summary(model_a)
anova(model_a)
# create full model for amazon_pre_covid
model_a_p=lm(y_a_p~a1_p+a2_p+a3_p)
summary(model_a)
anova(model_a)
# create full model for nvidia
model_b=lm(y_b~b1+b2+b3)
summary(model_b)
anova(model_b)
# create full model for nvidia_pre_covid
model_b_p=lm(y_b_p~b1_p+b2_p+b3_p)
summary(model_b_p)
anova(model_b_p)
# create full model for sp_500
model_c=lm(y_c~c1+c2+c3)
summary(model_c)
anova(model_c)

