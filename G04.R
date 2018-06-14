getwd()
list.files()
pf<-read.csv('HR_comma_sep.csv', stringsAsFactors = F)

library(ggplot2)
library(cluster)
library(gridExtra)
library(plotly)

names(pf)
dim(pf)
str(pf)



library(corrplot)

cor(pf[,1:8])
corrplot(cor(pf[,1:8]), method="circle")

#1 Left and last_evaluation
ggplot(pf,aes(last_evaluation)) +
   geom_density(fill = "#F8766D") +
   facet_wrap( ~ left, ncol = 2)

#2 Left and satisfaction_level
ggplot(pf,aes(satisfaction_level)) +
  geom_density(fill = "#00BA38") +
  facet_wrap( ~ left, ncol = 2)

ggplot(pf) + 
  geom_boxplot(aes(x=left,y=satisfaction_level))

#3 last_evaluation and satisfaction_level
pf$left <- as.factor(pf$left) 
ggplot(pf,aes(last_evaluation,satisfaction_level,colour= left ))+
  geom_point()

# phan thanh 2 bieu do

pf$left <- as.factor(pf$left) 
ggplot(pf,aes(last_evaluation,satisfaction_level,colour= left ))+
  geom_point()+
  facet_wrap( ~ left, ncol = 2) 

#3.1 lay du lieu nhan vien roi di
dt_left_1 <- subset(pf, left=='1');
str(dt_left_1)
dim(dt_left_1)

datatemp<-subset(dt_left_1, satisfaction_level > 0.69, last_evaluation >0.8 )                 

str (datatemp)

dim(datatemp)

# Muc luong nhan vien roi di
ggplot (datatemp, aes(salary,fill = "left"))+
  geom_bar()

# Thoi gian lam viec tai cong ty cua nhan vien roi di
ggplot (datatemp, aes(time_spend_company,fill="left "  ))+
  geom_bar()

#kha nang thang tien trong 5 nam 
ggplot (datatemp, aes(promotion_last_5years,fill="left "))+
  geom_bar()

# Nganh nghe
ggplot (datatemp, aes(sales,fill="left "))+
  geom_bar()

#4 number_project and left 

ggplot(pf,aes(x=number_project)) + 
  geom_histogram(binwidth=.3, alpha = 1, fill = "#AE3121")+
  facet_wrap( ~ left,ncol=2)

# Muc luong cua nhan vien lam 7 project
SLPR <- subset(pf,number_project=="7")

ggplot(SLPR,aes(x=number_project)) +
  geom_histogram(alpha = 1, fill = "#AE3121") +
  facet_wrap( ~ salary, ncol = 3)

#5 Phong va luong
ggplot(pf,aes(x= salary, col= salary, fill= salary)) +
  geom_bar()+
  facet_grid(~sales)+
  scale_x_discrete("Salary level") +
  scale_y_continuous("Nos of Employees ")+
  ggtitel("Employees by salary range")

#6 left and average_montly_hours
ggplot(pf,aes(average_montly_hours)) +
  geom_density(fill = "#619CFF")+
  facet_wrap( ~ left, ncol = 2)
#
ggplot(pf, aes(x = average_montly_hours))+
  geom_freqpoly(binwidth=10, aes(color= left ))

#7 number_project and average_montly_hours
ggplot(pf, aes(x = factor(number_project), y = average_montly_hours, fill = factor(left))) +
  geom_boxplot() + scale_fill_manual(values = c("yellow", "#00BA38"))

#8 left and salary

ggplot(pf,aes(x=salary)) + 
     geom_histogram( aes(fill = as.factor(salary)))+
    facet_wrap(~left, ncol=2)

#9 average_montly_hours and time_spend_company

ggplot(pf, aes(x =  factor(time_spend_company), y = average_montly_hours, fill = factor(left))) + 
  geom_boxplot() + scale_fill_manual(values = c("#56B4E9", "#00BA38")) + xlab("Time spend Company") + 
  ylab("Average Monthly Hours") 


#10 time_spend_company and salary
library(gridExtra)
 pf$time_spend_company <- as.factor(pf$time_spend_company)
 g1 <- ggplot(data = subset(pf, left =="1"), aes(x =salary , fill = salary))+
   geom_bar()+
   facet_grid(~time_spend_company)+
   scale_x_discrete("Time spend in company") +ggtitle("Left = 0")
 g2 <- ggplot(data = subset(pf, left =="0"), aes(x =salary , fill = salary))+
     geom_bar()+
     facet_grid(~time_spend_company)+
     scale_x_discrete("Time spend in company") +ggtitle("Left = 1")
 grid.arrange(g2,g1,ncol =1)
 