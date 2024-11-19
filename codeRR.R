#1. Doc du lieu
data <- read.csv("C:/Users/Admin/Desktop/BTL_XSTK/data.csv")
str(data)

#2. lam sach du lieu
#2.1. chon loc bien
new_data<-data[,c("layer_height","wall_thickness","infill_density","infill_pattern","nozzle_temperature"
        ,"bed_temperature","print_speed","material","fan_speed","roughness")]

head(new_data,6)

#2.2. kiem tra du lieu khuyet
apply(is.na(new_data),2,sum)

#3. Thong ke ta
const_var<-data[,c("layer_height","wall_thickness","infill_density","nozzle_temperature"
            ,"bed_temperature","print_speed","fan_speed","roughness")]
mean<-apply(const_var,2,mean)
sd<-apply(const_var,2,sd)
median<-apply(const_var,2,median)
min<-apply(const_var,2,min)
max<-apply(const_var,2,max)
t(data.frame(mean,sd,median,min,max))

hist(new_data$roughness,xlab="Độ nhám",ylab="Tần số",main="Đồ thị thể hiện độ nhám đo lường",
     col="cyan3",ylim=c(0,10),labels=TRUE)

table(new_data$infill_pattern)

table(new_data$material)

boxplot(roughness~infill_pattern,data=new_data,main="Đồ Thị Boxplot roughness Theo infill_pattern",col=c("gold","red"))

boxplot(roughness~material,data=new_data,main="Độ Thị Boxplot roughness Theo material",col=c("wheat","tan"))

plot(new_data$layer_height,new_data$roughness,xlab="layer_height",ylab="roughness"
     ,main="Đồ Thị layer_height & roughness",pch=19)

plot(new_data$wall_thickness,new_data$roughness,xlab="wall_thickness",ylab="roughness"
     ,main="Đồ Thị wall_thickness & roughness",pch=19)

plot(new_data$infill_density,new_data$roughness,xlab="infill_density",ylab="roughness"
     ,main="Đồ Thị infill_density & roughness",pch=19)


plot(new_data$nozzle_temperature,new_data$roughness,xlab="nozzle_temperature",ylab="roughness"
     ,main="Đồ Thị nozzle_temperature & roughness",pch=19)

plot(new_data$bed_temperature,new_data$roughness,xlab="bed_temperature",ylab="roughness"
     ,main="Đồ Thị bed_temperature & roughness",pch=19) 

plot(new_data$print_speed,new_data$roughness,xlab="print_speed",ylab="roughness"
     ,main="Đồ Thị print_speed & roughness",pch=19)

plot(new_data$fan_speed,new_data$roughness,xlab="fan_speed",ylab="roughness"
     ,main="Đồ Thị fan_speed & roughness",pch=19)
#Kiểm định 1 mẫu
qqnorm(new_data$roughness)
qqline(new_data$roughness) #Đồ thị Q-Q plot để giả định phân phối chuẩn

shapiro.test(new_data$roughness) #Kiểm định độ nhám có phân phối chuẩn không

t.test(new_data$roughness, mu=170) #Kiểm định độ nhám trung bình

qt(p=.05/2, df=50-1, lower.tail = FALSE) #Tính t0 ngưỡng 2 phía
#Kiểm định 2 mẫu

abs_data<-subset(new_data,material=="abs")
pla_data<-subset(new_data,material=="pla") #-	Chia bộ dữ liệu theo 2 nhóm vật liệu

qqnorm(abs_data$roughness)
qqline(abs_data$roughness) #Đồ thị Q-Q plot để giả định phân phối chuẩn độ nhám ở vật liệu abs

shapiro.test(abs_data$roughness) #Kiểm định độ nhám ở vật liệu abs có phân phối chuẩn không

qqnorm(pla_data$roughness)
qqline(pla_data$roughness) #Đồ thị Q-Q plot để giả định phân phối chuẩn độ nhám ở vật liệu pla

shapiro.test(pla_data$roughness) #Kiểm định độ nhám ở vật liệu pla có phân phối chuẩn không

sd1<-sd(abs_data$roughness) #So sánh S1,S2 nếu S1<S2 thì đặt H1: (σ1)^2<(σ2)^2
sd2<-sd(pla_data$roughness) #So sánh S1,S2 nếu S1>S2 thì đặt H1: (σ1)^2>(σ2)^2

var.test(abs_data$roughness,pla_data$roughness,alternative="greater") #Kiểm định phương sai độ nhám ở 2 nhóm vật liệu

xtb1<-mean(abs_data$roughness) #So sánh xtb1,xtb2 nếu x1<x2 thì đặt H1: μ1<μ2
xtb2<-mean(pla_data$roughness) #So sánh xtb1,xtb2 nếu x1>x2 thì đặt H1: μ1>μ2

t.test(abs_data$roughness,pla_data$roughness,alternative="greater",var.equal=TRUE) # Thực hiện kiểm định bằng t-test

#3 ma trận tương quan
cor(const_var)
cor_data<-cor(const_var)
library(corrplot)
corrplot(cor_data)

#4 mô hình hồi quy 
model_1<-lm(roughness~layer_height+wall_thickness+infill_density+nozzle_temperature
            +bed_temperature+print_speed+infill_pattern+material,new_data)
summary(model_1)
#4.1 mô hình hồi quy đã loại bớt biến
model_2<-lm(roughness~layer_height+nozzle_temperature
            +bed_temperature+print_speed+material,new_data)
summary(model_2)
#5 đồ thị của sai số
plot(model_2)


#6 kiểm tra phân phối chuẩn của sai số
data$roughness_dubao<-predict(model_2,newdata=data)
data$sai_so=data$roughness-data$roughness_dubao
shapiro.test(data$sai_so)
#7 bài toán kiểm định 
n<-length(data$sai_so)
xtb<-mean(data$sai_so)
dolechchuan_mau<-sd(data$sai_so)

data.frame(n,xtb,dolechchuan_mau)
qt(p=.05/2,df=49,lower.tail = FALSE)
t_test<-qt(p=.05/2,df=49,lower.tail = FALSE)*dolechchuan_mau/sqrt(n)
head(t_test)
#anova 1 nhân tố

data$bed_temperature<-as.factor(data$bed_temperature)

boxplot(roughness~bed_temperature,data=new_data,main="roughness & bed_temperature")

anova_model1<-aov(roughness~bed_temperature, data)
summary(anova_model1)



#kiểm tra điều kiện phân phối chuẩn ở các nhóm
nhom1<-subset(data,bed_temperature=="60")
shapiro.test(nhom1$roughness)

nhom2<-subset(data,bed_temperature=="65")
shapiro.test(nhom2$roughness)

nhom3<-subset(data,bed_temperature=="70")
shapiro.test(nhom3$roughness)

nhom4<-subset(data,bed_temperature=="75")
shapiro.test(nhom4$roughness)

nhom5<-subset(data,bed_temperature=="80")
shapiro.test(nhom5$roughness)
#kiểm tra giả định phương sai bằng nhau

library(car)
leveneTest(roughness~bed_temperature, data)



