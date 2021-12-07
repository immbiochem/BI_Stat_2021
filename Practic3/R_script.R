integrator <- function(path_to_file, ind){
  setwd(path_to_file) #меняем директорию на указанную в аргументе
  names_of_files <- dir(getwd()) #вектор со всеми файлами в директории
  is_ind <- function(x, ind){
    parts <- strsplit(x,".",fixed=TRUE)
    nparts <- length(parts[[1]]) 
    return(parts[[1]][nparts] == ind) 
  } #функция, определяющая наличие расширения ind
  mk_table_csv <- function(x){
    return(read.csv(x))
  } #открывает файл с помощью read.csv 
  mk_table_table <- function(x){
    return(read.table(x)) 
  } #открывает файл с помощью read.table
  table_maker <- function(named_vector, mk_table_f){
    just_table <- mk_table_f(named_vector[1])
    for (i in named_vector[-1]){
      just_table <- rbind(just_table, mk_table_f(i))
    }
    return(just_table)
  } #соединяет файлы из указанного вектора в один
  if (ind == "csv"){
    finish_table <- table_maker(names_of_files[sapply(names_of_files,is_ind, ind)], mk_table_csv)
    
  } else{
    finish_table <- table_maker(names_of_files[sapply(names_of_files,is_ind, ind)], mk_table_table)
    
  }
  return(finish_table)
}
practic <- integrator("C:/Users/pfft/OneDrive/Рабочий стол/БИ/R stat/Задание на практику 3/Data", "csv")
str(practic)
summary(practic)

# factors to factor
practic$gender <- factor(practic$gender)
practic$drug_type <- factor(practic$drug_type)
practic$is_relapse <- factor(practic$is_relapse)
practic$gender[practic$gender == "malle"] <- "male"
unique(practic$age)
practic$age[practic$age == "thirty-one"] <- "31"
practic$age[practic$age == "220"] <- NA
practic$age[practic$age == "350"] <- NA
practic$age <- as.numeric(practic$age)
practic <- na.omit(practic)
pairs(practic)

qqnorm(practic$days_in_hospital)
qqline(practic$days_in_hospital)

qqnorm(practic$days_in_hospital[practic$drug_type=="New_type_1"])
qqline(practic$days_in_hospital[practic$drug_type=="New_type_1"])

qqnorm(practic$days_in_hospital[practic$drug_type=="New_type_2"])
qqline(practic$days_in_hospital[practic$drug_type=="New_type_2"])

qqnorm(practic$days_in_hospital[practic$drug_type=="Old_type"])
qqline(practic$days_in_hospital[practic$drug_type=="Old_type"])

qqnorm(practic$days_in_hospital[practic$drug_type=="Placebo"])
qqline(practic$days_in_hospital[practic$drug_type=="Placebo"])

ggplot(practic, aes(x=days_in_hospital))+geom_density()+theme_bw()
ggplot(practic, aes(y=days_in_hospital, x=drug_type))+geom_boxplot()+theme_bw()
ggplot(practic, aes(y=days_in_hospital, x=drug_type, fill=gender))+geom_boxplot()+theme_bw()

ggplot(practic, aes(y=days_in_hospital, x=gender))+geom_boxplot()+
  ggtitle("The number of days for men and women that they spent in the hospital")+
  xlab("Gender")+ 
  ylab("Days in hospital")+
  theme_bw()
# удалим выбросы
practic <- practic %>% filter(days_in_hospital<37 & days_in_hospital>15)


practic %>% summarise(Mean=mean(days_in_hospital), SD=sd(days_in_hospital))




practic_female<- practic %>% filter(gender=="female", drug_type=="Placebo"|drug_type=="New_type_2")
# проверка на нормальность
shapiro.test(practic_female$days_in_hospital[practic_female$drug_type=="Placebo"])
shapiro.test(practic_female$days_in_hospital[practic_female$drug_type=="New_type_2"])
# тестируем непараметрикой
wil <- wilcox.test(days_in_hospital~drug_type, practic_female)
ggplot(practic_female, aes(y=days_in_hospital, x=drug_type))+geom_boxplot()+ ggtitle("Comparison of new type 2 drug with placebo in a female group")+xlab("Drug type") + ylab("Days in hospital")+theme_bw()
cat("Method:", wil$method, "Statistic:", wil$statistic, "P-value:", round(wil$p.value, 6))

fit <- aov(days_in_hospital~drug_type, practic) 
summary(fit)
# нет значимых различий между четырьмя группами
fit2 <- aov(days_in_hospital~drug_type*gender, practic)
summary(fit2)
# есть различия при изучении взаимодействия
fit3 <- TukeyHSD(fit2)
fit4 <-fit3$`drug_type:gender`
fit4[ ,4]<0.05
fit4 <- as.data.frame(fit4)
fit4[fit4[ ,4]<0.05, ] 
ggplot(practic, aes(y=days_in_hospital, x=drug_type, fill=gender))+
  geom_boxplot()+
  ggtitle("Comparison of days of hospitalization by type of medication in different gender groups")+
  xlab("Drug type")+ 
  ylab("Days in hospital")+
  theme_bw()
fit4[fit4[ ,4]<0.05, ][c(1,4)]
 
 
 
 