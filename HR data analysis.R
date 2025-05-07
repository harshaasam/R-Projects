df = read.csv(file.choose())
df[]
colnames(df)

summary.data.frame(df)
colnames(df)

# REMOVING UNNECESSARY COLUMNS(CONSTANTS)
df$EmployeeCount <- NULL
df$StandardHours <- NULL
df$Over18 <- NULL
df$EmployeeNumber <- NULL

# EXPLORATORY DATA ANALYSIS
install.packages("ggplot2")
library("ggplot2")


# 1. GENDER-WISE ATTRITION
a<- ggplot(data = df)+geom_bar(aes(x = Attrition,fill = Gender))
a
# 2. AGE-WISE ATTRITION
ggplot(data = df)+geom_histogram(binwidth = 2,aes(x = Age, fill = Attrition))

ggplot(data = df)+geom_density(binwidth = 2,alpha = 0.5,aes(x = Age, fill = Attrition))

ggplot(data = df)+geom_boxplot(aes(y = Age, x = Attrition))

ggplot(data = df)+geom_boxplot(aes(y = Age, x = Attrition), size = 1.2)

ggplot(data = df, x = Attrition)+geom_boxplot(aes(y = Age, x = Attrition), size = 1.0)

ggplot(data = df, aes(x = Attrition, y = Age))+geom_boxplot(fill = "lightblue")+
geom_jitter(color = 'darkblue', alpha = 0.1)


ggplot(data = df, aes(x = Attrition, y = Age))+geom_boxplot(fill = "lightblue")+
  geom_jitter(color = 'darkblue', alpha = 0.1)+ 
  labs(title = 'Age by attrition', x= 'Atrrition', y = 'Age')

# 3. Monthly income by Attrition
ggplot(data = df, aes(y = MonthlyIncome, x = Attrition))+geom_boxplot(fill = 'yellow')+
  geom_jitter(alpha = 1,size = 0.1)+
  labs(title = 'Monthly income by Attrition')

ggplot(data = df, aes(y = MonthlyIncome, x = Attrition))+geom_boxplot(fill = 'yellow')+
   labs(title = 'Monthly income by Attrition')+ 
  geom_jitter(alpha = 1,size = 0.1)+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )


ggplot(data = df, aes(y = MonthlyIncome, x = Attrition))+geom_boxplot(fill = 'yellow')+
  labs(title = 'Monthly income by Attrition')+ 
  geom_jitter(alpha = 1,size = 0.1)+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggplot(data = df, aes(y = MonthlyIncome, x = Attrition))+geom_boxplot(fill = 'yellow')+
  labs(title = 'Monthly income by Attrition')+ 
  geom_jitter(alpha = 1,size = 0.1)+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15,hjust = 0.5,colour = 'blue'),
    axis.title = element_text(size = 10, colour = 'darkorange')
  )

# 4.Attrition by overtime
ggplot(data = df)+geom_bar(aes(x = OverTime,fill = Attrition))+
  labs(title = 'Overtime by Attrition')+
  theme(plot.title = element_text(size = 15,hjust = 0.5),
        axis.title = element_text(color = 'darkblue'))

ggplot(data = df) +
  geom_bar(aes(x = OverTime, fill = Attrition)) +
  geom_text(
    aes(x = OverTime, label = ..count.., group = Attrition),
    stat = "count",
    position = position_stack(vjust = 0.5),
    color = "white", size = 4
  ) +
  labs(title = 'Overtime by Attrition', x = 'OverTime', y = 'Count') +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    axis.title = element_text(color = 'darkblue')
  )


colnames(df)
df$JobSatisfaction

# 5. Job satisfaction by Attrition

a<- ggplot(data = df, aes(x = JobSatisfaction))
a+geom_bar(aes(fill = Attrition))

# 6. Using facet grid

b <- ggplot(data = df, aes(y = YearsAtCompany))
b+geom_boxplot(aes(fill = Attrition))
b+geom_boxplot(aes(fill = Attrition))+facet_grid(Department~.)
b+geom_boxplot(aes(fill = Attrition))+facet_grid(Department~., scale = 'free')
unique(df$JobRole)

b+geom_boxplot(aes(fill = Attrition))+facet_grid(.~Department, scale = 'free')

