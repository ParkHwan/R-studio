library(dplyr)
emp <- read.csv("data/emp.csv")
emp

#1단계
#문제1
emp %>% filter(job == "MANAGER")

#문제2
emp %>% select(empno, ename, sal)

#문제3
emp %>% select(-empno)

#문제4
emp %>% select(ename,sal)

#문제5
emp %>% 
  group_by(job) %>%
  tally()

#문제6
emp %>% 
  select(ename,sal,deptno) %>%
  filter(sal >= 1000 & sal <=3000)

#문제7
emp %>% 
  select(ename,job,sal) %>%
  filter(job != "ANALYST")

#문제8
emp %>% 
  select(ename,job) %>%
  filter(job == "SALESMAN"| job == "ANALYST")

#문제9
emp %>% 
  group_by(deptno) %>%
  summarise(deptsum = sum(sal))

#문제10
emp %>% 
  arrange(sal)

#문제11
emp %>% 
  arrange(desc(sal)) %>%
  head(1)

#문제12
empnew <- emp %>% rename(salary= 'sal', commrate='comm')
str(empnew)

#문제13
emp %>%
  count(deptno) %>%
  arrange(desc(n)) %>%
  head(1) %>%
  select(deptno) 

#문제14
emp %>%
  mutate(enamelength = nchar(ename)) %>%
  arrange(enamelength) %>%
  select(ename, enamelength)

#문제15
emp %>%
  filter(comm != is.na(comm)) %>%
  summarise(n = n())
  

#2단계
mpg <- as.data.frame(ggplot2::mpg)

#문제16
#1
str(mpg)

#2
dim(mpg)

#3
head(mpg, 10)

#4
tail(mpg, 10)

#5
View(mpg)

#6
summary(mpg)

#7
mpg %>%
  group_by(manufacturer) %>%
  tally()

#8
mpg %>%
  group_by(manufacturer, model) %>%
  tally()

#문제17
#1
mpg <- mpg %>% rename(city = "cty",highway = "hwy")

#2
head(mpg)

#문제18
#1
mpg %>%
  mutate(newdispl = ifelse(displ >= 4, "5up", "4down")) %>%
  group_by(newdispl) %>%
  summarise(meanhighway = mean(highway))
  
#2
mpg %>%
  filter(manufacturer == "audi" | manufacturer == "toyota" ) %>%
  group_by(manufacturer) %>%
  summarise(meancity = mean(city))

#3
mpg %>%
  filter(manufacturer == "chevrolet" | manufacturer == "ford" | manufacturer == "honda" ) %>%
  summarise(meanhighway = mean(highway))

#문제19
#1
mpg3 <- mpg %>% select(class, city)
head(mpg3)

#2
mpg3 %>%
  filter(class =="suv" | class == "compact") %>%
  group_by(class) %>%
  summarise(meancity = mean(city))

#문제20
mpg %>%
  filter(manufacturer == "audi")%>%
  arrange(desc(highway)) %>%
  head(5)
