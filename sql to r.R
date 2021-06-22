install.packages('tidyverse')
install.packages("rstats-db/odbc")
install.packages("DBI")
install.packages('devtools')

install.packages("rJava")
install.packages("RJDBC")

library(rJava)
library(RJDBC)
library(dplyr)
library(broom)
library(tidyverse)
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
conn<-dbConnect(drv, "jdbc:oracle:thin:@//localhost:1521/xe","hr","123456")

conn


# db에서 가져오기 ---------------------------------------------------------------
employees<-dbGetQuery(conn, 'select * from employees')
departments <- dbGetQuery(conn, 'select * from departments')
View(employees)
colnames(employees) <- tolower(colnames(employees))
colnames(departments) <- tolower(colnames(departments))

departments %>%  head()
employees %>%  head()

class(Sys.Date())
class(employees$hire_date)

employees <- employees %>% mutate_at(vars(hire_date), as.Date)

employees <- employees %>% mutate(working_date = Sys.Date() - hire_date)
remove(emloyees)
table(employees$working_date)

employees %>% map(~lm(salary ~ working_date, data=.))

library(lme4)
install.packages('lme4')

lme4::lmList(salary ~ working_date | department_id, data = employees)

# 그룹별로 선형회귀 ---------------------------------------------------------------
employees %>% nest(-department_id) %>% 
  mutate(fit = map(data, ~lm(salary ~ working_date, data=.)),
         result = map(fit,glance)) %>% arrange(department_id) %>% 
  unnest(result)

library(broom)
df = group_by(data, sex) %>%
  do(m1 = lm(pituitaryVolume ~ age, data = .))
f1= employees %>% group_by(department_id) %>% 
  do(fit1=lm(salary ~ working_date, data=.))
f1=f1[-12,]
f1$fit1
tidy(f1, fit1)


# pivoting ----------------------------------------------------------------

#wider
sales_data <- dbGetQuery(conn, 'select * from sales_data')

sales_data
colnames(sales_data) <- tolower(colnames(sales_data))
sales_data %>% pivot_wider(names_from = week_day , values_from = sales)
?pivot_wider

income_wider <- income_longer %>% pivot_wider(names_from =  term, values_from = income)
income_wider %>% head()
#longer
sales<-dbGetQuery(conn, 'select * from sales')
colnames(sales) <- tolower(colnames(sales))
glance(sales_data)
glimpse(sales_data)

sales
sales %>% 
  pivot_longer(-c(employee_id, week_id), names_to = "week_day", values_to = "sales")

data("relig_income")
income_longer <- relig_income %>% pivot_longer(cols=-religion, names_to="term", values_to="income")

# join --------------------------------------------------------------------

Departments <- data.frame(Department = c(11, 12, 13, 14),
                   DepartmentName = c("Production", "Sales", "Marketing", "Research"),
                   Manager = c(1, 4, 5, NA)) #Departments 데이터 테이블1

Employees <-data.frame(Employee = c(1, 2, 3, 4, 5, 6),
                  EmployeeName = c("Alice", "Bob", "Carla", "Daniel", "Evelyn", "Ferdinand"),
                  Department = c(11, 11, 12, 12, 13, 21),
                  Salary = c(800, 600, 900, 1000, 800, 700)) #Employees 데이터 테이블2

Employees
Departments

#1. inner_join: 두 테이블의 교집합만 추출
inner_join(Employees, Departments, by="Department")

#2. left_join
left_join(Employees, Departments, by = "Department")

#3. right_join
right_join(Employees, Departments, by="Department")

#4. full_join: 두 테이블의 모든 정보 추출
full_join(Employees, Departments, by="Department")



# group_by ---------------------------------------------------------------

# rollup, cube 기능 구현

employees %>% select(department_id, job_id, salary) %>% 
  group_by(department_id, job_id) %>% 
  summarise(avg_sal = mean(salary)) %>% 
  mutate(salary_sum = cumsum(avg_sal))
  arrange(department_id,job_id) 


# 부서별 평균급여보다 많은 급여를 받는 사원 ------------------------------------------------
employees %>% select(first_name, salary, department_id) %>% 
  group_by(department_id) %>% 
  mutate(avg_salary = mean(salary)) %>% 
  filter(salary > mean(salary)) %>%  summarise(sum = colSums(salary))



# windowing --------------------------------------------------------------

employees %>% select()
  





# sqldf ------------------------------------------------------------------


