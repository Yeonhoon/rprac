install.packages('data.table')
library(data.table)


# DB에서 가져오기 --------------------------------------------------------------
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:/oraclexe/app/oracle/product/11.2.0/server/jdbc/lib/ojdbc6.jar")
conn<-dbConnect(drv, "jdbc:oracle:thin:@//localhost:1521/xe","hr","123456")

locations <- dbGetQuery(conn, 'select * from locations')
head(locations)
emp <- as.data.table(employees)
tables()

emp[, .(avg_sal=mean(salary)), by='department_id', ][order(department_id)]
emp[1:3,list(이름=first_name,성=last_name)]

dim(emp[emp[,.I]%%2==0])
dim(emp[emp[,.I]%%2==1])


#padding ------------------------------------------------------------
stringr::str_pad('string',width = 10, side='left', pad='#')

#subset --------------------
emp[(department_id ==50 & employee_id < 150)]
library(data.table)
emp[,.(x=seq_len(.N))]
emp[.N, ]

#columns
emp[, .(department_id, manager_id)]

emp[, .(avg_sal=trunc(mean(salary),2), .N), keyby=.(department_id,job_id)][order(department_id)]

#length
emp[, .N, department_id][order(department_id)]

#mutate columns


# coalesce ---------------------------------------------------------------

emp[,.(a=fcoalesce(commission_pct, salary+(salary*commission_pct)))]


# decode -----------------------------------------------------------------

emp[,.(new_salary= ifelse(tolower(job_id) == 'it_prog',salary * 1.1,
                        ifelse(tolower(job_id) == 'fi_account', salary * 1.5,  salary)), salary)]

# rollup, cube, grouping sets --------------------------------

rollup(emp, j = .(avg_sal=mean(salary)), by=c('department_id','job_id'))[order(department_id)]
rollup(emp, j = lapply(.SD, sum), .SDcols = 'salary', by=c('department_id','job_id'), id=T)[order(department_id)]

cube(emp, j=c(.(count=.N), lapply(.SD, sum)), .SDcols = 'salary', by = c('department_id','job_id'))[order(department_id)]

groupingsets(emp, j=c(.(count=.N), lapply(.SD, sum)), .SDcols = 'salary',  sets = list("department_id","job_id"), by = c("department_id","job_id"))[order(department_id)]
groupingsets(emp, j=c(.(count=.N), lapply(.SD, sum)), .SDcols = 'salary', sets = list('department_id','job_id'), by = c('department_id','job_id'))

# rename -----------------------------------------------------------------

setnames(emp, c('department_id','job_id'),c('d_id','j_id'))
emp

#unique
uniqueN(emp, by='department_id')


# lag, lead --------------------------------------------------------------


# mutate -----------------------------------------------------------------
#paste = ||''||
emp[,full_name := paste(first_name, last_name)]
emp


# pivoting ---------------------------------------------------------------

sales_data <- dbGetQuery(conn, 'select * from sales_data')
colnames(sales_data) <- tolower(colnames(sales_data))

#melt: wide to long
sales_long = melt(sales_wide,
                  id.vars = c('employee_id','week_id'), # 유지할 변수
                  measure.vars = 
                    c('SALES_MON','SALES_TUE','SALES_WED','SALES_THU', 'SALES_WED'), # 길어질 변수
                  variable.name = 'weekday', # 새로 지정할 변수명
                  value.name = 'sales') #값의 이름

sales_long

#dcast: long to wide
setDT(sales_data)
sales_wide = dcast(sales_data, employee_id + week_id ~ week_day, value.var = 'sales')


dcast(sales_long, employee_id + week_id ~ weekday, fun.aggregate = sum, margins = 'sales')

install.packages('dtplyr')
library(dtplyr)
transmute(emp, hire_date = as.Date(hire_date))
emp
library(dplyr)


# 결측처리 -------------------------------------------------------------------

head(employees)
library(data.table)
library(stringr)
emp <- data.table(employees)
emp[tolower(job_id)=='it_prog',.(str_pad(first_name,10,'right','*'),str_pad(salary,10,'left','*'))]
emp[,.(job_id)]

head(emp)

# SD: Subset of Data - 그룹핑을 제외한 모든 변수
# SDcols: 연산대상이 되는 특정 column
emp[,head(.SD,6)]


# subset -----------------------------------------------------------------
#.I = seq_len(nrow(x))
# 홀수 또는 짝수번째 열만 subset
emp[emp[,.I]%%2==0]
emp[emp[.,employee_id]%%2==0]
emp[emp[,.I]%%2==1]

# select  함수 -------------------------------------------------------------


emp[department_id==50, full_name:=paste(first_name, last_name)]
emp[department_id==50, .(full_name, salary)][,head(.SD)][order(-salary)]

#between
emp[department_id %between% c(50,60)]

#like
emp[job_id %like% "MAN",.(first_name, job_id)]


## na를 na fill로 채워서 하거나, 미리 na 를 0으로 바꾸기!!
#1. is na를 바꿔주기

#2.nafill
emp_copy <- emp
emp_copy
setnafill(emp_copy, cols = 'commission_pct', fill = 0)
nafill(emp_copy,"nocb")

emp[is.na(commission_pct)]
emp_copy[, new_salary := (salary + salary*commission_pct)]
emp[salary!=new_salary,.(salary, new_salary, sal_diff = new_salary - salary)]




# group 함수 ---------------------------------------------------------------

# 기초 통계량(sum,mean,min,max)
emp[!is.na(department_id),.(m=mean(salary),s = sum(salary),n=.N), by=department_id] # na 값 제외

emp[,hire_date]

#######group by로 나누고 group별로 나타내기(for문) ######
iris_dt = as.data.table(iris)


iris_dt[, lapply(.SD,mean), keyby = Species]
iris_dt[,by=Species]
iris_dt
format(as.Date(employees$hire_date), format="%Y")

emp[,.(mean_salary = round(mean(salary),0), Number = .N), by = format(as.Date(hire_date), '%Y')][order(format)] 


#rollup

#cube

#grouping sets

groupingsets(emp, j = c(.(count=.N),lapply(.SD, sum)), .SDcols='salary', sets = list('department_id','job_id'), by=c('department_id','job_id'))

# join -------------------------------------------------------------------
library(data.table)
A <- data.table::data.table(a=1:4, b=(1:4)^2)
B <- data.table(a=2:5, b=(3:6)^2)
A;B

A[B, on='a', bb :=i.b]

dept <- data.table::setDT(departments)
loc <- setDT(locations)
head(dept)
head(loc)
emp
colnames(loc) <- tolower(colnames(loc))
setnames(emp,'loc_id','location_id')
emp[dept, on='department_id', location_id:=i.location_id]
emp[loc, on='location_id', city :=i.city]
emp[,.(name = paste(first_name, last_name), department_id, location_id, city)]
a# Data Manipulation Language------------------------------------------------------
#insert
head(employees)
emp<-emp[-1]
emp <- as.data.table(employees)
setDT(emp)[,.(hire_date = as.Date(hire_date))]

#update
emp[,hire_date:=as.Date(hire_date)]

#delete
emp[,department_id:=NULL] # 열 삭제
emp[department_id==50]

emp[,.(sal = mean(salary)), by=department_id]
setDT(emp)[,.]
rbindlist()


# Form Transformation ----------------------------------------------------
aq_dt = as.data.table(airquality)
aq_m <- melt(aq_dt, id.vars = c('Month','Day'))
aq_m
aq_d <- dcast(aq_m, Month + Day ~ variable, value.var = 'value')

head(airquality)
aq <- as.data.table(airquality)
aq
aq_m <- melt(aq, id.vars = c('Month','Day'))
aq_d <- dcast(aq_m, Month + Day ~ variable, variable.names='variable', value.var = 'value')



# Analyze ----------------------------------------------------------------

#rank
emp[,.(employee_id,department_id, salary, rank=as.numeric(rank(-salary)), sal_num = seq_len(nrow(emp)))][order(-salary)]

emp[,rank := frank(-salary, ties.method = "min")] # method= min,dense, first
emp[,.(rank,salary)][order(-salary)]

#sum

emp[,.(sal_rank = sum(salary)), by=department_id][order(-sal_rank)]

#lag, lead

emp[,rank_lag:=shift(rank,1,type='lag')]
emp[,rank_lead:=shift(rank,1,type='lead')]

emp[,.(rank, rank_lag, rank_lead)][order(rank)]



#listagg
emp[,.(emp_list = paste(first_name, collapse=",")), by=department_id]
emp[order(hire_date), .(mean_salary = mean(salary), emp_num=.N, employee_list = paste(first_name, collapse = ",")), by=department_id][order(-department_id)]

# subquery ---------------------------------------------------------------
emp

emp[first_name=='Nancy',.(first_name,salary)]

dbGetQuery(conn, "select first_name, salary from employees where salary >= (select salary from employees where first_name = 'Nancy')")

dbGetQuery()


# iris 연습문제--------------------------------
x <- iris_dt[,0:4]
x[,rowid := seq_len(nrow(x))]
y <- iris_dt[,5]
y[,rowid := seq_len(nrow(y))] # rowid 주기
x
y
merge_dt = merge.data.table(x,y, by="rowid")
merge_dt[,rowid:=NULL]
merge_dt
summary(iris$Sepal.Length)
temp <- iris_dt[0:50,]
temp[,lapply(.SD, summary)]
summary(temp)

iris_x <- iris[,0:4]

iris_versicolor = iris_dt[Species=='versicolor']
head(iris_versicolor)



