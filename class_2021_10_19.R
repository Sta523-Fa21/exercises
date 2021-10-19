# Exercise 1

## The total costs in payroll for this company

# SELECT SUM(salary) FROM employees;

# SELECT SUM(salary) AS total_salary FROM employees;

## The average salary within each department

# SELECT AVG(salary) AS avg_salary FROM employees GROUP BY dept;

# SELECT dept, AVG(salary) AS avg_salary FROM employees GROUP BY dept ORDER BY avg_salary;

# SELECT dept, MEAN(salary) AS avg_salary FROM employees GROUP BY dept ORDER BY avg_salary;


# Exercise 2

# SELECT dept, AVG(salary) AS avg_salary FROM employees GROUP BY dept;

# SELECT name, 
#   dept,
#   salary,
#   salary - avg_salary AS above_avg
# FROM employees 
# NATURAL LEFT JOIN 
#     (SELECT dept, AVG(salary) AS avg_salary FROM employees GROUP BY dept);





