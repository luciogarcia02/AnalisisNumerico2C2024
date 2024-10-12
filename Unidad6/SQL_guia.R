#GUIA SQL
sqldf()
ejercicio1<-sqldf("Select e.FirstName, e.LastName, c.CustomerName, o.OrderID, p.ProductName, p.Unit as Presentación, p.Price, od.Quantity, od.Quantity * p.Price AS total 
                  from orders o 
                  inner join employees e ON o.EmployeeID = e.EmployeeID
                  inner join customers c ON o.CustomerID = c.CustomerID
                  inner join orderdetails od ON o.OrderID = od.OrderID
                  inner join products p ON od.ProductID = p.ProductID
                  WHERE o.EmployeeID = 4 AND c.CustomerName = 'Frankenversand'")
sqldf()
ejercicio2<-sqldf(" SELECT 
   p.ProductName AS 'Nombre del Producto',
    p.Unit AS 'Presentación (Unidades)',
    p.Price AS 'Precio Original',
    
    CASE
    WHEN P.Price > 75 THEN '12%'
    WHEN P.Price <= 75 AND P.Price >= 50 THEN '8%'
    WHEN P.Price > 50 AND p.price<30 THEN '4%'
    ELSE '0%'
    END AS 'DESCUENTO %',
    
    CASE
    WHEN P.Price > 75 THEN p.price*0.12
    WHEN P.Price <= 75 AND P.Price >= 50 THEN p.Price  * 0.08
    WHEN P.Price > 50 AND p.price<30 THEN p.price  *0.04
    ELSE 0
    END AS 'DESCUENTO($)',
    
    CASE
    WHEN P.Price > 75 THEN p.price*0.88
    WHEN P.Price <= 75 AND P.Price >= 50 THEN p.price*0.92
    WHEN P.Price > 50 AND p.price<30 THEN p.price*0.96
    ELSE p.price
    END AS 'Precio final'
  FROM products p")

sqldf()
ejercicio3<-sqldf("SELECT c.CategoryName as 'Categoria', max(p.Price) as 'Precio_maximo', min(p.Price) as 'Precio mínimo' 
             FROM products p INNER JOIN categories c ON p.CategoryID = c.CategoryID
             GROUP BY p.CategoryID
             ORDER BY Precio_maximo DESC")


sqldf()
ejercicio4<- sqldf("SELECT cu.CustomerName, c.CategoryName as 'Categoria', o.OrderID ,p.ProductName, p.Unit as Presentación, p.Price, od.Quantity, p.Price * od.Quantity AS 'Total'
                  FROM orders o 
                  inner join orderdetails od ON o.OrderID = od.OrderID
                  inner join products p ON od.ProductID = p.ProductID
                  inner join categories c ON p.CategoryID = c.CategoryID
                  inner join customers cu ON o.CustomerID = cu.CustomerID
                  WHERE cu.Country = 'Spain'
                  ")

##Ejercicio 5, no lo voy a hacer pero lo importante es el uso del operador LIKE
## WHEN Presentacion LIKE '%caja%' THEN 7
## WHEN Presentacion LIKE '%botella%' OR Presentacion LIKE '%paquete%' THEN 5
## Se usa dentro de un CASE en este caso
## WHERE Presentacion LIKE '%caja%'; ejemplo con where


sqldf()
ejercicio6<- sqldf("SELECT cu.CustomerName, COUNT(*) as 'Cantidad',SUM(p.Price*od.Quantity) AS Total
                   FROM orders o
                   INNER JOIN orderdetails od ON od.OrderID = o.OrderID
                   INNER JOIN products p ON p.ProductID = od.ProductID
                   INNER JOIN customers cu ON o.CustomerID = cu.CustomerID
                   WHERE cu.Country = 'USA'
                   GROUP BY cu.CustomerID
                   ")

sqldf()
ejercicio7<-sqldf("SELECT e.FirstName, e.LastName, Count(DISTINCT o.OrderID), SUM(p.Price*od.Quantity) as 'Total_vendido',
                  CASE 
                  WHEN SUM(p.Price*od.Quantity) >100000 THEN 20
                  WHEN SUM(p.Price*od.Quantity) <100000 AND SUM(p.Price*od.Quantity) >40000 THEN 10
                  WHEN SUM(p.Price*od.Quantity) <39000 AND SUM(p.Price*od.Quantity) >30000 THEN 5
                  ELSE 0
                  END AS 'COMISION'
                  FROM orders o 
                  INNER JOIN orderdetails od ON od.OrderID = o.OrderID
                  INNER JOIN products p ON p.ProductID = od.ProductID
                  INNER JOIN employees e ON e.employeeID = o.employeeID
                  GROUP BY o.EmployeeID
                  ORDER BY COMISION DESC
                  ")


sqldf()
ejercicio8<-sqldf("SELECT od.OrderID, s.SupplierName, s.City, s.Country, s.Phone, p.ProductName, c.CategoryName, od.Quantity, p.Price, p.Price*od.Quantity AS Total
                  FROM orders o
                  inner JOIN orderdetails od ON o.OrderID = od.OrderID
                  INNER JOIN products p ON p.ProductID = od.ProductID
                  INNER JOIN suppliers s ON p.SupplierID = s.SupplierID
                  inner join categories c ON p.CategoryID = c.CategoryID
                  WHERE s.Country = 'Japan'
                  ")




















