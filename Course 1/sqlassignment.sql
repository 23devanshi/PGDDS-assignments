-- Task 1: Understanding the data in hand 

-- A. Describe the data in hand in your own words. (Word Limit is 500)
-- The data consists of the following tables:
-- 1)	Market_fact (Ord_id, Prod_id, Ship_id, Cust_id, Sales, Discount, Order_Quantity, Profit, Shipping_Cost,	Product_Base_Margin)
-- (8399 rows and 10 attributes). 
-- For all transactions, it reports the sales, discount, order quantity, profit, shipping cost and product base margin. 
-- Product base margin is the percentage of selling price that was turned into profit. 
-- A combination of Ord_id, Prod_id and Ship_id uniquely identifies rows in this table. This table contains keys linking up all tables. 
-- Each order_id is placed by a customer (identified with cust_id), but may comprise of more than one products (captured with prod_id). 
-- Each shipment may contain more than one products, and so ship_id does not uniquely identify rows in this dataset.  

-- 2)	Cust_dimen (Cust_id, Customer_Name,	Province, Region, Customer_Segment) 
-- (1832 rows and 5 attributes)- It identifies details (name, region and segment) of 1832 customers. 
-- The customers belong to 13 provinces (Nunavut, Northwest Territories, Prince Edward Island, Manitoba, British Columbia, Nova Scotia, Ontario, Newfoundland, Quebec, New Brunswick, Saskachewan, Yukon and Alberta, 
-- or 8 regions (Nunavut, Northwest Territories, Atlantic, Prarie, West, Ontario, Quebec and Yukon). 
-- The customers are classified as per their segment (Consumer, corporate, home office or small business). 
-- The rows are uniquely identified by the column cust_id. 
-- It is linked to the market_fact table by the variable cust_id

-- 3)	Orders_dimen (Ord_id,	Order_ID, Order_Date, Order_Priority) 
-- (5506 rows and 4 attributes)- 
-- It lists the order id, date the order was placed and the priority level of the order (Critical, High, Medium, Low or Not Specified). 
-- Ord_id unieqlu identifies the rows of this dataset. 
-- It is linked to the market_fact table by Ord_id and shipping_dimen by Order_ID. 

-- 4)	Prod_dimen (Prod_id, Product_Category, Product_Sub_Category) 
-- (17 rows and 3 attributes)- It lists details of 17 products. 
-- The products are classified into several categories and sub categories: Furniture (Office Furnishings, Bookcases, Tables Or Chairs & Chairmats), Office Supplies (Storage & Organization, Appliances, Binders & Binder Accessories, Paper, Rubber Bands, Envelopes, Labels, Pens & Art Supplies or Scissors, Rulers & Trimmers) Or Technology (Telephones & Communication, Computer Peripherals, Copiers & Fax or Office Machines). 
-- Prod_id uniquely identifies the rows of this dataset. 
-- It is linked to the market_fact table by prod_id. 

-- 5)	Shipping_dimen (Order_ID, Ship_Mode, Ship_Date, Ship_id) (7701 rows and 4 attributes)- 
-- It contains details of order shipments. It records the shipping mode (delivery truck, express air or regular air) and the date of shipping. 
-- Ship_id uniquely identifies all rows in this dataset. 
-- It is linked to the market_fact table by ship_id and to the orders table by order_ID. 



-- B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table don’t have Primary Key or Foreign Key, then 
-- specifically mention it in your answer.)
-- Market_fact (Primary Key- Ord_id, Prod_id, Ship_id, Foreign Key- Ord_id, Prod_id, Ship_id, Cust_id)
-- Cust_dimen (Primary Key- cust_id, Foreign Key- No foreign key)
-- Orders_dimen (Primary Key- Ord_id, Foreign Key- No foreign key)
-- Prod_dimen (Primary Key- Prod_id, Foreign Key- No foreign key)
-- Shipping_dimen (Primary Key- Ship_id, Foreign Key- No foreign key)

-- Task 2: Basic Analysis
-- Write the SQL queries for the following:
-- A. Find the total and the average sales (display total_sales and avg_sales)
-- Total and average sales can be calculated by sales using aggregation function. Only the market_fact is needed for this.
-- The results are rounded to two decimal places for readability. 
select round(sum(sales), 2) as 'total_sales', round(avg(sales), 2) as 'avg_sales'
from market_fact;

-- B. Display the number of customers in each region in decreasing order of no_of_customers. The result should contain columns Region, 
-- no_of_customers
-- cust_dimen table captures details of all customers. 
-- Cust_id is the primary key of that table. A count of the cust_id by region will summarise the distribution of customers by region.
-- The results are ordered by descending order of the number of customers 
select region, count(Cust_id) as 'no_of_customers'
from cust_dimen
group by region
order by count(Cust_id) desc;

-- C. Find the region having maximum customers (display the region name and max(no_of_customers)
-- This is achieved by limiting the results of the above query to only one row. 
select region, count(Cust_id) as 'max(no_of_customers)'
from cust_dimen
group by region
order by count(Cust_id) desc
limit 1;

-- D. Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold)
-- market_fact table contains the product id and quantity of products sold
-- the query selects product id and the no. of products sold as sum of order_quantity
-- The results are ordered by descending order of the number of products sold 
select prod_id as 'product id', sum(Order_Quantity) as 'no_of_products sold' 
from market_fact
group by prod_id
order by count(Ord_id) desc;

-- E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer 
-- name, no_of_tables purchased)
-- the information on customer name is contained in the table cust_dimen, while the order quantity is stored in market_fact
-- the select statement calls on attributes from two tables. 
-- starting with the market_fact table, we left join it with cust_dimen (to take into account all orders in the record) on the cust_id key
-- The query also needs to specify product sub-category which is present in prod_dimen table 
-- Thus another left join is created with prod_dimen table on the key prod_id 
-- The where clause filters the product subcategory (present in prod_dimen) to Tables and the region (present in cust_dimen) to Atlantic 
-- The results are then grouped by Customer name to summarise the quantities ordered
select c.Customer_Name, sum(m.Order_Quantity) as 'no_of_tables purchased'
from market_fact m 
left join cust_dimen c on c.Cust_id = m.cust_id
left join prod_dimen p on p.prod_id = m.prod_id
where p.Product_Sub_Category = 'Tables' and c.region = 'Atlantic'
group by c.Customer_Name;

-- Task 3: Advanced Analysis
-- Write sql queries for the following:
-- A. Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, 
-- profits)?
-- the product category is present in prod_dimen while profits are documented in market_fact
-- Market_fact table is left joined with the prod_dimen table on the key prod_id
-- the results are then grouped by product category (present in prod_dimen)
-- the last line of the query arranges the results in decreasing order of profit. 
select p.product_category, round(sum(m.profit),2) as 'profits'
from market_fact m
left join prod_dimen p on m.prod_id = p.prod_id
group by p.product_category
order by profits desc; 

-- B. Display the product category, product sub-category and the profit within each sub-category in three columns. 
-- the product category and subcategory is present in prod_dimen while profits are documented in market_fact
-- Market_fact table is left joined with the prod_dimen table on the key prod_id
-- the results (profits) are grouped by both product_category and product_sub_category
-- the results are arranged in decreasing order of profits
-- the profits are rounded to two places after decimal for readability
select p.product_category, p.Product_Sub_Category, round(sum(m.profit),2) as 'profits'
from market_fact m left join prod_dimen p on m.prod_id = p.prod_id
group by p.product_category, p.Product_Sub_Category
order by profits desc; 

-- C. Where is the least profitable product subcategory shipped the most? 

-- the product category and subcategory is present in prod_dimen while profits are documented in market_fact
-- Market_fact table is left joined with the prod_dimen table on the key prod_id
-- the query calls for the product sub category ordered in ascending order of profit. 
-- Only one result (and hence the least profitable category) is then displayed
-- The least profitable subcategory can be found by
select p.Product_Sub_Category
from market_fact m 	left join prod_dimen p on m.prod_id = p.prod_id
group by p.Product_Sub_Category
order by round(sum(m.profit),2) 
limit 1

-- The least profitable subcategory is Tables 

-- For the least profitable product sub-category, 
-- display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits 
--(i.e. region, no_of_shipments, profit_in_each_region) 
-- Note: You can hardcode the name of the least profitable product sub-category
-- shipping informaation is present in shipping_dimen, the product category and subcategory is present in prod_dimen, profits are documented in market_fact and region is present in cust_dimen
-- Shipping_dimen table is left joined with the market_fact m table on the key ship_id
-- this is then left joined with cust_dimen on the key cust_id
-- finally this is left joined with the prod_dimen table on the key prod_id
-- the results are grouped by region and arranged in descending order of profits per region
select c.region, count(s.ship_id) as 'no_of_shipments', round(sum(m.profit),2) as 'profit_in_each_region'
from shipping_dimen s left join market_fact m on m.ship_id = s.ship_id
left join cust_dimen c on m.cust_id = c.cust_id
left join prod_dimen p on m.prod_id = p.prod_id
where p.Product_Sub_Category = 'Tables'
group by c.region
order by profit_in_each_region desc; 
