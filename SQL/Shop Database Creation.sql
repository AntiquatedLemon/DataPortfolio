/* Before using this, have a connected server running
Create Database:
- Right Click Databases, choose "New Database..."
- I left the settings alone other than provided a name
- If naming the database something else, dont forget to change the create table statement
[DB name].dbo.[table name]
*/
CREATE TABLE Commerce.dbo.[customer] (
    [customerID] int  NOT NULL PRIMARY KEY CLUSTERED,
    [customerFN] varchar(50)  NOT NULL ,
    [customerLN] varchar(50)  NOT NULL
);
CREATE TABLE Commerce.dbo.[address] (
    [addressID] int  NOT NULL PRIMARY KEY CLUSTERED,
    [deliveryAddress1] varchar(200)  NOT NULL ,
    [deliveryAddress2] varchar(200)  NULL ,
    [deliveryCity] varchar(50)  NOT NULL ,
    [deliveryState] varchar(2)  NOT NULL ,
    [deliveryZipcode] varchar(20)  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[product] (
    [productID] varchar(10)  NOT NULL PRIMARY KEY CLUSTERED,
    [sku] varchar(20) UNIQUE NOT NULL,
    [itemName] varchar(50)  NOT NULL ,
    [itemCat] varchar(50)  NOT NULL ,
    [itemSize] varchar(20)  NOT NULL ,
    [itemPrice] decimal(5,2)  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[shift] (
    [shiftID] varchar(20)  NOT NULL PRIMARY KEY CLUSTERED,
    [weekDay] varchar(10)  NOT NULL ,
    [startTime] time  NOT NULL ,
    [endTime] time  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[staff] (
    [staffID] varchar(20)  NOT NULL PRIMARY KEY CLUSTERED,
    [staffFN] varchar(50)  NOT NULL ,
    [staffLN] varchar(50)  NOT NULL ,
    [postion] varchar(100)  NOT NULL ,
    [hourlyRate] decimal(5,2)  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[rota] (
    [rowID] int  NOT NULL PRIMARY KEY CLUSTERED,
    [rotaID] varchar(20)  NOT NULL,
    [date] datetime UNIQUE NOT NULL,
    [shiftID] varchar(20)  NOT NULL FOREIGN KEY REFERENCES shift(shiftID),
    [staffID] varchar(20)  NOT NULL FOREIGN KEY REFERENCES staff(staffID),
);
CREATE TABLE Commerce.dbo.[order] (
    [rowID] int NOT NULL PRIMARY KEY CLUSTERED,
    [orderID] varchar(10) NOT NULL,
    [createdAt] datetime NOT NULL FOREIGN KEY REFERENCES rota(date), --needs to be unique to be used as FK
    [productID] varchar(10) NOT NULL FOREIGN KEY REFERENCES product(productID),
    [quantity] int NOT NULL,
    [customerID] int NOT NULL FOREIGN KEY REFERENCES customer(customerID),
    [delivery] bit NOT NULL,
    [addressID] int NOT NULL FOREIGN KEY REFERENCES address(addressID) 
);
CREATE TABLE Commerce.dbo.[ingredient] (
    [ingID] varchar(10)  NOT NULL PRIMARY KEY CLUSTERED,
    [ingName] varchar(200)  NOT NULL ,
    [ingWeight] int  NOT NULL ,
    [ingMeasure] varchar(20)  NOT NULL ,
    [ingPrice] decimal(5,2)  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[recipe] (
    [rowID] int  NOT NULL PRIMARY KEY CLUSTERED,
    [recipeID] varchar(20) NOT NULL FOREIGN KEY REFERENCES product(sku),
    [ingID] varchar(10) UNIQUE NOT NULL FOREIGN KEY REFERENCES ingredient(ingID),
    [quantity] int  NOT NULL ,
);
CREATE TABLE Commerce.dbo.[inventory] (
    [invID] int  NOT NULL PRIMARY KEY CLUSTERED,
    [productID] varchar(10) NOT NULL FOREIGN KEY REFERENCES recipe(ingID),
    [quantity] int  NOT NULL ,
);