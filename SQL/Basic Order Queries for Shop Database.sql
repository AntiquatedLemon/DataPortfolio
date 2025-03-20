/* from this, get total (orders, sales, items);
average order value
top sellers
sales by hour 
(could also investigate order if noticing that theres a skew between dollar amount and how much order)
by address or by delivery/pickup
*/
SELECT 
	o.orderID,
	p.itemPrice,
	o.quantity,
	p.itemCat,
	p.itemName,
	o.createdAt,
	a.deliveryAddress1,
	a.deliveryAddress2,
	a.deliveryCity,
	a.deliveryState,
	a.deliveryZipcode,
	o.delivery
FROM 
	orders o
	LEFT JOIN product p on o.productID = p.productID
	LEFT JOIN address a ON o.addressID = a.addressID

