SELECT 
s1. itemName,
s1.ingID,
s1.ingName,
s1.ingWeight,
s1.ingPrice,
s1.orderQuantity,
s1.recipeQuantity,
--calculated items
s1.orderQuantity*s1.recipeQuantity AS orderedWeight,
s1.ingPrice/s1.ingWeight AS unitCost,
(s1.orderQuantity+s1.recipeQuantity)*(s1.ingPrice/s1.ingWeight) AS ingredientCost
 
FROM (SELECT
	o.productID,
	p.sku,
	p.itemName,
	r.ingID,
	r.quantity AS recipeQuantity
	sum(o.quantity) AS orderQuantity

FROM 
	orders o
	LEFT JOIN product p ON o.productID = p.productID
	LEFT JOIN recipe r ON p.sku = r.recipeID
GROUP BY
	o.itemID,
	p.sku,
	p.itemName,
	r.ingID,
	r.quantity
	ingredient.ingName,
	ingredient.ingWeight,
	ingredient.ingPrice) s1


/*total quantity by ingredients
total cost of ingredients
cost of pizza
percentage of stock remaining of each ingredient*/


