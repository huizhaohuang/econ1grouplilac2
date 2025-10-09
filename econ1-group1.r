library(tidyverse)

## question A

demand <- function(qd) { -(qd/5) + 11}
supply <- function(qs) {qs/10 + 2}
quantity <- seq(from =0, to = 55, by = 5)
df <- data.frame(
	quantity = quantity, 
	supply_price = supply(quantity), 
	demand_price = demand(quantity)
	)

ggplot(df, aes(x = quantity)) +
  geom_line(aes(x = quantity, y = supply_price), color = "red") +
  geom_line(aes(x = quantity, y = demand_price), color = "blue") +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
  	  "text",
  	  x = max(quantity),
  	  y = demand(max(quantity)),
  	  label = "Demand",
  	  hjust = 0.6,
  	  vjust = -2.8,
  	  color = "blue"
  	) +
    annotate(
  	  "text",
  	  x = max(quantity),
  	  y = supply(max(quantity)),
  	  label = "Supply",
  	  hjust = 0.8,
  	  vjust = -2.5,
  	  color = "red"
  	) +
  labs(
  	  title = "Demand and Supply Curve of a EV's Market",
  	  x = "quantity", 
  	  y = "price"
  	)

## question E

  new_demand <- function(qd) { -(qd/5) + 12}
  df2 <- data.frame(
	quantity = quantity, 
	supply_price = supply(quantity), 
	demand_price = demand(quantity),
	new_demand_price = new_demand(quantity)
	)

  ggplot(df2, aes(x = quantity)) +
  geom_line(aes(x = quantity, y = supply_price), color = "red") +
  geom_line(aes(x = quantity, y = demand_price), color = "blue") +
  geom_line(aes(x = quantity, y = new_demand_price), color = "dark green") +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
  	  "text",
  	  x = max(quantity),
  	  y = demand(max(quantity)),
  	  label = "Demand",
  	  hjust = 0.6,
  	  vjust = -2.8,
  	  color = "blue"
  	) +
    annotate(
  	  "text",
  	  x = max(quantity),
  	  y = supply(max(quantity)),
  	  label = "Supply",
  	  hjust = 0.8,
  	  vjust = -2.5,
  	  color = "red"
  	) +
  	 annotate(
  	  "text",
  	  x = max(quantity),
  	  y = new_demand(max(quantity)),
  	  label = "New Demand",
  	  hjust = 0.8,
  	  vjust = -4,
  	  color = "dark green"
  	) +
  labs(
  	  title = "Demand and Supply Curve of a EV's Market",
  	  x = "quantity", 
  	  y = "price"
  	)

## question F

df3 <- data.frame(
	  x = c(100/3, 100/3, 0, 0),
	  y = c(13/3, 16/3, 16/3, 13/3)
	)

ggplot(df2, aes(x = quantity)) +
  geom_line(aes(x = quantity, y = supply_price), color = "red") +
  geom_line(aes(x = quantity, y = demand_price), color = "blue") +
  geom_line(aes(x = quantity, y = new_demand_price), color = "dark green") +
  geom_polygon(data = df3, mapping = aes(x = x, y = y), fill = "green", alpha = 0.3) +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
  	  "text",
  	  x = max(quantity),
  	  y = demand(max(quantity)),
  	  label = "Demand",
  	  hjust = 0.6,
  	  vjust = -2.8,
  	  color = "blue"
  	) +
    annotate(
  	  "text",
  	  x = max(quantity),
  	  y = supply(max(quantity)),
  	  label = "Supply",
  	  hjust = 0.8,
  	  vjust = -2.5,
  	  color = "red"
  	) +
  	 annotate(
  	  "text",
  	  x = max(quantity),
  	  y = new_demand(max(quantity)),
  	  label = "New Demand",
  	  hjust = 0.8,
  	  vjust = -4,
  	  color = "dark green"
  	) +
  labs(
  	  title = "Demand and Supply Curve of a EV's Market",
  	  x = "quantity", 
  	  y = "price"
  	)

  df4 <- data.frame(
	  x = c(100/3, 100/3, 30),
	  y = c(13/3, 16/3, 5)
	)

  ggplot(df2, aes(x = quantity)) +
  geom_line(aes(x = quantity, y = supply_price), color = "red") +
  geom_line(aes(x = quantity, y = demand_price), color = "blue") +
  geom_line(aes(x = quantity, y = new_demand_price), color = "dark green") +
  geom_polygon(data = df3, mapping = aes(x = x, y = y), fill = "green", alpha = 0.3) +
  geom_polygon(data = df4, mapping = aes(x = x, y = y), fill = "purple", alpha = 0.4) +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
  	  "text",
  	  x = max(quantity),
  	  y = demand(max(quantity)),
  	  label = "Demand",
  	  hjust = 0.6,
  	  vjust = -2.8,
  	  color = "blue"
  	) +
    annotate(
  	  "text",
  	  x = max(quantity),
  	  y = supply(max(quantity)),
  	  label = "Supply",
  	  hjust = 0.8,
  	  vjust = -2.5,
  	  color = "red"
  	) +
  	 annotate(
  	  "text",
  	  x = max(quantity),
  	  y = new_demand(max(quantity)),
  	  label = "New Demand",
  	  hjust = 0.8,
  	  vjust = -4,
  	  color = "dark green"
  	) +
  labs(
  	  title = "Demand and Supply Curve of a EV's Market",
  	  x = "quantity", 
  	  y = "price"
  	)