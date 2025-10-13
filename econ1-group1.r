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
    color = "blue",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = supply(max(quantity)),
    label = "Supply",
    hjust = 0.8,
    vjust = -2.5,
    color = "red",
    size = 2.5
  ) +
  labs(
    title = "Demand and Supply in the EV Market",
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
    color = "blue",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = supply(max(quantity)),
    label = "Supply",
    hjust = 0.8,
    vjust = -2.5,
    color = "red",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = new_demand(max(quantity)),
    label = "New Demand",
    hjust = 0.8,
    vjust = -4,
    color = "dark green",
    size = 2.5
  ) +
  labs(
    title = "Demand and Supply(before and after subsidies) in the EV Market",
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
  geom_polygon(data = df3, mapping = aes(x = x, y = y), fill = "green", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
    "text",
    x = max(quantity),
    y = demand(max(quantity)),
    label = "Demand",
    hjust = 0.6,
    vjust = -2.8,
    color = "blue",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = supply(max(quantity)),
    label = "Supply",
    hjust = 0.8,
    vjust = -2.5,
    color = "red",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = new_demand(max(quantity)),
    label = "New Demand",
    hjust = 0.8,
    vjust = -4,
    color = "dark green",
    size = 2.5
  ) +
  labs(
    title = "Demand and Supply(before and after subsidies) in the EV Market",
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
  geom_polygon(data = df3, mapping = aes(x = x, y = y), fill = "green", alpha = 0.5) +
  geom_polygon(data = df4, mapping = aes(x = x, y = y), fill = "purple", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  annotate(
    "text",
    x = max(quantity),
    y = demand(max(quantity)),
    label = "Demand",
    hjust = 0.6,
    vjust = -2.8,
    color = "blue",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = supply(max(quantity)),
    label = "Supply",
    hjust = 0.8,
    vjust = -2.5,
    color = "red",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = new_demand(max(quantity)),
    label = "New Demand",
    hjust = 0.8,
    vjust = -4,
    color = "dark green",
    size = 2.5
  ) +
  labs(
    title = "Demand and Supply(before and after subsidies) in the EV Market",
    x = "quantity", 
    y = "price"
  )





## question G
demand <- function(qd) { -(qd/5) + 11}
supply <- function(qs) {qs/10 + 2}
quantity <- seq(from =0, to = 55, by = 5)
marginal <- function(qd) { -(qd/5) + 11 + 2.5}
df5 <- data.frame(
  quantity = quantity, 
  supply_price = supply(quantity), 
  demand_price = demand(quantity),
  marginal_social_benefit = marginal(quantity)
)

ggplot(df5, aes(x = quantity)) +
  geom_line(aes(x = quantity, y = supply_price), color = "red") +
  geom_line(aes(x = quantity, y = demand_price), color = "blue") +
  geom_line(aes(x = quantity, y = marginal_social_benefit), color = "dark green") +
  scale_x_continuous(breaks = seq(0, 55, 5)) +
  scale_y_continuous(breaks = seq(0, 14, 1)) +
  annotate(
    "text",
    x = max(quantity),
    y = demand(max(quantity)),
    label = "Demand",
    hjust = 0.6,
    vjust = -2.8,
    color = "blue",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = supply(max(quantity)),
    label = "Supply",
    hjust = 0.8,
    vjust = -2.5,
    color = "red",
    size = 2.5
  ) +
  annotate(
    "text",
    x = max(quantity),
    y = marginal(max(quantity)),
    label = "Margical Social Benefit",
    hjust = 0.8,
    vjust = -5,
    color = "dark green",
    size = 2.5
  ) +
  labs(
    title = "Demand, Supply and Marginal Social Benefit in the EV's Market",
    x = "quantity", 
    y = "price"
  )