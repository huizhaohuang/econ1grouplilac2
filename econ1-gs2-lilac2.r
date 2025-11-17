market_demand <- function(x){-0.2 * x + 20}
marginal_cost <- function(x){0.5 * x + 5}
marginal_revenue <- function(x){-0.4 * x + 20}
quantity <- seq(from = 0, to = 50, by = 10)
df <- data.frame(
  quan = quantity,
  demand = market_demand(quantity),
  mc = marginal_cost(quantity),
  mr = marginal_revenue(quantity)
)

library(ggplot2)
library(ggrepel)

# Plot 1 ——————————————————————————————————————————————————————————————
ggplot(df) +
  geom_line(aes(x = quantity, y = mc, colour = "MC"),) +
  geom_line(aes(x = quantity, y = mr, colour = "MR")) +
  geom_line(aes(x = quantity, y = demand, colour = "Demand Curve")) +
  scale_colour_manual(
    name = "Curves",
    values = c(
      "Demand Curve" = "darkblue",
      "MC"           = "darkred",
      "MR"           = "darkgreen"
    )
  ) +
  geom_point(aes(x =  50/3, y = 40/3), col = "red") +
  annotate(
    "text", 
    x =  50/3, y = 40/3,
    label = "step 1: identify the quantity\nwhere MC=MR",
    size = 5,
    vjust = 3
  ) +
  geom_segment(aes(x = 50/3, xend = 50/3, y = 40/3, yend = 50/3), inherit.aes = FALSE, linetype = "dashed", linewidth = 0.5) +
  labs(
    x = "Quantity",
    y = "Price",
    title = "The monopolistic market of semi-conductor chips"
  ) +
  geom_point(aes(x =  50/3, y = 50/3), col = "red") +
  annotate(
    "text", 
    x =  50/3, y = 50/3,
    label = "step 2: look at demand curve to see\nthe price to charge for Q",
    size = 5,
    vjust = -1
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.87),
    legend.text = element_text(size = 13),
    legend.background = element_rect(fill = "white", colour = "gray30", size = 0.3),
    legend.box.background = element_rect(colour = "gray30", size = 0.8)
  )

# Plot 2 ——————————————————————————————————————————————————————————————

df_cs <- data.frame(
  x = c(50/3, 0, 0),
  y = c(50/3, 20, 50/3)
)

df_ps <- data.frame(
  x = c(0, 50/3, 50/3, 0),
  y = c(5, 40/3, 50/3, 50/3)
)

df_dwl <- data.frame(
  x = c(50/3, 50/3, 150/7),
  y = c(40/3, 50/3, 110/7)
)

ggplot(df) +
  geom_line(aes(x = quantity, y = mc, colour = "MC"),) +
  geom_line(aes(x = quantity, y = mr, colour = "MR")) +
  geom_line(aes(x = quantity, y = demand, colour = "Demand Curve")) +
  scale_colour_manual(
    name = "Curves",
    values = c(
      "Demand Curve" = "darkblue",
      "MC"           = "darkred",
      "MR"           = "darkgreen"
    )
  ) +
  geom_point(aes(x =  50/3, y = 40/3), col = "red") +
  geom_point(aes(x =  150/7, y = 110/7), col = "red") +
  geom_point(aes(x =  50/3, y = 50/3), col = "red") +
  geom_segment(
    aes(x = 50/3, xend = 50/3, y = 0, yend = 50/3), 
    inherit.aes = FALSE, 
    linetype = "dashed", linewidth = 0.5) +
  geom_segment(
    aes(x = 0, xend = 150/7, y = 110/7, yend = 110/7), 
    inherit.aes = FALSE, 
    linetype = "dashed", linewidth = 0.5) +
  geom_segment(
    aes(x = 0, xend = 50/3, y = 50/3, yend = 50/3), 
    inherit.aes = FALSE, 
    linetype = "dashed", linewidth = 0.5) +
  geom_segment(
    aes(x = 0, xend = 50/3, y = 40/3, yend = 40/3), 
    inherit.aes = FALSE, 
    linetype = "dashed", linewidth = 0.5) +
  geom_segment(
    aes(x = 150/7, xend = 150/7, y = 0, yend = 110/7), 
    inherit.aes = FALSE, 
    linetype = "dashed", linewidth = 0.5) +
  geom_polygon(data = df_cs, 
               mapping = aes(x = x, y = y), 
               fill = "#67a9cf", alpha = 0.5) +
  geom_polygon(data = df_ps, 
               mapping = aes(x = x, y = y), 
               fill = "#d6604d", alpha = 0.5) +
  geom_polygon(data = df_dwl, 
               mapping = aes(x = x, y = y), 
               fill = "gray", alpha = 0.5) +
  annotate(
    "text", 
    x =  1, y = 18,
    label = "CS",
    size = 5,
    color = "#2166ac"
  ) +
  annotate(
    "text", 
    x =  1, y = 10,
    label = "PS",
    size = 5,
    color = "#b2182b"
  ) +
  annotate(
    "text", 
    x =  54/3, y = 15,
    label = "DWL",
    size = 4,
    color = "gray30"
  ) +
  labs(
    x = "Quantity",
    y = "Price",
    title = "The monopolistic market of semi-conductor chips"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 15),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.87),
    legend.text = element_text(size = 13),
    legend.background = element_rect(fill = "white", colour = "gray30", size = 0.3),
    legend.box.background = element_rect(colour = "gray30", size = 0.8)
  )
