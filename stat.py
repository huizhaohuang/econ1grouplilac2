import numpy as np
import matplotlib.pyplot as plt

# Parameters
w1 = 10
w2 = 12

# Optimal points
L1, C1 = 6, w1 * (12 - 6)
L2, C2 = 6, w2 * (12 - 6)

# Utility levels
U1 = C1 * L1
U2 = C2 * L2

# Leisure range (avoid L=0 to prevent asymptote)
L_vals = np.linspace(0.5, 12, 400)

# Budget lines
C_w1 = w1 * (12 - L_vals)
C_w2 = w2 * (12 - L_vals)

# Indifference curves
IC1 = U1 / L_vals
IC2 = U2 / L_vals

# --------------------------
#   GRAPH FOR w = 10
# --------------------------

plt.figure(figsize=(7,5))
plt.plot(L_vals, C_w1, color="black", linewidth=2, label="Budget line (w=10)")
plt.plot(L_vals, IC1, color="blue", linestyle="--", linewidth=1.8, label="Indifference curve")

# Shade area under budget constraint
plt.fill_between(L_vals, C_w1, alpha=0.3, color="gold")

plt.scatter([L1], [C1], color="red", s=60)
plt.text(L1+0.3, C1, "(6, 60)", fontsize=10)

plt.ylim(0, 200)     # << clean y-axis
plt.xlim(0, 12)

plt.xlabel("Leisure (L)")
plt.ylabel("Consumption (C)")
plt.title("Optimal Choice at Wage w = 10")
plt.grid(alpha=0.3)
plt.legend()
plt.show()

# --------------------------
#   GRAPH FOR w = 12
# --------------------------

plt.figure(figsize=(7,5))
plt.plot(L_vals, C_w2, color="black", linewidth=2, label="Budget line (w=12)")
plt.plot(L_vals, IC2, color="blue", linestyle="--", linewidth=1.8, label="Indifference curve")

# Shade area under the budget constraint
plt.fill_between(L_vals, C_w2, alpha=0.3, color="gold")

plt.scatter([L2], [C2], color="red", s=60)
plt.text(L2+0.3, C2, "(6, 72)", fontsize=10)

plt.ylim(0, 200)      # << clean y-axis
plt.xlim(0, 12)

plt.xlabel("Leisure (L)")
plt.ylabel("Consumption (C)")
plt.title("Optimal Choice at Wage w = 12")
plt.grid(alpha=0.3)
plt.legend()
plt.show()
