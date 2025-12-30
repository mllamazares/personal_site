import numpy as np
import seaborn as sns
from sklearn.preprocessing import StandardScaler

# data
df = sns.load_dataset("penguins").dropna()
X = df[["bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"]].values
y = df["species"].astype("category").cat.codes.values

# Preprocessing verification
means = np.mean(X, axis=0)
stds = np.std(X, axis=0)

print("--- Preprocessing ---")
print(f"Means: {means}")
print(f"Stds:  {stds}")

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

print("\nFirst 5 rows (Scaled):")
print(X_scaled[:5])

# Forward Prop Verification (using fixed weights for comparison if needed, 
# but let's just use the seed from the script)
np.random.seed(0)
D = X.shape[1]
H = 16
C = len(np.unique(y))

W1 = np.random.randn(D, H) * 0.01
b1 = np.zeros(H)
W2 = np.random.randn(H, C) * 0.01
b2 = np.zeros(C)

# Let's take row 0 of the entire dataset (scaled)
x0 = X_scaled[0:1]
z1 = x0 @ W1 + b1
h = np.maximum(0, z1)
z2 = h @ W2 + b2
exp = np.exp(z2 - np.max(z2, axis=1, keepdims=True))
probs = exp / np.sum(exp, axis=1, keepdims=True)

# Backward Prop Verification (Row 0)
y0_oh = np.zeros((1, C))
y0_oh[0, y[0]] = 1
dz2 = probs - y0_oh
dW2 = h.T @ dz2
db2 = dz2.mean(axis=0)

dh = dz2 @ W2.T
dz1 = dh * (z1 > 0).astype(float)
dW1 = x0.T @ dz1
db1 = dz1.mean(axis=0)

print("\n--- Backward Prop (Row 0) ---")
print(f"y[0]: {y[0]}")
print(f"dz2: {dz2}")
print(f"dW2[0:5,0]:\n{dW2[0:5, 0]}")
print(f"dz1[0,0:5]: {dz1[0, 0:5]}")
print(f"dW1[0,0:5]: {dW1[0, 0:5]}")
print(f"db1[0:5]:   {db1[0:5]}")
