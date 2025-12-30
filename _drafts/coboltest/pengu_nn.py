import numpy as np
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

# data
df = sns.load_dataset("penguins").dropna()
X = df[["bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"]].values
y = df["species"].astype("category").cat.codes.values

scaler = StandardScaler()
X = scaler.fit_transform(X)

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42
)

# helpers
def relu(z):
    return np.maximum(0, z)

def relu_grad(z):
    return (z > 0).astype(float)

def softmax(z):
    exp = np.exp(z - np.max(z, axis=1, keepdims=True))
    return exp / np.sum(exp, axis=1, keepdims=True)

def one_hot(y, k):
    out = np.zeros((len(y), k))
    out[np.arange(len(y)), y] = 1
    return out

def cross_entropy(probs, y):
    return -np.mean(np.log(probs[np.arange(len(y)), y]))

# init
np.random.seed(0)
D = X.shape[1]
H = 16
C = len(np.unique(y))

W1 = np.random.randn(D, H) * 0.01
b1 = np.zeros(H)
W2 = np.random.randn(H, C) * 0.01
b2 = np.zeros(C)

lr = 0.1

# training
for epoch in range(150):
    # forward
    z1 = X_train @ W1 + b1
    h = relu(z1)
    z2 = h @ W2 + b2
    probs = softmax(z2)

    loss = cross_entropy(probs, y_train)

    # backward
    y_oh = one_hot(y_train, C)
    dz2 = probs - y_oh
    dW2 = h.T @ dz2 / len(X_train)
    db2 = dz2.mean(axis=0)

    dh = dz2 @ W2.T
    dz1 = dh * relu_grad(z1)
    dW1 = X_train.T @ dz1 / len(X_train)
    db1 = dz1.mean(axis=0)

    W1 -= lr * dW1
    b1 -= lr * db1
    W2 -= lr * dW2
    b2 -= lr * db2

    if epoch % 50 == 0:
        print(f"epoch {epoch}, loss {loss:.4f}")

# evaluation
z1 = X_test @ W1 + b1
h = relu(z1)
z2 = h @ W2 + b2
preds = np.argmax(softmax(z2), axis=1)

accuracy = (preds == y_test).mean()
print("test accuracy:", accuracy)