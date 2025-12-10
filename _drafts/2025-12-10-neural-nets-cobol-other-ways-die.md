---
layout: post
slug: passing-oswe
title: Neural Networks in COBOL or [...]
tags: [cert, oswe, pentesting]
---

https://crusdg.app/ 


Yesterday I got a weird message. You know the kind. The type that pops up while you’re sipping coffee, thinking you might have a normal day for once.

It said this:

> **THE HANDLER:**
> Listen, employee. Your next assignment involves critical bio-surveillance operations. We have intercepted subversive penguins attempting to infiltrate corporate aquaculture infrastructure.
>
> Your task is to classify these traitorous birds based on flipper length and other obscene morphological features.
>
> And you will do it in COBOL.
>
> Do not disappoint me. The shareholders already do.

I stared at my screen. Penguins. Infiltrating corporate aquaculture infrastructure. And COBOL. There’s satire, there’s absurdity, and then there’s whatever this is.

So I replied:

> **Me:** Can I at least sketch the thing in Python first?

> **THE HANDLER:**
> Fine. Babysit your little scripting language. But in the end, you return to the mainframe. You always return to the mainframe.
>
> Now classify those penguins before they unionize.

Alright. Let’s do this.

---

## the plan

First I’ll outline what we’re trying to build, so you don’t feel like you’re following a fever dream.

We want a single hidden layer neural network that takes penguin measurements and predicts a class.
A simple feedforward network.

Mathematically, for an input vector ( x \in \mathbb{R}^n ):

[
h = \sigma(W_1 x + b_1)
]

[
\hat{y} = \text{softmax}(W_2 h + b_2)
]

Training will involve gradient descent:

[
W := W - \alpha \frac{\partial L}{\partial W}
]

Pretty normal stuff. Except for the COBOL part. That’s where the pain begins later.

Before that, let’s build a clean Python version so we know the formulas are right and the logic is sound.

## playin with the snake

No frameworks, just numpy. A tiny network, a couple hundred lines tops, and no black-box magic.

```python
import numpy as np

def softmax(z):
    exp = np.exp(z - np.max(z, axis=1, keepdims=True))
    return exp / np.sum(exp, axis=1, keepdims=True)

def relu(x):
    return np.maximum(0, x)

def relu_deriv(x):
    return (x > 0).astype(float)

class TinyNN:
    def __init__(self, input_dim, hidden_dim, output_dim):
        self.W1 = np.random.randn(input_dim, hidden_dim) * 0.01
        self.b1 = np.zeros((1, hidden_dim))
        self.W2 = np.random.randn(hidden_dim, output_dim) * 0.01
        self.b2 = np.zeros((1, output_dim))

    def forward(self, X):
        self.z1 = X @ self.W1 + self.b1
        self.h1 = relu(self.z1)
        self.z2 = self.h1 @ self.W2 + self.b2
        self.y_hat = softmax(self.z2)
        return self.y_hat

    def backward(self, X, y, lr=0.01):
        m = X.shape[0]
        y_hat = self.y_hat
        
        dz2 = (y_hat - y) / m
        dW2 = self.h1.T @ dz2
        db2 = np.sum(dz2, axis=0, keepdims=True)
        
        dh1 = dz2 @ self.W2.T
        dz1 = dh1 * relu_deriv(self.z1)
        
        dW1 = X.T @ dz1
        db1 = np.sum(dz1, axis=0, keepdims=True)
        
        self.W1 -= lr * dW1
        self.b1 -= lr * db1
        self.W2 -= lr * dW2
        self.b2 -= lr * db2

    def fit(self, X, y, epochs=500):
        for i in range(epochs):
            self.forward(X)
            self.backward(X, y)
            if i % 100 == 0:
                loss = -np.sum(y * np.log(self.y_hat + 1e-8)) / X.shape[0]
                print(i, loss)

# Example training call:
# nn = TinyNN(4, 8, 3)
# nn.fit(X, y_onehot)
```

This works. It’s clean. It’s readable. You can tweak the hidden layer, loss, whatever. You can reason about it without wanting to scream.

Naturally, The Handler hates this.

> **THE HANDLER:**
> Enough with your cheerful toy language. The mainframe thirsts.

Great.

## cobol attempt

You want neural networks in COBOL?
Enjoy the pain.

COBOL was built for accountants, not gradient descent. You get fixed-width fields, no arrays the way you want them, no dynamic memory, and arithmetic that feels like chiseling numbers into wet clay.

But fine. Here’s a sketch. It won’t run fast. It won’t run pretty. But it’ll gesture in the vague direction of a neural net.

This demonstrates:
• reading feature inputs
• computing a forward pass with hard-coded dimensions
• storing weights
• performing a single gradient step (in the most cursed way possible)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PENGUIN-NN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Dimensions
       01 INPUT-SIZE        PIC 9(02) VALUE 04.
       01 HIDDEN-SIZE       PIC 9(02) VALUE 05.
       01 OUTPUT-SIZE       PIC 9(02) VALUE 03.

       * Inputs
       01 X-VEC.
          05 X-FEATURE       OCCURS 4 TIMES PIC S9V9(4).

       * Weights for layer 1
       01 W1.
          05 W1-ROW OCCURS 4 TIMES.
             10 W1-COL OCCURS 5 TIMES PIC S9V9(4).

       01 B1.
          05 B1-B IAS OCCURS 5 TIMES PIC S9V9(4).

       * Hidden layer
       01 HIDDEN.
          05 HIDDEN-NODE OCCURS 5 TIMES PIC S9V9(4).

       * Weights layer 2
       01 W2.
          05 W2-ROW OCCURS 5 TIMES.
             10 W2-COL OCCURS 3 TIMES PIC S9V9(4).

       01 B2.
          05 B2-B IAS OCCURS 3 TIMES PIC S9V9(4).

       * Output
       01 OUTPUT.
          05 OUT-NODE OCCURS 3 TIMES PIC S9V9(4).

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           DISPLAY "ENTER 4 FEATURES:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
              ACCEPT X-FEATURE(I)
           END-PERFORM.

           *> Forward pass: hidden = relu(W1 * x + b1)
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
              MOVE 0 TO HIDDEN-NODE(J)
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 4
                 COMPUTE HIDDEN-NODE(J) = 
                   HIDDEN-NODE(J) + (W1-COL(I,J) * X-FEATURE(I))
              END-PERFORM
              COMPUTE HIDDEN-NODE(J) =
                   HIDDEN-NODE(J) + B1-B-J
              IF HIDDEN-NODE(J) < 0 THEN
                 MOVE 0 TO HIDDEN-NODE(J)
              END-IF
           END-PERFORM.

           *> Output layer
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 3
              MOVE 0 TO OUT-NODE(K)
              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
                 COMPUTE OUT-NODE(K) =
                   OUT-NODE(K) + (W2-COL(J,K) * HIDDEN-NODE(J))
              END-PERFORM
              COMPUTE OUT-NODE(K) = OUT-NODE(K) + B2-B-K
           END-PERFORM.

           DISPLAY "OUTPUT LOGITS:" OUT-NODE(1) " "
                                       OUT-NODE(2) " "
                                       OUT-NODE(3).

           STOP RUN.
```

This thing barely computes a forward pass. Backpropagation in COBOL is theoretically possible, but so is doing neurosurgery with a spoon. Everything becomes manual loops, manually tracked indexes, manually computed derivatives, and prayers to gods that don’t answer email.

But hey, it technically meets The Handler’s demands.
Penguins classified. Shareholders appeased. Reality degraded.

## wh00t wh00t

> **THE HANDLER:**
> Satisfactory. The penguin insurgency will be contained... For now.


