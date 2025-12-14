---
layout: post
slug: neural-networks-cobol
title: neural networks in COBOL and other creative ways to k*ll yourself
tags: [cobol, humor, mainframe, neuralnetwork]
---

<style>
@import url('https://fonts.googleapis.com/css2?family=Barriecito&display=swap');

html {
    cursor: url('/assets/img/cruelty-cursor.webp'), auto;
}

a:hover {
    cursor: url('/assets/img/cruelty-hand1.webp'), auto;
    animation: cursor 250ms linear infinite;
}

.handler-chat {
    display: flex;
    align-items: flex-start;
    margin: 1.5rem 0;
    gap: 1rem;
    background: black;
    padding: 1rem;
    border: 3px solid #00ff00;
    color: #00ff00;
    font-family: 'Barriecito', monospace;
    font-size: 1.2em;
    box-shadow: 0 8px 6px -6px black;
}

.handler-avatar {
    width: 100px;
    height: 100px;
    background-size: cover;
    background-position: center;
    background-image: url("/assets/img/handler1.webp");
    flex-shrink: 0;
    border: 2px solid #00ff00;
    animation: handler 1000ms linear infinite;
}

@keyframes cursor {
    0% {
        cursor: url("/assets/img/cruelty-hand1.webp"), pointer;
    }
    50% {
        cursor: url("/assets/img/cruelty-hand2.webp"), pointer;
    }
    100% {
        cursor: url("/assets/img/cruelty-hand3.webp"), pointer;
    }
}


@keyframes handler {
    0% {
        background-image: url("/assets/img/handler1.webp");
    }
    25% {
        background-image: url("/assets/img/handler2.webp");
    }
    75% {
        background-image: url("/assets/img/handler3.webp");
    }
    100% {
        background-image: url("/assets/img/handler4.webp");
    }
}

.handler-message {
    flex-grow: 1;
}

.handler-message p {
    margin: 0 0 0.8rem 0;
}

.handler-message p:last-child {
    margin: 0;
}

.handler-name {
    font-weight: bold;
    margin-bottom: 0.2rem;
    display: block;
}

@media (max-width: 768px) {
    .handler-chat {
        flex-direction: column;
    }

    .handler-avatar {
       margin: 0;
    }
    
    .handler-avatar {
        margin-bottom: 1rem;
    }
}
</style>

_**Disclaimer**: this is a tribute to the [Cruelty Squad](https://www.youtube.com/watch?v=CHm2d3wf8EU) video game. Just to showcase how COBOL can still be used in bizarre ways to maximize shareholder value._

---

Yesterday I got a *weird* message from [The Handler](https://crueltysquad.fandom.com/wiki/The_Handler). You know the kind. The type that pops up while you're sipping coffee, thinking you might have a normal day for once.

It said this:

<div class="handler-chat">
  <div class="handler-avatar" aria-label="the handler"></div>
  <div class="handler-message">
    <p>Listen, employee. Your next assignment involves critical bio-surveillance operations: we have intercepted subversive penguins attempting to infiltrate corporate aquaculture infrastructure.</p>
    <p>Your task is to classify these traitorous birds based on flipper length and other obscene morphological features. </p>
    <p>BTW, you need to do it in COBOL (hope's is not a problem).</p>
    <p>Do not disappoint me. The shareholders already do.</p>
  </div>
</div>

Naturally, I asked if I could at least sketch the thing in Python first. I got this reply:

<div class="handler-chat">
  <div class="handler-avatar" aria-label="the handler"></div>
  <div class="handler-message">
    <p>Fine. Babysit your little scripting language. But in the end, you return to the mainframe.</p> 
    <p><i>You always return to the mainframe.</i></p>
    <p>Now classify those penguins before they unionize.</p>
  </div>
</div>

Alright. Let's do this.



## neural what?

A neural network is a function $$ f(x) $$ that takes an input vector $$ x $$ and returns an output vector $$ y $$.

In this context, we want a single hidden layer neural network that takes penguin measurements and predicts a class.
A simple feedforward network.

Before we dive in, I want to recommend the [A Programmer's Introduction to Mathematics](https://pimbook.org/) book. It's a great resource for anyone who wants to understand the math and speak symbols like the grown ups if you have a coding background.

Mathematically, for an input vector $$ ( x \in \mathbb{R}^n ) $$:

$$ h = \sigma(W_1 x + b_1) $$

$$ \hat{y} = \text{softmax}(W_2 h + b_2) $$

In python we would do:

```python
def softmax(z):
    exp = np.exp(z - np.max(z, axis=1, keepdims=True))
    return exp / np.sum(exp, axis=1, keepdims=True)
```

Training will involve gradient descent:


$$ W := W - \alpha \frac{\partial L}{\partial W} $$

Pretty normal stuff. Except for the COBOL part. That's where the pain begins later.

Before that, let's build a clean Python version so we know the formulas are right and the logic is sound.


## housekeeping

First I'll outline what we're trying to build, so you don't feel like you're following a fever dream.

First let's inspect the penguin dataset:

```python
import pandas as pd
import numpy as np

penguins = pd.read_csv("penguins.csv")
penguins.head()
```

| species | island | bill_length_mm | bill_depth_mm | flipper_length_mm | body_mass_g | sex |
|---------|--------|----------------|---------------|-------------------|-------------|-----|
| Adelie  | Torgersen | 39.1 | 18.7 | 181 | 3750 | male |
| Adelie  | Torgersen | 39.5 | 17.4 | 186 | 3800 | female |
| Adelie  | Torgersen | 40.3 | 18.0 | 195 | 3250 | female |
| Adelie  | Torgersen | 36.7 | 19.3 | 193 | 3450 | male |
| Adelie  | Torgersen | 39.3 | 20.6 | 190 | 3650 | male |


Very cool. That means that the features are:

$$ x = [bill\_length\_mm, bill\_depth\_mm, flipper\_length\_mm, body\_mass\_g] $$   

OK, so these features will be our vectors.

And the target is $$ y = [species] $$


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

This works. It's clean. It's readable. You can tweak the hidden layer, loss, whatever. You can reason about it without wanting to scream.

Naturally, the handler hates this.

<div class="handler-chat">
  <div class="handler-avatar" aria-label="the handler"></div>
  <div class="handler-message">
    <p>Enough with your cheerful toy language. The mainframe thirsts.</p>
  </div>
</div>

Great.

## cobol attempt

You want neural networks in COBOL?
Enjoy the pain.

COBOL was built for accountants, not gradient descent. You get fixed-width fields, no arrays the way you want them, no dynamic memory, and arithmetic that feels like chiseling numbers into wet clay.

But fine. Here's a sketch. It won't run fast. It won't run pretty. But it'll gesture in the vague direction of a neural net.

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

This thing barely computes a forward pass. Backpropagation in COBOL is theoretically possible, but so is doing neurosurgery with a spoon. Everything becomes manual loops, manually tracked indexes, manually computed derivatives, and prayers to gods that don't answer email.

But hey, it technically meets the handler's demands.
Penguins classified. Shareholders appeased. Reality degraded.

## wh00t wh00t

<div class="handler-chat">
  <div class="handler-avatar" aria-label="the handler"></div>
  <div class="handler-message">
    <p>Satisfactory. The penguin insurgency will be contained... For now.</p>
  </div>
</div>


