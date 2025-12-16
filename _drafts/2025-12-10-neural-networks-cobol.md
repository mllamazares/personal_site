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

.cursor {
    display: inline;
    color: #00ff00;
    animation: blink 1s step-end infinite;
    margin-left: 2px;
}

@keyframes blink {
    0%, 100% { opacity: 1; }
    50% { opacity: 0; }
}
</style>

_**Disclaimer**: this is a tribute to the [Cruelty Squad](https://www.youtube.com/watch?v=CHm2d3wf8EU) video game. Just to showcase how COBOL can still be used in bizarre ways to maximize shareholder value._ ✨

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

A neural network is a function
$$
f(x) = y
$$
that takes an input vector $$x$$ and returns an output vector $$y$$.

In this context, we want a single hidden layer neural network that takes penguin measurements and predicts a class.
A simple feedforward network.

Before we dive in, I want to recommend the [A Programmer's Introduction to Mathematics](https://pimbook.org/) book. It's a great resource for anyone who wants to understand the math and speak symbols like the grown ups if you have a coding background.

## data as numbers, not vibes

Unfortunately, a neural network does not understand penguins. It understands vectors.

There is one thing we want you know which is the pengin species. We will call this our label. There are three specicies, and we will work with numbers, we will translate that to:
$$
y \in {0, 1, 2}
$$

To predict that label, we need features. Then, each penguin becomes a vector like:

$$
x = [\text{bill length}, \text{bill depth}, \text{flipper length}, \text{body mass}]
$$

Additionally we will normalize that vectors using the mean.

Why it matters? Gradient descent assumes roughly similar scales. If you skip this, training becomes unstable for stupid reasons.

<div class="handler-chat">
  <div class="handler-avatar" aria-label="the handler"></div>
  <div class="handler-message">
    <p>Gradient descent? That sounds like the title of grindcore metal band...</p>
  </div>
</div>

No worries, we will get into that later. Bear with me for once.

## weights, biases, and linear lies

Each layer does a linear transformation:

$$
z = Wx + b
$$

Where:

* $$W$$ is a weight matrix
* $$b$$ is a bias vector

This is just linear algebra. No intelligence yet.

```python
def linear(x, W, b):
    return x @ W + b
```

If we stopped here, the whole network would collapse into a single matrix multiply. Useless.

## nonlinearity or nothing works

We need a nonlinear function between layers.

ReLU is the default for a reason:

$$
\text{ReLU}(z) = \max(0, z)
$$

```python
def relu(z):
    return np.maximum(0, z)

def relu_grad(z):
    return (z > 0).astype(float)
```

Without this, your network is just fancy linear regression pretending to be deep.

## the network architecture

One hidden layer:

$$
h = \text{ReLU}(xW_1 + b_1)
$$

Output layer:

$$
o = hW_2 + b_2
$$

We turn outputs into probabilities using softmax:

$$
\text{softmax}(o_i) = \frac{e^{o_i}}{\sum_j e^{o_j}}
$$

```python
def softmax(z):
    exp = np.exp(z - np.max(z, axis=1, keepdims=True))
    return exp / np.sum(exp, axis=1, keepdims=True)
```

## loss function: how wrong are you?

For classification, use cross entropy:

$$
L = -\log(p_{\text{true class}})
$$

Averaged over samples.

```python
def cross_entropy(probs, y):
    n = len(y)
    return -np.mean(np.log(probs[np.arange(n), y]))
```

This loss punishes confident wrong answers brutally. Good.

## backpropagation, not magic

We compute gradients using the chain rule.

For softmax + cross entropy, the gradient simplifies:

$$
\frac{\partial L}{\partial o} = \hat{y} - y
$$

Where $$y$$ is one hot encoded.

Python helpers:

```python
def one_hot(y, num_classes):
    out = np.zeros((len(y), num_classes))
    out[np.arange(len(y)), y] = 1
    return out
```

## training loop

Gradient descent update rule:

$$
W := W - \eta \frac{\partial L}{\partial W}
$$

Python training step:

```python
def train_step(X, y, params, lr):
    W1, b1, W2, b2 = params

    # forward
    z1 = X @ W1 + b1
    h = relu(z1)
    z2 = h @ W2 + b2
    probs = softmax(z2)

    # loss
    y_oh = one_hot(y, probs.shape[1])
    loss = cross_entropy(probs, y)

    # backward
    dz2 = probs - y_oh
    dW2 = h.T @ dz2 / len(X)
    db2 = dz2.mean(axis=0)

    dh = dz2 @ W2.T
    dz1 = dh * relu_grad(z1)
    dW1 = X.T @ dz1 / len(X)
    db1 = dz1.mean(axis=0)

    # update
    W1 -= lr * dW1
    b1 -= lr * db1
    W2 -= lr * dW2
    b2 -= lr * db2

    return loss, (W1, b1, W2, b2)
```

## full working code

This is everything. No frameworks. No abstractions. Just math and loops.

```python
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
for epoch in range(500):
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
```

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
- reading feature inputs
- computing a forward pass with hard-coded dimensions
- storing weights
- performing a single gradient step (in the most cursed way possible)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEURAL-NETWORK.
       AUTHOR. CLAUDE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PENGUIN-FILE ASSIGN TO "PENGUINS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  PENGUIN-FILE.
       01  PENGUIN-RECORD.
           05  P-BILL-LENGTH       PIC 9(3)V9(2).
           05  P-BILL-DEPTH        PIC 9(3)V9(2).
           05  P-FLIPPER-LENGTH    PIC 9(3)V9(2).
           05  P-BODY-MASS         PIC 9(4)V9(2).
           05  P-SPECIES           PIC 9.
       
       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  MAX-SAMPLES         PIC 9(3) VALUE 333.
           05  TRAIN-SAMPLES       PIC 9(3) VALUE 266.
           05  TEST-SAMPLES        PIC 9(2) VALUE 67.
           05  NUM-FEATURES        PIC 9 VALUE 4.
           05  HIDDEN-SIZE         PIC 99 VALUE 16.
           05  NUM-CLASSES         PIC 9 VALUE 3.
           05  LEARNING-RATE       COMP-2 VALUE 0.1.
           05  NUM-EPOCHS          PIC 9(3) VALUE 500.
       
       01  WS-COUNTERS.
           05  I                   PIC 9(3).
           05  J                   PIC 9(3).
           05  K                   PIC 9(3).
           05  EPOCH               PIC 9(3).
           05  SAMPLE-COUNT        PIC 9(3) VALUE 0.
       
       01  WS-INDEXES.
           05  IDX1                PIC 9(3).
           05  IDX2                PIC 9(3).
           05  IDX3                PIC 9(3).
       
       01  WS-TEMP-VARS.
           05  TEMP-FLOAT          COMP-2.
           05  TEMP-SUM            COMP-2.
           05  TEMP-MAX            COMP-2.
           05  TEMP-EXP            COMP-2.
           05  TEMP-LOSS           COMP-2.
           05  TEMP-ACC            COMP-2.
           05  CORRECT-COUNT       PIC 9(3) VALUE 0.
       
       01  WS-RANDOM-SEED          PIC 9(9) VALUE 123456789.
       
       01  WS-DATA-STORAGE.
           05  X-DATA OCCURS 333 TIMES.
               10  X-FEATURES OCCURS 4 TIMES COMP-2.
           05  Y-DATA OCCURS 333 TIMES PIC 9.
           05  TRAIN-INDICES OCCURS 266 TIMES PIC 9(3).
           05  TEST-INDICES OCCURS 67 TIMES PIC 9(3).
       
       01  WS-SCALING.
           05  FEATURE-MEAN OCCURS 4 TIMES COMP-2.
           05  FEATURE-STD OCCURS 4 TIMES COMP-2.
       
       01  WS-NETWORK-WEIGHTS.
           05  W1 OCCURS 4 TIMES.
               10  W1-VALS OCCURS 16 TIMES COMP-2.
           05  B1 OCCURS 16 TIMES COMP-2.
           05  W2 OCCURS 16 TIMES.
               10  W2-VALS OCCURS 3 TIMES COMP-2.
           05  B2 OCCURS 3 TIMES COMP-2.
       
       01  WS-GRADIENTS.
           05  DW1 OCCURS 4 TIMES.
               10  DW1-VALS OCCURS 16 TIMES COMP-2.
           05  DB1 OCCURS 16 TIMES COMP-2.
           05  DW2 OCCURS 16 TIMES.
               10  DW2-VALS OCCURS 3 TIMES COMP-2.
           05  DB2 OCCURS 3 TIMES COMP-2.
       
       01  WS-FORWARD-PASS.
           05  Z1 OCCURS 266 TIMES.
               10  Z1-VALS OCCURS 16 TIMES COMP-2.
           05  H OCCURS 266 TIMES.
               10  H-VALS OCCURS 16 TIMES COMP-2.
           05  Z2 OCCURS 266 TIMES.
               10  Z2-VALS OCCURS 3 TIMES COMP-2.
           05  PROBS OCCURS 266 TIMES.
               10  PROB-VALS OCCURS 3 TIMES COMP-2.
       
       01  WS-BACKWARD-PASS.
           05  DZ2 OCCURS 266 TIMES.
               10  DZ2-VALS OCCURS 3 TIMES COMP-2.
           05  DH OCCURS 266 TIMES.
               10  DH-VALS OCCURS 16 TIMES COMP-2.
           05  DZ1 OCCURS 266 TIMES.
               10  DZ1-VALS OCCURS 16 TIMES COMP-2.
       
       01  WS-TEST-FORWARD.
           05  Z1-TEST OCCURS 67 TIMES.
               10  Z1-TEST-VALS OCCURS 16 TIMES COMP-2.
           05  H-TEST OCCURS 67 TIMES.
               10  H-TEST-VALS OCCURS 16 TIMES COMP-2.
           05  Z2-TEST OCCURS 67 TIMES.
               10  Z2-TEST-VALS OCCURS 3 TIMES COMP-2.
           05  PROBS-TEST OCCURS 67 TIMES.
               10  PROB-TEST-VALS OCCURS 3 TIMES COMP-2.
           05  PREDICTIONS OCCURS 67 TIMES PIC 9.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "NEURAL NETWORK FOR PENGUIN CLASSIFICATION"
           DISPLAY "==========================================="
           
           PERFORM LOAD-DATA
           PERFORM COMPUTE-SCALING
           PERFORM STANDARDIZE-DATA
           PERFORM TRAIN-TEST-SPLIT
           PERFORM INITIALIZE-WEIGHTS
           PERFORM TRAINING-LOOP
           PERFORM EVALUATE-MODEL
           
           STOP RUN.
       
       LOAD-DATA.
           DISPLAY "Loading data..."
           OPEN INPUT PENGUIN-FILE
           MOVE 0 TO SAMPLE-COUNT
           PERFORM UNTIL SAMPLE-COUNT >= MAX-SAMPLES
               READ PENGUIN-FILE
                   AT END EXIT PERFORM
               END-READ
               ADD 1 TO SAMPLE-COUNT
               MOVE P-BILL-LENGTH TO 
                   X-FEATURES(SAMPLE-COUNT, 1)
               MOVE P-BILL-DEPTH TO 
                   X-FEATURES(SAMPLE-COUNT, 2)
               MOVE P-FLIPPER-LENGTH TO 
                   X-FEATURES(SAMPLE-COUNT, 3)
               MOVE P-BODY-MASS TO 
                   X-FEATURES(SAMPLE-COUNT, 4)
               MOVE P-SPECIES TO Y-DATA(SAMPLE-COUNT)
           END-PERFORM
           CLOSE PENGUIN-FILE
           DISPLAY "Loaded " SAMPLE-COUNT " samples".
       
       COMPUTE-SCALING.
           DISPLAY "Computing feature scaling..."
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-FEATURES
               MOVE 0 TO TEMP-SUM
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SAMPLE-COUNT
                   ADD X-FEATURES(J, I) TO TEMP-SUM
               END-PERFORM
               DIVIDE TEMP-SUM BY SAMPLE-COUNT 
                   GIVING FEATURE-MEAN(I)
               
               MOVE 0 TO TEMP-SUM
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SAMPLE-COUNT
                   SUBTRACT FEATURE-MEAN(I) FROM X-FEATURES(J, I)
                       GIVING TEMP-FLOAT
                   MULTIPLY TEMP-FLOAT BY TEMP-FLOAT 
                       GIVING TEMP-FLOAT
                   ADD TEMP-FLOAT TO TEMP-SUM
               END-PERFORM
               DIVIDE TEMP-SUM BY SAMPLE-COUNT GIVING TEMP-FLOAT
               COMPUTE FEATURE-STD(I) = FUNCTION SQRT(TEMP-FLOAT)
           END-PERFORM.
       
       STANDARDIZE-DATA.
           DISPLAY "Standardizing features..."
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SAMPLE-COUNT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-FEATURES
                   SUBTRACT FEATURE-MEAN(J) FROM X-FEATURES(I, J)
                   DIVIDE X-FEATURES(I, J) BY FEATURE-STD(J)
                       GIVING X-FEATURES(I, J)
               END-PERFORM
           END-PERFORM.
       
       TRAIN-TEST-SPLIT.
           DISPLAY "Splitting train/test data..."
           MOVE 1 TO IDX1
           MOVE 1 TO IDX2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SAMPLE-COUNT
               PERFORM PSEUDO-RANDOM
               IF TEMP-FLOAT < 0.8
                   MOVE I TO TRAIN-INDICES(IDX1)
                   ADD 1 TO IDX1
               ELSE
                   MOVE I TO TEST-INDICES(IDX2)
                   ADD 1 TO IDX2
               END-IF
           END-PERFORM
           SUBTRACT 1 FROM IDX1 GIVING TRAIN-SAMPLES
           SUBTRACT 1 FROM IDX2 GIVING TEST-SAMPLES
           DISPLAY "Train samples: " TRAIN-SAMPLES
           DISPLAY "Test samples: " TEST-SAMPLES.
       
       PSEUDO-RANDOM.
           MULTIPLY WS-RANDOM-SEED BY 1103515245
           ADD 12345 TO WS-RANDOM-SEED
           DIVIDE WS-RANDOM-SEED BY 2147483648 
               GIVING TEMP-FLOAT REMAINDER WS-RANDOM-SEED
           COMPUTE TEMP-FLOAT = FUNCTION ABS(TEMP-FLOAT).
       
       INITIALIZE-WEIGHTS.
           DISPLAY "Initializing network weights..."
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-FEATURES
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   PERFORM PSEUDO-RANDOM
                   COMPUTE W1-VALS(I, J) = (TEMP-FLOAT - 0.5) * 0.02
               END-PERFORM
           END-PERFORM
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               MOVE 0 TO B1(I)
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   PERFORM PSEUDO-RANDOM
                   COMPUTE W2-VALS(I, J) = (TEMP-FLOAT - 0.5) * 0.02
               END-PERFORM
           END-PERFORM
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-CLASSES
               MOVE 0 TO B2(I)
           END-PERFORM.
       
       TRAINING-LOOP.
           DISPLAY "Starting training..."
           PERFORM VARYING EPOCH FROM 0 BY 1 UNTIL EPOCH >= NUM-EPOCHS
               PERFORM FORWARD-PASS
               PERFORM COMPUTE-LOSS
               PERFORM BACKWARD-PASS
               PERFORM UPDATE-WEIGHTS
               
               IF FUNCTION MOD(EPOCH, 50) = 0
                   DISPLAY "Epoch " EPOCH " Loss: " TEMP-LOSS
               END-IF
           END-PERFORM.
       
       FORWARD-PASS.
      *    Z1 = X_train @ W1 + b1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
               MOVE TRAIN-INDICES(I) TO IDX1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > NUM-FEATURES
                       MULTIPLY X-FEATURES(IDX1, K) BY W1-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   ADD B1(J) TO TEMP-SUM
                   MOVE TEMP-SUM TO Z1-VALS(I, J)
                   
      *            ReLU activation
                   IF Z1-VALS(I, J) > 0
                       MOVE Z1-VALS(I, J) TO H-VALS(I, J)
                   ELSE
                       MOVE 0 TO H-VALS(I, J)
                   END-IF
               END-PERFORM
           END-PERFORM
           
      *    Z2 = H @ W2 + b2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > HIDDEN-SIZE
                       MULTIPLY H-VALS(I, K) BY W2-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   ADD B2(J) TO TEMP-SUM
                   MOVE TEMP-SUM TO Z2-VALS(I, J)
               END-PERFORM
               
      *        Softmax
               PERFORM SOFTMAX-SAMPLE VARYING I FROM I BY 0
                   UNTIL I > TRAIN-SAMPLES
           END-PERFORM.
       
       SOFTMAX-SAMPLE.
           MOVE -999999 TO TEMP-MAX
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
               IF Z2-VALS(I, J) > TEMP-MAX
                   MOVE Z2-VALS(I, J) TO TEMP-MAX
               END-IF
           END-PERFORM
           
           MOVE 0 TO TEMP-SUM
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
               SUBTRACT TEMP-MAX FROM Z2-VALS(I, J) 
                   GIVING TEMP-FLOAT
               COMPUTE TEMP-EXP = FUNCTION EXP(TEMP-FLOAT)
               MOVE TEMP-EXP TO PROB-VALS(I, J)
               ADD TEMP-EXP TO TEMP-SUM
           END-PERFORM
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
               DIVIDE PROB-VALS(I, J) BY TEMP-SUM 
                   GIVING PROB-VALS(I, J)
           END-PERFORM.
       
       COMPUTE-LOSS.
           MOVE 0 TO TEMP-LOSS
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
               MOVE TRAIN-INDICES(I) TO IDX1
               ADD 1 TO Y-DATA(IDX1) GIVING J
               COMPUTE TEMP-FLOAT = 
                   FUNCTION LOG(PROB-VALS(I, J))
               SUBTRACT TEMP-FLOAT FROM TEMP-LOSS
           END-PERFORM
           DIVIDE TEMP-LOSS BY TRAIN-SAMPLES GIVING TEMP-LOSS.
       
       BACKWARD-PASS.
      *    Compute dZ2 = probs - y_one_hot
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
               MOVE TRAIN-INDICES(I) TO IDX1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   SUBTRACT 1 FROM J GIVING K
                   IF K = Y-DATA(IDX1)
                       SUBTRACT 1 FROM PROB-VALS(I, J)
                           GIVING DZ2-VALS(I, J)
                   ELSE
                       MOVE PROB-VALS(I, J) TO DZ2-VALS(I, J)
                   END-IF
                   ADD 1 TO J GIVING J
               END-PERFORM
           END-PERFORM
           
      *    Compute dW2 = H.T @ dZ2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 
                       UNTIL K > TRAIN-SAMPLES
                       MULTIPLY H-VALS(K, I) BY DZ2-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   DIVIDE TEMP-SUM BY TRAIN-SAMPLES 
                       GIVING DW2-VALS(I, J)
               END-PERFORM
           END-PERFORM
           
      *    Compute dB2
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
               MOVE 0 TO TEMP-SUM
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
                   ADD DZ2-VALS(I, J) TO TEMP-SUM
               END-PERFORM
               DIVIDE TEMP-SUM BY TRAIN-SAMPLES GIVING DB2(J)
           END-PERFORM
           
      *    Compute dH = dZ2 @ W2.T
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 
                       UNTIL K > NUM-CLASSES
                       MULTIPLY DZ2-VALS(I, K) BY W2-VALS(J, K)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   MOVE TEMP-SUM TO DH-VALS(I, J)
                   
      *            Apply ReLU gradient
                   IF Z1-VALS(I, J) > 0
                       MOVE DH-VALS(I, J) TO DZ1-VALS(I, J)
                   ELSE
                       MOVE 0 TO DZ1-VALS(I, J)
                   END-IF
               END-PERFORM
           END-PERFORM
           
      *    Compute dW1 = X.T @ dZ1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-FEATURES
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 
                       UNTIL K > TRAIN-SAMPLES
                       MOVE TRAIN-INDICES(K) TO IDX1
                       MULTIPLY X-FEATURES(IDX1, I) BY DZ1-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   DIVIDE TEMP-SUM BY TRAIN-SAMPLES 
                       GIVING DW1-VALS(I, J)
               END-PERFORM
           END-PERFORM
           
      *    Compute dB1
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
               MOVE 0 TO TEMP-SUM
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > TRAIN-SAMPLES
                   ADD DZ1-VALS(I, J) TO TEMP-SUM
               END-PERFORM
               DIVIDE TEMP-SUM BY TRAIN-SAMPLES GIVING DB1(J)
           END-PERFORM.
       
       UPDATE-WEIGHTS.
      *    W1 -= lr * dW1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-FEATURES
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   MULTIPLY DW1-VALS(I, J) BY LEARNING-RATE
                       GIVING TEMP-FLOAT
                   SUBTRACT TEMP-FLOAT FROM W1-VALS(I, J)
               END-PERFORM
           END-PERFORM
           
      *    b1 -= lr * db1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               MULTIPLY DB1(I) BY LEARNING-RATE GIVING TEMP-FLOAT
               SUBTRACT TEMP-FLOAT FROM B1(I)
           END-PERFORM
           
      *    W2 -= lr * dW2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HIDDEN-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   MULTIPLY DW2-VALS(I, J) BY LEARNING-RATE
                       GIVING TEMP-FLOAT
                   SUBTRACT TEMP-FLOAT FROM W2-VALS(I, J)
               END-PERFORM
           END-PERFORM
           
      *    b2 -= lr * db2
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-CLASSES
               MULTIPLY DB2(I) BY LEARNING-RATE GIVING TEMP-FLOAT
               SUBTRACT TEMP-FLOAT FROM B2(I)
           END-PERFORM.
       
       EVALUATE-MODEL.
           DISPLAY "Evaluating on test set..."
           
      *    Forward pass on test data
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEST-SAMPLES
               MOVE TEST-INDICES(I) TO IDX1
               
      *        Z1 = X @ W1 + b1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > HIDDEN-SIZE
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 
                       UNTIL K > NUM-FEATURES
                       MULTIPLY X-FEATURES(IDX1, K) BY W1-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   ADD B1(J) TO TEMP-SUM
                   MOVE TEMP-SUM TO Z1-TEST-VALS(I, J)
                   
      *            ReLU
                   IF Z1-TEST-VALS(I, J) > 0
                       MOVE Z1-TEST-VALS(I, J) TO H-TEST-VALS(I, J)
                   ELSE
                       MOVE 0 TO H-TEST-VALS(I, J)
                   END-IF
               END-PERFORM
               
      *        Z2 = H @ W2 + b2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   MOVE 0 TO TEMP-SUM
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > HIDDEN-SIZE
                       MULTIPLY H-TEST-VALS(I, K) BY W2-VALS(K, J)
                           GIVING TEMP-FLOAT
                       ADD TEMP-FLOAT TO TEMP-SUM
                   END-PERFORM
                   ADD B2(J) TO TEMP-SUM
                   MOVE TEMP-SUM TO Z2-TEST-VALS(I, J)
               END-PERFORM
               
      *        Softmax and argmax
               MOVE -999999 TO TEMP-MAX
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   IF Z2-TEST-VALS(I, J) > TEMP-MAX
                       MOVE Z2-TEST-VALS(I, J) TO TEMP-MAX
                   END-IF
               END-PERFORM
               
               MOVE 0 TO TEMP-SUM
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   SUBTRACT TEMP-MAX FROM Z2-TEST-VALS(I, J)
                       GIVING TEMP-FLOAT
                   COMPUTE TEMP-EXP = FUNCTION EXP(TEMP-FLOAT)
                   MOVE TEMP-EXP TO PROB-TEST-VALS(I, J)
                   ADD TEMP-EXP TO TEMP-SUM
               END-PERFORM
               
               MOVE 0 TO PREDICTIONS(I)
               MOVE -1 TO TEMP-MAX
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-CLASSES
                   DIVIDE PROB-TEST-VALS(I, J) BY TEMP-SUM
                       GIVING PROB-TEST-VALS(I, J)
                   IF PROB-TEST-VALS(I, J) > TEMP-MAX
                       MOVE PROB-TEST-VALS(I, J) TO TEMP-MAX
                       SUBTRACT 1 FROM J GIVING PREDICTIONS(I)
                       ADD 1 TO J GIVING J
                   END-IF
               END-PERFORM
           END-PERFORM
           
      *    Compute accuracy
           MOVE 0 TO CORRECT-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TEST-SAMPLES
               MOVE TEST-INDICES(I) TO IDX1
               IF PREDICTIONS(I) = Y-DATA(IDX1)
                   ADD 1 TO CORRECT-COUNT
               END-IF
           END-PERFORM
           
           DIVIDE CORRECT-COUNT BY TEST-SAMPLES GIVING TEMP-ACC
           DISPLAY "Test Accuracy: " TEMP-ACC
           DISPLAY "Correct predictions: " CORRECT-COUNT 
               " out of " TEST-SAMPLES.
       
       END PROGRAM NEURAL-NETWORK.
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



<script>
document.addEventListener("DOMContentLoaded", () => {
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const target = entry.target;
                if (!target.dataset.typed) {
                    target.dataset.typed = "true";
                    const nodesToType = target._nodesToType;
                    target.innerHTML = '';
                    
                    const cursor = document.createElement('span');
                    cursor.className = 'cursor';
                    cursor.textContent = '█';
                    
                    typeNodes(target, nodesToType, cursor).then(() => {
                         cursor.remove();
                    });
                }
            }
        });
    }, { threshold: 0.1 });

    document.querySelectorAll('.handler-message').forEach(msg => {
        // Prevent layout shift by setting min-height
        msg.style.minHeight = msg.offsetHeight + 'px';
        
        // Clone child nodes to an array to preserve structure
        msg._nodesToType = Array.from(msg.childNodes).map(n => n.cloneNode(true));
        
        // Clear content initially
        msg.innerHTML = ''; 
        
        observer.observe(msg);
    });

    async function typeNodes(parent, nodes, cursor) {
        for (const node of nodes) {
            if (node.nodeType === Node.TEXT_NODE) {
                const text = node.textContent;
                const textNode = document.createTextNode('');
                parent.appendChild(textNode);
                parent.appendChild(cursor);
                for (const char of text) {
                    textNode.textContent += char;
                    // Random delay for human-like typing
                    await new Promise(r => setTimeout(r, Math.random() * 30 + 20)); 
                }
            } else if (node.nodeType === Node.ELEMENT_NODE) {
                const newEl = node.cloneNode(false); // Clone element without children
                parent.appendChild(newEl);
                parent.appendChild(cursor);
                // Recursively type children
                await typeNodes(newEl, Array.from(node.childNodes), cursor);
            }
        }
    }
});
</script>
