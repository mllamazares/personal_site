       IDENTIFICATION DIVISION.
       PROGRAM-ID. PENGU-NN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SPECIFY THE SOURCE DATASET FILE FOR CLASSIFICATION.
           SELECT CSV-FILE ASSIGN TO "penguins.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD                  PIC X(200).

       WORKING-STORAGE SECTION.
      *    INTERNAL SYSTEM CONSTANTS AND CAPACITY LIMITS.
       01  WS-CONSTANTS.
           05  MAX-ROWS                PIC 9(4)  VALUE 500.

      *    FLAGS AND ROW COUNTERS FOR DATA INGESTION CONTROL.
       01  WS-EOF                      PIC X     VALUE 'N'.
           88  END-OF-FILE                       VALUE 'Y'.
       01  WS-VALID-ROWS               PIC 9(4)  VALUE 0.
       01  WS-TRAIN-ROWS               PIC 9(4).
       01  WS-TEST-ROWS                PIC 9(4).

      *    VARIABLES FOR MEAN-STD SCALING (STANDARD SCALER).
      *    FORMULA: X_SCALED = (X - MEAN) / STD_DEV.
       01  WS-STATS.
           05  WS-SUM-X1               PIC S9(9)V9(9) VALUE 0.
           05  WS-SUM-X2               PIC S9(9)V9(9) VALUE 0.
           05  WS-SUM-X3               PIC S9(9)V9(9) VALUE 0.
           05  WS-SUM-X4               PIC S9(9)V9(9) VALUE 0.
           05  WS-MEAN-X1              PIC S9(9)V9(9) VALUE 0.
           05  WS-MEAN-X2              PIC S9(9)V9(9) VALUE 0.
           05  WS-MEAN-X3              PIC S9(9)V9(9) VALUE 0.
           05  WS-MEAN-X4              PIC S9(9)V9(9) VALUE 0.
           05  WS-VAR-X1               PIC S9(9)V9(9) VALUE 0.
           05  WS-VAR-X2               PIC S9(9)V9(9) VALUE 0.
           05  WS-VAR-X3               PIC S9(9)V9(9) VALUE 0.
           05  WS-VAR-X4               PIC S9(9)V9(9) VALUE 0.
           05  WS-STD-X1               PIC S9(9)V9(9) VALUE 0.
           05  WS-STD-X2               PIC S9(9)V9(9) VALUE 0.
           05  WS-STD-X3               PIC S9(9)V9(9) VALUE 0.
           05  WS-STD-X4               PIC S9(9)V9(9) VALUE 0.
           05  WS-TEMP-MATH            PIC S9(9)V9(9).

      *    DATA INDEXING AND RANDOM SHUFFLE CONTROL (FISHER-YATES).
       01  WS-INDICES.
           05  WS-IDX OCCURS 500 TIMES PIC 9(4).
       01  WS-RAND-VAL                 PIC S9(9)V9(9).
       01  WS-SWAP-IDX                 PIC 9(4).
       01  WS-TEMP-IDX                 PIC 9(4).
       01  J-IDX                       PIC 9(4).
       
      *    NETWORK ARCHITECTURE AND LEARNING HYPERPARAMETERS.
       01  WS-HYPERPARAMS.
           05  WS-LR                   PIC S9(9)V9(9) VALUE 0.1.
           05  WS-EPOCHS               PIC 9(4)  VALUE 500.
           05  WS-INPUT-DIM            PIC 9(4)  VALUE 4.
           05  WS-HIDDEN-DIM           PIC 9(4)  VALUE 16.
           05  WS-OUTPUT-DIM           PIC 9(4)  VALUE 3.
           05  WS-EPOCH-CTR            PIC 9(4).

      *    LEARNABLE PARAMETERS: WEIGHTS AND BIASES FOR 2-LAYER MLP.
       01  WS-WEIGHTS.
           05  WS-W1-TABLE.
               10  W1-ROW OCCURS 4 TIMES.
                   15  W1-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  WS-B1-TABLE.
               10  B1-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  WS-W2-TABLE.
               10  W2-ROW OCCURS 16 TIMES.
                   15  W2-VAL OCCURS 3 TIMES PIC S9(9)V9(9).
           05  WS-B2-TABLE.
               10  B2-VAL OCCURS 3 TIMES PIC S9(9)V9(9).

      *    ACCUMULATORS FOR PARTIAL DERIVATIVES (GRADIENTS).
       01  WS-GRADS.
           05  WS-DW1-TABLE.
               10  DW1-ROW OCCURS 4 TIMES.
                   15  DW1-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  WS-DB1-TABLE.
               10  DB1-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  WS-DW2-TABLE.
               10  DW2-ROW OCCURS 16 TIMES.
                   15  DW2-VAL OCCURS 3 TIMES PIC S9(9)V9(9).
           05  WS-DB2-TABLE.
               10  DB2-VAL OCCURS 3 TIMES PIC S9(9)V9(9).

      *    STATE STORAGE FOR INTERMEDIATE ACTIVATIONS.
      *    Z1 = XW1 + B1 | H = RELU(Z1) | Z2 = HW2 + B2 | P = SOFTMAX(Z2).
       01  WS-ACTIVATIONS.
           05  Z1-TABLE.
               10  Z1-ROW OCCURS 500 TIMES.
                   15  Z1-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  H-TABLE.
               10  H-ROW OCCURS 500 TIMES.
                   15  H-VAL OCCURS 16 TIMES PIC S9(9)V9(9).
           05  Z2-TABLE.
               10  Z2-ROW OCCURS 500 TIMES.
                   15  Z2-VAL OCCURS 3 TIMES PIC S9(9)V9(9).
           05  P-TABLE.
               10  P-ROW OCCURS 500 TIMES.
                   15  P-VAL OCCURS 3 TIMES PIC S9(9)V9(9).
      
      *    METRICS FOR PERFORMANCE AUDITING.
       01  WS-PERFORMANCE.
           05  WS-LOSS                 PIC S9(9)V9(9).
           05  WS-ACCURACY             PIC S9(9)V9(9).
           05  WS-PRED-CLASS           PIC 9.
       
      *    ITERATION POINTERS FOR DATA AND LAYER TRAVERSAL.
       01  WS-COUNTERS.
           05  IDX-I                   PIC 9(4).
           05  IDX-J                   PIC 9(4).
           05  IDX-K                   PIC 9(4).
           05  IDX-S                   PIC 9(4).
           05  IDX-ROW                 PIC 9(4).

      *    BUFFER VARIABLES FOR CSV TOKEN EXTRACTION.
       01  WS-PARSING.
           05  WS-UNSTRING-PTR         PIC 9(4).
           05  WS-FIELD-STR            PIC X(50).
           05  WS-VAL-NUM              PIC 9(5)V9(2).
           05  WS-SPECIES-STR          PIC X(20).
           05  WS-ISLAND               PIC X(20).
           05  WS-BILL-LEN             PIC X(10).
           05  WS-BILL-DEP             PIC X(10).
           05  WS-FLIP-LEN             PIC X(10).
           05  WS-BODY-MASS            PIC X(10).
           05  WS-SEX                  PIC X(10).

      *    PRIMARY DATA STORAGE FOR LOADED SAMPLES.
       01  DATASET-TABLE.
           05  DATA-ROW OCCURS 500 TIMES INDEXED BY I-ROW.
               10  D-SPECIES-NAME      PIC X(20).
               10  D-X1                PIC S9(9)V9(9).
               10  D-X2                PIC S9(9)V9(9).
               10  D-X3                PIC S9(9)V9(9).
               10  D-X4                PIC S9(9)V9(9).
               10  D-Y                 PIC 9.

      *    UTILITY VARIABLES FOR BOX-MULLER AND RANDOM SEEDING.
       01  WS-TEMP-VARS.
           05  WS-RAND-U1              PIC S9(9)V9(9).
           05  WS-RAND-U2              PIC S9(9)V9(9).
           05  WS-GAUSSIAN             PIC S9(9)V9(9).
           05  WS-PI                   PIC S9(9)V9(9) VALUE 3.141592653.
           05  WS-TIMESTAMP.
               10  WS-DATE-PART        PIC 9(8).
               10  WS-TIME-PART        PIC 9(8).
               10  WS-TZ-PART          PIC X(5).

      *    WORKSPACE FOR PER-SAMPLE BACKWARD PASS COMPUTATIONS.
       01  WS-BACKPROP-VARS.
           05  BP-DZ2       OCCURS 3 TIMES  PIC S9(9)V9(9).
           05  BP-DH        OCCURS 16 TIMES PIC S9(9)V9(9).
           05  BP-DZ1       OCCURS 16 TIMES PIC S9(9)V9(9).

      *    DIAGNOSTIC DATA FOR CLASS DISTRIBUTION BALANCING.
       01  WS-DEBUG-VARS.
           05  WS-C1                   PIC 9(4).
           05  WS-C2                   PIC 9(4).
           05  WS-C3                   PIC 9(4).

       PROCEDURE DIVISION.
      *    ORCHESTRATE THE NETWORK LIFECYCLE: DATA PREP, INIT, TRAIN.
           PERFORM 0000-HOUSEKEEPING
           PERFORM 0400-INIT-WEIGHTS
           PERFORM 0500-TRAINING-LOOP
           PERFORM 0600-EVALUATE
           STOP RUN.

       0000-HOUSEKEEPING.
      *    AGGREGATE DATA PREPARATION: LOAD, STANDARDIZE, AND SHUFFLE.
           PERFORM 0100-LOAD-DATA
           DISPLAY "LOADED " WS-VALID-ROWS " VALID ROWS."
           PERFORM 0200-PREPROCESS
           PERFORM 0300-SHUFFLE-DATA
           DISPLAY "DATA HOUSEKEEPING COMPLETED.".

       0100-LOAD-DATA.
      *    ITERATIVE LOADING OF CSV RECORDS DISCARDING HEADER.
           OPEN INPUT CSV-FILE
           READ CSV-FILE INTO CSV-RECORD
               AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE
               READ CSV-FILE INTO CSV-RECORD
                   AT END SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 0110-PARSE-ROW
               END-READ
           END-PERFORM
           CLOSE CSV-FILE.

       0110-PARSE-ROW.
      *    DELEVEL RECORDS INTO INDIVIDUAL NUMERIC OR CATEGORICAL ITEMS.
           UNSTRING CSV-RECORD DELIMITED BY ","
               INTO WS-SPECIES-STR WS-ISLAND WS-BILL-LEN 
                    WS-BILL-DEP WS-FLIP-LEN WS-BODY-MASS WS-SEX
           END-UNSTRING

      *    VALIDATE AGAINST MISSING DATA (NA) TO MATCH PYTHON CLEANING.
           IF WS-BILL-LEN = "NA" OR WS-BILL-DEP = "NA" OR 
              WS-FLIP-LEN = "NA" OR WS-BODY-MASS = "NA" OR
              WS-SEX = "NA" THEN
               CONTINUE
           ELSE
               ADD 1 TO WS-VALID-ROWS
               MOVE WS-SPECIES-STR TO D-SPECIES-NAME(WS-VALID-ROWS)
      *        ENCODE TARGET SPECIES AS INTEGER LABELS (0-2).
               EVALUATE WS-SPECIES-STR
                   WHEN "Adelie"    MOVE 0 TO D-Y(WS-VALID-ROWS)
                   WHEN "Chinstrap" MOVE 1 TO D-Y(WS-VALID-ROWS)
                   WHEN "Gentoo"    MOVE 2 TO D-Y(WS-VALID-ROWS)
               END-EVALUATE
      *        TRANSFORM TEXTUAL NUMERIC FIELDS TO COMPUTATION TYPES.
               COMPUTE D-X1(WS-VALID-ROWS) = 
                       FUNCTION NUMVAL(WS-BILL-LEN)
               COMPUTE D-X2(WS-VALID-ROWS) = 
                       FUNCTION NUMVAL(WS-BILL-DEP)
               COMPUTE D-X3(WS-VALID-ROWS) = 
                       FUNCTION NUMVAL(WS-FLIP-LEN)
               COMPUTE D-X4(WS-VALID-ROWS) = 
                       FUNCTION NUMVAL(WS-BODY-MASS)
           END-IF.

       0200-PREPROCESS.
      *    COMPUTE DATA STATISTICS FOR Z-SCORE NORMALIZATION.
      *    STEP 1: SUMMATION OF ALL SAMPLES BY FEATURE.
           PERFORM VARYING IDX-ROW FROM 1 BY 1 
                   UNTIL IDX-ROW > WS-VALID-ROWS
               ADD D-X1(IDX-ROW) TO WS-SUM-X1
               ADD D-X2(IDX-ROW) TO WS-SUM-X2
               ADD D-X3(IDX-ROW) TO WS-SUM-X3
               ADD D-X4(IDX-ROW) TO WS-SUM-X4
           END-PERFORM

      *    STEP 2: ARITHMETIC MEAN CALCULATION (SUM / N).
           COMPUTE WS-MEAN-X1 = WS-SUM-X1 / WS-VALID-ROWS
           COMPUTE WS-MEAN-X2 = WS-SUM-X2 / WS-VALID-ROWS
           COMPUTE WS-MEAN-X3 = WS-SUM-X3 / WS-VALID-ROWS
           COMPUTE WS-MEAN-X4 = WS-SUM-X4 / WS-VALID-ROWS

      *    STEP 3: COMPUTE VARIANCE (MEAN SQUARED DIFFERENCES).
           PERFORM VARYING IDX-ROW FROM 1 BY 1 
                   UNTIL IDX-ROW > WS-VALID-ROWS
               COMPUTE WS-TEMP-MATH = D-X1(IDX-ROW) - WS-MEAN-X1
               COMPUTE WS-VAR-X1 = WS-VAR-X1 + 
                       (WS-TEMP-MATH * WS-TEMP-MATH)
               COMPUTE WS-TEMP-MATH = D-X2(IDX-ROW) - WS-MEAN-X2
               COMPUTE WS-VAR-X2 = WS-VAR-X2 + 
                       (WS-TEMP-MATH * WS-TEMP-MATH)
               COMPUTE WS-TEMP-MATH = D-X3(IDX-ROW) - WS-MEAN-X3
               COMPUTE WS-VAR-X3 = WS-VAR-X3 + 
                       (WS-TEMP-MATH * WS-TEMP-MATH)
               COMPUTE WS-TEMP-MATH = D-X4(IDX-ROW) - WS-MEAN-X4
               COMPUTE WS-VAR-X4 = WS-VAR-X4 + 
                       (WS-TEMP-MATH * WS-TEMP-MATH)
           END-PERFORM

      *    STEP 4: STANDARD DEVIATION (SQRT OF VARIANCE).
           COMPUTE WS-STD-X1 = 
                   FUNCTION SQRT(WS-VAR-X1 / WS-VALID-ROWS)
           COMPUTE WS-STD-X2 = 
                   FUNCTION SQRT(WS-VAR-X2 / WS-VALID-ROWS)
           COMPUTE WS-STD-X3 = 
                   FUNCTION SQRT(WS-VAR-X3 / WS-VALID-ROWS)
           COMPUTE WS-STD-X4 = 
                   FUNCTION SQRT(WS-VAR-X4 / WS-VALID-ROWS)

      *    STEP 5: APPLY Z-SCORE TRANSFORMATION TO ALL SAMPLES.
           PERFORM VARYING IDX-ROW FROM 1 BY 1 
                   UNTIL IDX-ROW > WS-VALID-ROWS
               COMPUTE D-X1(IDX-ROW) = (D-X1(IDX-ROW) - 
                        WS-MEAN-X1) / WS-STD-X1
               COMPUTE D-X2(IDX-ROW) = (D-X2(IDX-ROW) - 
                        WS-MEAN-X2) / WS-STD-X2
               COMPUTE D-X3(IDX-ROW) = (D-X3(IDX-ROW) - 
                        WS-MEAN-X3) / WS-STD-X3
               COMPUTE D-X4(IDX-ROW) = (D-X4(IDX-ROW) - 
                        WS-MEAN-X4) / WS-STD-X4
           END-PERFORM.

       0300-SHUFFLE-DATA.
      *    INDEX-BASED DATA SHUFFLE TO ENSURE STOCHASTIC INDEPENDENCE.
           PERFORM VARYING IDX-ROW FROM 1 BY 1 
                   UNTIL IDX-ROW > WS-VALID-ROWS
               MOVE IDX-ROW TO WS-IDX(IDX-ROW)
           END-PERFORM
      *    DYNAMIC SEEDING USING HOST SYSTEM CLOCK.
           MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP
           COMPUTE WS-RAND-VAL = FUNCTION RANDOM(WS-TIME-PART)
      *    FISHER-YATES IN-PLACE SHUFFLE ALGORITHM.
           PERFORM VARYING IDX-ROW FROM WS-VALID-ROWS BY -1 
                   UNTIL IDX-ROW < 2
               COMPUTE WS-RAND-VAL = FUNCTION RANDOM
               COMPUTE J-IDX = (WS-RAND-VAL * IDX-ROW) + 1
               IF J-IDX > IDX-ROW MOVE IDX-ROW TO J-IDX END-IF
               IF J-IDX < 1 MOVE 1 TO J-IDX END-IF
               MOVE WS-IDX(IDX-ROW) TO WS-TEMP-IDX
               MOVE WS-IDX(J-IDX) TO WS-IDX(IDX-ROW)
               MOVE WS-TEMP-IDX TO WS-IDX(J-IDX)
           END-PERFORM
      *    DIVIDE SHUFFLED INDICES INTO TRAINING (80%) AND TEST (20%).
           COMPUTE WS-TRAIN-ROWS = WS-VALID-ROWS * 0.8
           COMPUTE WS-TEST-ROWS = WS-VALID-ROWS - WS-TRAIN-ROWS.

       0400-INIT-WEIGHTS.
      *    GAUSSIAN WEIGHT INITIALIZATION USING BOX-MULLER TRANSFORM.
      *    G(X, Y) = SQRT(-2LN(U1)) * COS(2PI * U2).
           PERFORM VARYING IDX-I FROM 1 BY 1 UNTIL IDX-I > 4
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 16
                   COMPUTE WS-RAND-U1 = FUNCTION RANDOM
                   COMPUTE WS-RAND-U2 = FUNCTION RANDOM
                   COMPUTE WS-GAUSSIAN = 
                       FUNCTION SQRT(-2 * FUNCTION LOG(WS-RAND-U1)) *
                       FUNCTION COS(2 * WS-PI * WS-RAND-U2)
      *            SCALE WEIGHTS DOWN (0.01) TO PREVENT GRADIENT EXPLOSION.
                   COMPUTE W1-VAL(IDX-I, IDX-J) = WS-GAUSSIAN * 0.01
               END-PERFORM
           END-PERFORM
           INITIALIZE WS-B1-TABLE
           PERFORM VARYING IDX-I FROM 1 BY 1 UNTIL IDX-I > 16
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   COMPUTE WS-RAND-U1 = FUNCTION RANDOM
                   COMPUTE WS-RAND-U2 = FUNCTION RANDOM
                   COMPUTE WS-GAUSSIAN = 
                       FUNCTION SQRT(-2 * FUNCTION LOG(WS-RAND-U1)) *
                       FUNCTION COS(2 * WS-PI * WS-RAND-U2)
                   COMPUTE W2-VAL(IDX-I, IDX-J) = WS-GAUSSIAN * 0.01
               END-PERFORM
           END-PERFORM
           INITIALIZE WS-B2-TABLE.

       0500-TRAINING-LOOP.
      *    RECURSIVE GRADIENT DESCENT OPTIMIZATION.
           PERFORM VARYING WS-EPOCH-CTR FROM 0 BY 1 
                   UNTIL WS-EPOCH-CTR > WS-EPOCHS
               PERFORM 0510-FORWARD-PROP
               PERFORM 0520-CALC-LOSS
      *        PERIODIC TELEMETRY OF TRAINING PROGRESS.
               IF FUNCTION MOD(WS-EPOCH-CTR, 50) = 0 THEN
                   DISPLAY "EPOCH " WS-EPOCH-CTR " LOSS: " WS-LOSS
               END-IF
               PERFORM 0530-BACKWARD-PROP
               PERFORM 0540-UPDATE-WEIGHTS
           END-PERFORM.

       0510-FORWARD-PROP.
      *    CALCULATE ACTIVATIONS FOR ALL HIDDEN AND OUTPUT NEURONS.
           PERFORM VARYING IDX-S FROM 1 BY 1 
                   UNTIL IDX-S > WS-TRAIN-ROWS
               COMPUTE IDX-I = WS-IDX(IDX-S)
      *        HIDDEN LAYER COMPUTATION: Z1 = X * W1 + B1.
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 16
                   MOVE B1-VAL(IDX-J) TO Z1-VAL(IDX-I, IDX-J)
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) +
                           (D-X1(IDX-I) * W1-VAL(1, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) +
                           (D-X2(IDX-I) * W1-VAL(2, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) +
                           (D-X3(IDX-I) * W1-VAL(3, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) +
                           (D-X4(IDX-I) * W1-VAL(4, IDX-J))
      *            NON-LINEAR ACTIVATION: RELU(Z) = MAX(0, Z).
                   IF Z1-VAL(IDX-I, IDX-J) > 0
                       MOVE Z1-VAL(IDX-I, IDX-J) TO H-VAL(IDX-I, IDX-J)
                   ELSE
                       MOVE 0 TO H-VAL(IDX-I, IDX-J)
                   END-IF
               END-PERFORM
      *        OUTPUT LAYER COMPUTATION: Z2 = H * W2 + B2.
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   MOVE B2-VAL(IDX-J) TO Z2-VAL(IDX-I, IDX-J)
                   PERFORM VARYING IDX-K FROM 1 BY 1 UNTIL IDX-K > 16
                       COMPUTE Z2-VAL(IDX-I, IDX-J) = 
                               Z2-VAL(IDX-I, IDX-J) + 
                               (H-VAL(IDX-I, IDX-K) * 
                                W2-VAL(IDX-K, IDX-J))
                   END-PERFORM
               END-PERFORM
      *        PROBABILITY ESTIMATION: SOFTMAX(Z2).
      *        P_i = EXP(Z_i) / SUM(EXP(Z_j)).
               COMPUTE P-VAL(IDX-I, 1) = FUNCTION EXP(Z2-VAL(IDX-I, 1))
               COMPUTE P-VAL(IDX-I, 2) = FUNCTION EXP(Z2-VAL(IDX-I, 2))
               COMPUTE P-VAL(IDX-I, 3) = FUNCTION EXP(Z2-VAL(IDX-I, 3))
               MOVE 0 TO WS-TEMP-MATH
               ADD P-VAL(IDX-I, 1) P-VAL(IDX-I, 2) P-VAL(IDX-I, 3) 
                 TO WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 1) = P-VAL(IDX-I, 1) / WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 2) = P-VAL(IDX-I, 2) / WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 3) = P-VAL(IDX-I, 3) / WS-TEMP-MATH
           END-PERFORM.

       0520-CALC-LOSS.
      *    CROSS-ENTROPY LOSS: L = -SUM(Y_TRUE * LOG(P_PRED)).
           MOVE 0 TO WS-LOSS
           PERFORM VARYING IDX-S FROM 1 BY 1 
                   UNTIL IDX-S > WS-TRAIN-ROWS
               COMPUTE IDX-I = WS-IDX(IDX-S)
               COMPUTE IDX-J = D-Y(IDX-I) + 1
               COMPUTE WS-LOSS = WS-LOSS - 
                                 FUNCTION LOG(P-VAL(IDX-I, IDX-J))
           END-PERFORM
           COMPUTE WS-LOSS = WS-LOSS / WS-TRAIN-ROWS.

       0530-BACKWARD-PROP.
      *    COMPUTE LAYER GRADIENTS USING THE CHAIN RULE.
           INITIALIZE WS-GRADS
           PERFORM VARYING IDX-S FROM 1 BY 1 
                   UNTIL IDX-S > WS-TRAIN-ROWS
               COMPUTE IDX-I = WS-IDX(IDX-S)
      *        DERIVATIVE OF SOFTMAX CW CROSS-ENTROPY: DZ2 = P - Y_TRUE.
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   MOVE P-VAL(IDX-I, IDX-J) TO BP-DZ2(IDX-J)
               END-PERFORM
               COMPUTE IDX-K = D-Y(IDX-I) + 1
               SUBTRACT 1 FROM BP-DZ2(IDX-K)
      *        ACCUMULATE DW2 = H^T * DZ2 | DB2 = DZ2.
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   COMPUTE DB2-VAL(IDX-J) = DB2-VAL(IDX-J) + 
                                            BP-DZ2(IDX-J)
                   PERFORM VARYING IDX-K FROM 1 BY 1 UNTIL IDX-K > 16
                       COMPUTE DW2-VAL(IDX-K, IDX-J) = 
                               DW2-VAL(IDX-K, IDX-J) + 
                               (H-VAL(IDX-I, IDX-K) * BP-DZ2(IDX-J))
                   END-PERFORM
               END-PERFORM
      *        BACKPROP TO HIDDEN LAYER: DH = DZ2 * W2^T.
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 16
                   MOVE 0 TO BP-DH(IDX-J)
                   PERFORM VARYING IDX-K FROM 1 BY 1 UNTIL IDX-K > 3
                       COMPUTE BP-DH(IDX-J) = BP-DH(IDX-J) + 
                               (BP-DZ2(IDX-K) * W2-VAL(IDX-J, IDX-K))
                   END-PERFORM
      *            DERIVATIVE OF RELU: DZ1 = DH IF Z1 > 0 ELSE 0.
                   IF Z1-VAL(IDX-I, IDX-J) > 0
                       MOVE BP-DH(IDX-J) TO BP-DZ1(IDX-J)
                   ELSE
                       MOVE 0 TO BP-DZ1(IDX-J)
                   END-IF
      *            ACCUMULATE DW1 = X^T * DZ1 | DB1 = DZ1.
                   COMPUTE DB1-VAL(IDX-J) = DB1-VAL(IDX-J) + 
                                            BP-DZ1(IDX-J)
                   IF BP-DZ1(IDX-J) NOT = 0
                       COMPUTE DW1-VAL(1, IDX-J) = 
                         DW1-VAL(1, IDX-J) + 
                         (D-X1(IDX-I) * BP-DZ1(IDX-J))
                       COMPUTE DW1-VAL(2, IDX-J) = 
                         DW1-VAL(2, IDX-J) + 
                         (D-X2(IDX-I) * BP-DZ1(IDX-J))
                       COMPUTE DW1-VAL(3, IDX-J) = 
                         DW1-VAL(3, IDX-J) + 
                         (D-X3(IDX-I) * BP-DZ1(IDX-J))
                       COMPUTE DW1-VAL(4, IDX-J) = 
                         DW1-VAL(4, IDX-J) + 
                         (D-X4(IDX-I) * BP-DZ1(IDX-J))
                   END-IF
               END-PERFORM
           END-PERFORM.

       0540-UPDATE-WEIGHTS.
      *    PERFORM PARAMETER UPDATES: PARAM = PARAM - LR * GRADIENT.
           COMPUTE WS-TEMP-MATH = WS-LR / WS-TRAIN-ROWS
           PERFORM VARYING IDX-I FROM 1 BY 1 UNTIL IDX-I > 16
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   COMPUTE W2-VAL(IDX-I, IDX-J) = 
                           W2-VAL(IDX-I, IDX-J) - 
                           (WS-TEMP-MATH * DW2-VAL(IDX-I, IDX-J))
               END-PERFORM
               COMPUTE B1-VAL(IDX-I) = B1-VAL(IDX-I) - 
                                       (WS-TEMP-MATH * DB1-VAL(IDX-I))
           END-PERFORM
           PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
               COMPUTE B2-VAL(IDX-J) = B2-VAL(IDX-J) - 
                                       (WS-TEMP-MATH * DB2-VAL(IDX-J))
           END-PERFORM
           PERFORM VARYING IDX-I FROM 1 BY 1 UNTIL IDX-I > 4
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 16
                   COMPUTE W1-VAL(IDX-I, IDX-J) = 
                           W1-VAL(IDX-I, IDX-J) - 
                           (WS-TEMP-MATH * DW1-VAL(IDX-I, IDX-J))
               END-PERFORM
           END-PERFORM.

       0600-EVALUATE.
      *    VALIDATE GENERALIZATION PERFORMANCE ON UNSEEN TEST DATA.
           MOVE 0 TO WS-ACCURACY
           PERFORM VARYING IDX-S FROM 1 BY 1 UNTIL IDX-S > WS-TEST-ROWS
               COMPUTE IDX-I = WS-IDX(WS-TRAIN-ROWS + IDX-S)
      *        SINGLE-SAMPLE FORWARD PASS (INFERENCE).
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 16
                   MOVE B1-VAL(IDX-J) TO Z1-VAL(IDX-I, IDX-J)
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) + 
                           (D-X1(IDX-I) * W1-VAL(1, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) + 
                           (D-X2(IDX-I) * W1-VAL(2, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) + 
                           (D-X3(IDX-I) * W1-VAL(3, IDX-J))
                   COMPUTE Z1-VAL(IDX-I, IDX-J) = 
                           Z1-VAL(IDX-I, IDX-J) + 
                           (D-X4(IDX-I) * W1-VAL(4, IDX-J))
                   IF Z1-VAL(IDX-I, IDX-J) > 0 
                      MOVE Z1-VAL(IDX-I, IDX-J) TO H-VAL(IDX-I, IDX-J)
                   ELSE 
                      MOVE 0 TO H-VAL(IDX-I, IDX-J) 
                   END-IF
               END-PERFORM
               PERFORM VARYING IDX-J FROM 1 BY 1 UNTIL IDX-J > 3
                   MOVE B2-VAL(IDX-J) TO Z2-VAL(IDX-I, IDX-J)
                   PERFORM VARYING IDX-K FROM 1 BY 1 UNTIL IDX-K > 16
                       COMPUTE Z2-VAL(IDX-I, IDX-J) = 
                               Z2-VAL(IDX-I, IDX-J) + 
                               (H-VAL(IDX-I, IDX-K) * 
                                W2-VAL(IDX-K, IDX-J))
                   END-PERFORM
               END-PERFORM
               COMPUTE P-VAL(IDX-I, 1) = FUNCTION EXP(Z2-VAL(IDX-I, 1))
               COMPUTE P-VAL(IDX-I, 2) = FUNCTION EXP(Z2-VAL(IDX-I, 2))
               COMPUTE P-VAL(IDX-I, 3) = FUNCTION EXP(Z2-VAL(IDX-I, 3))
               MOVE 0 TO WS-TEMP-MATH
               ADD P-VAL(IDX-I, 1) P-VAL(IDX-I, 2) P-VAL(IDX-I, 3) 
                 TO WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 1) = P-VAL(IDX-I, 1) / WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 2) = P-VAL(IDX-I, 2) / WS-TEMP-MATH
               COMPUTE P-VAL(IDX-I, 3) = P-VAL(IDX-I, 3) / WS-TEMP-MATH
      *        PREDICATE SELECTION: ARGMAX PROBABILITY.
               EVALUATE TRUE
                   WHEN P-VAL(IDX-I, 1) >= P-VAL(IDX-I, 2) AND 
                        P-VAL(IDX-I, 1) >= P-VAL(IDX-I, 3)
                       MOVE 0 TO WS-PRED-CLASS
                   WHEN P-VAL(IDX-I, 2) >= P-VAL(IDX-I, 1) AND 
                        P-VAL(IDX-I, 2) >= P-VAL(IDX-I, 3)
                       MOVE 1 TO WS-PRED-CLASS
                   WHEN OTHER
                       MOVE 2 TO WS-PRED-CLASS
               END-EVALUATE
               IF WS-PRED-CLASS = D-Y(IDX-I) 
                  ADD 1 TO WS-ACCURACY 
               END-IF
           END-PERFORM
           COMPUTE WS-ACCURACY = WS-ACCURACY / WS-TEST-ROWS
           DISPLAY "TEST ACCURACY: " WS-ACCURACY
           PERFORM 0610-ACCURACY-CHECK.
           
       0610-ACCURACY-CHECK.
      *    AGGREGATE ACCURACY CALCULATION FOR FINAL SUMMARY.
           MOVE 0 TO WS-ACCURACY
           PERFORM VARYING IDX-S FROM 1 BY 1 
                   UNTIL IDX-S > WS-TRAIN-ROWS
               COMPUTE IDX-I = WS-IDX(IDX-S)
               EVALUATE TRUE
                   WHEN P-VAL(IDX-I, 1) >= P-VAL(IDX-I, 2) AND 
                        P-VAL(IDX-I, 1) >= P-VAL(IDX-I, 3)
                       MOVE 0 TO WS-PRED-CLASS
                   WHEN P-VAL(IDX-I, 2) >= P-VAL(IDX-I, 1) AND 
                        P-VAL(IDX-I, 2) >= P-VAL(IDX-I, 3)
                       MOVE 1 TO WS-PRED-CLASS
                   WHEN OTHER
                       MOVE 2 TO WS-PRED-CLASS
               END-EVALUATE
               IF WS-PRED-CLASS = D-Y(IDX-I) 
                  ADD 1 TO WS-ACCURACY 
               END-IF
           END-PERFORM
           COMPUTE WS-ACCURACY = WS-ACCURACY / WS-TRAIN-ROWS
           DISPLAY "TRAIN ACCURACY: " WS-ACCURACY.
