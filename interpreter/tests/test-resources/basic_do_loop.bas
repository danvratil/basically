REM Test DO WHILE loop
i = 1
DO WHILE i <= 5
    PRINT i
    i = i + 1
LOOP

REM Test DO UNTIL loop
j = 1
DO UNTIL j > 5
    PRINT j
    j = j + 1
LOOP

REM Test DO...LOOP WHILE
k = 1
DO
    PRINT k
    k = k + 1
LOOP WHILE k <= 5

REM Test DO...LOOP UNTIL
m = 1
DO
    PRINT m
    m = m + 1
LOOP UNTIL m > 5

REM Test nested DO loops
x = 1
DO WHILE x <= 3
    y = 1
    DO WHILE y <= 2
        PRINT x
        PRINT y
        y = y + 1
    LOOP
    x = x + 1
LOOP

REM Test complex conditions
a = 10
b = 5
DO WHILE a > 0 AND b < 10
    PRINT a
    PRINT b
    a = a - 2
    b = b + 1
LOOP 