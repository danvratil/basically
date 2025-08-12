REM Test basic WHILE loop
i = 1
WHILE i <= 5
    PRINT i
    i = i + 1
WEND

REM WHILE loop with complex condition
a = 10
b = 5
WHILE a > 0 AND b < 10
    PRINT a
    PRINT b
    a = a - 2
    b = b + 1
WEND

REM Nested WHILE loops
x = 1
WHILE x <= 3
    y = 1
    WHILE y <= 2
        PRINT x
        PRINT y
        y = y + 1
    WEND
    x = x + 1
WEND 