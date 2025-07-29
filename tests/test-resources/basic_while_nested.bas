REM Nested WHILE loops test
x = 1
WHILE x <= 2
    y = 1
    WHILE y <= 2
        PRINT x
        PRINT y
        y = y + 1
    WEND
    x = x + 1
WEND 