REM Test basic FOR loop
FOR i = 1 TO 5
    PRINT i
NEXT i

REM Test negative step
FOR j = 5 TO 1 STEP -1
    PRINT j
NEXT j

REM Test step value other than 1
FOR k = 0 TO 10 STEP 2
    PRINT k
NEXT k

REM Test nested FOR loops
FOR x = 1 TO 3
    FOR y = 1 TO 2
        PRINT x
        PRINT y
    NEXT y
NEXT x

REM Test expressions in FOR parameters
start = 2
finish = 8
FOR m = start TO finish STEP 2
    PRINT m
NEXT m