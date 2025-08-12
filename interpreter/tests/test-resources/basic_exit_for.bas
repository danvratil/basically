REM Test EXIT FOR in FOR loop
FOR i = 1 TO 10
    PRINT i
    IF i = 5 THEN
        EXIT FOR
    ENDIF
NEXT i
PRINT "Exited FOR loop"

REM Test EXIT FOR with STEP
FOR j = 10 TO 1 STEP -1
    PRINT j
    IF j = 6 THEN
        EXIT FOR
    ENDIF
NEXT j
PRINT "Exited FOR loop with STEP"

REM Test nested FOR loops with EXIT FOR
FOR x = 1 TO 3
    FOR y = 1 TO 5
        PRINT x
        PRINT y
        IF y = 2 THEN
            EXIT FOR
        ENDIF
    NEXT y
NEXT x
PRINT "Exited nested FOR loops"

REM Test EXIT FOR inside DO loop
k = 1
DO WHILE k <= 3
    FOR m = 1 TO 4
        PRINT k
        PRINT m
        IF m = 2 THEN
            EXIT FOR
        ENDIF
    NEXT m
    k = k + 1
LOOP
PRINT "Exited FOR inside DO loop" 