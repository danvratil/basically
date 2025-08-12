REM Test EXIT DO in DO WHILE loop
i = 1
DO WHILE i <= 10
    PRINT i
    IF i = 5 THEN
        EXIT DO
    ENDIF
    i = i + 1
LOOP
PRINT "Exited DO loop"

REM Test EXIT DO in DO UNTIL loop
j = 1
DO UNTIL j > 10
    PRINT j
    IF j = 3 THEN
        EXIT DO
    ENDIF
    j = j + 1
LOOP
PRINT "Exited DO UNTIL loop"

REM Test EXIT DO in DO...LOOP WHILE
k = 1
DO
    PRINT k
    IF k = 4 THEN
        EXIT DO
    ENDIF
    k = k + 1
LOOP WHILE k <= 10
PRINT "Exited DO...LOOP WHILE"

REM Test nested DO loops with EXIT DO
x = 1
DO WHILE x <= 3
    y = 1
    DO WHILE y <= 5
        PRINT x
        PRINT y
        IF y = 2 THEN
            EXIT DO
        ENDIF
        y = y + 1
    LOOP
    x = x + 1
LOOP
PRINT "Exited nested DO loops" 