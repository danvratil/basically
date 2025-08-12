i = 1
DO WHILE i <= 5
    PRINT i
    IF i = 3 THEN
        EXIT DO
    ENDIF
    i = i + 1
LOOP
PRINT "Done" 