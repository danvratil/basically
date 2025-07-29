REM Test SELECT CASE with mixed expression types
TOTAL = 8
SELECT CASE TOTAL
    CASE IS >= 10
        PRINT "Maximum risk"
    CASE 6 TO 9
        PRINT "High risk"  
    CASE 1 TO 5
        PRINT "Moderate risk"
    CASE 0
        PRINT "Zero risk"
    CASE ELSE
        PRINT "Invalid risk"
END SELECT

REM Test with expression as test value
A = 3
B = 2
SELECT CASE A + B
    CASE 1, 2, 3
        PRINT "Small sum"
    CASE 4 TO 6
        PRINT "Medium sum"
    CASE IS > 6
        PRINT "Large sum"
END SELECT