REM Test SELECT CASE with range expressions
SCORE = 85
SELECT CASE SCORE
    CASE 90 TO 100
        PRINT "A grade"
    CASE 80 TO 89
        PRINT "B grade"
    CASE 70 TO 79
        PRINT "C grade"
    CASE 60 TO 69
        PRINT "D grade"
    CASE ELSE
        PRINT "F grade"
END SELECT

REM Test range with exact match first (first match wins)
VALUE = 50
SELECT CASE VALUE
    CASE 50
        PRINT "Exact fifty"
    CASE 40 TO 60
        PRINT "In range"
    CASE ELSE
        PRINT "Out of range"
END SELECT

REM Test overlapping ranges (first match wins)
NUM = 25
SELECT CASE NUM
    CASE 20 TO 30
        PRINT "First range"
    CASE 25 TO 35
        PRINT "Second range"
    CASE ELSE
        PRINT "No range"
END SELECT