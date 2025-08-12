REM Test SELECT CASE with relational expressions
RISK = 12
SELECT CASE RISK
    CASE IS >= 10
        PRINT "High risk"
    CASE IS >= 5
        PRINT "Medium risk"
    CASE IS > 0
        PRINT "Low risk"
    CASE ELSE
        PRINT "No risk"
END SELECT

REM Test different relational operators
X = 7
SELECT CASE X
    CASE IS < 5
        PRINT "Less than 5"
    CASE IS = 7
        PRINT "Equal to 7"
    CASE IS > 10
        PRINT "Greater than 10"
    CASE ELSE
        PRINT "Between 5 and 10"
END SELECT

REM Test with string comparison
LETTER$ = "M"
SELECT CASE LETTER$
    CASE IS < "F"
        PRINT "Early alphabet"
    CASE IS > "P"
        PRINT "Late alphabet"
    CASE ELSE
        PRINT "Middle alphabet"
END SELECT