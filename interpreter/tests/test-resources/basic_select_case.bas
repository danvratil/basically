REM Test basic SELECT CASE with exact matches
A = 2
SELECT CASE A
    CASE 1
        PRINT "One"
    CASE 2 
        PRINT "Two"
    CASE 3
        PRINT "Three"
    CASE ELSE
        PRINT "Other"
END SELECT

REM Test SELECT CASE with string
NAME$ = "BASIC"
SELECT CASE NAME$
    CASE "Pascal"
        PRINT "Pascal language"
    CASE "BASIC"
        PRINT "BASIC language"
    CASE "C"
        PRINT "C language"
    CASE ELSE
        PRINT "Unknown language"
END SELECT

REM Test SELECT CASE with no matching case (should use CASE ELSE)
B = 99
SELECT CASE B
    CASE 1
        PRINT "Small"
    CASE 10
        PRINT "Medium"
    CASE ELSE
        PRINT "Large"
END SELECT