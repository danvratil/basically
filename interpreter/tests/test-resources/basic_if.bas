REM Test basic IF...THEN...ENDIF statement
A = 5
IF A = 5 THEN
    PRINT "A equals 5"
ENDIF

REM Test IF...THEN...ELSE...ENDIF
B = 10
IF B < 5 THEN
    PRINT "B is small"
ELSE
    PRINT "B is not small"
ENDIF

REM Test IF...THEN...ELSEIF...ELSE...ENDIF with multiple ELSEIF
C = 15
IF C < 10 THEN
    PRINT "C is small"
ELSEIF C < 20 THEN
    PRINT "C is medium"
ELSEIF C < 30 THEN
    PRINT "C is large"
ELSE
    PRINT "C is very large"
ENDIF

REM Test logical AND operator
D = 7
IF D > 5 AND D < 10 THEN
    PRINT "D is between 5 and 10"
ENDIF

REM Test logical OR operator
E = 2
IF E < 3 OR E > 8 THEN
    PRINT "E is either small or large"
ENDIF

REM Test logical NOT operator
F = 0
IF NOT F = 1 THEN
    PRINT "F is not 1"
ENDIF

REM Test relational operators
G = 12
IF G <> 10 THEN
    PRINT "G does not equal 10"
ENDIF

IF G >= 12 THEN
    PRINT "G is greater than or equal to 12"
ENDIF

IF G <= 15 THEN
    PRINT "G is less than or equal to 15"
ENDIF

REM Test nested IF statements
H = 3
IF H > 0 THEN
    PRINT "H is positive"
    IF H < 5 THEN
        PRINT "H is small and positive"
        IF H = 3 THEN
            PRINT "H is exactly 3"
        ENDIF
    ENDIF
ENDIF

REM Test string comparisons
NAME$ = "BASIC"
IF NAME$ = "BASIC" THEN
    PRINT "Programming language is BASIC"
ENDIF

REM Test complex logical expressions
I = 8
J = 4
IF (I > 5 AND J < 6) OR I = J * 2 THEN
    PRINT "Complex condition satisfied"
ENDIF

REM Test type coercion in comparisons
K% = 10
L! = 10.0
IF K% = L! THEN
    PRINT "Integer equals float through coercion"
ENDIF

REM Test false conditions to ensure ELSE branches work
M = 20
IF M < 10 THEN
    PRINT "This should not print"
ELSE
    PRINT "M is not less than 10"
ENDIF

REM Test multiple conditions in ELSEIF chain where later condition matches
N = 25
IF N < 10 THEN
    PRINT "N is small"
ELSEIF N < 20 THEN
    PRINT "N is medium"
ELSEIF N < 30 THEN
    PRINT "N is large"
ENDIF