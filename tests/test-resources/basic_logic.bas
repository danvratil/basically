REM Test basic logical operations
PRINT "Testing logical operations:"

REM Test relational operators
PRINT 5 = 5
PRINT 5 = 3
PRINT 5 <> 3
PRINT 5 <> 5
PRINT 5 > 3
PRINT 3 > 5
PRINT 5 >= 5
PRINT 5 >= 3
PRINT 3 >= 5
PRINT 3 < 5
PRINT 5 < 3
PRINT 5 <= 5
PRINT 5 <= 3
PRINT 3 <= 5

REM Test logical AND operator
PRINT "Testing AND:"
PRINT -1 AND -1
PRINT -1 AND 0
PRINT 0 AND -1
PRINT 0 AND 0

REM Test logical OR operator  
PRINT "Testing OR:"
PRINT -1 OR -1
PRINT -1 OR 0
PRINT 0 OR -1
PRINT 0 OR 0

REM Test logical NOT operator
PRINT "Testing NOT:"
PRINT NOT -1
PRINT NOT 0

REM Test with variables
PRINT "Testing variables:"
A = 5
B = 3
PRINT A = B
PRINT A > B
PRINT A AND B
PRINT A OR B

REM Test string comparisons
PRINT "Testing string comparisons:"
NAME1$ = "BASIC"
NAME2$ = "BASIC"
NAME3$ = "FORTRAN"
PRINT NAME1$ = NAME2$
PRINT NAME1$ = NAME3$
PRINT NAME1$ <> NAME3$

REM Test complex expressions
PRINT "Testing complex expressions:"
PRINT (5 > 3) AND (2 < 4)
PRINT (5 < 3) OR (2 > 1)
PRINT NOT (5 = 3)

REM Test type coercion in comparisons
PRINT "Testing type coercion:"
PRINT 5 = 5.0
PRINT 10 > 9.5
