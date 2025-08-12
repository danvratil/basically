10 PRINT "Line 10"
GOTO MixedLabel
20 PRINT "Line 20 - should not print"
MixedLabel:
PRINT "At mixed label"
GOTO 30
PRINT "After label - should not print"
30 PRINT "Back to line numbers"