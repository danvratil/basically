PRINT "Starting program"
GOTO EndLabel
PRINT "This should not print"
MiddleLabel:
PRINT "At middle label"
GOTO StartAgain
EndLabel:
PRINT "At end label"
GOTO MiddleLabel
StartAgain:
PRINT "Program complete"