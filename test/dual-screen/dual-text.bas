10 rem dual-screen test: vic-ii 40 columns and vdc 80 columns at once
20 graphic 0:scnclr:color 0,1:color 4,1:color 5,6
30 print "vic-ii  40 columns":print
40 print "this picture belongs on the port":print "showing 40 columns."
50 graphic 5:scnclr:color 6,1:color 5,4
60 print "vdc  80 columns":print
70 print "this picture belongs on the port showing 80 columns."
80 n=n+1
90 graphic 0:char 1,0,6,"vic count"+str$(n)
100 graphic 5:char 1,0,6,"vdc count"+str$(n)
110 get k$:if k$="" goto 80
120 graphic 0:print:print:print "stopped."
