10 rem vic-ii bitmap on the 40 column screen, vdc text log on the 80 column screen
20 graphic 5:scnclr:color 6,1:color 5,8
30 print "vdc  80 columns: text log. the vic-ii is drawing on the 40 column screen."
40 print "press any key to stop.":print
50 color 0,1:color 4,1:color 1,6:graphic 1,1
60 box 1,0,0,319,199:char 1,11,2,"vic-ii  40 columns"
70 x=20:y=40:dx=4:dy=3:n=0:w=40:h=24
80 box 1,x,y,x+w,y+h
90 nx=x+dx:ny=y+dy
100 if nx<4 or nx+w>315 then dx=-dx:goto 90
110 if ny<32 or ny+h>195 then dy=-dy:goto 90
120 box 0,x,y,x+w,y+h:box 1,nx,ny,nx+w,ny+h:x=nx:y=ny:n=n+1
130 if n/10=int(n/10) then print "frame";n;"  box at";x;",";y
140 get k$:if k$="" goto 90
150 graphic 0,1:graphic 5:print:print "stopped."
