5 poke 55,0:poke 56,64:clr:cf=0
10 print chr$(147)"17-start: 2-byte transfers"
15 print "bad 1st/2nd byte, bl: started on badline"
20 b=57088:x=peek(b):gosub 900:dim nr(8)
30 poke 8195,0:sys 8192:print "prepare: status"peek(8210)
35 if cf then poke 16389,99
40 for m=1 to 8:poke 8195,m:sys 8192:gosub 100:next
50 gosub 900:print "photo, then a key for the lines";
60 if cf=0 then poke 198,0:wait 198,1:poke 198,0
70 print chr$(147)"lines of bad runs (* = badline)"
75 for m=1 to 8:if nr(m)=0 then 95
80 print "mode"m":";:p=49408+64*(m-1)
85 for i=0 to nr(m)-1:l=peek(p+i):c=peek(p+32+i):y=l+256*(c>127)*-1
90 print y;:if c<128 and (c and 16)=16 and l>=48 and l<=247 and (l and 7)=(c and 7) then print "*";
92 next:print
95 next
96 rem the top of basic back to $a000, or a basic program run next puts its strings there
97 poke 55,0:poke 56,160:clr:end
100 t=8196:f1=peek(t)+256*peek(t+1):f2=peek(t+2)+256*peek(t+3)
105 g1=peek(t+4)+256*peek(t+5):g2=peek(t+6)+256*peek(t+7)
110 bl=peek(t+8)+256*peek(t+9):u=peek(t+10)+256*peek(t+11):nr(m)=peek(t+12)
115 d$="off":if (m and 1)=0 then d$="on"
120 on (m+1)/2 goto 130,135,140,145
130 print "verify "d$":"f1;f2"of 16384 bl"bl;:goto 150
135 print "stash "d$":"f1;f2"of 2000 bl"bl;:goto 150
140 print "fetch "d$":"f1;f2"of 2000 bl"bl;:goto 150
145 print "swap "d$":"f1;f2"/"g1;g2"of 2000 bl"bl;
150 if u>0 then print " not run"u;
155 print:return
