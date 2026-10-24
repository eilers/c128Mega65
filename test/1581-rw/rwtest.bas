10 rem 1581 read write test
20 print chr$(147):print "1581 read/write test"
30 d=8:input "device (8 or 9)";d
40 if d<>8 and d<>9 then 30
50 open15,d,15:gosub1000
60 if ec<>73 and ec<>0 then 900
70 print#15,"s0:rwtemp":gosub1000
80 print "writing 500 records..."
90 open2,d,2,"0:rwtemp,s,w":gosub1000
100 if ec<>0 then 900
110 for i=1 to 500
120 a=i*37+11:b=a*13+7
130 print#2,i;",";a;",";b
140 next i
150 close2:gosub1000
160 if ec<>0 then 900
170 print "reading and verifying..."
180 open2,d,2,"0:rwtemp,s,r":gosub1000
190 if ec<>0 then 900
200 for i=1 to 500
210 input#2,j,a,b
220 x=i*37+11:y=x*13+7
230 if j<>i or a<>x or b<>y then 920
240 next i
250 close2:gosub1000
260 if ec<>0 then 900
270 print "scratching test file..."
280 print#15,"s0:rwtemp":gosub1000
290 if ec<>1 and ec<>0 then 900
300 open2,d,2,"0:rwtemp,s,r":gosub1000
310 close2
320 if ec<>62 then print "warning: scratch status";ec:goto340
330 print "scratch verified."
340 print:print "pass: read/write ok on device";d
350 close15:end
900 print:print "fail: drive error";ec;em$;et;es:goto950
920 print:print "fail: record";i;"read";j;a;b
930 print "expected";i;x;y
950 close2:close15:end
1000 input#15,ec,em$,et,es
1010 print "drive";ec;em$;et;es
1020 return
