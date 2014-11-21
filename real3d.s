; d3 - shift offset
; a0 - source
; a1 - dest
; blit modulo1,modulo2,size
blit:	macro
	ror	#4,d3
	eor	#$09f0,d3
	move	d3,$40(a6)	;bltcon0
	clr	$42(a6)		;bltcon1
	move.l  #-1,$44(a6)	;mask read all
	move	#?1,$64(a6)	;source		mod
	move	#?2,$66(a6)	;destination	mod
	move	#$8440,$96(a6)
	move.l	a0,$50(a6)	;source		A
	move.l	a1,$54(a6)	;Destination	D
	move	#?3,$58(a6)
	move	#$0400,$96(a6)
endm

Bltcon0 = $040	;blitter address offseted to $00dff000
Bltcon1 = $042
Bltafwm = $044
Bltalwm = $046
Bltcpth = $048
Bltcptl = $04A
Bltbpth = $04C
Bltbptl = $04E
Bltapth = $050
Bltaptl = $052
Bltdpth = $054
Bltdptl = $056
Bltsize = $058
Bltcmod = $060
Bltbmod = $062
Bltamod = $064
Bltdmod = $066
Bltcdat = $070
Bltbdat = $072
Bltadat = $074

maxobjects = 4
duration = 200*50		;length of VB's for one object to last
centreX = 160
centreY = 132
max = 100		;max amount of lines/connects possible!!

st:	clr.l	0.w

	move	#$4000,$dff09a

	bsr	doscrn

	move.l	#scrollarea,d0
	lea	cap(pc),a0
	move	d0,6(a0)
	swap	d0
	move	d0,2(a0)
	
	move.l	#logo+16,d0
	lea	yak1(pc),a0
	moveq	#2,d1
mkY2	move	d0,6(a0)
	swap	d0
	move	d0,2(a0)
	swap	d0
	add.l	#40,d0
	addq.l	#8,a0
	dbf	d1,mkY2

	lea	logo,a0
	lea	puk1(pc),a1
	moveq	#7,d0
mkC2	move	(a0)+,2(a1)
	addq.l	#4,a1
	dbf	d0,mkC2



	lea	$dff000,a6
	bsr	newobject

	move	#0,Xrot
	move	#6,Yrot
	move	#0,Zrot
	bclr	#1,$bfe001

	move	#$83ff,$96(A6)

	jsr	mt_init


main1:	cmp.b	#$d0,6(a6)
	bne.s	main1
	move.l	#copper,$84(a6)
	bsr	doyavector

	btst	#6,$bfe001
	bne	main1

exit	clr	$80(a6)
	jsr	mt_end
	bclr	#1,$bfe001
	rts

doyavector
	bsr	doscroll
	bsr	dobounce
	jsr	mt_music

	btst	#10,$dff016
	bne.s	okcont
	rts
okcont	add	#-2,Xrot		;add in steps of 2
	add	#2,Yrot
	add	#2,Zrot
	and	#$1fe,Xrot
	and	#$1fe,Yrot
	and	#$1fe,Zrot
	bsr	clear
	moveq	#0,d0
 add	#18,yrot
	bsr	dovector
 sub	#18,yrot
	moveq	#40,d0
	bsr	dovector
	bsr	change_object
	rts

vecOFF:	dc.w 0		;upto 12
vecTAB:	dc.w $12f,$13f,$24f,$25f,$36f,$37f,$48f,$49f
	dc.w $48f,$37f,$36f,$25f,$24f,$13f,$12f,$12e

masks:	dc.w $ffff,$7fff,$7ffe,$3ffe,$3ffc,$1ffc,$1ff8,$0ff8,$0ff0
	dc.w $07f0,$07e0,$03e0,$03c0,$01c0,$0180,$0080,$0000
	dc.w $0000,$0080,$0180,$01c0,$03c0,$03e0,$07e0,$07f0,$0ff0
	dc.w $0ff8,$1ff8,$1ffc,$3ffc,$3ffe,$7ffe,$7fff,$ffff

objectDEL:	dc.w	50*200	;no VB's to miss
mask_del:	dc.w	0
change_object:
	tst	objectDEL
	beq.s	changeit
	subq	#1,objectDEL
	rts

changeit:
	cmp	#34,mask_del
	bge.s	changeit2
	move	mask_del(pc),d0
	addq	#2,mask_del
	cmp	#32,mask_del
	ble.s	m1ok
	bra.s	doNO1
m1ok:	move	#1,objectDEL
	lea	masks(pc),a0
	move	(a0,d0.w),texture+2
	rts
changeit2:
	move	#1,objectDEL
	move	mask_del(pc),d0
	addq	#2,mask_del
	cmp	#34*2,mask_del
	ble.s	m2ok
	move	#duration,objectDEL
	clr	mask_del
	rts
m2ok:	lea	masks(pc),a0
	move	(a0,d0.w),texture+2
	rts

doNO1:	move	#1,objectDEL

	bra	newobject
return:	rts

*----------------------------------------

clear	move.l	#$70000,Bltdpth(a6)
	Move.l	#$01000000,Bltcon0(a6)
	Clr	Bltadat(a6)
	Clr	Bltdmod(a6)
	Move	#128*64+40,Bltsize(a6)
	rts

*---------  do vectors here !! -----------
dovector:
	Move.l	#$70000,a0
	add	d0,a0
	Move	main_num(pc),d7
	Lea	Sintable+$40(pc),a1
	Lea	Rotated_coords(pc),a2
	Lea	Persp(pc),a3
	Lea	main_points(pc),a4

ROT8:	Move	(a4)+,d0
	Move	d0,d2
	Move	(a4)+,d1
	Move	d1,d3

	Move	Zrot(pc),d6
	Muls	$40(a1,d6.w),d0
	Muls	-$40(a1,d6.w),d1
	Sub.l	d1,d0
	add.l	d0,d0
	Swap	d0		;d0 holds intermediate x coord
	Muls	-$40(a1,d6.w),d2
	Muls	$40(a1,d6.w),d3
	Add.l	d3,d2
	add.l	d2,d2
	Swap	d2		;d2 holds intermediate y coord
	Move	d2,d4

	Move	(a4)+,d1	;z coord
	Move	d1,d3
	Move	Xrot(pc),d6
	Muls	$40(a1,d6.w),d2
	Muls	-$40(a1,d6.w),d1
	Sub.l	d1,d2
	add.l	d2,d2
	Swap	d2		;d2 holds the final y coord

	Muls	$40(a1,d6.w),d3
	Muls	-$40(a1,d6.w),d4
	Add.l	d4,d3
	add.l	d3,d3
	Swap	d3		;d3 holds intermediate z coord

	Move	d0,d1
	Move	d3,d4
	Move	Yrot(pc),d6
	Muls	$40(a1,d6.w),d3
	Muls	-$40(a1,d6.w),d0
	Sub.l	d0,d3
	add.l	d3,d3
	Swap	d3		;d3 holds the final z coord
	Muls	-$40(a1,d6.w),d4
	Muls	$40(a1,d6.w),d1
	Add.l	d4,d1
	add.l	d1,d1
	Swap	d1		;d1 holds the final x coord

	Add	#230,d3
	And	#$7fe,d3
	Move	(a3,d3.w),d5
	Muls	d5,d1
	Muls	d5,d2
	add.l	d1,d1
	Swap	d1
	Add	#centreX,d1
	add.l	d2,d2
	Swap	d2

	Add	#centreY,d2
	move	d1,(a2)+
	move	d2,(a2)+
	Dbf	d7,ROT8


	Move	main_num+2(pc),d7
	Lea	main_connect(pc),a3
	Lea	Rotated_coords(pc),a4
	move	#80,d0
	Move.l	d0,a1
B_wait2:Btst	#14,2(a6)
	Bne.s	B_wait2

	Move	#$ffff,Bltafwm(a6)
	Move	a1,$60(a6)	;Bltcmod
	Move	a1,$66(a6)	;Bltdmod
texture:Move	#-1,$72(a6)	;Bltbdat


Draw_loop:
	Move	(a3)+,d6
	add	d6,d6
	add	d6,d6
	Move	(a4,d6.w),d0
	Move	2(a4,d6.w),d1
	Move	(a3)+,d6
	add	d6,d6
	add	d6,d6
	Move	(a4,d6.w),d2
	Move	2(a4,d6.w),d3

	asr	#1,d1
	asr	#1,d3

	Cmp	d0,d2
	Bne.s	Draw
	Cmp	d1,d3
	Beq.s	Nodraw
Draw:	Bsr	Line
Nodraw:	Dbf	d7,Draw_loop
Bwit:	Btst	#14,2(a6)
	Bne.s	Bwit
;	bsr	fill
	Rts

;----------- Line Draw ------------
Line:	Move	a1,d4
	Mulu	d1,d4
	Moveq	#-$10,d5
	And	d0,d5
	Lsr	#3,d5
	Add	d5,d4
	Add.l	a0,d4

	moveq	#0,d5
	Sub	d1,d3
	Roxl.b	#1,d5
	Tst	d3
	Bge.s	Y2gy1
	Neg	d3
Y2gy1:	Sub	d0,d2
	Roxl.b	#1,d5
	Tst	d2
	Bge.s	X2gx1
	Neg	d2
X2gx1:	Move	d3,d1
	Sub	d2,d1
	Bge.s	Dygdx
	Exg	d2,d3
Dygdx:	Roxl.b	#1,d5
	Move.b	Octant_table(pc,d5),d5
	Add	d2,d2
Wblit:	Btst	#14,2(a6)
	Bne.s	Wblit

	move	d2,$62(a6)	;Bltbmod
	sub	d3,d2
	bge.s	Signn1
	or.b	#$40,d5		;make $42 for filling
Signn1:	move	d2,$52(a6)	;Bltaptl
	sub	d3,d2
	move	d2,$64(a6)	;Bltamod

	move	#$8000,$74(a6)	;Bltadat
	and	#$f,d0
	ror.w	#4,d0
	or	#$0bca,d0
	move	d0,$40(a6)	;Bltcon0
	move	d5,$42(a6)	;Bltcon1
	move.l	d4,$48(a6)	;Bltcpth
	move.l	d4,$54(a6)	;Bltdpth
	lsl	#6,d3
	addq	#2,d3
	move	d3,$58(a6)	;Bltsize
	rts
******************************************
Octant_table:	Dc.b	1,17,9,21,5,25,13,29
*нннннннннннннннннннннннннннннннннннннннн*
objectNO:	dc.w	0
newobject:	moveq	#0,d0
		move	objectno(pc),d0
		lsl.l	#2,d0
		lea	main_points(pc),a1
		lea	main_connect(pc),a2
		lea	object_ptr(pc),a0
		move.l	(a0,d0.w),a0
		move	(a0),d0
		move	2(a0),d1
		subq	#1,d0
		subq	#1,d1
		move	d0,-4(a1)
		move	d1,-2(a1)
		addq.l	#4,a0
copyobj_p:	 move	(a0)+,(a1)+
		 move	(a0)+,(a1)+
		 move	(a0)+,(a1)+
		dbf	d0,copyobj_p
copyobj_c:	 move	(a0)+,(a2)+
		 move	(a0)+,(a2)+
		dbf	d1,copyobj_c
		rts

**********  vars ************
Xrot:		Dc.w	$100
Yrot:		Dc.w	0
Zrot:		Dc.w	0


scrollhigh = 8	 ; pixels
scrollwidth = 24 ; words
scrolDEL:	dc.w 3
scrolOFF:	dc.w 0
scrolVAR1:	dc.w 14,3
scrolSPD:	dc.w 1
doscroll:
	moveq	#14,d3
	lea	scrollarea+2(pc),a0
	lea	scrollarea(pc),a1
	blit	0,0,scrollhigh*64+scrollwidth

	tst.w	scrolDEL		;test for time to put new char
	beq.s	doscroly
	subq.w	#1,scrolDEL
	rts

doscroly:
	move	#3,scrolDEL			;read char and
	moveq	#0,d0				;check for funct..
	moveq	#0,d1
	moveq	#0,d2
	move.w	scrolOFF,d0			;then blit char..
	addq.w	#1,scrolOFF
	lea	text(pc),a0
	move.b	2(a0,d0.w),d1
	move.b	1(a0,d0.w),d2
	move.b	0(a0,d0.w),d0
	and	#$7f,d0
	bne.s	notzero
	moveq	#32,d0
	clr	scrolOFF

notzero cmp.b	#'#',d0		; key  ' # ' for vect
	bne.s	notPAUSE
	sub.b	#'0',d1
	sub.b	#'0',d2
	and	#15,d1
	and	#15,d2
	mulu	#10,d2
	add	d2,d1
	move	d1,objectNO
	addq	#2,scrolOFF
	move	#1,objectDEL
	moveq	#32,d0
notPAUSE
	cmp.b	#10,d0
	bne.s	notCR
	moveq	#32,d0
notCR:	sub.b	#32,d0		;sub 32 to get relative offset
	lsl	#3,d0		;multiply by 32 for address of char..
	lea	font(pc),a0
	add.l	d0,a0
	lea	scrollarea+40(pc),a1
	moveq	#7,d0
copychar:			;change to a blit of char.. is bigger!
	move.b	(a0)+,(a1)
	add.l	#48,a1
	dbf	d0,copychar
	rts

booOFF	dc.w	0
booTAB	dc.w	1,3,5,8,10,13,15,18,20,22,24,26
	dc.w	28,30,32,33,34,35,36,37,37,38,38,38
	dc.w	38,37,37,36,35,34,33,32,30,28,26,24
	dc.w	22,20,18,15,13,10,8,5,3,2,1

dobounce
	move	booOFF(pc),d0
	lea	booTAB(pc),a0
	add	d0,d0
	move	(a0,d0.w),d0
	moveq	#40,d1
	sub	d0,d1
	move	d1,d0
	move.b	d0,xx1
	addq	#8,d0
	move.b	d0,xx2
	cmp	#45,booOFF
	ble.s	notKK
	clr	booOFF
notKK	addq	#1,boooff
	rts


scrollarea:	blk.b 48*scrollhigh,0

;********** Copperlists ***********
Copper:
 dc.l $008e2826,$009039d8,$00920038,$009400d0,$01080050
 dc.l $010a0050,$01003300,$01800000
 dc.l $01200000,$01220000,$01240000,$01260000
 dc.l $01280000,$012a0000,$012c0000,$012e0000
 dc.l $01300000,$01320000,$01340000,$01360000
 dc.l $01380000,$013a0000,$013c0000,$013e0000

puk1
 dc.l $01800000,$01820000,$01840000,$01860000,$01880000
 dc.l $018a0000,$018c0000,$018e0000
yak1
 dc.l $00e00000,$00e20000,$00e40000,$00e60000,$00e80000,$00ea0000


 dc.l $5f01fffe,$01800adf,$00e20028,$00e00007,$01820fff,$01080000
 dc.l $6009fffe,$01009300,$01800003
 dc.l $e009fffe,$01000300,$01800adf
 dc.l $e109fffe,$01800000

 dc.l $ffdffffe

xx1    dc.l $0009fffe
cap	dc.l $00e00000,$00e20000,$01820fff,$01001300,$01080008
xx2    dc.l $0709fffe,$01000300

 dc.l $3809fffe,$8a0000,-2

;--------- Vector coords !! -------------
; X , Y , Z
main_num:	dc.w	13,18	;actual nmbers  -1
main_points:	blk.w	max*6,0
main_connect:	blk.l	max,0	;connectors connect coord c1 --> c2
object_ptr:	dc.l	disk,ship,cube,prism,saff

*ннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн*
ship:	dc.w	14,19		;# of points,# of connect lines
ship_p:	Dc.w	0,110,0		; x , y , z
	Dc.w	0,50,0
	Dc.w	18,57,0		;wing front
	Dc.w	128,-75,0	;wing back
	Dc.w	45,-52,0
	Dc.w	35,-55,0
	Dc.w	25,-44,-15
	Dc.w	-25,-44,-15
	Dc.w	-35,-55,0
	Dc.w	-45,-52,0
	Dc.w	-128,-75,0	;wing back
	Dc.w	-18,57,0	;wing front
	Dc.w	27,-52,10
	Dc.w	-27,-52,10
ship_c:	Dc.w	0,1,1,2,2,3,3,4
	Dc.w	4,5,5,6,6,7,7,8
	Dc.w	8,9,9,10,10,11,11,1
	Dc.w	5,12,12,13,13,8,12,1
	Dc.w	13,1,6,1,7,1

cube:	dc.w	12,14
cube_p:	dc.w	-50, 50, 50
	dc.w	 50, 50, 50
	dc.w	 50,-50, 50
	dc.w	-50,-50, 50
	dc.w	-50, 50,-50
	dc.w	 50, 50,-50
	dc.w	 50,-50,-50
	dc.w	-50,-50,-50
	dc.w	0,0,50
	dc.w	0,0,170
	dc.w	0,0,-50
	dc.w	0,0,-170
cube_c:	dc.w	0,1 ,0,3 ,0,4
	dc.w	2,1 ,2,6 ,2,3
	dc.w	7,3 ,7,4 ,7,6
	dc.w	5,4 ,5,1 ,5,6
	dc.w	8,9 ,10,11

prism:	dc.w	4,6
prism_p:dc.w	 0,110, 0
	dc.w	-70, 0, 70
	dc.w	 70, 0, 70
	dc.w	  0, 0,-70
prism_c:dc.w 0,1 ,0,3 ,0,2
	dc.w 3,2 ,3,1 ,2,1

disk:	dc.w	22,25
disk_p:
	dc.w	-80,80,0
	dc.w	80,80,0
	dc.w	80,-80,0
	dc.w	-70,-80,0
	dc.w	-80,-70,0
	dc.w	-80,80,8	;
	dc.w	80,80,8
	dc.w	80,-80,8
	dc.w	-70,-80,8
	dc.w	-80,-70,8
	dc.w	72,75,8
	dc.w	78,75,8
	dc.w	78,69,8
	dc.w	72,69,8
	dc.w	-68,80,8
	dc.w	-68,-20,8
	dc.w	68,-20,8
	dc.w	68,80,8

	dc.w	-50,-80,8
	dc.w	-50,-30,8
	dc.w	50,-30,8
	dc.w	50,-80,8

disk_c:	dc.w	0,1,1,2,2,3,3,4,5,6,6,7,7,8,8,9
	dc.w	0,5,1,6,2,7,3,8,4,9,0,4,5,9
	dc.w	10,11,11,12,12,13,13,10
	dc.w	14,15,15,16,16,17
	dc.w	18,19,19,20,20,21

saff:	dc.w	14,22
saff_p	dc.w	-50,50,50
	dc.w	50,50,50
	dc.w	-50,-50,50
	dc.w	50,-50,50
	dc.w	-50,-50,-50
	dc.w	50,-50,-50
	dc.w	50,50,-50
	dc.w	-50,50,-50
	dc.w	-150,0,0
	dc.w	150,0,0
	dc.w	0,-170,0
	dc.w	0,-50,0
	dc.w	0,50,0
	dc.w	0,170,0

	dc.w	0,1 ,1,3 ,3,2 ,2,0
	dc.w	4,5 ,5,6 ,6,7 ,7,4
	dc.w	0,7 ,1,6 ,5,3 ,4,2
	dc.w	8,0 ,8,7 ,8,4 ,8,2
	dc.w	9,1 ,9,6 ,9,5 ,9,3
	dc.w	10,11 ,12,13 	


*ннннннннннннннннннннннннннн*
Rotated_coords:		ds.l	max,0
Sintable:		ds.b	1024,0
Persp:			ds.w	1024,0
font			ds.b	768,0
text			ds.b	7000,0
*нннннннннннннннннннннннн*
doscrn	lea	bps1(pc),a0
	move.l	#pic1,d0
	move.l	#pic1-40,d1
	move	d0,6(a0)
	swap	d0
	move	d0,2(a0)
	addq.l	#8,a0
	move	d1,6(a0)
	swap	d1
	move	d1,2(a0)

	move.l	#copper3,$dff084
	clr	$dff08a

tsta1	btst	#6,$bfe001
	bne.s	tsta1

	lea	bps2(pc),a0
	move.l	#pic2+64,d0
	moveq	#4,d1
mkbps2	move	d0,6(a0)
	swap	d0
	move	d0,2(a0)
	swap	d0
	addq.l	#8,a0
	add.l	#40,d0
	dbf	d1,mkbps2

	lea	pic2(pc),a0
	lea	$dff180,a1
	moveq	#31,d1
mkcol2	move	(a0)+,(a1)+
	dbf	d1,mkcol2

	move.l	#copper4,$dff084
	clr	$dff08a

tsta2	btst	#10,$dff016
	bne.s	tsta2

	rts

copper3
 dc.l $008e3060,$009030d8,$00920038,$009400d0,$01020001
 dc.l $01080000,$010a0000,$01002300
 dc.l $01200000,$01220000,$01240000,$01260000
 dc.l $01280000,$012a0000,$012c0000,$012e0000
 dc.l $01300000,$01320000,$01340000,$01360000
 dc.l $01380000,$013a0000,$013c0000,$013e0000
bps1
 dc.l $00e00000,$00e20000,$00e40000,$00e60000
 dc.l $01800000,$01820fff,$01860ccc,$01840444
 dc.l $ffdffffe,$3809fffe,$008a0000,-2

copper4
 dc.l $008e3060,$009030d8,$00920038,$009400d0,$01020000
 dc.l $010800a0,$010a00a0,$01005300,$01800000
 dc.l $01200000,$01220000,$01240000,$01260000
 dc.l $01280000,$012a0000,$012c0000,$012e0000
 dc.l $01300000,$01320000,$01340000,$01360000
 dc.l $01380000,$013a0000,$013c0000,$013e0000
bps2
 dc.l $00e00000,$00e20000,$00e40000,$00e60000
 dc.l $00e80000,$00ea0000,$00ec0000,$00ee0000
 dc.l $00f00000,$00f20000
 dc.l $ffdffffe,$3809fffe,$008a0000,-2

			ds.b	40,0
pic1			ds.b	40*256,0
pic2			ds.b	40*256*5+64,0
logo			ds.b	40*66*3+16,0


;  NoisetrackerV1.0 replayroutine 
;  Mahoney & Kaktus - HALLONSOFT 1989
;  Speeded up by - QUASAR / DAWN-ACU

Stracker   = $438		;SoundTracker 2.3+
Ntracker   = $43c		;NoiseTracker 1.0+
track      = Ntracker		;SoundTracker Type to be used

********	********
playit:	bsr.S	mt_init		;play testing routine here
	lea	$dff000,a6
testVB	cmp.b	#$48,$6(a6)
	bne.s	testVB
	move	#$14f,$180(a6)
	bsr	mt_music
	clr	$180(a6)
testVB1 cmp.b	#$70,$6(a6)
	bne.s	testVB1
	move	#$ff0,$180(a6)
	move	mt_specDAT1,testVB2+2
testVB2 cmp.b	#0,$6(a6)
	bne.s	testVB2
	move	#$f00,$180(a6)
	btst	#6,$bfe001
	bne.s	testVB
	rts
********	********


;				jump here to set it all up
mt_init move	#$800f,$dff096
	lea	mt_data(pc),a0
	move.b	#$78,$3b7(a0)
	move.l	a0,a1
	add.l	#$3b8,a1
	moveq	#$7f,d0
	moveq	#0,d1
mt_loop move.l	d1,d2
	subq	#1,d0
mt_loop2
	move.b	(a1)+,d1
	cmp.b	d2,d1
	bgt.s	mt_loop
	dbf	d0,mt_loop2
	addq.b	#1,d2

	lea	mt_samplestarts(pc),a1
	asl.l	#8,d2			;mulu #64*16,d2
	asl.l	#2,d2
	add.l	#track,d2
	add.l	a0,d2
	move.l	d2,a2
	moveq	#30,d0
mt_loop3
	clr.l	(a2)
	move.l	a2,(a1)+
	moveq	#0,d1
	move	42(a0),d1
	add.l	d1,d1		;mulu #2,d1
	add.l	d1,a2
	add.l	#30,a0
	dbf	d0,mt_loop3

	or.b	#2,$bfe001
	move.b	#6,mt_speed
	lea	$dff000,a6
	clr	$a8(a6)
	clr	$b8(a6)
	clr	$c8(a6)
	clr	$d8(a6)
	clr.b	mt_songpos
	clr.b	mt_counter
	clr	mt_pattpos
	rts

mt_end				;jump here at end
	lea	$dff000,a6
	clr	$a8(a6)
	clr	$b8(a6)
	clr	$c8(a6)
	clr	$d8(a6)
	move	#$f,$96(a6)
	rts

mt_music			;jump here every VB frame
	movem.l	d0-d4/a0-a3/a5-a6,-(a7)
	lea	mt_data(pc),a0
	addq.b	#1,mt_counter
	move.b	mt_counter(pc),D0
	cmp.b	mt_speed(pc),D0
	blt.s	mt_nonew
	clr.b	mt_counter
	bra	mt_getnew

mt_nonew
	lea	mt_voice1(pc),a6
	lea	$dff0a0,a5
	jsr	mt_checkcom(pc)
	lea	mt_voice2(pc),a6
	lea	$dff0b0,a5
	jsr	mt_checkcom(pc)
	lea	mt_voice3(pc),a6
	lea	$dff0c0,a5
	jsr	mt_checkcom(pc)
	lea	mt_voice4(pc),a6
	lea	$dff0d0,a5
	jsr	mt_checkcom(pc)
	bra	mt_endr

mt_arpeggio
	moveq	#0,d0
	move.b	mt_counter(pc),d0
	divs	#3,d0
	swap	d0
	tst	d0
	beq.s	mt_arp2
	cmp	#2,d0
	beq.s	mt_arp1

	moveq	#0,d0
	move.b	3(a6),d0
	lsr.b	#4,d0
	bra.s	mt_arp3
mt_arp1 moveq	#0,d0
	move.b	3(a6),d0
	and.b	#$f,d0
	bra.s	mt_arp3
mt_arp2 move	16(a6),d2
	bra.s	mt_arp4
mt_arp3 asl	#1,d0
	moveq	#0,d1
	move	$10(a6),d1
	lea	mt_periods(pc),a0
	moveq	#36,d7
mt_arploop
	move	(a0,d0.w),d2
	cmp	(a0)+,d1
	bge.s	mt_arp4
	dbf	d7,mt_arploop
	rts
mt_arp4 move	d2,6(a5)
	rts

mt_getnew
	lea	mt_data(pc),a0
	move.l	a0,a3
	move.l	a0,a2
	add.l	#12,a3
	add.l	#$3b8,a2
	add.l	#track,a0

	moveq	#0,d0
	move.l	d0,d1
	move.b	mt_songpos(pc),d0
	move.b	(a2,d0.w),d1
	asl.l	#8,d1
	asl.l	#2,d1
	add	mt_pattpos(pc),d1
	clr	mt_dmacon

	lea	$dff0a0,a5
	lea	mt_voice1(pc),a6
	bsr.s	mt_playvoice
	lea	$dff0b0,a5
	lea	mt_voice2(pc),a6
	bsr.s	mt_playvoice
	lea	$dff0c0,a5
	lea	mt_voice3(pc),a6
	bsr.s	mt_playvoice
	lea	$dff0d0,a5
	lea	mt_voice4(pc),a6
	bsr.s	mt_playvoice
	bra	mt_setdma

mt_playvoice
	move.l	(a0,d1.l),(a6)
	addq.l	#4,d1
	moveq	#0,d2
	move.b	2(a6),d2
	and.b	#$f0,d2
	lsr.b	#4,d2
	move.b	(a6),d0
	and.b	#$f0,d0
	or.b	d0,d2
	tst.b	d2
	beq.s	mt_setregs
	moveq	#0,d3
	lea	mt_samplestarts(pc),a1
	move.l	d2,d4
	subq.l	#1,d2
	asl.l	#2,d2
	mulu	#30,d4
	move.l	(a1,d2.l),$4(a6)
	move	(a3,d4.l),$8(a6)
	move	$2(a3,d4.l),$12(a6)
	move	$4(a3,d4.l),d3
	tst	d3
	beq.s	mt_noloop
	move.l	4(a6),d2
	asl	#1,d3
	add.l	d3,d2
	move.l	d2,$a(a6)
	move	4(a3,d4.l),d0
	add	6(a3,d4.l),d0
	move	d0,8(a6)
	move	6(a3,d4.l),14(a6)
	move	18(a6),8(a5)
	bra.s	mt_setregs
mt_noloop
	move.l	4(a6),d2
	add.l	d3,d2
	move.l	d2,10(a6)
	move	6(a3,d4.l),14(a6)
	move	18(a6),8(a5)
mt_setregs
	move	(a6),d0
	and	#$fff,d0
	beq	mt_checkcom2
	move.b	2(a6),d0
	and.b	#$F,d0
	cmp.b	#3,d0
	bne.s	mt_setperiod
	bsr	mt_setmyport
	bra	mt_checkcom2
mt_setperiod
	move	(a6),16(a6)
	and	#$fff,16(a6)
	move	20(a6),$dff096
	clr.b	$1b(a6)
	move.l	4(a6),(a5)
	move	$8(a6),$4(a5)
	move	16(a6),d0
	and	#$fff,d0
	move	d0,6(a5)
	move	20(a6),d0
	or	d0,mt_dmacon
	bra	mt_checkcom2

mt_setdma
	lea	$dff000,a5
	move	mt_dmacon(pc),d0
	or	#$8000,d0
	move	#$a0,d1
mt_del	dbf	d1,mt_del
	move	d0,$96(a5)
	lea	mt_voice4(pc),a6
	move.l	$a(a6),$d0(a5)
	move	$e(a6),$d4(a5)
	lea	mt_voice3(pc),a6
	move.l	$a(a6),$c0(a5)
	move	$e(a6),$c4(a5)
	lea	mt_voice2(pc),a6
	move.l	$a(a6),$b0(a5)
	move	$e(a6),$b4(a5)
	lea	mt_voice1(pc),a6
	move.l	$a(a6),$a0(a5)
	move	$e(a6),$a4(a5)
	add	#16,mt_pattpos		;get to next line
	cmp	#64*16,mt_pattpos
	bne.s	mt_endr
mt_nex	clr	mt_pattpos
	clr.b	mt_break
	addq.b	#1,mt_songpos
	and.b	#$7f,mt_songpos
	move.b	mt_songpos(pc),d1
	cmp.b	mt_data+$3b6,d1
	bne.s	mt_endr
	clr.b	mt_songpos
mt_endr tst.b	mt_break
	bne.s	mt_nex

	moveq	#3,d0		;4 channels
	move	mt_specADD,d1
	move	mt_specLOW,d2
	move	mt_specMAX,d3
	lea	mt_specDAT1(pc),a5
	lea	mt_voice1(pc),a6
mt_spec tst	(a6)		;if instrument used then..
	beq.s	mt_yes1		;  move D3 into spec
	move	d3,(a5)
mt_yes1 sub	d1,(a5)		;de-crease spec
	move	(a5),d4
	cmp	d2,d4		;if under D2 then..
	bge.s	mt_not1		;  put D2 into spec
	move	d2,(a5)
mt_not1 addq.l	#2,a5		;increase address to next..
	add.l	#28,a6
	dbf	d0,mt_spec
	movem.l	(a7)+,d0-d4/a0-a3/a5-a6
	rts
;		spectro analyzer variables .......
mt_specADD	dc.w 2		;adding var
mt_specLOW	dc.w $a1	;groung level var
mt_specMAX	dc.w $e1	;max value
mt_specDAT1	dc.w 0
mt_specDAT2	dc.w 0
mt_specDAT3	dc.w 0
mt_specDAT4	dc.w 0

mt_setmyport
	move	(a6),d2
	and	#$fff,d2
	move	d2,$18(a6)
	move	$10(a6),d0
	clr.b	$16(a6)
	cmp	d0,d2
	beq.s	mt_clrport
	bge.s	mt_rt
	move.b	#$1,$16(a6)
	rts
mt_clrport 
	clr	$18(a6)
mt_rt	rts

mt_myport
	move.b	$3(a6),d0
	beq.s	mt_myslide
	move.b	d0,$17(a6)
	clr.b	$3(a6)
mt_myslide
	tst	$18(a6)
	beq.s	mt_rt
	moveq	#0,d0
	move.b	$17(a6),d0
	tst.b	$16(a6)
	bne.s	mt_mysub
	add	d0,$10(a6)
	move	$18(a6),d0
	cmp	$10(a6),d0
	bgt.s	mt_myok
	move	$18(a6),$10(a6)
	clr	$18(a6)
mt_myok move	$10(a6),$6(a5)
	rts
mt_mysub
	sub	d0,$10(a6)
	move	$18(a6),d0
	cmp	$10(a6),d0
	blt.s	mt_myok
	move	$18(a6),$10(a6)
	clr	$18(a6)
	move	$10(a6),$6(a5)
	rts

mt_vib	move.b	$3(a6),d0
	beq.s	mt_vi
	move.b	d0,$1a(a6)

mt_vi	move.b	$1b(a6),d0
	lea	mt_sin(pc),a4
	lsr	#2,d0
	and	#$1f,d0
	moveq	#0,d2
	move.b	(a4,d0.w),d2
	move.b	$1a(a6),d0
	and	#$f,d0
	mulu	d0,d2
	lsr	#6,d2
	move	$10(a6),d0
	tst.b	$1b(a6)
	bmi.s	mt_vibmin
	add	d2,d0
	bra.s	mt_vib2
mt_vibmin
	sub	d2,d0
mt_vib2 move	d0,$6(a5)
	move.b	$1a(a6),d0
	lsr	#2,d0
	and	#$3c,d0
	add.b	d0,$1b(a6)
	rts

mt_nop	move	$10(a6),$6(a5)
	rts

mt_checkcom
	move	$2(a6),d0
	and	#$fff,d0
	beq.s	mt_nop
	move.b	$2(a6),d0
	and.b	#$f,d0
	tst.b	d0
	beq	mt_arpeggio
	cmp.b	#1,d0
	beq.s	mt_portup
	cmp.b	#2,d0
	beq	mt_portdown
	cmp.b	#3,d0
	beq	mt_myport
	cmp.b	#4,d0
	beq	mt_vib
	move	$10(a6),$6(a5)
	cmp.b	#$a,d0
	beq.s	mt_volslide
	rts

mt_volslide
	moveq	#0,d0
	move.b	$3(a6),d0
	lsr.b	#4,d0
	tst.b	d0
	beq.s	mt_voldown
	add	d0,$12(a6)
	cmp	#64,$12(a6)
	bmi.s	mt_vol2
	move	#64,$12(a6)
mt_vol2 move	$12(a6),$8(a5)
	rts

mt_voldown
	moveq	#0,d0
	move.b	$3(a6),d0
	and.b	#$f,d0
	sub	d0,$12(a6)
	bpl.s	mt_vol3
	clr	$12(a6)
mt_vol3 move	$12(a6),$8(a5)
	rts

mt_portup
	moveq	#0,d0
	move.b	$3(a6),d0
	sub	d0,$10(a6)
	move	$10(a6),d0
	and	#$fff,d0
	cmp	#$71,d0
	bpl.s	mt_por2
	and	#$f000,$10(a6)
	or	#$71,$10(a6)
mt_por2 move	$10(a6),d0
	and	#$fff,d0
	move	d0,$6(a5)
	rts

mt_portdown
	moveq	#0,d0
	move.b	$3(a6),d0
	add	d0,$10(a6)
	move	$10(a6),d0
	and	#$fff,d0
	cmp	#$358,d0
	bmi.s	mt_por3
	and	#$f000,$10(a6)
	or	#$358,$10(a6)
mt_por3 move	$10(a6),d0
	and	#$fff,d0
	move	d0,$6(a5)
	rts

mt_checkcom2
	move.b	$2(a6),d0
	and.b	#$f,d0
	cmp.b	#$e,d0
	beq.s	mt_setfilt
	cmp.b	#$d,d0
	beq.s	mt_pattbreak
	cmp.b	#$b,d0
	beq.s	mt_posjmp
	cmp.b	#$c,d0
	beq.s	mt_setvol
	cmp.b	#$f,d0
	beq.s	mt_setspeed
	rts

mt_setfilt
	move.b	$3(a6),d0
	and.b	#$1,d0
	asl.b	#$1,d0
	and.b	#$fd,$bfe001
	or.b	d0,$bfe001
	rts
mt_pattbreak
	not.b	mt_break
	rts
mt_posjmp
	move.b	$3(a6),d0
	subq.b	#$1,d0
	move.b	d0,mt_songpos
	not.b	mt_break
	rts
mt_setvol
	cmp.b	#64,$3(a6)
	ble.s	mt_vol4
	move.b	#64,$3(a6)
mt_vol4 move.b	$3(a6),$8(a5)
	rts
mt_setspeed
	move.b	$3(a6),d0
	and	#$1f,d0
	beq.s	mt_rts2
	clr.b	mt_counter
	move.b	d0,mt_speed
mt_rts2 rts

mt_sin
dc.b $00,$18,$31,$4a,$61,$78,$8d,$a1,$b4,$c5,$d4,$e0,$eb,$f4,$fa,$fd
dc.b $ff,$fd,$fa,$f4,$eb,$e0,$d4,$c5,$b4,$a1,$8d,$78,$61,$4a,$31,$18

mt_periods
dc.w $0358,$0328,$02fa,$02d0,$02a6,$0280,$025c,$023a,$021a,$01fc,$01e0
dc.w $01c5,$01ac,$0194,$017d,$0168,$0153,$0140,$012e,$011d,$010d,$00fe
dc.w $00f0,$00e2,$00d6,$00ca,$00be,$00b4,$00aa,$00a0,$0097,$008f,$0087
dc.w $007f,$0078,$0071,$0070,$006f

mt_speed	dc.b	$6
mt_songpos	dc.b	$0
mt_pattpos	dc.w	$0
mt_counter	dc.b	$0

mt_break	dc.b	$0
mt_dmacon	dc.w	$0
mt_samplestarts ds.l	32,0
mt_voice1	ds.l	5,0
		dc.w	1,0,0,0
mt_voice2	ds.l	5,0
		dc.w	2,0,0,0
mt_voice3	ds.l	5,0
		dc.w	4,0,0,0
mt_voice4	ds.l	5,0
		dc.w	8,0,0,0
mt_data		ds.w	110000/2,0

>extern 'df0:bin/sin',Sintable,-1
>extern 'df0:bin/sin',Sintable+512,-1
>extern 'df0:bin/persp',persp,-1
>extern 'df0:real3d.txt',text,-1
>extern 'df0:real3d.p2',pic1,-1
>extern 'df0:real3d.logo',logo,-1
>extern 'df0:real3d.p',pic2,-1
>extern 'df0:f/star',font,-1
;>extern "df0:mod.tg2",mt_data,-1   ;only SEKA HERE

