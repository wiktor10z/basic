ifeq ($(N),1)
	N2=2
	TIMES=1
	LEVEL=0
else ifeq ($(N),2)
	N2=4
	TIMES=1
	LEVEL=0
else ifeq ($(N),3)
	N2=8
	TIMES=1
	LEVEL=0
else ifeq ($(N),4)
	N2=16
	TIMES=1
	LEVEL=0
else ifeq ($(N),5)
	N2=32
	TIMES=1
	LEVEL=0
else ifeq ($(N),6)
	N2=64
	TIMES=1
	LEVEL=0
else ifeq ($(N),7)
	N2=128
	TIMES=4
	LEVEL=4
else ifeq ($(N),8)
	N2=256
	TIMES=6
	LEVEL=3
else ifeq ($(N),9)
	N2=512
	TIMES=36
	LEVEL=2
else ifeq ($(N),10)
	N2=1024
	TIMES=12
	LEVEL=2
else ifeq ($(N),11)
	N2=2048
	TIMES=4
	LEVEL=2
else ifeq ($(N),12)
	N2=4096
	TIMES=1
	LEVEL=2
else ifeq ($(N),13)
	N2=8192
	TIMES=700
	LEVEL=1
else ifeq ($(N),14)
	N2=16384
	TIMES=200
	LEVEL=1
else ifeq ($(N),15)
	N2=32768
	TIMES=50
	LEVEL=1
else ifeq ($(N),16)
	N2=65536
	TIMES=13
	LEVEL=1
else ifeq ($(N),17)
	N2=131072
	TIMES=4
	LEVEL=1
else ifeq ($(N),18)
	N2=262144
	TIMES=1
	LEVEL=1
else ifeq ($(N),19)
	N2=524288
	TIMES=50000
	LEVEL=0
else ifeq ($(N),20)
	N2=1048576
	TIMES=25000
	LEVEL=0
else ifeq ($(N),21)
	N2=2097152
	TIMES=13000
	LEVEL=0
else ifeq ($(N),22)
	N2=4194304
	TIMES=7000
	LEVEL=0
else ifeq ($(N),23)
	N2=8388608
	TIMES=3300
	LEVEL=0
else ifeq ($(N),24)
	N2=16777216
	TIMES=1600
	LEVEL=0
else ifeq ($(N),25)
	N2=33554432
	TIMES=1000
	LEVEL=-1
else ifeq ($(N),26)
	N2=67108864
	TIMES=500
	LEVEL=-1
else ifeq ($(N),27)
	N2=134217728
	TIMES=240
	LEVEL=-1
else ifeq ($(N),28)
	N2=268435456
	TIMES=120
	LEVEL=-1
else ifeq ($(N),29)
	N2=536870912
	TIMES=45
	LEVEL=-1
else ifeq ($(N),30)
	N2=1073741824
	TIMES=20
	LEVEL=-1
else ifeq ($(N),31)
	N2=2147483648
	TIMES=1
	LEVEL=-1
else ifeq ($(N),32)
	N2=4294967296
	TIMES=1
	LEVEL=-1
else ifeq ($(N),33)
	N2=8589934592
	TIMES=1
	LEVEL=-1
else ifeq ($(N),34)
	N2=17179869184
	TIMES=1
	LEVEL=-1
endif


all: sciezka

sciezka: sciezka3.cpp
	g++ -std=c++11 -O3 sciezka3.cpp -o sciezka -DN=$(N) -DN2=$(N2) -DTIMES=$(TIMES) -DLEVEL=$(LEVEL)

.PHONY: sciezka
