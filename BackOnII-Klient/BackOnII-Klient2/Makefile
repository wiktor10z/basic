CC = g++

TARGETS = klient usluga service_script

FLAGS = -std=c++0x -pedantic -Wall

all: $(TARGETS)

klient: basic_functions.o err.o 3des.o klient.o
	$(CC) $(FLAGS) basic_functions.o err.o 3des.o klient.o -o klient -lpthread -lcrypto
	
usluga: basic_functions.o err.o 3des.o usluga.o
	$(CC) $(FLAGS) basic_functions.o err.o 3des.o usluga.o -o usluga -lpthread -lcrypto

service_script: service_script.c
	$(CC) $(FLAGS) service_script.c -o service_script 

klient.o: klient.cpp
	$(CC) $(FLAGS) -DPROGRAM_TYPE=0 -c klient.cpp
	
usluga.o: klient.cpp
	$(CC) $(FLAGS) -DPROGRAM_TYPE=1 -c -o usluga.o klient.cpp

basic_functions.o: basic_functions.cpp basic_functions.h
	$(CC) $(FLAGS) -c basic_functions.cpp

err.o: err.c err.h
	$(CC) $(FLAGS) -c err.c
	
3des.o: 3des.c 3des.h
	$(CC) $(FLAGS) -c 3des.c -lcrypto

clean:
	rm -f *.o $(TARGETS)
