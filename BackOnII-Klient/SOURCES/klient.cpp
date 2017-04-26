#include <cstdio>
#include <iostream>
#include <cstring>
#include <cstdlib>
//#include <pthread.h>
#include <unistd.h>
#include <limits.h>
#include <netdb.h>
//#include <csignal>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>

#include <arpa/inet.h>

#include "basic_functions.h"
#include "err.h"
#include "3des.h"

#define PASSWORD_LEN 5
#define FAST_DEBUG 0

//00010203040FFA9708090A0B0C0D0E0F0001020304050607
//0901020304050607

using namespace std;

//int PROGRAM_TYPE;					//0-klient,1-usługa
char* PORT;
char* SERVER_NAME;
int sockTCP;
struct sysinfo system_info;
FILE *glob_file;
unsigned char *key,*iv;

/*
static void catch_int (int sig) {
	close(sockTCP);
	fprintf(stderr,"signal\n");
	exit(1);
}
*/

void connect_TCP(){
	struct addrinfo addr_hintsTCP;
	int err;
	struct addrinfo *addr_result;
	bool connected=false;
	memset(&addr_hintsTCP, 0, sizeof(struct addrinfo));
	addr_hintsTCP.ai_family = AF_UNSPEC;
	addr_hintsTCP.ai_socktype = SOCK_STREAM;
	addr_hintsTCP.ai_protocol = IPPROTO_TCP;
	while(!connected){
		err = getaddrinfo(SERVER_NAME, PORT, &addr_hintsTCP, &addr_result);
		if (err != 0){
			syserr("getaddrinfo: %s\n", gai_strerror(err));
		}
		sockTCP = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
		if(sockTCP < 0){
			syserr("socketTCP,client");
		}
		
		if(connect(sockTCP, addr_result->ai_addr, addr_result->ai_addrlen) < 0){			//TODO ustawić timeout jak się nie może połączyć
			usleep(500000);
			fprintf(stderr,"connection attepmt failed\n");
		}else{
			connected=true;
		}
	}
}

string receive_TCP(){
	ssize_t len;
	char buffer[1000];
	memset(buffer, 0, sizeof(buffer));
	len = read(sockTCP, buffer, sizeof(buffer)-1);
	if(len < 0){
		syserr("receive error");
	}
	string ret(buffer);
	return ret;
}

void send_TCP_message(string message){
	int lenTCP=message.length();
	if(write(sockTCP,message.c_str(),lenTCP)!=lenTCP){
		fprintf(stderr,"message send error\n");
	}
}


void read_hex_from_file(unsigned char * dest){
	char buff[100];
	fscanf(glob_file,"%s",buff);				//TODO sprawdzić czy to jest hex i czy dobrej długości
	string temp_str(buff);
	temp_str=from_hex(temp_str);
	copy(temp_str.begin(),temp_str.end(),dest);
}

int common_start(){
	srand(time(NULL));
	glob_file=fopen("global_data","r+");
	if(glob_file==NULL){
		fprintf(stderr,"cannot open global data file\n");
		return 1;
	}
	key=(unsigned char *)malloc(24*sizeof(unsigned char));								//stałe wielkości-> zmienić na tablicę+ wskaźnik
	iv=(unsigned char *)malloc(8*sizeof(unsigned char));
	SERVER_NAME=(char*)malloc(INET_ADDRSTRLEN*sizeof(char));
	PORT=(char*)malloc(10*sizeof(char)); 
	read_hex_from_file(key);																//TODO weryfikacja poprawności pliku
	read_hex_from_file(iv);
	initialize_3des();
	return 0;
}



int klient(){
	char login[1000],password[1000],hostname[HOST_NAME_MAX],service_password[PASSWORD_LEN];			//TODO zapytać o te długości
	int fresh_start;
	fscanf(glob_file,"%s",SERVER_NAME);
	fresh_start=(strlen(SERVER_NAME)==0);
	if(!fresh_start){								//tak na prawdę sprawdzenie pustości linijki roboczej
		do{
			printf("Usługa jest już zarejestrowana, lub zmieniony został plik z danymi\n");
			printf("Spróbować dokonać ponownej rejestracji (R), kontynuować z aktualnymi danymi (K), czy przerwać działanie programu (P)?\n");
			printf("(ponowna rejestracja nadpisze stare dane rejestracji usługi)\n");
			scanf("%s",login);
		}while((strcmp(login,"R")!=0)&&(strcmp(login,"K")!=0)&&(strcmp(login,"P")!=0));
		if(strcmp(login,"P")==0){
			return 1;
		}else if(strcmp(login,"K")==0){
			return 0;
		}
	}
	if(FAST_DEBUG){
		SERVER_NAME=(char *)"10.0.2.2";
		PORT=(char *)"6000";	
	}else{		
		//wczytywanie adresu i portu
		struct sockaddr_in sa;
		SERVER_NAME=(char*)malloc(INET_ADDRSTRLEN*sizeof(char));
		printf("proszę podać adres ip serwera BackOnII\n");
		scanf("%s",SERVER_NAME);												//TODO zadbać o nieprzepełnienie bufora we wszystkich scanf
		while(inet_pton(AF_INET,SERVER_NAME,&(sa.sin_addr))!=1){
			printf("to nie jest poprawny adres ip_v4,\nproszę podać poprawny\n");
			scanf("%s",SERVER_NAME);
		}
		int temp;
		char temp2;
		printf("proszę podać numer portu serwera BackOnII\n");
		while(((scanf("%d%c",&temp,&temp2)!=2 || temp2!='\n')&& clean_stdin())|| (temp<0) || (temp>65535)){
			printf("to nie jest poprawny numer portu ip_v4\nproszę podać poprawny\n");
		}
		PORT=(char*)malloc(10*sizeof(char));
		sprintf(PORT,"%d",temp);
	}

	connect_TCP();
	gethostname(hostname,HOST_NAME_MAX);					//TODO to powinno być jednoznaczne, więc może jakiś hostid zamiast hostname
	if(receive_TCP()=="BackOnII Serwer\r\n"){
		if(FAST_DEBUG){
			strcpy(login,"ADMINISTRATOR");
			strcpy(password,"admin");		
		}else{
			//poznanie loginu i hasła
			printf("login:\n");
			scanf("%s",login);
			printf("hasło:\n");
			scanf("%s",password);
		}
	}else{
		syserr("wrong banner");
	}
	unsigned char *plaintext=(unsigned char *)password;
	unsigned char ciphertext[128];
	int ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	string message="Login|21|";	
	message+=char_to_string((char*)hostname)+"|";
	message+=char_to_string((char*)login)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"\r\n";
	cout <<message<<endl;
	send_TCP_message(message);
	if(receive_TCP()!="OK\r\n"){
		syserr("login communication failed");
	}
	string str1=random_password(PASSWORD_LEN);
	copy(str1.begin(),str1.end(),service_password);
	plaintext=(unsigned char *)service_password;
	ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	message="ZKL|";
	message+=char_to_string((char*)hostname)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"\r\n";	
	cout<<message<<endl;
	send_TCP_message(message);
	string message2;
	if((message2=receive_TCP())!="OK\r\n"){
		cerr<<"brak obsługi wiadomości: "<<message2<<endl;		
		syserr("ZKL communication failed");
	}else{
		printf("Komputer został zarejestrowany w systemie BackOnII\n");
	}
	if(fresh_start){fprintf(glob_file,"x");}
	fprintf(glob_file,"\n%s\n",SERVER_NAME);
	fprintf(glob_file,"%s\n",PORT);
	fprintf(glob_file,"%s\n",login);
	fprintf(glob_file,"%s\n",str1.c_str());
	return 0;
}

int usluga(){
	char login[1000],hostname[HOST_NAME_MAX],service_password[PASSWORD_LEN];
	fscanf(glob_file,"%s",SERVER_NAME);						// wczytanie dodatkowej linijki roboczej "xxx"
	fscanf(glob_file,"%s",SERVER_NAME);						//TODO weryfikacja poprawności pliku
	if(strlen(SERVER_NAME)<=0){
		printf("usługa nie została jeszcze zarejestrowana, lub zmieniony został plik z danymi\n");
		return 1;
	}
	fscanf(glob_file,"%s",PORT);
	fscanf(glob_file,"%s",login);
	fscanf(glob_file,"%s",service_password);
	connect_TCP();
	if(receive_TCP()!="BackOnII Serwer\r\n"){	
		syserr("wrong banner");
	}
	gethostname(hostname,HOST_NAME_MAX);
	unsigned char ciphertext[128];
	unsigned char * plaintext=(unsigned char *)service_password;
	int ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	string message="Login|22|";
	message+=char_to_string((char*)hostname)+"|";
	message+=char_to_string((char*)login)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"|2.0.0.1\r\n";
	cout <<message<<endl;
	send_TCP_message(message);
	string message2;
	if((message2=receive_TCP())!="OK\r\n"){
		cout<<message2<<endl;
		syserr("login communication failed");
	}
	message="ZOZ|"+char_to_string((char*)login)+"\r\n";
	for(;;){
		sleep(30);								//TODO może jakaś efektywniejsza forma czekania
		send_TCP_message(message);
		cout<<"message sent"<<endl;
		if((message2=receive_TCP())!="OK\r\n"){
			cerr<<"brak obsługi wiadomości: "<<message2<<endl;
		}
	}
	return 0;
}

int main(){
	if(common_start()) return 1;
	if(PROGRAM_TYPE){
		return usluga();
	}else{
		return klient();
	}
}
