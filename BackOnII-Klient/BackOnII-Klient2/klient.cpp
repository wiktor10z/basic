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
string login_message;

/*
static void catch_int (int sig) {
	close(sockTCP);
	fprintf(stderr,"signal\n");
	exit(1);
}
*/
const char* install_script="\
OS=$(lsb_release -si)\n\
VER=$(lsb_release -sr)\n\
if [ $OS = \"Ubuntu\" ]; then\n\
		mkdir -p /opt/BackOnII-Klient\n\
		cp usluga /opt/BackOnII-Klient\n\
		cp global_data /opt/BackOnII-Klient\n\
	if dpkg --compare-versions $VER \"gt\" \"14.10\" ; then\n\
		cp service_script /opt/BackOnII-Klient\n\
		cp -r BackOnII-Klient.service /etc/systemd/system/BackOnII-Klient.service\n\
		systemctl enable BackOnII-Klient\n\
		systemctl start BackOnII-Klient\n\
	else\n\
		cp -r BackOnII-Klient.conf /etc/init/BackOnII-Klient.conf\n\
		service BackOnII-Klient start\n\
	fi\n\
else\n\
	echo \"Nie obsługiwany system operacyjny\"\n\
fi";

const char* uninstall_script="\
OS=$(lsb_release -si)\n\
VER=$(lsb_release -sr)\n\
if [ $OS = \"Ubuntu\" ]; then\n\
	if dpkg --compare-versions $VER \"gt\" \"14.10\" ; then\n\
		systemctl stop BackOnII-Klient\n\
		systemctl disable BackOnII-Klient\n\
		rm /var/log/BackOnII-Klient.log\n\
		rm /var/log/BackOnII-Klient-err.log\n\
		rm -r /opt/BackOnII-Klient\n\
		rm /etc/systemd/system/BackOnII-Klient.service\n\
	else\n\
		service BackOnII-Klient stop\n\
		rm /var/log/BackOnII-Klient.log\n\
		rm /var/log/BackOnII-Klient-err.log\n\
		rm -r /opt/BackOnII-Klient\n\
		rm /etc/init/BackOnII-Klient.conf\n\
	fi\n\
else\n\
	echo \"Nie obsługiwany system operacyjny\"\n\
fi";

void send_TCP_message(string message);
pair<string,int> receive_TCP();


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
	if(receive_TCP().first!="BackOnII Serwer\r\n"){	
		syserr("wrong banner");
	}
}

void reconnect_TCP(){
	fprintf(stderr,"server disconnected\n");	
	connect_TCP();
	send_TCP_message(login_message);
	pair<string,int> ret_message=receive_TCP();
	if(ret_message.first!="OK\r\n"){
		cout<<ret_message.first<<endl;
		syserr("recconection login communication failed");
	}else{
		fprintf(stderr,"reconnected to server\n");			
	}
}


void send_TCP_message(string message){//TODO może trzeba będzie i tutaj sprawdzać rozłączenie serwera
	int lenTCP=message.length();
	if(write(sockTCP,message.c_str(),lenTCP)!=lenTCP){
		fprintf(stderr,"message send error\n");
	}
}

pair<string,int> receive_TCP(){//pierwszy argument to odczytana wiadomość,drugi 0 - wszystko ok 
	ssize_t len;// 1 - w między czasie nastąpiło rozłączenie i ponowne połączenie - należy powtórzyć komunikację
	char buffer[1000];
	memset(buffer, 0, sizeof(buffer));
	len = read(sockTCP, buffer, sizeof(buffer)-1);
	if(len==0){
		reconnect_TCP();
		return make_pair("OK\r\n",1);//TODO recconect message - wykonaj ponownie komunikację (czy tylko w jedną stronę?)
	}
	if(len < 0){
		syserr("receive error");
	}
	string ret(buffer);
	return make_pair(ret,0);
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


int klient(){//TODO dodać obsługę rozłączenia serwera
	char login[1000],password[1000],hostname[HOST_NAME_MAX],service_password[PASSWORD_LEN];			//TODO zapytać o te długości
	pair<string,int> ret_message;
	int fresh_start;
	fscanf(glob_file,"%s",SERVER_NAME);
	fresh_start=(strlen(SERVER_NAME)==0);
	if(!fresh_start){								//tak na prawdę sprawdzenie pustości linijki roboczej
		do{
			printf("Usługa jest już zarejestrowana, lub zmieniony został plik z danymi.\n");
			printf("Spróbować dokonać ponownej rejestracji (R),\nkontynuować z aktualnymi danymi (K),\n");
			printf("przerwać działanie programu (P),\nczy wyrejestrować usługę (W)?\n");
			printf("(ponowna rejestracja nadpisze stare dane rejestracji usługi)\n");
			scanf("%s",login);
		}while((strcmp(login,"R")!=0)&&(strcmp(login,"K")!=0)&&(strcmp(login,"P")!=0)&&(strcmp(login,"W")!=0));
		if(strcmp(login,"W")==0){
			return 2;
		}else if(strcmp(login,"P")==0){
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
		printf("proszę podać adres ip serwera BackOnII: ");
		scanf("%s",SERVER_NAME);												//TODO zadbać o nieprzepełnienie bufora we wszystkich scanf
		while(inet_pton(AF_INET,SERVER_NAME,&(sa.sin_addr))!=1){
			printf("to nie jest poprawny adres ip_v4,\nproszę podać poprawny: ");//TODO dodać możliwość inet_v6
			scanf("%s",SERVER_NAME);
		}
		int temp;
		char temp2;
		printf("proszę podać numer portu serwera BackOnII: ");
		while(((scanf("%d%c",&temp,&temp2)!=2 || temp2!='\n')&& clean_stdin())|| (temp<0) || (temp>65535)){
			printf("to nie jest poprawny numer portu ip_v4\nproszę podać poprawny: ");
		}
		PORT=(char*)malloc(10*sizeof(char));
		sprintf(PORT,"%d",temp);
	}

	connect_TCP();
	gethostname(hostname,HOST_NAME_MAX);					//TODO to powinno być jednoznaczne, więc może jakiś hostid zamiast hostname
	if(FAST_DEBUG){
		strcpy(login,"ADMINISTRATOR");
		strcpy(password,"admin");		
	}else{
		//poznanie loginu i hasła
		printf("login: ");
		scanf("%s",login);
		for(uint i=0;i<strlen(login);++i){
			login[i]=toupper(login[i]);
		}
		printf("hasło: ");
		scanf("%s",password);
	}
	unsigned char *plaintext=(unsigned char *)password;
	unsigned char ciphertext[128];
	int ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	string message="Login|21|";	
	message+=char_to_string((char*)hostname)+"|";
	message+=char_to_string((char*)login)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"\r\n";
	cout <<message<<endl;
	login_message=message;
	send_TCP_message(message);
	if(receive_TCP().first!="OK\r\n"){
		syserr("login communication failed");
	}
	string str1=random_password(PASSWORD_LEN);
	copy(str1.begin(),str1.end(),service_password);
	plaintext=(unsigned char *)service_password;
	ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	message="ZKL|";
	message+=char_to_string((char*)hostname)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"\r\n";
	sleep(30);
	do{
		cout<<message<<endl;
		send_TCP_message(message);
		ret_message=receive_TCP();
		if(ret_message.first!="OK\r\n"){
			cerr<<"brak obsługi wiadomości: "<<ret_message.first<<endl;		
			syserr("ZKL communication failed");
		}else if(ret_message.second==0){
			printf("Komputer został zarejestrowany w systemie BackOnII\n");
		}
	}while(ret_message.second!=0);
	if(fresh_start){fprintf(glob_file,"x");}
	fprintf(glob_file,"\n%s\n",SERVER_NAME);
	fprintf(glob_file,"%s\n",PORT);
	fprintf(glob_file,"%s\n",login);
	fprintf(glob_file,"%s\n",str1.c_str());
	return 0;
}

int usluga(){
	char login[1000],hostname[HOST_NAME_MAX],service_password[PASSWORD_LEN];
	pair<string,int> ret_message;
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
	gethostname(hostname,HOST_NAME_MAX);
	unsigned char ciphertext[128];
	unsigned char * plaintext=(unsigned char *)service_password;
	int ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	string message="Login|22|";
	message+=char_to_string((char*)hostname)+"|";
	message+=char_to_string((char*)login)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"|2.0.0.1\r\n";
	login_message=message;
	cout <<message<<endl;
	send_TCP_message(message);
	ret_message=receive_TCP();
	if(ret_message.first!="OK\r\n"){
		cout<<ret_message.first<<endl;
		syserr("login communication failed");
	}
	message="ZOZ|"+char_to_string((char*)login)+"\r\n";
	for(;;){
		sleep(30);								//TODO może jakaś efektywniejsza forma czekania
		cout << "mes"<<endl;
		send_TCP_message(message);
		cout<<"hello message sent"<<endl;
		ret_message=receive_TCP();
		if(ret_message.first!="OK\r\n"){
			cerr<<"brak obsługi wiadomości: "<<ret_message.first<<endl;
		}
	}
	return 0;
}

int main(){
	int k;
	if(common_start()) return 1;
	if(PROGRAM_TYPE){
		return usluga();
	}else{
		k=klient();
		fclose(glob_file);		
		if(k==0){
			system(install_script);
		}else if(k==2){
			system(uninstall_script);	//TODO może zmodyfikować plik glob		
		}
	}
}
