#include <cstdio>
#include <iostream>
#include <cstring>
//#include <string>
#include <cstdlib>
//#include <pthread.h>
#include <unistd.h>
#include <limits.h>
#include <netdb.h>
//#include <csignal>
#include <algorithm>

//#include <sys/wait.h>			//waitpid get_sys...
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>
#include <sys/ioctl.h>

#include <arpa/inet.h>

#include "basic_functions.h"
#include "err.h"
#include "3des.h"

#include <fstream>


#define PASSWORD_LEN 5	//TODO ustawić właściwe
#define FAST_DEBUG 0
#ifndef PROGRAM_TYPE
	#define PROGRAM_TYPE 0
#endif
#ifndef VERSION
	#define VERSION "2.0.0.1"
#endif

//00010203040FFA9708090A0B0C0D0E0F0001020304050607
//0901020304050607

//TODO dokumentacja (w szczególności licencja do biblioteki szyfrującej)
//TODO sklejenie dwóch wiadomości przy czytaniu do wyczerpania źródła
//TODO usunięcie rzeczy testowych
//TODO czas miedzy hello z 10 do 30
//TODO klucz szyfru - tutaj w global_data - plusy i minusy (w pliku łatwiej zmienić, jak tutaj to można tworzyć global_data od zera)
//TOO dokładny format global_data - jak się zmieni pomiedzy wersjami to dużo roboty


using namespace std;

//int PROGRAM_TYPE;					//0-klient,1-usługa
char* PORT;
char* SERVER_NAME;
char login[1000],service_password[PASSWORD_LEN];
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


string install_script( char* version){ //oba
string script="\
OS=$(lsb_release -si)\n\
VER=$(lsb_release -sr)\n\
if [ $OS = \"Ubuntu\" ]; then\n\
		apt-get install cpuid\n\
		apt-get install sshpass\n\
		mkdir -p /opt/BackOnII-Klient_";
		script=script+version+"\n\
		cp usluga /opt/BackOnII-Klient_"+version+"\n\
		cp global_data /opt/BackOnII-Klient_"+version+"\n\
	if dpkg --compare-versions $VER \"gt\" \"14.10\" ; then\n\
		cp service_script /opt/BackOnII-Klient_"+version+"\n\
		cp -r BackOnII-Klient_"+version+".service /etc/systemd/system/BackOnII-Klient_"+version+".service\n\
		systemctl enable BackOnII-Klient_"+version+"\n\
		systemctl start BackOnII-Klient_"+version+"\n\
	else\n\
		cp -r BackOnII-Klient_"+version+".conf /etc/init/BackOnII-Klient_"+version+".conf\n\
		service BackOnII-Klient_"+version+" start\n\
	fi\n\
else\n\
	echo \"Nieobsługiwany system operacyjny\"\n\
fi";
return script;
}

string uninstall_script( char* version){// oba
string script="\
OS=$(lsb_release -si)\n\
VER=$(lsb_release -sr)\n\
if [ $OS = \"Ubuntu\" ]; then\n\
	if dpkg --compare-versions $VER \"gt\" \"14.10\" ; then\n\
		systemctl stop BackOnII-Klient_";
		script=script+version+"\n\
		systemctl disable BackOnII-Klient_"+version+"\n\
		rm -r /opt/BackOnII-Klient_"+version+"\n\
		rm /etc/systemd/system/BackOnII-Klient_"+version+".service\n\
	else\n\
		service BackOnII-Klient_"+version+" stop\n\
		rm -r /opt/BackOnII-Klient_"+version+"\n\
		rm /etc/init/BackOnII-Klient_"+version+".conf\n\
	fi\n\
else\n\
	echo \"Nieobsługiwany system operacyjny\"\n\
fi";
return script;
}

void uninstall_all(){//TODO tylko klient
	string info=get_system_output((char*)"ls /opt -1");
	while(info.find("BackOnII-Klient_")!=string::npos){
		info=info.substr(info.find("BackOnII-Klient_")+16);
		string info1=info.substr(0,info.find("\n"));
		system((char*) uninstall_script((char*)info1.c_str()).c_str());
		//cout << uninstall_script((char*)info1.c_str()) <<endl;
	}
	system("rm /var/log/BackOnII-Klient.log\n rm /var/log/BackOnII-Klient-err.log");
}


void send_TCP_message(string message);
pair<string,int> receive_TCP();


void connect_TCP(){//oba
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
			syserr("%sgetaddrinfo: %s\n", time_string().c_str(),gai_strerror(err));
		}
		sockTCP = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
		if(sockTCP < 0){
			syserr("%ssocketTCP,client",time_string().c_str());
		}
		
		if(connect(sockTCP, addr_result->ai_addr, addr_result->ai_addrlen) < 0){			//TODO ustawić timeout jak się nie może połączyć
			usleep(500000);
			fprintf(stderr,"%sconnection attepmt failed\n",time_string().c_str());
		}else{
			connected=true;
		}
	}
	if(receive_TCP().first!="BackOnII Serwer\r\n"){	
		syserr("%swrong banner",time_string().c_str());
	}
}

void reconnect_TCP(){//oba
	fprintf(stderr,"%sserver disconnected\n",time_string().c_str());	
	connect_TCP();
	send_TCP_message(login_message);
	pair<string,int> ret_message=receive_TCP();
	if(ret_message.first!="OK\r\n"){
		cerr<<time_string()<<"wrong login answer: "<<ret_message.first<<endl;
		syserr("%srecconection login communication failed",time_string().c_str());
	}else{
		fprintf(stderr,"%sreconnected to server\n",time_string().c_str());			
	}
}

void send_TCP_message(string message){//TODO może trzeba będzie i tutaj sprawdzać rozłączenie serwera //oba
	int lenTCP=message.length();
	if(write(sockTCP,message.c_str(),lenTCP)!=lenTCP){
		fprintf(stderr,"%smessage send error\n",time_string().c_str());
	}
}

pair<string,int> receive_TCP(){//pierwszy argument to odczytana wiadomość,drugi 0 - wszystko ok //oba
	ssize_t len;// 1 - w między czasie nastąpiło rozłączenie i ponowne połączenie - należy powtórzyć komunikację
	char buffer[1000];
	memset(buffer, 0, sizeof(buffer));
	len = read(sockTCP, buffer, sizeof(buffer)-1);
	if(len==0){
		reconnect_TCP();
		return make_pair("OK\r\n",1);//TODO recconect message - wykonaj ponownie komunikację (czy tylko w jedną stronę?)
	}
	if(len < 0){
		syserr("%sreceive error",time_string().c_str());
	}
	string ret(buffer);
	return make_pair(ret,0);
}

string prod_name(string info){//oba - na razie
	string name3="???";
	if(info.find("product:")!=string::npos){
		string info1=info.substr(info.find("product:")+9);
		name3=info1.substr(0,info1.find("\n"));
		if(info.find("description:")!=string::npos){
			string info1=info.substr(info.find("description:")+13);
			name3=name3+" "+info1.substr(0,info1.find("\n"));
		}
	}else if(info.find("description:")!=string::npos){
		string info1=info.substr(info.find("description:")+13);
		name3=info1.substr(0,info1.find("\n"));
	}
	return name3;
}

string software_info(){//można też zrobić z tego stały string obliczany przy włączeniu programu//oba - na razie
	char hostname[HOST_NAME_MAX];
	gethostname(hostname,HOST_NAME_MAX);
	string host(hostname);
	string system=get_system_output((char*)"lsb_release -si");
	string version=get_system_output((char*)"lsb_release -sr");
	system=system+" "+version;	
	string processor=get_system_line((char *)"cpuid | grep \"brand = \"","brand = \"");
	if(processor[processor.length()-1]=='\"'){
		processor=processor.substr(0,processor.length()-2);
	}
	string uuid=get_system_line((char*)"dmidecode -t system | grep UUID:","UUID:");
	string manufacturer=get_system_line((char*)"dmidecode -t system | grep Manufacturer:","Manufacturer:");
	string model=get_system_line((char*)"dmidecode -t system | grep \"Product Name:\"","Product Name:");
	string serial=get_system_line((char*)"dmidecode -t system | grep \"Serial Number:\"","Serial Number:");
	
	struct sysinfo info;
	sysinfo(&info);
	char ram[20];
	sprintf(ram,"%.2fGB",((1.0*info.totalram)/(1024*1024*1024)));
	string computer_serial1="????????????";
	string soft_info="\
update komputery set SystemOperacyjny='"+system+"',\
 ProductID='"+uuid+"', Serial='"+serial+"', Procesor='"+processor+"',\
 RAM='"+ram+"', Producent='"+manufacturer+"',  NumerSeryjnyKomputera='"+computer_serial1+"', Model='"+model+"'\
 where nazwa_komputera='"+host+"';\r\n\
delete from KomponentyKomputera where NazwaKomputera='"+host+"';\r\n";
	string diskinfo=get_system_output((char*)"lshw -class disk");
	size_t pos1,pos2;
	while(((pos1=diskinfo.find("*-cdrom"))!=string::npos)||((pos2=diskinfo.find("*-disk"))!=string::npos)){
		bool b;
		if((pos1!=string::npos)&&(pos2!=string::npos)){
			b=(pos1<pos2);
		}else{
			b=(pos1!=string::npos);
		}
		if(b){
			diskinfo=diskinfo.substr(diskinfo.find("*-cdrom"));
			diskinfo=diskinfo.substr(diskinfo.find("\n")+1);
			string cdrominfo=diskinfo;
			if(cdrominfo.find("*-cdrom")!=string::npos){
				cdrominfo=cdrominfo.substr(0,cdrominfo.find("*-cdrom"));
			}
			if(cdrominfo.find("*-disk")!=string::npos){
				cdrominfo=cdrominfo.substr(0,cdrominfo.find("*-disk"));
			}
			soft_info = soft_info+"insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1)\
 values ('"+host+"',3,'"+prod_name(cdrominfo)+"');\r\n";
		}else{
			diskinfo=diskinfo.substr(diskinfo.find("*-disk"));
			diskinfo=diskinfo.substr(diskinfo.find("\n")+1);
			string diskinfo1=diskinfo;
			if(diskinfo1.find("*-cdrom")!=string::npos){
				diskinfo1=diskinfo1.substr(0,diskinfo1.find("*-cdrom"));
			}
			if(diskinfo1.find("*-disk")!=string::npos){
				diskinfo1=diskinfo1.substr(0,diskinfo1.find("*-disk"));
			}
			string name3=prod_name(diskinfo1);
			string name4=extract_line(diskinfo1,"size:");
			string name5=extract_line(diskinfo1,"serial:");
			soft_info = soft_info+"insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1,Nazwa2,Nazwa3,Nazwa4)\
 values ('"+host+"',2,'"+name3+"','"+name3+"','"+name4+"','"+name5+"');\r\n";
		}
	}
	string netinfo=get_system_output((char*)"lshw -class network");
		if(netinfo.find("*-network")!=string::npos){
		string serial1=extract_line(netinfo,"serial:");
		soft_info = soft_info+"insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1, Nazwa2) values\
 ('"+host+"',1,'"+prod_name(netinfo)+"','"+serial1+"');";
	}
	return soft_info;
}

string software_audit(int Sl3){//oba - na razie
	string script=software_info();
	string T64=Encode64(script,0);
	string TMD5=md5_encode(script);
	return "PAS|"+to_string(Sl3)+"|"+T64+"|"+TMD5+"\r\n";
}

void save_update_script(string script){//TODO tylko usluga
	string decoded=Decode64(script,1);
	cout << time_string() <<"zapisano skrypt aktualizujący"<<endl;
	ofstream output("aktLinux.sh",ios::binary);
	output <<decoded<<endl;
}


int common_start(){//oba
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
	read_hex_from_file(glob_file,key);													//TODO weryfikacja poprawności pliku
	read_hex_from_file(glob_file,iv);
	initialize_3des();
	return 0;
}

int klient(){//TODO dodać obsługę rozłączenia serwera
	char hostname[HOST_NAME_MAX];		//TODO zapytać o tą długość
	unsigned char *plaintext;
	unsigned char ciphertext[128];	
	string message, password;
	int ciphertext_len, tries_count=0, fresh_start;
	bool passed_login;
	pair<string,int> ret_message;
	fscanf(glob_file,"%s",SERVER_NAME);
	fresh_start=(strlen(SERVER_NAME)==0);
	if(!fresh_start){								//tak na prawdę sprawdzenie pustości linijki roboczej
		do{
			printf("Usługa jest już zarejestrowana, lub zmieniony został plik z danymi.\n");
			printf("Spróbować dokonać ponownej rejestracji (R),\nkontynuować z aktualnymi danymi (K),\n");
			printf("przerwać działanie programu (P),\nczy wyrejestrować usługę (W)?\n");
			printf("przeprowadzić i wyświetlić audyt sprzętu (A)?\n");			//TODO usunąć
			printf("(ponowna rejestracja nadpisze stare dane rejestracji usługi)\n");
			scanf("%s",login);
		//}while((strcmp(login,"R")!=0)&&(strcmp(login,"K")!=0)&&(strcmp(login,"P")!=0)&&(strcmp(login,"W")!=0));
		//if(strcmp(login,"W")==0){
		
		}while((strcmp(login,"R")!=0)&&(strcmp(login,"K")!=0)&&(strcmp(login,"P")!=0)&&(strcmp(login,"W")!=0)&&(strcmp(login,"A")!=0));			//TODO usunąć
		if(strcmp(login,"A")==0){			//TODO usunąć
			return 3;			//TODO usunąć
		}else if(strcmp(login,"W")==0){			//TODO usunąć
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

	connect_TCP();							//TODO przerywanie w razie braku połączenia/ zapytanie o nowy adres???
	gethostname(hostname,HOST_NAME_MAX);					//TODO to powinno być jednoznaczne, więc może jakiś hostid zamiast hostname
	passed_login=false;
	while((tries_count<3) &&(!passed_login)){
		if(FAST_DEBUG){
			strcpy(login,"ADMINISTRATOR");
			password="admin";
			//strcpy(password,"admin");	
		}else{
			//poznanie loginu i hasła
			printf("login: ");
			scanf("%s",login);
			for(uint i=0;i<strlen(login);++i){
				login[i]=toupper(login[i]);
			}
			clean_stdin();
			printf("hasło: ");
			password=get_password();
		}
		plaintext=(unsigned char *)(password.c_str());
		ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
		message="Login|21|";	
		message+=char_to_string((char*)hostname)+"|";
		message+=char_to_string((char*)login)+"|";
		message+=to_hex(ciphertext,ciphertext_len)+"\r\n";
		cout <<time_string() <<"sent message: "<<message<<endl;
		login_message=message;
		send_TCP_message(message);
		if(receive_TCP().first!="OK\r\n"){
			cerr<<time_string()<<"wrong login answer: "<<ret_message.first<<endl;
			printf("logowanie nie powiodło się, spróbuj ponownie\n");
			//syserr("login communication failed");
		}else{
			passed_login=true;
		}
		tries_count++;
	}
	if(tries_count==3){//i tak nic nie robi, bo serwer blokuj po 2 nieudanym
		syserr("login communication failed");
	}
	string str1=random_password(PASSWORD_LEN);
	copy(str1.begin(),str1.end(),service_password);
	plaintext=(unsigned char *)service_password;
	ciphertext_len=encrypt(plaintext,strlen((char *)plaintext),key,iv,ciphertext);
	message="ZKL|";
	message+=char_to_string((char*)hostname)+"|";
	message+=to_hex(ciphertext,ciphertext_len)+"\r\n";
	do{
		cout <<time_string() <<"sent message: "<<message<<endl;
		send_TCP_message(message);
		ret_message=receive_TCP();
		if(ret_message.first!="OK\r\n"){
			cerr<<time_string()<<"wrong ZKL answer: "<<ret_message.first<<endl;		
			syserr("ZKL communication failed");
		}else if(ret_message.second==0){
			printf("%sKomputer został zarejestrowany w systemie BackOnII\n",time_string().c_str());
		}
	}while(ret_message.second!=0);
	if(fresh_start){fprintf(glob_file,"x");}
	fprintf(glob_file,"\n%s\n",SERVER_NAME);
	fprintf(glob_file,"%s\n",PORT);
	fprintf(glob_file,"%s\n",login);
	fprintf(glob_file,"%s\n",str1.c_str());
	return 0;
}

void update_confirmation(){//TODO tylko usługa
	char* str1=(char*)malloc(1000*sizeof(char));
	if(fscanf(glob_file,"%s",str1)>0){
		string str2(str1);
		if(str2!=VERSION){
			system(uninstall_script((char *)str2.c_str()).c_str());
			fscanf(glob_file,"%s",str1);
			fclose(glob_file);
			glob_file=fopen("global_data","w");
			string content1=to_hex(key,24)+"\n"+to_hex(iv,8);
			transform(content1.begin(), content1.end(),content1.begin(), ::toupper);
			fprintf(glob_file,"%s\n\nx\n",content1.c_str());
			fprintf(glob_file,"%s\n%s\n%s\n%s\n",SERVER_NAME,PORT,login,service_password);
			string message="PWZ|";
			message=message+str1;
			send_TCP_message(message+"\r\n");
			cout<<time_string()<<"update confirmation sent ("<<message<<")"<<endl;
			pair<string,int> ret_message=receive_TCP();
			if(ret_message.first!="OK\r\n"){
				cerr<<time_string()<<"wrong update confirmation answer: "<<ret_message.first<<endl;
				syserr("update %s confirmation communication failed",str1);
			}
		}
		fclose(glob_file);
		glob_file=fopen("global_data","r+");
		for(int i=0;i<8;++i) fscanf(glob_file,"%s",str1);	
	}
	free(str1);
}


int usluga(){//TODO tylko usluga
	char hostname[HOST_NAME_MAX];
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
	message+=to_hex(ciphertext,ciphertext_len)+"|"+VERSION+"\r\n";
	login_message=message;
	cout <<time_string()<<"sent message: "<<message<<endl;
	send_TCP_message(message);
	ret_message=receive_TCP();
	if(ret_message.first!="OK\r\n"){
		cerr<<time_string()<<"wrong login answer: "<<ret_message.first<<endl;
		syserr("login communication failed");
	}
	update_confirmation();
	message="ZOZ|"+char_to_string((char*)login)+"\r\n";
	for(;;){
		sleep(10);								//TODO może jakaś efektywniejsza forma czekania TODO 30
		send_TCP_message(message);
		cout<<time_string()<<"hello message sent"<<endl;
		ret_message=receive_TCP();
		if(ret_message.first.find("AS|")==0){
			string AS_message=software_audit(stoi(ret_message.first.substr(ret_message.first.rfind("|")+1),NULL));
			cout <<time_string()<<"sent message: "<<AS_message<<endl;
			send_TCP_message(AS_message);
		}else if(ret_message.first.find("StLF")==0){//TODOTODO co jak nie zmieści się w 1000 bajtach
			cout <<time_string()<<"odbiór długiej wiadomości: "<<endl;//TODOTODO co jak to jest długa wiadomość ale inna
			string long_msg=ret_message.first;			
			while(ret_message.first.find("|STLF")==string::npos){
				ret_message=receive_TCP();
				long_msg=long_msg+ret_message.first;
			}
			cout<<"długa wiadomość: "<<endl;

			int msg_num=stoi(long_msg.substr(long_msg.rfind("|")+1));
			long_msg=long_msg.substr(6,long_msg.find("|STLF")-6);			
			cout << long_msg<<endl;
			cout <<"numer wiadomości: "<<endl;
			cout << msg_num<<endl;
						
			fprintf(glob_file,"\n%s\n%d",VERSION,msg_num);
			fclose(glob_file);
			save_update_script(long_msg);
			string akt_order="bash aktLinux.sh ";
			akt_order=akt_order+SERVER_NAME;
			system(akt_order.c_str());//TODO nazwa pliku może być odczytywana z polecenia
			return 0;
			//ret_message=receive_TCP();
			//cerr<<time_string()<<"brak obsługi wiadomości: "<<ret_message.first<<endl;
		}else if(ret_message.first!="OK\r\n"){
			cerr<<time_string()<<"brak obsługi wiadomości: "<<ret_message.first<<endl;
			int count;			
			ioctl(sockTCP,FIONREAD,&count);
			while(count>0){
				ret_message=receive_TCP();
				cerr<<time_string()<<"dalsza część wiadomości: "<<ret_message.first<<endl;
				ioctl(sockTCP,FIONREAD,&count);
			}			
		}
	}
	
	return 0;
}


int main(int argc, char * argv[]){
	int k;
	if(common_start()) return 1;
	if(PROGRAM_TYPE==1){
		k=usluga();
		fclose(glob_file);
		return k;
	}else{
		k=klient();
		fclose(glob_file);		
		if(k==0){
			system(install_script((char*)VERSION).c_str());
		}else if(k==2){
			uninstall_all();
		}else if(k==3){				//TODO usunąć
			cout<<software_info()<<endl;			//TODO usunąć
		}
	}
}
