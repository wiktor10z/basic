#include <cstdio>
#include <iostream>
#include <cstring>
#include <cstdlib>
//#include <pthread.h>
#include <unistd.h>
#include <limits.h>
#include <netdb.h>
//#include <csignal>

#include <sys/wait.h>			//waitpid GetSys...
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>
#include <sys/ioctl.h>

#include <arpa/inet.h>

#include "basic_functions.h"
#include "err.h"
#include "3des.h"

#define PASSWORD_LEN 5
#define FAST_DEBUG 0

//00010203040FFA9708090A0B0C0D0E0F0001020304050607
//0901020304050607

//TODO przy audycie trzeba zakładać, że którejść informacji nie da się wyciągnąć - zmienione pliki, brak informacji itp.
//TODO sklejenie dwóch wiadomości przy czytaniu do wyczerpania źródła

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

string GetSystemOutput(char* cmd){
        int buff_size = 32;
    char* buff = new char[buff_size];

        string str = "";

    int fd[2];
    int old_fd[3];
    pipe(fd);


        old_fd[0] = dup(STDIN_FILENO);
        old_fd[1] = dup(STDOUT_FILENO);
        old_fd[2] = dup(STDERR_FILENO);

        int pid = fork();
        switch(pid){
                case 0:
                        close(fd[0]);
                        close(STDOUT_FILENO);
                        close(STDERR_FILENO);
                        dup2(fd[1], STDOUT_FILENO);
                        dup2(fd[1], STDERR_FILENO);
                        system(cmd);
                        //execlp((const char*)cmd, cmd,0);
                        close (fd[1]);
                        exit(0);
                        break;
                case -1:
                        cerr << "GetSystemOutput/fork() error\n" << endl;
                        exit(1);
                default:
                        close(fd[1]);
                        dup2(fd[0], STDIN_FILENO);
						int rc = 1;
                        while (rc > 0){
                                rc = read(fd[0], buff, buff_size);
                                str.append(buff, rc);
                                //memset(buff, 0, buff_size);
                        }



                        waitpid(pid, NULL, 0);
                        close(fd[0]);
        }

        dup2(STDIN_FILENO, old_fd[0]);
        dup2(STDOUT_FILENO, old_fd[1]);
        dup2(STDERR_FILENO, old_fd[2]);
	if(str[str.length()-1]=='\n'){
		return str.substr(0,str.length()-1);
	}else{
		return str;
	}
}


const char* install_script="\
OS=$(lsb_release -si)\n\
VER=$(lsb_release -sr)\n\
if [ $OS = \"Ubuntu\" ]; then\n\
		apt-get install cpuid\n\
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

void reconnect_TCP(){
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

void send_TCP_message(string message){//TODO może trzeba będzie i tutaj sprawdzać rozłączenie serwera
	int lenTCP=message.length();
	if(write(sockTCP,message.c_str(),lenTCP)!=lenTCP){
		fprintf(stderr,"%smessage send error\n",time_string().c_str());
	}
}

pair<string,int> receive_TCP(){//pierwszy argument to odczytana wiadomość,drugi 0 - wszystko ok 
	ssize_t len;// 1 - w między czasie nastąpiło rozłączenie i ponowne połączenie - należy powtórzyć komunikację
	char buffer[1000];//2 - 
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

/*
Audyt sprzętu.
Klient robi własną inwetaryzację sprzętu i całość wysyła jako skrypt SQL (cały skrypt poniżej – wydaje się na tyle jasny, że powinieneś wiedzieć co trzeba sprawdzić
– robi update danych komputera, usuwa aktualne komponenty i dodaje nowe – indeksem jest nazwa komputera (zawsze upcase)).
Po otrzymaniu komunikatu AS … powinieneś zrobić inwetaryzację i przygotować skryto, wyliczyć jego sumę MD5, zakodować go za pomocą Base64 i odesłać ciąg znaków:
PAS|'+Sl[3]+'|'+T64+'|'+TMD5
Gdzie SL[3] to numer zadania, a ten numer przyszedł w zadaniu AS na pozycji 4 (znak | w zadaniu jest separatorem, w delphi tablica liczona od 0 więc pozycja nr 3).

U mnie wygląda to tak:
     T64:=Encode64(DBackOnIIMain.SLSprzetuKomputera.Text);
    TMD5:=StrMD5(DBackOnIIMain.SLSprzetuKomputera.Text);
     DodajZapis('PAS|'+Sl[3]+'|'+T64+'|'+TMD5,false,stmp);

Skrypt:
update komputery set SystemOperacyjny='Microsoft Windows 7 Professional ',  Dodatek='Service Pack 1',  ProductID='00371-177-6532507-85985', 
* Serial='XXXX',  Procesor='Intel(R) Xeon(R) CPU E3-1231 v3 @ 3.40GHz',  RAM='3,02GB',  Producent='innotek GmbH',  NumerSeryjnyKomputera='SN508382512',
*  Model='VirtualBox' where nazwa_komputera='FPP-D2007-W7';
delete from KomponentyKomputera where NazwaKomputera='FPP-D2007-W7';
insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1) values ('FPP-D2007-W7',3,'VBOX CD-ROM ATA Device');
insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1,Nazwa2,Nazwa3,Nazwa4) values ('FPP-D2007-W7',2,'VBOX HARDDISK ATA Device',
* 'VBOX HARDDISK ATA Device','40GB','42566434376230343765352d6237346466342037');
insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1,Nazwa2,Nazwa3,Nazwa4) values ('FPP-D2007-W7',2,'VBOX HARDDISK ATA Device',
* 'VBOX HARDDISK ATA Device','60GB','42563936626332656364362d6538633233612030');
insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1, Nazwa2) values ('FPP-D2007-W7',1,'Karta Intel(R) PRO/1000 MT Desktop Adapter',
* '08:00:27:27:2D:C2');
*/
 //R9VRVY5
 
string prod_name(string info){
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

string software_info(){//można też zrobić z tego stały string obliczany przy włączeniu programu
	char hostname[HOST_NAME_MAX];
	gethostname(hostname,HOST_NAME_MAX);
	string host(hostname);
	string system=GetSystemOutput((char*)"lsb_release -si");
	string version=GetSystemOutput((char*)"lsb_release -sr");
	string processor=GetSystemOutput((char*)"cpuid | grep \"brand = \"");
	string uuid=GetSystemOutput((char*)"dmidecode -t system | grep UUID:");
	string manufacturer=GetSystemOutput((char*)"dmidecode -t system | grep Manufacturer:");
	string model=GetSystemOutput((char*)"dmidecode -t system | grep \"Product Name:\"");
	string serial=GetSystemOutput((char*)"dmidecode -t system | grep \"Serial Number:\"");
	system=system+" "+version;	
	if(processor.find("brand =")!=string::npos){
		processor=processor.substr(processor.find("brand =")+9);
		processor=processor.substr(0,processor.find("\"\n"));
		processor=processor.substr(processor.find_first_not_of(" "));	
	}else{
		processor="???";
	}
	if(uuid.find("UUID")!=string::npos){
		uuid=uuid.substr(uuid.find("UUID")+6);
	}else{
		uuid="???";
	}
	if(manufacturer.find("Manufacturer")!=string::npos){		
		manufacturer=manufacturer.substr(manufacturer.find("Manufacturer")+14);
	}else{
		manufacturer="???";
	}
	if(model.find("Product Name")!=string::npos){
		model=model.substr(model.find("Product Name")+14);
	}else{
		model="???";
	}
	if(serial.find("Serial Number")!=string::npos){	
		serial=serial.substr(serial.find("Serial Number")+15);
	}else{
		serial="???";
	}
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
	string diskinfo=GetSystemOutput((char*)"lshw -class disk");
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
			string name4="???";
			if(diskinfo1.find("size:")!=string::npos){
				string diskinfo2=diskinfo1.substr(diskinfo1.find("size:")+6);
				name4=diskinfo2.substr(0,diskinfo2.find("\n"));
			}
			string name5="???";
			if(diskinfo1.find("serial:")!=string::npos){
				string diskinfo2=diskinfo1.substr(diskinfo1.find("serial:")+8);
				name5=diskinfo2.substr(0,diskinfo2.find("\n"));
			}
			soft_info = soft_info+"insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1,Nazwa2,Nazwa3,Nazwa4)\
 values ('"+host+"',2,'"+name3+"','"+name3+"','"+name4+"','"+name5+"');\r\n";
		}
	}
	string netinfo=GetSystemOutput((char*)"lshw -class network");
		if(netinfo.find("*-network")!=string::npos){
		string serial1="???";
		if(netinfo.find("serial:")!=string::npos){
			string info1=netinfo.substr(netinfo.find("serial:")+8);
			serial1=info1.substr(0,info1.find("\n"));
		}
		soft_info = soft_info+"insert into KomponentyKomputera (NazwaKomputera,TypKomponentu, Nazwa1, Nazwa2) values\
 ('"+host+"',1,'"+prod_name(netinfo)+"','"+serial1+"');";
	}
	return soft_info;
}





string software_audit(int Sl3){
	string script1=software_info();
	string T64=Encode64(script1);
	string TMD5=md5_encode(script1);
	return "PAS|"+to_string(Sl3)+"|"+T64+"|"+TMD5+"\r\n";
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
	read_hex_from_file(glob_file,key);																//TODO weryfikacja poprawności pliku
	read_hex_from_file(glob_file,iv);
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
	cout <<time_string() <<"sent message: "<<message<<endl;
	login_message=message;
	send_TCP_message(message);
	if(receive_TCP().first!="OK\r\n"){
		cerr<<time_string()<<"wrong login answer: "<<ret_message.first<<endl;
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
	cout <<time_string()<<"sent message: "<<message<<endl;
	send_TCP_message(message);
	ret_message=receive_TCP();
	if(ret_message.first!="OK\r\n"){
		cerr<<time_string()<<"wrong login answer: "<<ret_message.first<<endl;
		syserr("login communication failed");
	}
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
		}else if(ret_message.first!="OK\r\n"){
			cerr<<time_string()<<"brak obsługi wiadomości: "<<ret_message.first<<endl;
			int count;			
			ioctl(sockTCP,FIONREAD,&count);
			while(count>0){
				ret_message=receive_TCP();
				cerr<<time_string()<<"next message part: "<<ret_message.first<<endl;
				ioctl(sockTCP,FIONREAD,&count);
			}			
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
		//}
		}else if(k==3){				//TODO usunąć
			cout<<software_info()<<endl;			//TODO usunąć
		}			//TODO usunąć
	}
}
