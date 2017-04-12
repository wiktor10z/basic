
#include <cstdio>
#include <iostream>
#include <cstring>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <netdb.h>
#include <csignal>
#include <sstream>

#include <openssl/md5.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>

#include "err.h"

using namespace std;

char* PORT=(char*)"6000";
char* SERVER_NAME=(char*)"10.0.2.2";
int sockTCP;
struct sysinfo system_info;


string Codes64="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/";

string Encode64(string s){
	int a=0,x,b=0;
	string result="";
	for(uint i=0;i<s.length();++i){
		x=(int)s[i];
		b=b*256+x;
		a+=8;
		while(a>=6){
			a-=6;
			x=b/(1<<a);
			b%=(1<<a);
			result+=Codes64[x];
		}
	}
	if(a>0){
		x=b<<(6-a);
		result+=Codes64[x];
	}
	return result;
}

string Decode64(string s){
	int a=0,x,b=0;
	string result="";
	for(uint i=0;i<s.length();++i){
		x=Codes64.find(s[i]);
		if(x!=(int)string::npos){
			b=b*64+x;
			a+=6;
			if(a>=8){
				a-=8;
				x=b>>a;
				b=b%(1<<a);
				x%=256;
				result+=(char)x;
			}
		}else{
			return result;
		}
	}
	return result;
}


string md5_encode(string s){
	unsigned char hash[MD5_DIGEST_LENGTH];
	MD5((const unsigned char*)s.c_str(),s.length(),hash);
	stringstream ss("");
	for(int i=0;i<MD5_DIGEST_LENGTH;++i){
		ss<<hex<<(uint)hash[i];
	}
	return ss.str();
}
























static void catch_int (int sig) {
	close(sockTCP);
	fprintf(stderr,"signal\n");
	exit(1);
}

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
		if(connect(sockTCP, addr_result->ai_addr, addr_result->ai_addrlen) < 0){
			usleep(500000);
			fprintf(stderr,"not connected\n");
		}else{
			fprintf(stderr,"connected\n");
			connected=true;
		}
	}
}

void receive_TCP_report(){
	ssize_t len;
	char buffer[1000];
	memset(buffer, 0, sizeof(buffer));
	len = read(sockTCP, buffer, sizeof(buffer)-1);
	if(len < 0){
		fprintf(stderr,"receive error");
	}
	printf("%s",buffer);
}



void send_TCP_report(){
	
	string message="Login|11|Komputer|XXX\n";
	int lenTCP=message.length();
	if(write(sockTCP,message.c_str(),lenTCP)!=lenTCP){
		fprintf(stderr,"message send error\n");
	}else{
		fprintf(stderr,"message send\n");
	}
}

void audit(){
	if(sysinfo(&system_info)!=0){
		fprintf(stderr,"audit error\n");
	}
}

void *report_TCP_sender(void *s_ptr){
	connect_TCP();
	//for(;;){
		//sleep(1);
		receive_TCP_report();
		send_TCP_report();		
	//}
	return(0);
}



int main(int argc, char *argv[]){
	
	//2 wątki jeden łączący się z serwerem i wysyłający hello
	//drugi wykonujący audyt sprzętu na żądanie + co tam trzeba
	int rc;
	pthread_t t;
	if (signal(SIGINT, catch_int) == SIG_ERR) {
		perror("Unable to change signal handler\n");
		exit(EXIT_FAILURE);
	}
	
	rc = pthread_create(&t, 0, report_TCP_sender, 0);
	if (rc == -1) {
		perror("pthread_create");
		exit(EXIT_FAILURE);
    }
	rc = pthread_detach(t);
	if (rc == -1) {
		perror("pthread_detach");
		exit(EXIT_FAILURE);
	}
	audit();
	printf("uptime: %ld\n",system_info.uptime);
	printf("load 1m: %ld\n",system_info.loads[0]);
	printf("load 5m: %ld\n",system_info.loads[1]);
	printf("load 15m: %ld\n",system_info.loads[2]);
	printf("total memory: %ld MB\n",system_info.freeram/(1024*1024));
	printf("shared memory: %ld MB\n",system_info.sharedram/(1024*1024));
	printf("memory used by buffers: %ld MB\n",system_info.bufferram/(1024*1024));
	printf("...\n");
	string s="abcdef";
	cout <<md5_encode(s)<<endl;
	for(;;){

	}	
	
}
