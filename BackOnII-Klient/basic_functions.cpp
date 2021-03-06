#include <cstdio>
#include <iostream>//TODO może zunifikować wypisywanie do jednej biblioteki standardowej
#include <cstring>
//#include <string>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <sstream>
#include <iomanip>
#include <termios.h>
#include <unistd.h>
#include <sys/wait.h>	
#include <openssl/md5.h>

#include "basic_functions.h"

using namespace std;

int clean_stdin(){
	while (getchar()!='\n');
	return 1;
	}

string char_to_string(char* chararr){
	string ret(chararr);
	return ret;
}

string to_hex(unsigned char* str1, int len){//TODO można się pobawić w zrobienie tego upcase
	stringstream ss("");
	for(int i=0;i<len;++i){
		ss<<hex << setfill('0') << setw(2) << (uint)str1[i];
	}
	return ss.str();
}

string from_hex(string str1){
	string res;
    int temp;	
	res.reserve(str1.size()/2);
	for(uint i=0;i<str1.length();i+=2){
    stringstream ss(str1.substr(i,2));
    ss >> std::hex >> temp;
    res += static_cast<char>(temp);
}
return res;
}

void read_hex_from_file(FILE * file,unsigned char * dest){
	char buff[100];
	fscanf(file,"%s",buff);				//TODO sprawdzić czy to jest hex i czy dobrej długości
	string temp_str(buff);
	temp_str=from_hex(temp_str);
	copy(temp_str.begin(),temp_str.end(),dest);
}

string time_string(){
	time_t rawtime;
	char buffer[80];
	struct tm * timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime (buffer,80,"%F %T    ",timeinfo);
	string str(buffer);
	return str;
}

string random_password(int len){					//TODO zapytać o to jakiej długości, czy ma być odczytywalne dla człowieka, jak bezpiecznie zapisać
	string res;
	for(int i=0;i<len;++i){
		res+=33+rand()%(127-33);
	}
	return res;
}

int getch(){
	int ch;
	struct termios t_old, t_new;
	tcgetattr(STDIN_FILENO, &t_old);
	t_new = t_old;
	t_new.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &t_new);
	ch = getchar();
	tcsetattr(STDIN_FILENO, TCSANOW, &t_old);
	return ch;
}

string get_password(bool show_asterisk){
	string password;
	unsigned char ch=0;
	while((ch=getch())!=10){
		if(ch==127){
			if(password.length()!=0){
				if(show_asterisk) cout <<"\b \b";
				password.resize(password.length()-1);
			}
		}else{
			password+=ch;
			if(show_asterisk) cout <<'*';
		}
	}
	cout <<endl;
	return password;
}

string get_system_output(char* cmd){
	int buff_size=100;
	char* buff=new char[buff_size];
	string str="";
	int fd[2],old_fd[3];
	pipe(fd);
	old_fd[0]=dup(STDIN_FILENO);
	old_fd[1]=dup(STDOUT_FILENO);
	old_fd[2]=dup(STDERR_FILENO);
	
	int pid=fork();
	switch(pid){
		case 0:
			close(fd[0]);
			close(STDOUT_FILENO);
			close(STDERR_FILENO);
			dup2(fd[1],STDOUT_FILENO);
			dup2(fd[1],STDERR_FILENO);
			system(cmd);
			close(fd[1]);
			exit(0);
			break;
		case -1:
			fprintf(stderr,"get_system_output/fork() error\n");
			exit(1);
		default:
			close(fd[1]);
			dup2(fd[0],STDIN_FILENO);
			int rc=1;
			while(rc>0){
				rc=read(fd[0],buff,buff_size);
				str.append(buff,rc);
			}
			waitpid(pid,NULL,0);
			close(fd[0]);
	}
	
	dup2(STDIN_FILENO,old_fd[0]);
	dup2(STDOUT_FILENO,old_fd[1]);
	dup2(STDERR_FILENO,old_fd[2]);
	
	if(str[str.length()-1]=='\n'){
		return str.substr(0,str.length()-1);
	}else{
		return str;
	}
}

string extract_line(string str,const char* pattern){
	if(str.find(pattern)!=string::npos){
		str=str.substr(str.find(pattern)+strlen(pattern));
		str=str.substr(str.find_first_not_of(" "));
		return str.substr(0,str.find("\n"));
	}else{
		return "???";
	}
}

string get_system_line(char* command,const char* pattern){
	return extract_line(get_system_output(command),pattern);
}


string Codes64_1="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/";
string Codes64_2="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

string Encode64(string s,int file){
	string Codes;
	if(file){
		Codes=Codes64_2;
	}else{
		Codes=Codes64_1;
	}
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
			result+=Codes[x];
		}
	}
	if(a>0){
		x=b<<(6-a);
		result+=Codes[x];
	}
	return result;
}

string Decode64(string s,int file){
		string Codes;
	if(file){
		Codes=Codes64_2;
	}else{
		Codes=Codes64_1;
	}
	int a=0,x,b=0;
	string result="";
	for(uint i=0;i<s.length();++i){
		x=Codes.find(s[i]);
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
	string hex=to_hex(hash,MD5_DIGEST_LENGTH);
	transform(hex.begin(), hex.end(),hex.begin(), ::toupper);
	return hex;
}



