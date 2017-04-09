#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <sstream>
#include <iomanip>
//#include <openssl/md5.h>

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

string to_hex(unsigned char* str1, int len){
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

string random_password(int len){					//TODO zapytać o to jakiej długości, czy ma być odczytywalne dla człowieka, jak bezpiecznie zapisać
	string res;
	for(int i=0;i<len;++i){
		res+=33+rand()%(127-33);
	}
	return res;
}

/*
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
*/


