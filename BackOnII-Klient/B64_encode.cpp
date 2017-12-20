#include <cstdio>
#include <iostream>
#include <cstring>
#include <cstdlib>
//#include <pthread.h>
#include <unistd.h>
#include <limits.h>
#include <netdb.h>
//#include <csignal>

#include <sys/wait.h>			//waitpid get_sys...
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/sysinfo.h>
#include <sys/ioctl.h>

#include <arpa/inet.h>

#include <fstream>

#include "basic_functions.h"

using namespace std;

int main(){
	char buffer[3000],buffer2[10000];
	string str1,str2;
	ifstream in("update",ios::in | ios::binary);
	//ofstream in("update2",ios::out | ios::binary);
	in.read(buffer,3000);
	str1=buffer;
	str2=Encode64(str1);
	cout <<str2;
}
