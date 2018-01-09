#include <cstdlib>
#include <string>
#ifndef VERSION
	#define VERSION "2.0.0.1"
#endif

using namespace std;

int main(){
    string script="chdir /opt/BackOnII-Klient_";
    script=script+VERSION+"/\n \
    exec ./usluga >/var/log/BackOnII-Klient.log 2>/var/log/BackOnII-Klient-err.log";	
    system("echo $$ > /var/run/BackOnII-Klient.pid");
    system(script.c_str());
    return 0;
}
