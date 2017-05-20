#include <stdlib.h>
int main(){
    system("echo $$ > /var/run/BackOnII-Klient.pid");
    system("chdir /opt/BackOnII-Klient/\n \
    exec ./usluga >/var/log/BackOnII-Klient.log 2>/var/log/BackOnII-Klient-err.log");
    return 0;
}
