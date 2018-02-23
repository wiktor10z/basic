#ifndef _BASIC_FUNCTIONS_
#define _BASIC_FUNCTIONS_

using namespace std;

extern int clean_stdin();

extern string char_to_string(char* chararr);

extern string to_hex(unsigned char* str1, int len);

extern string from_hex(string str1);

extern void read_hex_from_file(FILE * file,unsigned char * dest);

extern string time_string();

extern string random_password(int len);

extern string get_password(bool show_asterisk=true);

extern string get_system_output(char* cmd);

extern string extract_line(string str,const char* pattern);

extern string get_system_line(char* command,const char* pattern);


extern string Encode64(string s, int file);

extern string Decode64(string s, int file);

extern string md5_encode(string s);



#endif
