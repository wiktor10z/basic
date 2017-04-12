#ifndef _BASIC_FUNCTIONS_
#define _BASIC_FUNCTIONS_

using namespace std;

extern int clean_stdin();

extern string char_to_string(char* chararr);

extern string to_hex(unsigned char* str1, int len);

extern string from_hex(string str1);

extern string random_password(int len);

/*
extern string Encode64(string s);

extern string Decode64(string s);

extern string md5_encode(string s);
*/

#endif
