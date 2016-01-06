#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int i){
	printf("%d",i);
}

void printString(char* s){//sprawdzić, czy działa
	printf("%s",s);
}

//error i concat
char* concat(char *str1, char *str2){
	char *str3 = malloc(strlen(str1)+strlen(str2)+1);
	strcpy(str3,str1);
	strcat(str3,str2);
	return str3;
}

int readInt(){//sprawdzić, czy działa
	int i;
	scanf("%d",&i);
	return i;
}

char* readString(){	//TODO sprawdzić jak sobie radzi z testami - łamanie wczytuje łamanie linii co może nie być dobre
	char* s;
	size_t len=0;
	getline(&s,&len,stdin);
	return s;
}
	
