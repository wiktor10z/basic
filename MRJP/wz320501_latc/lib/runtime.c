#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int i){
	printf("%d\n",i);
}

void printString(char* s){//sprawdzić, czy działa
	printf("%s\n",s);
}

void error(){
	printf("runtime error\n");
	exit(1);
}

int readInt(){
	int i;
	scanf("%d\n",&i);
	return i;
}

char* readString(){
	char* s;
	size_t len=0;
	ssize_t length = getline(&s,&len,stdin);
	s[length-1]='\0';
	return s;
}
	
char* concat(char *str1, char *str2){
	char *str3 = malloc(strlen(str1)+strlen(str2)+1);
	strcpy(str3,str1);
	strcat(str3,str2);
	return str3;
}
