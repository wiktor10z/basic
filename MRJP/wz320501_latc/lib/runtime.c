#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void printInt(int i){
	printf("%d\n",i);
}

void printString(char* s){
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

void* allocatearr(int size,int length){
	void* a = calloc(1,size*length+4);
	int *b=a;
	b[0]=length;
	return a;
}

void* allocatearrofpointers (int length, long long int default1){
	void* a = calloc(1,8*length+4);
	int *b=a;
	b[0]=length;
	long long int *c=a+4;
	int i;
	for(i=0;i<length;++i){
		c[i]=default1;
	}
	return a;
}
