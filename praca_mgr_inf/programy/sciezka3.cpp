#include <cstdio>
#include <cstdlib>
#include <bitset>
#include <algorithm>
#include <memory>
#include <vector>

#ifndef N
#define N 8
#endif
#ifndef N2
#define N2 256
#endif
#ifndef TIMES
#define TIMES 1
#endif
#ifndef LEVEL
#define LEVEL 0
#endif


// 22 - 3000/0
// 23 - 1700/0
// 24 - 1000/0
// 25 - 700/0
// 26 - 400/0
// 27 - 200/0
// 28 - 100/0
// 29 - 40/0
// 30 - 15/0

using namespace std;

bitset<N2> map[LEVEL+1];


vector<int> exact_rec(bitset<N2> map,int a,int dim_used){
	int k,l,max=0;
	vector<int> val,val2;
	bitset<N2> map2=map;
	for(int j=1;j<N2;j*=2){
		if((a/j)%2){
			l=-1*j;
		}else{
			l=j;
		}
		map2.set(a+l);
	}
	k=1;
	for(int j=0;j<dim_used;++j){
		if((a/k)%2){
			l=-1*k;
		}else{
			l=k;
		}
		if(!map[a+l]){
			val=exact_rec(map2,a+l,dim_used);
			if(val.size()>max){
				max=val.size();
				val2=val;
			}
		}
		k*=2;
	}
	if(dim_used!=N){
		val=exact_rec(map2,a+k,dim_used+1);
		if(val.size()>max){
			max=val.size();
			val2=val;
		}

	}
	val2.push_back(a);
	return val2;
}


vector<int> random_ind(int a, int dim_used){
	int move;
	vector<int> children;
	vector<int> val;
	val.push_back(a);
	int k,l;
	for(;;){
		k=1;
		for(int j=0;j<dim_used;++j){
			if((a/k)%2){
				l=-1*k;
			}else{
				l=k;
			}
			if(!map[0][a+l]){
				children.push_back(l);
			}
			k*=2;
		}
		if(dim_used!=N){
			children.push_back(k);
		}
		for(int j=1;j<N2;j*=2){
			if((a/j)%2){
				l=-1*j;
			}else{
				l=j;
			}
			map[0].set(a+l);
		}
		if(!children.empty()){
			move=children[rand()%children.size()];
			a=a+move;
			val.push_back(a);
			if(move==k){
				dim_used++;			
			}
		}else{
			return val;
		}
		children.clear();
	}
}

void extra_big_examples(){
	vector<int> val,val2,children;
	int a,l,move,max=0;
	unique_ptr< bitset<N2> > ip { new bitset<N2> };
	auto &map = *ip;	
	for(int i=0;i<TIMES;++i){
		vector<int> val;
		val.push_back(0);
		val.push_back(1);
		val.push_back(3);
		val.push_back(7);
		for(int m=0;m<4;++m){
			a=val[m];
			for(int j=1;j<N2;j*=2){
				if((a/j)%2){
					l=-1*j;
				}else{
					l=j;
				}
				map.set(a+l);
			}
		}
		children.push_back(-1);
		children.push_back(8);
		while(!children.empty()){
			move=children[rand()%children.size()];
			a=a+move;
			val.push_back(a);		
			children.clear();
			for(int j=1;j<N2;j*=2){
				if((a/j)%2){
					l=-1*j;
				}else{
					l=j;
				}
				if(!map[a+l]){
					children.push_back(l);
					map.set(a+l);	
				}

			}
		}
		map.reset();
		if(val.size()>max){
			val2=val;
			max=val.size();
		}
		val.clear();
	}
	printf("%ld\n",val2.size());
	for(int i=0;i<val2.size();++i){
		printf("%d - ",i+1);
		for(long int j=N2/2;j>0;j/=2){
			printf("%ld",(val2[i]/j)%2);
		}
		printf("\n");			
	}
	
}

void long_long_examples(){
	vector<long long int> val,children;
	long long int a,l,move,max=0;
	unique_ptr< bitset<N2> > ip { new bitset<N2> };
	auto &map = *ip;
	val.push_back(0);
	val.push_back(1);
	val.push_back(3);
	val.push_back(7);
	for(int m=0;m<4;++m){
		a=val[m];
		for(long long int j=1;j<N2;j*=2){
			if((a/j)%2){
				l=-1*j;
			}else{
				l=j;
			}
			map.set(a+l);
		}
	}
	children.push_back(-1);
	children.push_back(8);
	while(!children.empty()){
		move=children[rand()%children.size()];
		a=a+move;
		val.push_back(a);		
		children.clear();
		for(long long int j=1;j<N2;j*=2){
			if((a/j)%2){
				l=-1*j;
			}else{
				l=j;
			}
			if(!map[a+l]){
				children.push_back(l);
				map.set(a+l);	
			}
		}
	}
	printf("%ld\n",val.size());
}


vector<int> NMCS(int a,int level,int dim_used){
	int k,l,best=-1,continue1=0;
	vector<int> val,val2,val3,val4,children;//val - budowany jako wynik, val2- wynik operacji z poziomu niżej, val3- najlepszy z tych wyników, val4 - najlepszy po wszystkich iteracjach
	if(!level){
		return random_ind(a,dim_used);
	}
	val4.push_back(a);
	for(;;){
		val.push_back(a);
		continue1=0;
		k=1;
		for(int j=0;j<dim_used;++j){
			if((a/k)%2){
				l=-1*k;
			}else{
				l=k;
			}
			if(!map[level][a+l]){
				children.push_back(a+l);
			}
			k*=2;
		}
		for(int j=1;j<N2;j*=2){
			if((a/j)%2){
				l=-1*j;
			}else{
				l=j;
			}
			map[level].set(a+l);
		}
		if((dim_used==N)){//brak dzieci lub dokładnie jedno = nie potrzeba uruchamiać poziomu niżej
			if(children.size()==0){
				return val4;
			}else if(children.size()==1){
				a=children[0];
				continue1=1;
			}
		}else{
			if(children.size()==0){
				a=a+k;
				dim_used++;
				continue1=1;
			}
		}
		if(!continue1){
			for(int j=0;j<children.size();++j){
				for(int m=0;m<TIMES;++m){
					map[level-1]=map[level];
					val2=NMCS(children[j],level-1,dim_used);
					if(val2.size()>val3.size()){
						val3=val2;
						best=children[j];
					}
				}
			}
			if(dim_used!=N){
				for(int m=0;m<TIMES;++m){
					map[level-1]=map[level];
					val2=NMCS(a+k,level-1,dim_used+1);
					if(val2.size()>val3.size()){
						val3=val2;
						best=a+k;
					}
				}
			}
			if(best==a+k){
				dim_used++;
			}
			if(val.size()+val3.size()>val4.size()){
				val4=val;
				val4.insert(val4.end(),val3.begin(),val3.end());
			}
			a=best;
		}
		children.clear();
		val3.clear();
	}
}






int main(){
	srand(time(NULL));
	if(N>30){
		long_long_examples();
	}else if(N>24){
		extra_big_examples();
	}else if(N<3){
		vector<int> val=exact_rec(map[0],0,0);
		printf("%ld\n",val.size());	
		for(int i=val.size()-1;i>=0;--i){
			printf("%ld - ",val.size()-i);
			for(int j=1;j<N2;j*=2){
				printf("%d",(val[i]/j)%2);
			}
			printf("\n");
		}
	}else{
		int l,a;
		vector<int> val;
		val.push_back(0);
		val.push_back(1);
		val.push_back(3);
		for(int i=0;i<3;++i){
			a=val[i];
			for(int k=1;k<N2;k*=2){
				if((a/k)%2){
					l=-1*k;
				}else{
					l=k;
				}
				map[LEVEL].set(a+l);
			}
		}
		vector<int> val2;
		if(N<7){
			val2=exact_rec(map[0],7,3);
			reverse(val2.begin(),val2.end());
		}else if(!LEVEL){
			vector<int> val3;
			int max=0;
			bitset<N2> map2=map[0];
			for(int i=0;i<TIMES;++i){
				val3=random_ind(7,3);
				if(val3.size()>val2.size()){
					val2=val3;
				}
				map[0]=map2;
			}
		}else{
			val2=NMCS(7,LEVEL,3);
		}

		printf("%ld\n",val2.size()+3);
		for(int i=0;i<3;++i){
			printf("%d - ",i+1);
			for(long int j=N2/2;j>0;j/=2){
				printf("%ld",(val[i]/j)%2);
			}
			printf("\n");			
		}
		for(long int i=0;i<val2.size();++i){
			printf("%ld - ",i+4);
			for(long int j=N2/2;j>0;j/=2){
				printf("%ld",(val2[i]/j)%2);
			}
			printf("\n");
		}
	}
}
