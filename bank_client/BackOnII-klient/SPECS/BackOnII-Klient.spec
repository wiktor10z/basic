Summary: 	BackONII-Klient
Name:		BackOnII-Klient
Version:	2.0.0.1

%build
make

%post
./klient
