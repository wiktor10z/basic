
sudo sshpass -p BackOnII sftp -o StrictHostKeyChecking=no BackOnII@10.0.2.2:Klient/BackOnII-Klient_2.0.0.2.tgz .
tar -xzvf BackOnII-Klient_2.0.0.2.tgz
OS=$(lsb_release -si)
VER=$(lsb_release -sr)
if [ $OS = "Ubuntu" ]; then
	apt-get install cpuid
	mkdir -p /opt/BackOnII-Klient_2.0.0.2
	cp global_data /opt/BackOnII-Klient_2.0.0.2
	cp BackOnII-Klient_2.0.0.2/usluga  /opt/BackOnII-Klient_2.0.0.2
	cp BackOnII-Klient_2.0.0.2/klient  /opt/BackOnII-Klient_2.0.0.2
	if dpkg --compare-versions  "gt" "14.10" ; then
		cp BackOnII-Klient_2.0.0.2/service_script /opt/BackOnII-Klient_2.0.0.2
		cp -r BackOnII-Klient_2.0.0.2/BackOnII-Klient_2.0.0.2.service /etc/systemd/system/BackOnII-Klient_2.0.0.2.service
		systemctl enable BackOnII-Klient_2.0.0.2
		systemctl start BackOnII-Klient_2.0.0.2
	else
		cp -r BackOnII-Klient_2.0.0.2/BackOnII-Klient_2.0.0.2.conf /etc/init/BackOnII-Klient_2.0.0.2.conf
		service BackOnII-Klient_2.0.0.2 start
	fi
else
	echo "Nieobsługiwany system operacyjny"
fi
rm -r BackOnII-Klient_2.0.0.2
rm BackOnII-Klient_2.0.0.2.tgz

