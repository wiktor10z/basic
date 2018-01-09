
sudo sshpass -p BackOnII sftp -o StrictHostKeyChecking=no BackOnII@10.0.2.2:Klient/BackOnII-Klient_2.0.0.1.tgz .
tar -xzvf BackOnII-Klient_2.0.0.1.tgz
sleep 1
OS=$(lsb_release -si)
VER=$(lsb_release -sr)
if [ $OS = "Ubuntu" ]; then
	apt-get install cpuid
	mkdir -p /opt/BackOnII-Klient_2.0.0.1
	cp BackOnII-Klient/usluga  /opt/BackOnII-Klient_2.0.0.1
	cp global_data /opt/BackOnII-Klient_2.0.0.1
	cp BackOnII-Klient/klient  /opt/BackOnII-Klient_2.0.0.1
	if dpkg --compare-versions  "gt" "14.10" ; then
		cp BackOnII-Klient/service_script /opt/BackOnII-Klient_2.0.0.1
		cp -r BackOnII-Klient/BackOnII-Klient_2.0.0.1.service /etc/systemd/system/BackOnII-Klient_2.0.0.1.service
		systemctl enable BackOnII-Klient_2.0.0.1
		systemctl start BackOnII-Klient_2.0.0.1
	else
		cp -r BackOnII-Klient/BackOnII-Klient_2.0.0.1.conf /etc/init/BackOnII-Klient_2.0.0.1.conf
		service BackOnII-Klient_2.0.0.1 start
	fi
else
	echo "Nieobsługiwany system operacyjny"
fi
rm -r BackOnII-Klient
rm BackOnII-Klient_2.0.0.1.tgz

