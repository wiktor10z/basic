VERSION1='2.0.0.2'
#TODO VERSION1 jako argument skryptu
make VERSION="\"\\\"$VERSION1\\\"\""
make clean

echo "00010203040FFA9708090A0B0C0D0E0F0001020304050607
0901020304050607" >global_data

echo "[Unit]
Description = BackOnII klient linux
After = network.target

[Service]
Type=simple
PIDFile=/var/run/BackOnII-Klient.pid
ExecStart=/opt/BackOnII-Klient_$VERSION1/service_script
ExecReload=/opt/BackOnII-Klient_$VERSION1/service_script
Restart=always

[Install]
WantedBy=default.target" > BackOnII-Klient_$VERSION1.service 

echo "description \"BackOnII klient linux\"
author \"Wiktor Zuba\"

start on runlevel [2345]

stop on runlevel [016]

respawn

script
    echo \$\$ > /var/run/BackOnII-Klient.pid
    chdir /opt/BackOnII-Klient_$VERSION1
    exec ./usluga >/var/log/BackOnII-Klient.log 2>/var/log/BackOnII-Klient-err.log
end script

post-stop script
    rm -f /var/run/BackOnII-Klient.pid
end script" > BackOnII-Klient_$VERSION1.conf


rm -rf BackOnII-Klient_$VERSION1
mkdir -p BackOnII-Klient_$VERSION1
mv klient BackOnII-Klient_$VERSION1
mv usluga BackOnII-Klient_$VERSION1
mv service_script BackOnII-Klient_$VERSION1
mv BackOnII-Klient_$VERSION1.conf BackOnII-Klient_$VERSION1
mv BackOnII-Klient_$VERSION1.service BackOnII-Klient_$VERSION1
cp global_data BackOnII-Klient_$VERSION1
cp README BackOnII-Klient_$VERSION1

tar -czvf BackOnII-Klient_$VERSION1.tgz BackOnII-Klient_$VERSION1 

echo "
sudo sshpass -p "BackOnII" sftp -o StrictHostKeyChecking=no BackOnII@10.0.2.2:Klient/BackOnII-Klient_$VERSION1.tgz .
tar -xzvf BackOnII-Klient_$VERSION1.tgz
OS=\$(lsb_release -si)
VER=\$(lsb_release -sr)
if [ \$OS = \"Ubuntu\" ]; then
	apt-get install cpuid
	mkdir -p /opt/BackOnII-Klient_$VERSION1
	cp global_data /opt/BackOnII-Klient_$VERSION1
	cp BackOnII-Klient_$VERSION1/usluga  /opt/BackOnII-Klient_$VERSION1
	cp BackOnII-Klient_$VERSION1/klient  /opt/BackOnII-Klient_$VERSION1
	if dpkg --compare-versions $VER \"gt\" \"14.10\" ; then
		cp BackOnII-Klient_$VERSION1/service_script /opt/BackOnII-Klient_$VERSION1
		cp -r BackOnII-Klient_$VERSION1/BackOnII-Klient_$VERSION1.service /etc/systemd/system/BackOnII-Klient_$VERSION1.service
		systemctl enable BackOnII-Klient_$VERSION1
		systemctl start BackOnII-Klient_$VERSION1
	else
		cp -r BackOnII-Klient_$VERSION1/BackOnII-Klient_$VERSION1.conf /etc/init/BackOnII-Klient_$VERSION1.conf
		service BackOnII-Klient_$VERSION1 start
	fi
else
	echo \"Nieobsługiwany system operacyjny\"
fi
rm -r BackOnII-Klient_$VERSION1
rm BackOnII-Klient_$VERSION1.tgz
" > aktLinux.sh


