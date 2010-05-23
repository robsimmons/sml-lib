
MC=mysql_config
# MC=/usr0/src/mysql-standard-5.0.4-beta-pc-linux-gnu-i686/bin/mysql_config

LO=`echo "-L/usr0/src/mysql-standard-5.0.4-beta-pc-linux-gnu-i686/lib -lmysqlclient_r -lz -lpthread -lcrypt -lnsl -lm -lpthread -lc -lnss_files -lnss_dns -lresolv -lc -lnss_files -lnss_dns -lresolv" | sed -e 's/ / -link-opt /g'`

# -expert true -enable-ann 'allowPrim true' -default-ann 'allowPrim true'
test : *.sml mysql.o test.cm
	mlton -default-ann 'allowFFI true' -const 'Exn.keepHistory true' -link-opt ${LO} test.cm mysql.o


mysql.o : mysql.c
	gcc -o $@ -c -O `${MC} --cflags` `${MC} --include` $<