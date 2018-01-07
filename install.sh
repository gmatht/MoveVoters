alias sudo=`which sudo` # If sudo is not installed, assume root user
export LD_RUN_PATH=/usr/local/lib
if [ -d /var/www/html ]
then
	WWW=/var/www/html
else
	WWW=/var/www
fi
	
make && 
if ./movevoters > test.log
then
	echo Success
	(
	sudo cp movevoters /usr/lib/cgi-bin/movevoters.cgi ;
	sudo cp movevoters.html $WWW;
	sudo chmod a+r $WWW/movevoters.html
	)
else
	echo SELF DIAGNOSTIC FAILED
fi
