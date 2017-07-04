# NTU MOOC X Coursera Data Analytics
* Aims at deriving insights from NTU MOOCs data 
* Provide Overview, Participation, Engagement, Discussion and Wordcloud sections
* Use shiny R as an interactive website framwork
* Retrieve Course Data from Coursera Data Export API
<br><br>
<!--![snapshot](about/snapshot.png)-->

## Table of Content
* [Environment](#Environment)
* [R packages](#R-packages)
* [Setup - Ubuntu on VMWare Workstation 12 Player](#Setup---Ubuntu-on-VMWare-Workstation-12-Player)
* [Setup - Shiny Serevr on Ubuntu 12.04 or later](#Setup---Shiny-Serevr-on-Ubuntu-12.04-or-later)
* [Setup - R packages](#Setup---R-packages)
* [Coursera Data Export API](#Coursera-Data-Export-API)
 
## Environment
* R-3.3.2
* [Shiny-Server-1.5.1.834](https://www.rstudio.com/products/shiny/download-server/)
* Java-OpenSDK-7
* Ubuntu-14.04
* VMWare-Workstation-Player-12 

## R packages
* [Shiny](https://shiny.rstudio.com/)
 	+ shiny, shinyjs 
* Data Manipulation
	+ dplyr, DT, reshape, RMySQL
* Plot
	+ lattice, ggplot2, plotly, scales
* Word Processing
	+ rJava, RColorBrewer, Rwordseg, tm, tmcn, wordcloud

## Setup - Ubuntu on VMWare Workstation 12 Player
* VMWare Workstation 12 Player
	+ Download [VMNETCFG.EXE](https://ninety9names.wordpress.com/2013/12/18/download-vmnetcfg-exe-vmnetcfglib-dll-for-vmware-player-6-x-7-x/comment-page-1/#comment-17) for Virtual Network Editor of VMWare Workstation 12 Player
	+ Edit NAT setting
		+ Open Virtual Network Editor
		+ Add Port Forwarding
			+ Host port
			+ Virtual machine IP and port of the service (__:3838__ for shiny-server)
	+ Set intbound rule for the service port (__:3838__ for shiny-server) on the host's firewall
* xRDP-Ubuntu-Mate-Desktop [[1](http://expertisenpuru.com/how-to-fix-grey-screen-with-x-cursor-problem-in-ubuntu-14-04-or-higher/)]
	+ Install xRDP-Ubuntu-Mate-Desktop
	```
	$ apt-get install xrdp
	$ apt-add-repository ppa:ubuntu-mate-dev/ppa
	$ apt-add-repository ppa:ubuntu-mate-dev/trusty-mate
	$ apt-get update
	$ # apt-get upgrade
	$ apt-get install ubuntu-mate-core ubuntu-mate-desktop
	$ mate-session >~/.xsession
	$ service xrdp restart
	```
	+ Edit NAT setting for Remote Control
		+ Open Virtual Network Editor
		+ Add Port Forwarding
			+ Host port (e.g. __:3390__)
			+ Virtual machine IP and rdp port (e.g. __:3389__ default)
	+ Set inbound rule for the rdp port (__:3390__) on the host's firewall

	
## Setup - Shiny Serevr on Ubuntu 12.04 or later
* Download Shiny Server [[1](https://www.rstudio.com/products/shiny/download-server/)]
```
$ sudo apt-get install r-base
$ sudo su - \ -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
$ sudo apt-get install gdebi-core```
$ wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
$ sudo gdebi shiny-server-1.5.1.834-amd64.deb
```
* Update R version [[1](https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04)]
```
$ sudo sh -c 'echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list'
$ gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
$ gpg -a --export E084DAB9 | sudo apt-key add -
$ sudo apt-get update
$ sudo apt-get -y install r-base
```
* Application
	+ __/srv/shiny-server/__
	+ __/srv/shiny-server/server.R__ and __/srv/shiny-server/ui.R__ (__index.html__) are required
	+ In __server.R__, the way to initiate shiny-server is ``` shinyServer(function(input, output, session){ ... }) ```
	+ If there is ``` renderPlot({ ... }) ``` or ``` renderPlotly({ ... }) ``` plot output in the application then put ``` pdf(NULL) ``` [[1](https://github.com/ropensci/plotly/issues/494)] in the top of the code
* Configuration 
	+ __/etc/shiny-server/shiny-server.conf__
	``` 	
	run_as shiny;
	server {
		listen 3838;
		location /app-moocs {
		    site_dir /srv/shiny-server/app-moocs;
		    log_dir /var/log/shiny-server/app-moocs;
		    directory_index on;
		}
	}
	```
## Setup - R packages
* Most of the packages can be installed through R command ``` install.packages("package_name") ```  (or install from source)
	+ tmcn
	``` 
	install.packages("tmcn", repos="http://R-Forge.R-project.org") 
	```
	+ Rwordseg
	``` 
	install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
	```
* However, some of the packages have dependency problems, so run terminal command before R command
	+ plotly [[1](http://unix.stackexchange.com/questions/320594/how-to-install-r-plotly-in-debian)]
	```
	$ apt-get install libssl-dev
	$ apt-get install libcurl4-openssl-dev
	$ R
	> install.packages("plotly")
	```
	+ rJava [[1](http://stackoverflow.com/questions/34212378/installation-of-rjava)] [[2](http://stackoverflow.com/questions/26797428/failing-to-install-rjava-package-in-r-with-error-bootstrap-class-path-not-set-i)]
	```
	$ apt-get install openjdk-7-jdk
	$ R CMD javareconf
	$ reboot
	$ apt-get install liblzma-dev
	$ R
	> install.packages("rJava")
	```
	+ RMySQL [[1](http://blogs.candoerz.com/question/269278/error-installing-rmysql-mysql-5537-in-ubuntu-1404-.aspx)] [[2](http://stats.stackexchange.com/questions/194515/r-package-rmysql-installation-returns-a-configuration-failed-error)]
	```
	$ apt-get install libmysql++-dev
	$ apt-get install libmysqlclient-dev
	$ apt-get install r-cran-rmysql
	$ R
	> install.packages("RMySQL")
	```
## Coursera Data Export API
* Install python 2.7 distribution of Anaconda [[1](https://www.continuum.io/downloads)]
```
$ bash ~/Anaconda2-4.4.0-Linux-x86_64.sh
```
* install python packages for coursera data export
```
$ pip install courseraresearchexports
$ pip install autocomplete
$ activate-global-python-argcomplete
$ pip install courseraoauth2client
```
*
