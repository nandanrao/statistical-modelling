# RStudio Server with BigReg

### EC2
Launch an EC2 instance. It can technically be any size, but you'll be limited to those resources in RStudio, of course. Might be smart to go with an instance with a few cores, as the script installs multi-core libraries. I'm using a C4.2XLarge - $0.48/hr.

* Choose the AMI "nandan-bigreg" from community AMI's. ID: ami-6197df12

* Get to the "Add Storage" page in the instance configuration (Step 4).

* Change the Volume Type of the EBS volume from "General Purpose SSD" to "Throughput Optimized HDD", and change the size to 4000GB. EBS costs hardly anything to run for a few hours, and HDD is cheap, so it will be < 1$. It's the read performance we want. 

* Open port 8787 in the security group attached to your EC2 instance.


### Run RStudio Server
SSH into your instance and run the following commands (you can just copy and paste the whole block into your SSH terminal):

```
sudo mount /dev/xvdb /bigreg && mkdir /bigreg/tmp 
sudo chmod 777 /bigreg/tmp && chmod 777 /bigreg/HIGGS && chmod 777 /bigreg/airlines
sudo blockdev --setra 2048 /dev/xvdb
sudo service docker restart
sudo docker run -d -p 8787:8787 -v /bigreg/tmp/:/tmp -v /bigreg/:/bigreg rocker/rstudio
```

Get the public IP address of your EC2 instance, and navigate to port 8787 in your browser:

PUBLIC_IP:8787

Login to RStudio with the following credentials:

password: rstudio
username: rstudio

### Run the script

Just copy and paste bigreg_nandan.R into a new R Script and run it, should work!
