# DocReg+Web

A distributed document management system web interface.
Created by Scott Abernethy (github @scott-abernethy).

![Example of application](/example.png)

## Development

### Dev setup on Ubuntu
The following setup has been tested on Ubuntu 12.04.2 LTS desktop.

*NOTE* this setup results in a live version of DocReg: any document submissions etc will be replicated on production DocReg systems.

```bash
# 0. Setup
PROJECTROOT=~/docreg-web
YOURUSERNAME=sabernethy
YOURPASSWORD=...

# 1. Install git, sshfs, mysql-server
sudo apt-get install git sshfs mysql-server

# 2. Install Oracle Java 6 (there are many ways to do this, my preference is below)
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java6-installer

# 3. Get the project source
git clone https://github.com/scott-abernethy/docreg-web.git $PROJECTROOT

# 4. Create the database
cd $PROJECTROOT
mysql -u root -p < ./src/main/resources/schema

# 5. Mirror a DocReg server home directory (because the app requires local filesystem access)
sudo mkdir -p /home/docreg
sudo chown $YOURUSERNAME /home/docreg
sudo gpasswd -a $YOURUSERNAME fuse
sshfs -o idmap=user,nonempty docreg@shelob: /home/docreg

# 6. Configure the app
cat >>/tmp/docreg-web.conf <<EOF
db {
   driver = com.mysql.jdbc.Driver
   url = jdbc:mysql://localhost/docregweb
   user = root
}
agent {
   server = shelob
   home = /home/docreg
   secure = true
}
ldap {
   url = "ldap://dcgnetnz1.gnet.global.vpn:3268"
   user = "gnet\\$YOURUSERNAME"
   password = "$YOURPASSWORD"
}
EOF
sudo mv /{tmp,etc}/docreg-web.conf

# 7. Start the app (note this will take a _long_ time as SBT downloads all the project dependencies)
cd $PROJECTROOT
./sbt
> container:start

# 8. Open a browser on http://localhost:8080/ to view the running app. Note that the app will take some time to parse and cache the DocReg database on first run.
```

### About 

This project is predominately a [http://liftweb.net](Lift) Web app written in [http://www.scala-lang.org](Scala). Both [http://liftweb.net](Lift) and [http://www.scala-lang.org](Scala) have good online documentation and strong community mailing lists for support.

## License

DocReg+Web is distributed under the [GNU General Public License v3](http://www.gnu.org/licenses/gpl-3.0.html).
