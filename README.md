# DocReg+Web

A distributed document management system web interface.
Created by Scott Abernethy (github @scott-abernethy).

![Example of application](/example.png)

## Development

### Dev setup
The following setup has been tested on Ubuntu 12.04.2 LTS desktop.

1. Install git, mysql-server-5+, java6+

    ```bash
    # On Ubuntu, using webupd8 team PPA for java
    sudo add-apt-repository ppa:webupd8team/java
    sudo apt-get update
    sudo apt-get install git mysql-server oracle-java6-installer
    ```

2. Fork the project source from https://github.com/ververve/docreg-web.git
3. Clone your fork to a local project directory

    ```bash
    git clone https://github.com/YOURGITHUBACCOUNT/docreg-web.git
    ```

4. Create a new database 'docregweb', using the project schema

    ```bash
    mysql -u root -p < src/main/resources/schema
    ```

5. Start SBT console

    ```bash
    # On Ubuntu
    ./sbt
    ```

    ```bash
    # On Windows
    sbt.bat
    ```

6. (In SBT) Start the app (note this will take a _long_ time as SBT downloads all the project dependencies)

    ```bash
    > container:start
    ```

    Note that during development, to automatically reload the app after making a file change, use this command instead

    ```bash
    > ~;container:start; container:reload /
    ```

7. Open a browser on http://localhost:9090/ to view the running app.

    In development mode, login with username *bruce* or *peter* or *sue*, with no password.

8. (In SBT) Setup Eclipse or IntelliJ IDEA project

    ```bash
    # either ...
    > eclipse with-source=true
    # ... or ...
    > idea with-source=true
    ```

### About 

This project is predominately a [Lift](http://liftweb.net) Web app written in [Scala](http://www.scala-lang.org). Both [Lift](http://liftweb.net) and [Scala](http://www.scala-lang.org) have good online documentation and strong community mailing lists for support.

## License

DocReg+Web is distributed under the [GNU General Public License v3](http://www.gnu.org/licenses/gpl-3.0.html).

