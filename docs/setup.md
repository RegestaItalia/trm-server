# trm-server Setup

> The server-side component of TRM is identified as trm-server

## First install

The first install must be performed using [abapGit](https://abapgit.org/).

AbapGit is an open source git client for ABAP.

If you haven't installed it already, follow the official guide at [this link](https://docs.abapgit.org/user-guide/getting-started/install.html).

The minimum installation of abapGit requires the [standalone version](https://docs.abapgit.org/user-guide/getting-started/install.html#install-standalone-version).

Once abapGit installed, you can either install trm-server as an online or offline repository.

> If your system has [connection to Github](https://docs.abapgit.org/user-guide/setup/ssl-setup.html), it's recommended to install it as an online repository.

### Online repo

1. Run abapGit (standalone/developer)
2. On the repository list page, select "New Online"
3. Enter `https://github.com/RegestaItalia/trm-server` for the URL
4. Enter package name `ZTRM`
5. Select "Clone Online Repo"
6. Select "Pull"

### Offline repo

First download the latest release source code from
1. Direct link [https://github.com/RegestaItalia/trm-server/archive/refs/heads/main.zip](https://github.com/RegestaItalia/trm-server/archive/refs/heads/main.zip)
2. Open the [Github repo page](https://github.com/RegestaItalia/trm-server)
   1. Press the button "<> Code"
   2. Press "Download ZIP"

With the source code zip file saved on your computer
1. Run abapGit (standalone/developer)
2. On the repository list page, select "New Offline"
3. Enter repo name `trm-server` and package name `ZTRM`
1. Select "Create Offline Repo"
1. Select "Import <sup>zip</sup>"
1. Select the _trm-server-main.zip_ file
1. Select "Pull <sup>zip</sup>"

## User authorization maintenance

This package exposes RFC enabled function modules.

In order to make it safer, all users are, by default, not authorized to call most of the RFC functions.

Before using trm-server, maintenance of the authorized users must be performed.

First, run transaction `SM30` and maintain table `ZTRM_USERS`.

<p align="center">
    <img src="https://raw.githubusercontent.com/RegestaItalia/trm-server/main/docs/images/sm30_ztrm_users1.png" alt="SM30">
</p>

Press on **New Entries**.

<p align="center">
    <img src="https://raw.githubusercontent.com/RegestaItalia/trm-server/main/docs/images/sm30_ztrm_users2.png" alt="SM30">
</p>

Enter the names of the users allowed to run TRM RFC functions.

<p align="center">
    <img src="https://raw.githubusercontent.com/RegestaItalia/trm-server/main/docs/images/sm30_ztrm_users3.png" alt="SM30">
</p>

## Update

With trm-server already installed on your system, there are two ways to keep it up to date:

1. You may keep using [abapGit](https://abapgit.org/)
   - If it's an online repo, simply pull from the "main" branch
   - If it's an offline repo, download the "main" branch source code as "zip" from Github and import
2. You may use TRM Client and get from the Public Registry the latest release.
   
   In this case, simply run the command:
   
   `trm update trm-server`

Both methods will assure you the latest version installed on your system.

## Check installation

To verify trm-server is installed properly:
1. Open transaction "SE80"
2. Open package "ZTRM"
3. Right click on the root package and press "Check > Package Check > Package Objects (Including Subpackages)"

This check should not give any errors.

### Verify RFC Authorization

To verify a user has access to RFC functions exposed by trm-server, log into your system as the user you want to test and execute the function module `ZTRM_PING`.

If the user is authorized, it shouldn't give any errors.

> Remember that, for security reasons, TRM doesn't allow any user to execute RFC functions.

> You can maintain allowed RFC function users in table ZTRM_USERS
