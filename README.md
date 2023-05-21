# trm-server
TRM Server-Side is essential for operations between TRM and SAP System.

Find more about TRM [here](https://github.com/RegestaItalia/trm-cli).

## Status
TRM is under development.

Issues and pull requests are currently limited to developers during this stage.

## Install
Download the source code of the main branch from [here](https://github.com/RegestaItalia/trm-server/archive/refs/heads/main.zip).

Once downloaded, if not done already, [install abapGit](https://docs.abapgit.org/guide-install.html#install-standalone-version) (standalone version is enough).

Execute transaction SE38 and run the abapGit program.

Once abapGit is running, you can follow [these steps](https://docs.abapgit.org/guide-offline-install.html).

You can name the package however you want (for example $TRM).

It is recommended to name is with the prefix $, because the package is not meant to be transported but should only stay on the development system.

## Update
With TRM CLI installed, update with the following command
```
trm update trm-server
```
If prompted, choose the public registry.
