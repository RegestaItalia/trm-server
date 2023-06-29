# trm-server
![Custom badge](https://img.shields.io/endpoint?url=https://www.trmregistry.com/public/shieldio/license?package=trm-server)
![Custom badge](https://img.shields.io/endpoint?url=https://www.trmregistry.com/public/shieldio/version?package=trm-server)
![Custom badge](https://img.shields.io/endpoint?url=https://www.trmregistry.com/public/shieldio/downloads?package=trm-server)

TRM Server is the essential component for operations between TRM and your development SAP® system.

Find more about TRM [here](https://github.com/RegestaItalia/trm-client).

## Install
### First setup
Download the source code of the main branch from [here](https://github.com/RegestaItalia/trm-server/archive/refs/heads/main.zip).

![image](https://github.com/RegestaItalia/trm-server/assets/87023474/bc654cff-25cf-4f9e-9b95-eb52465cff7a)

Once downloaded, [install abapGit](https://docs.abapgit.org/guide-install.html#install-standalone-version) (standalone version is enough).

Execute transaction SE38 and run the abapGit program.

With abapGit report running, you can follow [these steps](https://docs.abapgit.org/guide-offline-install.html).

![image](https://github.com/RegestaItalia/trm-server/assets/87023474/63c7c7c9-e13c-4bad-8f6b-4410a445fc1f)

Create a new offline repository.

![image](https://github.com/RegestaItalia/trm-server/assets/87023474/fdd3440a-b512-48d2-9b6c-cb36eb6c4c76)

You can name the package whatever you prefer, although it's good practice to name it **$TRM**: this package is not meant to be transported.

![image](https://github.com/RegestaItalia/trm-server/assets/87023474/995b7d18-4c14-4471-905d-1f57484d96f4)

### Update
With [trm-cli](https://github.com/RegestaItalia/trm-client) installed, update with the following command:
```
trm update trm-server
```

## FAQ
For any question regarding trm-server, you can open an issue.
Before opening an issue, check if it's trm-server related: all other issues should be opened in their respective GitHub repos.
