# <a href="https://docs.trmregistry.com/#/server/README"><img src="https://docs.trmregistry.com/logo.png" height="40" alt="TRM"></a>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-1.3.0-4baaaa.svg)](https://github.com/RegestaItalia/trm-docs/blob/main/CODE_OF_CONDUCT.md)
[![TRM License](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/license/trm-server)](https://trmregistry.com/package/trm-server)
[![TRM Latest version](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/version/trm-server)](https://trmregistry.com/package/trm-server)
[![TRM Installs](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/downloads/trm-server)](https://trmregistry.com/package/trm-server)

| 🚀 This project is funded and maintained by 🏦 | 🔗                                                             |
|-------------------------------------------------|----------------------------------------------------------------|
| Regesta S.p.A.                                  | [https://www.regestaitalia.eu/](https://www.regestaitalia.eu/) |
| Clarex S.r.l.                                   | [https://www.clarex.it/](https://www.clarex.it/)               |

**trm-server** is the server-side component of TRM.

🚚 **TRM (Transport Request Manager)** is a package manager inspired solution built leveraging CTS that simplifies SAP ABAP transports.

<p align="center">
  <img src="https://docs.trmregistry.com/logo.png" alt="TRM Logo" />
</p>

TRM introduces **package-based software delivery** to the SAP ecosystem, bringing with it semantic versioning, dependency management, and automated deployment activities.

> [!NOTE]
> If you're looking for the REST implementation of **trm-server**, head over to [**trm-rest**](https://github.com/RegestaItalia/trm-rest)

It acts as the secure bridge between the **SAP application server** and the **TRM client**.

It exposes the APIs that allow TRM to import/export transports, and, as a consequence, it must be installed **only** on source and target systems, as its not required on any other system in the landscape.

```
              ┌──────────────────────────┐
              │        TRM Client        │
              └─────────────┬────────────┘
                            │
                            │  System Connector
                            │  (RFC SDK / REST / BTP / ...)
                            ▼
              ┌──────────────────────────┐
              │        trm-server        │
              │  (Installed on SAP)      │
              └─────────────┬────────────┘
                            │
                            │  Native SAP APIs
                            ▼
              ┌──────────────────────────┐
              │        SAP System        │
              └──────────────────────────┘
```

# Documentation

Full documentation can be seen at [https://docs.trmregistry.com](https://docs.trmregistry.com).

# Basic Usage

In a typical scenario, TRM Client connects to a SAP system through a **System Connector** (RFC SDK, REST, BTP, etc.).  
After checking that **trm-server** is installed on the system, the client uses its APIs to perform secure and controlled transport export and import operations.  
If not installed, the client can still communicate directly with the SAP system, but with limited capabilities and without extended TRM features.

---

<!-- START OF SETUP.MD -->

# Server setup

**trm-server** is the server-side component of TRM.

It acts as the secure bridge between the **SAP application server** and the **TRM client**.

It exposes the APIs that allow TRM to import/export transports, and, as a consequence, it must be installed **only** on source and target systems, as its not required on any other system in the landscape.

## First Installation

The first installation must be performed using [abapGit](https://abapgit.org/).

If you have not installed it yet, follow the official guide:\
https://docs.abapgit.org/user-guide/getting-started/install.html

The minimum required installation is the **Standalone Version**:\
https://docs.abapgit.org/user-guide/getting-started/install.html#install-standalone-version

Once abapGit is installed, you can install `trm-server` either as an **online** or **offline** repository.

## Online Repository Installation

1.  Run **abapGit** (standalone or developer version)

2.  On the repository list page, select **"New Online"**

3.  Enter the repository URL:

    `https://github.com/RegestaItalia/trm-server`

4.  Enter package name:

    `$TRM`

5.  Select **"Clone Online Repo"**

6.  Select **"Pull"**

## Offline Repository Installation

### Download Source Code

Download the latest release source code:

-   Direct link:\
    https://github.com/RegestaItalia/trm-server/archive/refs/heads/main.zip

OR

-   Open the GitHub repository page:\
    https://github.com/RegestaItalia/trm-server
    1.  Click **"\<\> Code"**
    2.  Click **"Download ZIP"**

Save the ZIP file locally (usually named `trm-server-main.zip`).

### Import Using abapGit

1.  Run **abapGit**
2.  On the repository list page, select **"New Offline"**
3.  Enter:
    -   Repository name: `trm-server`
    -   Package name: `$TRM`
4.  Select **"Create Offline Repo"**
5.  Select **"Import ZIP"**
6.  Select the file `trm-server-main.zip`
7.  Select **"Pull ZIP"**

## User Authorization Maintenance

This package exposes **RFC-enabled function modules**.

For security reasons, all users are **not authorized by default** to execute most RFC functions.

Before using `trm-server`, authorized users must be maintained.

### Maintain Authorized Users

1.  Execute transaction `SE16`
2.  Create a new record in table `ZTRM_USERS`

<p align="center">
    <img src="https://docs.trmregistry.com/server/images/se16_1.png" alt="SE16">
</p>

After adding the user, press **SAVE**.

<p align="center">
    <img src="https://docs.trmregistry.com/server/images/se16_2.png" alt="SE16">
</p>

> For security reasons, by default, TRM does not allow any user to execute RFC functions unless explicitly authorized.

## Update

With `trm-server` already installed, there are two ways to keep it up to date.

### Using TRM Client

Run:

`trm update trm-server`

### Using abapGit

-   **Online repository** → Pull from branch `main`
-   **Offline repository** → Download latest `main` ZIP from GitHub and
    import again

## Package Consistency Check

1.  Open transaction `SE80`

2.  Open package `ZTRM`

3.  Right-click the root package

4.  Select:

    `Check → Package Check → Package Objects (Including Subpackages)`

The check should not return any errors.

### Verify RFC Authorization

To verify that a user has access to RFC functions:

1.  Log into the system as the user you want to test
2.  Execute function module `ZTRM_PING`

If the user is authorized, no errors should occur. If not authorized,
access will be denied.

<!-- END OF SETUP.MD -->

---

# Contributing

Like every other TRM open-soruce projects, contributions are always welcomed ❤️.

Make sure to open an issue first.

Contributions will be merged upon approval.

[Click here](https://docs.trmregistry.com/#/CONTRIBUTING) for the full list of TRM contribution guidelines.

[<img src="https://trmregistry.com/public/contributors?image=true">](https://docs.trmregistry.com/#/?id=contributors)
