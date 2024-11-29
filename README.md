# <a href="https://docs.trmregistry.com/#/server/README"><img src="https://docs.trmregistry.com/_media/logo.png" height="40" alt="TRM"></a>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-1.3.0-4baaaa.svg)](https://github.com/RegestaItalia/trm-docs/blob/main/CODE_OF_CONDUCT.md)
[![TRM License](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/license?package=trm-server)](https://trmregistry.com/#/package/trm-server)
[![TRM Latest version](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/version?package=trm-server)](https://trmregistry.com/#/package/trm-server)
[![TRM Installs](https://img.shields.io/endpoint?url=https://trmregistry.com/public/shieldio/downloads?package=trm-server)](https://trmregistry.com/#/package/trm-server)

TRM Server is the essential component for operations between TRM Client and your development SAP system.

<p align="center">
  <img src="https://docs.trmregistry.com/_media/schema.png" />
</p>

> This package should only be installed on your development systems!

# Basic usage

Typically, trm-server is used by a TRM Client.

When a TRM Client (like [trm-client](https://github.com/RegestaItalia/trm-client)) has to execute any of the functions of trm-server, it makes use of the [SAP NW RFC SDK](https://support.sap.com/en/product/connectors/nwrfcsdk.html).

The SDK connects to the development system, where this package exists, and executes the function.

The result of the function is then returned to the client and the its execution can continue.

## Where should I install this package?

As explained before, on any development system where you want to use a TRM Client.

The reason behind the need to only have this package on a development system (hence the **recommended installation as a temporary package**) is the definition of a TRM package life-cycle.

The packages are installed on a development system and are then transported manually across your SAP system landscape.

# Documentation <!-- {docsify-remove} -->

Full documentation can be seen at [https://docs.trmregistry.com](https://docs.trmregistry.com).

<!-- START TABLE_OF_CONTENTS.md -->
- [Setup Server](docs/setup.md)
- [RFC Functions](docs/rfcFunctions.md)
<!-- END TABLE_OF_CONTENTS.md -->

## Install <!-- {docsify-remove} -->

The first install of this package should be done with [abapGit](https://abapgit.org/).

Updates can be done either by abapGit or with [TRM](https://docs.trmregistry.com).

Follow the install steps documented [here](/docs/setup.md).

## Security: Authorizing users <!-- {docsify-remove} -->

By default, most of the RFC functions in this package are disabled for all users in your system.

This is done for security reasons. A full list of the RFC functions can be seen [here](/docs/rfcFunctions.md).

To enable users, follow [this guide](/docs/setup.md#user-authorization-maintenance).

# Contributing <!-- {docsify-remove} -->

Like every other TRM open-soruce projects, contributions are always welcomed ❤️.

Make sure to open an issue first.

Contributions will be merged upon approval.

[Click here](https://docs.trmregistry.com/#/CONTRIBUTING) for the full list of TRM contribution guidelines.
