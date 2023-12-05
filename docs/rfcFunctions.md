# Authorizing a user

Most of the RFC functions exposed by trm-server require explicit authorization of the logon user in order to be used (and not raise an exception).

This is done by design in order to keep the system as safe as possible.

To be sure your account is authorized, [check this step](/docs/setup.md#verify-rfc-authorization).

If errors are encountered, [maintain the users table](/docs/setup.md#user-authorization-maintenance).

# RFC Functions

This package will expose RFC enabled function modules used by the TRM client.

## Transports

### Add objects to transport request

- Function module `ZTRM_ADD_OBJS_TR`
- Importing
    - IV_LOCK - **required** - `FLAG`
        
        Lock the objects.

    - IV_TRKORR - **required** - `TRKORR`
        
        Transport request number where the objects should be added.

- Exporting
    - ET_LOG - `SPROT_U_TAB`

        Log.

- Tables
    - IT_E071 - `E071`

        Objects to add to the transport.

### Add language translations to transport request

- Function module `ZTRM_ADD_LANG_TR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`
        
        Transport request number where the translations should be added.

- Tables
    - IT_DEVCLASS - `LXE_TT_PACKG_LINE`

        Range of devclass.

### Source TRM Transport

To indicate a transport was generated from the current system, a table is maintained.

- Function module `ZTRM_ADD_SRC_TRKORR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to add.

### Ignore TRM Transport

When a TRM package is installed into a system, its transports are imported.

Once a transport is imported, it cannot be removed.

The only way to tell TRM to ignore a specific transport is to add it to an ignore table.

This table will also ignore any transport inside the source table.

- Function module `ZTRM_ADD_SKIP_TRKORR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request number to ignore.

### Create transport of copies

The transport request will be created as a transport of copies (T).

- Function module `ZTRM_CREATE_TOC`
- Importing
    - IV_TEXT - **required** - `AS4TEXT`

        Transport request description.

    - IV_TARGET - **required** - `TR_TARGET`

        Transport request target (for release).

- Exporting
    - EV_TRKORR - `TRKORR`

    Generated transport request number.

### Create import transport request

The transport request will be created as a workbench transport (K).

- Function module `ZTRM_CREATE_IMPORT_TR`
- Importing
    - IV_TEXT - **required** - `AS4TEXT`

        Transport request description.

    - IV_TARGET - `TR_TARGET`

        Transport system target.

- Exporting
    - EV_TRKORR - `TRKORR`

        Generated transport request number.

### Delete transport request

- Function module `ZTRM_DELETE_TRANSPORT`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to delete.
        
### Include transport request objects from source to target

- Function module `ZTRM_TR_COPY`
- Importing
    - IV_FROM - **required** - `TRKORR`

        Source transport request.

    - IV_TO - **required** - `TRKORR`

        Target transport request.

    - IV_DOC - `TRPARFLAG`

        Include documentation.

### Dequeue transport request

- Function module `ZTRM_DEQUEUE_TR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to dequeue.

### Release transport request

- Function module `ZTRM_RELEASE_TR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to release.

    - IV_LOCK - **required** - `FLAG`

        Check lock flag in release.

### Add transport request to TMS queue

- Function module `ZTRM_FORWARD_TR`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to add to TMS queue.

    - IV_TARGET - **required** - `TMSSYSNAM`

        Target.

    - IV_SOURCE - **required** - `TMSSYSNAM`

        Source.

    - IV_IMPORT_AGAIN - `FLAG`

        Import again (if already in queue).

### Import transport request

- Function module `ZTRM_FORWARD_TR`
- Importing
    - IV_SYSTEM - **required** - `TMSSYSNAM`

        System where the transport should be imported.

    - IV_TRKORR - **required** - `TRKORR`

        Transport request number.

### Read TMS queue

This function will also refresh a TMS queue.

- Function module `ZTRM_READ_TMS_QUEUE`
- Importing
    - IV_TARGET - **required** - `TMSSYSNAM`

        Target queue.

- Exporting
    - ET_REQUESTS - `TMSIQREQS`

        Transport request numbers and status in queue.

### Get transport number where object is locked

- Function module `ZTRM_GET_OBJ_LOCK_TR`
- Importing
    - IV_PGMID - **required** - `PGMID`

        Object program ID.

    - IV_OBJECT - **required** - `TROBJTYPE`

        Object type.

    - IV_OBJ_NAME - **required** - `TROBJ_NAME`

        Object name.

- Exporting
    - EV_TRKORR - `TRKORR`

        Transport where the object is locked (if any).

### Get default transport layer

- Function module `ZTRM_GET_TRANSPORT_LAYER`
- Exporting
    - EV_LAYER - `DEVLAYER`

        Default transport layer.

### List object types

- Function module `ZTRM_LIST_OBJECT_TYPES`
- Tables
    - ET_OBJECT_TEXT - `KO100`

        Object types (and their descriptions) that can be imported into the system.

### Change transport request description

- Function module `ZTRM_RENAME_TRANSPORT_REQUEST`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request to rename.

    - IV_AS4TEXT - **required** - `AS4TEXT`

        New description.

### Set transport request documentation

- Function module `ZTRM_SET_TRANSPORT_DOC`
- Importing
    - IV_TRKORR - **required** - `TRKORR`

        Transport request where documentation should be added.

- Tables
    - IT_DOC - `TLINE`

        Documentation.

## Devclass

A devclass (or DEVC) is what's identified in SAP as a package.

In order to avoid confusion with the definition of package given by TRM, SAP packages are referred to devclasses.

### Create devclass

- Function module `ZTRM_CREATE_PACKAGE`
- Importing
    - IS_DATA - **required** - `SCOMPKDTLN`

        Data of the package to generate.

### Get devclass objects

- Function module `ZTRM_GET_DEVCLASS_OBJS`
- Importing
    - IV_DEVCLASS - **required** - `DEVCLASS`

        Objects devclass.

- Exporting
    - ET_TADIR - `SCTS_TADIR`

        Table containing objects.

### Mark package devclass as installed

Setting a package devclass as installed allows TRM to automatically get some data whenever a package is re-installed.

For example, if a package has to be updated, the devclass names are already known, and there's no need to change them (unless forced).

- Function module `ZTRM_SET_INSTALL_DEVC`
- Tables
    - IT_INSTALLDEVC - `ZTRM_INSTALLDEVC`

        Records to add/modify based on the package name and registry.

## Application server

### Get file system

Depending on the operating system, file path separators are different.

- Function module `ZTRM_GET_FILE_SYS`
- Exporting
    - EV_FILE_SYS - `FILESYS`

        File system.

### Get DIR_TRANS

DIR_TRANS is the parameter that contains the path where transport files are stored.

This parameter can also be seen in transaction `AL11`.

- Function module `ZTRM_GET_DIR_TRANS`
- Exporting
    - EV_DIR_TRANS - `PFEVALUE`

        Path where transport files are stored.

### Get file

- Function module `ZTRM_GET_BINARY_FILE`
- Importing
    - IV_FILE_PATH - **required** - `STRING`

        File path of the file on application server.

- Exporting
    - EV_FILE - `XSTRING`

        Binary file.

### Write file

- Function module `ZTRM_WRITE_BINARY_FILE`
- Importing
    - IV_FILE_PATH - **required** - `STRING`

        Path on application server where the file should be written.

    - IV_FILE - **required** - `XSTRING`

        Binary file to write.

## TRM Package

TRM Package related functions.

### Update integrity

Integrity is used to verify package content.

- Function module `ZTRM_SET_INTEGRITY`
- Importing
    - IS_INTEGRITY - **required** - `ZTRM_INTEGRITY`

        Package integrity.

## Interfaces

Interfaces are used to modify objects.

### Tadir

- Function module `ZTRM_TADIR_INTERFACE`
- Importing
    - IV_PGMID - **required** - `PGMID`

        Object program ID.

    - IV_OBJECT - **required** - `TROBJTYPE`

        Object type.

    - IV_OBJ_NAME - **required** - `SOBJ_NAME`

        Object name.

    - IV_DEVCLASS - `DEVCLASS`

        New object devclass.

    - IV_SRCSYSTEM - `SRCSYSTEM`

        New object source system.
    
    - IV_AUTHOR - `RESPONSIBL`

        New object author.
    
    - IV_SET_GENFLAG - `GENFLAG`

        New object generation flag.

### Tdevc

- Function module `ZTRM_TDEVC_INTERFACE`
- Importing
    - IV_DEVCLASS - **required** - `DEVCLASS`

        Devclass.

    - IV_RM_PARENTCL - `FLAG`

        Remove devclass from super package.

    - IV_PARENTCL - `DEVCLASS`

        New super package. Won't have effect if IV_RM_PARENTCL is equal to `X`.
