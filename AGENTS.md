# ECC 6.0 EHP2 / SAP_BASIS 702 compatibility

The lowest supported target is ECC 6.0 EHP2 with SAP_BASIS 702 SP12. Modern S/4
activation and a clean abaplint run are necessary, but they do not prove that an import
will generate successfully on that release.

## What abaplint can enforce

`abaplint.json` pins `syntax.version` to `v702` and enables the syntax, parser, DDIC,
unknown-type, include, implementation and XML consistency checks. Keep that version
pinned and run `npm run lint` before considering a change complete. It reliably catches
language constructs that are outside the configured release and many unresolved static
references represented by the configured dependency model.

Do not treat a zero-issue abaplint result as an ECC import simulation. The dependency
bundle is not the target system's exact DDIC/API catalog, and abaplint currently does not
reliably catch all of these SAP_BASIS 702 differences:

- a type, class, interface, method or function module that exists in the lint dependency
  set or on S/4 but not on the real ECC system;
- `ORDER BY PRIMARY KEY` applied to an object that is a database view on the target;
- old-kernel method-call grammar differences, especially section keywords inside a
  functional call;
- generated function-group/include syntax and other errors that only appear during the
  target system's import generation phase.

The actual ECC import generation log remains the final compatibility check.

## Rules learned from the 7.02 import campaign

- Never use release-specific system aliases such as `SYST_BATCH`, `SYST_SUBRC`,
  `SYST_MSGID`, `SYST_MSGNO`, `SYST_MSGTY` or `SYST_MSGV`. Type saved system values with
  the corresponding fields: `TYPE sy-batch`, `sy-subrc`, `sy-msgid`, `sy-msgno`,
  `sy-msgty`, and `sy-msgv1` through `sy-msgv4`.
- Do not use `ORDER BY PRIMARY KEY` for a query that can address a view. When only one
  matching row is required, use `SELECT SINGLE`. Otherwise name the ordering fields
  explicitly. Before transport, search the complete source set for `ORDER BY PRIMARY
  KEY`; the compatibility sweep intentionally leaves none in `/ATRM/CL_TRANSPORT`.
- In a functional method call, do not write section keywords inside the parentheses.
  For a returning call, use `result = class=>method( parameter = value )`, without
  `EXPORTING`. For calls needing `IMPORTING`, `RECEIVING`, `CHANGING` or `EXCEPTIONS`,
  use classic `CALL METHOD` syntax.
- On code paths that showed old-parser ambiguity for a static classic call, use the
  fully dynamic form, for example `CALL METHOD ('/ATRM/CL_ABAPGIT')=>('SERIALIZE') ...`.
  This is also the required pattern when the class or method may not exist on an older
  release.
- Avoid static references to optional platform APIs. Known examples from this campaign
  include `CL_ABAP_PSE`, `/UI2/CL_JSON`, newer package APIs, RAP/workbench APIs and LXE
  translation types/classes. Resolve them dynamically and catch `CX_ROOT` so absence
  degrades cleanly.
- Do not depend on standard type `SYMSG`/`SYMSG_TAB` being present. The project-owned
  `/ATRM/SYMSG` structure and `/ATRM/SYMSG_TAB` table type are the portable contract.
- Features unavailable on the target must disappear completely from the UI. Do not
  leave an empty or untitled tab when online installation is unsupported.
- After ARC-1 changes, verify the active SAP source again before releasing the request.
  In particular, check function-module includes: a later synchronization from an older
  mirror can silently restore an incompatible call. `src/` is still never edited
  locally; update the mirror only through the normal SAP/abapGit workflow.

For a compatibility sweep, search at least for:

```text
ORDER BY PRIMARY KEY
TYPE SYST_
=>method( followed by EXPORTING/IMPORTING/RECEIVING/CHANGING/EXCEPTIONS
CALL METHOD class=>method followed by parameter sections
```

Then run SAP syntax checks, activate every changed object (including function modules,
not only their function group), and confirm that the inactive-object list is empty.

# Writing an object dependency handler

Scope: `src/objs/`, the `/ATRM/CL_OBJECT_*` family. Read this before adding or changing
a dependency handler for an object type.

## Golden rule: `src/` is read-only from here

`src/` is the abapGit push/commit mirror of this repository, nothing in it is authored
locally. Every change described in this document is made **in the SAP system** through
ARC-1. Never hand-edit or hand-create files under `src/`.

## How a handler gets picked: `/atrm/cl_object_dispacher`

`/atrm/cl_object_dispacher=>get( key )` builds a class name dynamically —
`'/ATRM/CL_OBJECT_' && key-object` (uppercased, `key-object` being the TADIR object
type, e.g. `DOMA`, `DEVC`, `MSAG`) — and tries `CREATE OBJECT ro_instance TYPE
(lv_class_name)`. If that class doesn't exist (`CATCH cx_dynamic_check`), it falls back
to the default `/atrm/cl_object`. A BAdI can still swap the instance afterwards, but
that's out of scope here.

Consequence: a handler for object type `XYZG` **must** be named exactly
`/ATRM/CL_OBJECT_XYZG`, `PUBLIC`, `CREATE PUBLIC`, `INHERITING FROM /atrm/cl_object`.
Nothing else needs to reference it — the dispatcher finds it purely by name, and if the
object type doesn't exist in a given system release the dynamic `CREATE OBJECT` simply
falls through to the default, no error.

Note also: `/atrm/cl_object_dispacher=>get_package_dependencies` runs one `SORT ...
BY tabname tabkey. DELETE ADJACENT DUPLICATES ... COMPARING tabname tabkey.` on every
object's result *after* `get_dependencies` returns. Duplicate entries inside a single
handler are harmless — the dispatcher cleans them up. Don't add dedup logic for its own
sake.

## The contract: `/atrm/if_object~get_dependencies`

```
METHODS get_dependencies EXPORTING dependencies TYPE /atrm/object_dependency_t.
```

Each row of `/atrm/object_dependency_t` is `TABNAME` + `TABKEY` (the identity of the
dependency) plus `DEVCLASS` / `TRM_PACKAGE_NAME` / `TRM_PACKAGE_REGISTRY` (where that
dependency resolves to, if it's part of an installed TRM package).

The default implementation, in `/atrm/cl_object`, calls `REPOSITORY_ENVIRONMENT_ALL` for
the object's key and maps every `SENVI` row through `/atrm/cl_senvi_map=>get(...)` /
`map_dependencies`. This is the "repository environment" mentioned throughout this doc —
SAP's generic where-used/environment API. It's good enough for some object types, only
partially maps others, and is flat-out useless for a few (it returns nothing meaningful,
or nothing at all).

## Building a dependency row — never construct one by hand

`/atrm/cl_object` exposes protected class-methods that are the *only* sanctioned way to
produce a `/atrm/object_dependency` row:

- `get_tadir_dependency( object, obj_name )` — resolves a TADIR entry (`R3TR <object>
  <obj_name>`) to its devclass / TRM package.
- `get_tfdir_dependency( funcname )` — same, for a function module (via TFDIR/V_FDIR ->
  its function group's TADIR entry).
- `get_cds_dependency( entity )` / `get_entity_dependency( entity )` — CDS/DDIC entity
  resolution built on top of the two above.
- Loop helpers that call the above per row and append the result:
  `append_table_dependencies`, `append_typed_dependencies`, `append_senvi_table_deps`,
  `append_composite_deps`, `append_lrep_dependencies`, `append_sdok_class_deps`. Prefer
  one of these over a hand-rolled `SELECT` + `LOOP` when the shape fits.

**Never populate `TABNAME`/`TABKEY`/`DEVCLASS`/`TRM_PACKAGE_*` yourself and `APPEND` that
struct.** Always get the row from `get_tadir_dependency`/`get_tfdir_dependency` (directly,
or via one of the `append_*` helpers) and append *that*. This is what "never append to
the dependencies table" means in practice — appending the struct these methods hand back
is fine and is exactly what every existing handler does (`/atrm/cl_object_devc`,
`/atrm/cl_object_doma`, `/atrm/cl_object_srvb`, ...); hand-building the struct is not.
These methods also transparently pick up dependencies the dispatcher already resolved
earlier in the same package scan (see the `LOOP AT /atrm/cl_object_dispacher=>dependencies`
at the top of `get_tadir_dependency`), which a manual `SELECT` against TADIR would miss.

`/atrm/cl_object` can in principle grow more such accessor methods later if a new case
needs one — for now, stick to what exists.

## Three handler shapes

Pick one per object type. Look at what `REPOSITORY_ENVIRONMENT_ALL` actually returns for
a sample object of that type before deciding.

### 1. Mixed — call `super`, then fill the gaps

Use when the repository environment gets *some* dependencies right but misses others.
Call super first, then append what it missed, checking for duplicates only if you want to
avoid redundant lookups (the dispatcher will dedup regardless).

Reference: `/atrm/cl_object_doma` — super handles the generic environment, the handler
additionally resolves the domain's conversion-exit function group/module, which
`REPOSITORY_ENVIRONMENT_ALL` doesn't surface.

```abap
CLASS /atrm/cl_object_xxxx DEFINITION
  PUBLIC
  INHERITING FROM /atrm/cl_object
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS /atrm/if_object~get_dependencies REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /atrm/cl_object_xxxx IMPLEMENTATION.

  METHOD /atrm/if_object~get_dependencies.
    DATA: ls_dependency TYPE /atrm/object_dependency.

    super->/atrm/if_object~get_dependencies(
      IMPORTING dependencies = dependencies
    ).

    " ... resolve whatever super misses, via get_tadir_dependency / get_tfdir_dependency,
    " each lookup wrapped in its own TRY/CATCH cx_root ...
  ENDMETHOD.

ENDCLASS.
```

### 2. Custom only — skip `super`

Use when the repository environment is useless for this object type (empty, or noise).
Don't call `super`; resolve everything yourself from the object type's own DDIC tables.

Reference: `/atrm/cl_object_devc` — packages resolve their dependencies from `TDEVC`
(parent/main/enhanced package, component, default interface, switch id), `PERMISSION`
and `SFW_PACKAGE`, none of which `REPOSITORY_ENVIRONMENT_ALL` covers usefully for `DEVC`.
Note it still reads `TDEVC` fields dynamically via `ASSIGN COMPONENT` (see "write
dynamically" below) because not every field (e.g. `ENHANCED_PACKAGE`) exists on every
release, even though `TDEVC` itself always does.

### 3. Empty — the object type cannot have dependencies

Use when the object type is a leaf by design (nothing it could meaningfully depend on).
Redefine the method and leave it empty — don't call `super` (it would just add noise/cost
for no benefit).

Reference: `/atrm/cl_object_msag`:

```abap
METHOD /atrm/if_object~get_dependencies.
  " object cannot have dependencies
ENDMETHOD.
```

## Write dynamically outside of the `/atrm/cl_object` super call

This codebase ships into many different SAP releases. An object type's handler is only
ever instantiated when that object type actually exists there (see the dispatcher
fallback above) — but the ABAP *inside* the handler still has to activate and lint clean
everywhere, including on releases where some DDIC table, class, interface or function
module the handler touches doesn't exist at all. A static reference to something absent
is a hard syntax/activation error, not something a `TRY/CATCH` can save you from — so it
must never be written statically in the first place.

Rule of thumb: anything outside the `super->...get_dependencies(...)` call that isn't one
of the always-present base tables (`TADIR`, `TFDIR`/`V_FDIR`, `TDEVC`, `PERMISSION`,
`SFW_PACKAGE`, ...) or `/atrm/cl_object`'s own methods must be reached dynamically, and
the surrounding logic wrapped in `TRY ... CATCH cx_root` so a missing piece degrades to
"no extra dependency found" instead of blowing up the whole package scan.

Concrete patterns, all already used in this folder — reuse them rather than inventing new
ones:

| Need | Static (avoid for optional objects) | Dynamic (use instead) |
|---|---|---|
| Read an optional DDIC table/structure | `DATA ls_row TYPE ddldependency.` | `CREATE DATA lr_row TYPE ('DDLDEPENDENCY'). ASSIGN lr_row->* TO <ls_row>.` |
| Select from it | `SELECT * FROM ddldependency ...` | `SELECT * FROM (lv_table) INTO TABLE <lt_rows> WHERE (lv_where).` |
| Read a field that may not exist on every release | `ls_tdevc-enhanced_package` | `ASSIGN COMPONENT 'ENHANCED_PACKAGE' OF STRUCTURE ls_tdevc TO <fs>. CHECK sy-subrc = 0.` |
| Call a class/API that may not exist | `cl_wb_object_operator=>create_instance( ... )` | `CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE') ... RECEIVING result = lo_operator.` |
| Call a method whose defining interface may not exist | `lo_operator->if_wb_object_operator~read( ... )` | `CALL METHOD lo_operator->('IF_WB_OBJECT_OPERATOR~READ') ... .` |
| Instantiate an optional class | `CREATE OBJECT lo_x TYPE cl_optional_thing.` | `CREATE OBJECT lo_x TYPE ('CL_OPTIONAL_THING').` |

See `/atrm/cl_object_srvb` for the fullest example: fully dynamic class/method
resolution, a dynamically-typed data reference for the result payload, and dynamic
`ASSIGN COMPONENT` to drill into it — the whole thing in one `TRY ... CATCH cx_root`
block, so on a release without RAP service bindings the class still activates and simply
returns no dependencies. `/atrm/cl_object_devc` is the milder version of the same idea:
`TDEVC` itself is safe to reference statically (it's ancient/universal), but its
newer/optional fields are still read through `ASSIGN COMPONENT`.

Per-lookup `TRY ... CATCH cx_root.` (with a one-line comment on *why* it can fail) around
each individual `get_tadir_dependency`/`get_tfdir_dependency` call is the existing
convention — it isolates one missing dependency from the rest of the handler's results.

## abaplint

`abaplint.json` pins `syntax.version` to `v702`. Concretely, that means:

- No inline declarations (`DATA(x) = ...`, `NEW #( )`) — classic `DATA:`/`TYPES:` blocks
  and `CREATE OBJECT`/`CREATE DATA`, as in every existing handler.
- Fully-dynamic invocation (`CALL METHOD (class)=>(method)`, `CALL METHOD obj->(method)`)
  needs the old `CALL METHOD` syntax — functional call syntax (`obj->method( )`) can't
  express a dynamic method name, so don't try to "modernize" it.
- String templates (`|...|`) are fine.

Before considering a handler done, run `SAPLint` on it. A static `TYPE` reference to a
DDIC object, or a static `CALL FUNCTION '...'`/method call naming something that doesn't
exist in the target system, will fail `check_ddic`/`unknown_types`/`check_syntax` — that
failure is the signal you needed the dynamic form from the table above instead.

## Checklist for a new handler

1. Find the object's TR type code (e.g. from `SAPSearch`/TADIR) — that's the `<XXXX>` in
   `/ATRM/CL_OBJECT_<XXXX>`.
2. Investigate what `REPOSITORY_ENVIRONMENT_ALL` actually returns for a real object of
   that type, and what DDIC tables/APIs hold the dependencies it misses (if any).
3. Pick a shape: mixed (call super) / custom-only (skip super) / empty (cannot depend on
   anything) — see above.
4. `SAPWrite` the class in SAP: `PUBLIC INHERITING FROM /atrm/cl_object CREATE PUBLIC`,
   redefining `/atrm/if_object~get_dependencies`. Resolve every dependency row through
   `get_tadir_dependency`/`get_tfdir_dependency` (or an `append_*` helper) — never by
   hand. Reach anything outside of always-present base tables/`/atrm/cl_object` methods
   dynamically, each optional lookup in its own `TRY ... CATCH cx_root`.
5. `SAPLint` it, fix anything flagged.
6. `SAPActivate` it.
