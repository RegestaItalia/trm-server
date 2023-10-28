*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTRM_INSTALLDEVC................................*
DATA:  BEGIN OF STATUS_ZTRM_INSTALLDEVC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_INSTALLDEVC              .
CONTROLS: TCTRL_ZTRM_INSTALLDEVC
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZTRM_SKIP_TRKORR................................*
DATA:  BEGIN OF STATUS_ZTRM_SKIP_TRKORR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_SKIP_TRKORR              .
CONTROLS: TCTRL_ZTRM_SKIP_TRKORR
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTRM_SRC_TRKORR.................................*
DATA:  BEGIN OF STATUS_ZTRM_SRC_TRKORR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_SRC_TRKORR               .
CONTROLS: TCTRL_ZTRM_SRC_TRKORR
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZTRM_USERS......................................*
DATA:  BEGIN OF STATUS_ZTRM_USERS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTRM_USERS                    .
CONTROLS: TCTRL_ZTRM_USERS
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZTRM_INSTALLDEVC              .
TABLES: *ZTRM_SKIP_TRKORR              .
TABLES: *ZTRM_SRC_TRKORR               .
TABLES: *ZTRM_USERS                    .
TABLES: ZTRM_INSTALLDEVC               .
TABLES: ZTRM_SKIP_TRKORR               .
TABLES: ZTRM_SRC_TRKORR                .
TABLES: ZTRM_USERS                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
