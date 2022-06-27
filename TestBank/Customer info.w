&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS wWin 
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 

/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-customerDeatil NO-UNDO
    FIELD CustId    AS INTEGER   LABEL "CustId"
    FIELD FirstName AS CHARACTER LABEL "FirstName"
    FIELD LastName  AS CHARACTER LABEL "LastName"
    FIELD Mobile    AS CHARACTER LABEL "Mobile"
    FIELD Address1  AS CHARACTER LABEL "Address1"
    FIELD Address2  AS CHARACTER LABEL "Address2"
    FIELD City      AS CHARACTER LABEL "City"
    FIELD State     AS CHARACTER LABEL "State"
    FIELD Country   AS CHARACTER LABEL "Country"
    FIELD ZipCode   AS CHARACTER LABEL "ZipCode" .
    
DEFINE TEMP-TABLE tt-customerAccount NO-UNDO
    FIELD AcctNum     AS INTEGER   LABEL "Account#"
    FIELD AccountType AS CHARACTER LABEL "AccountType" FORMAT "x(30)"
    FIELD AccSubType     AS CHARACTER   LABEL "AccSubType".
    

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 02/03/22 -  3:58 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-customerAccount

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-customerAccount.AcctNum tt-customerAccount.AccountType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-customerAccount
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-customerAccount.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-customerAccount
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-customerAccount


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FILL-IN-custid BUTTON-search ~
BUTTON-advancesearch FILL-IN-firstname FILL-IN-lastname COMBO-BOX-gender ~
FILL-IN-addrsl1 FILL-IN-Addressline2 FILL-IN-city FILL-IN-state ~
FILL-IN-country FILL-IN-postalcode BUTTON-add BUTTON-Update BUTTON-Delete ~
BROWSE-2 BUTTON-Details 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-custid FILL-IN-firstname ~
FILL-IN-lastname COMBO-BOX-gender FILL-IN-addrsl1 FILL-IN-Addressline2 ~
FILL-IN-city FILL-IN-state FILL-IN-country FILL-IN-postalcode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-add 
     LABEL "Add" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-advancesearch 
     LABEL "Advance Search" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-Delete 
     LABEL "Delete" 
     SIZE 12 BY 1.14.

DEFINE BUTTON BUTTON-Details 
     LABEL "Details" 
     SIZE 28 BY 1.14.

DEFINE BUTTON BUTTON-search 
     LABEL "Search" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-Update 
     LABEL "Update" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE COMBO-BOX-gender AS CHARACTER FORMAT "X(256)":U 
     LABEL "Gender" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Addressline2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address Line 2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-addrsl1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address Line 1" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-city AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-custid AS CHARACTER FORMAT "X(30)":U 
     LABEL "Customer Id" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-firstname AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-lastname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-postalcode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-state AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 2.38.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 8.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-customerAccount SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 wWin _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-customerAccount.AcctNum
    tt-customerAccount.AccountType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 114 BY 4.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-custid AT ROW 2.91 COL 19 COLON-ALIGNED WIDGET-ID 2
     BUTTON-search AT ROW 2.91 COL 41 WIDGET-ID 4
     BUTTON-advancesearch AT ROW 2.91 COL 61 WIDGET-ID 6
     FILL-IN-firstname AT ROW 5.76 COL 19 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-lastname AT ROW 5.76 COL 71 COLON-ALIGNED WIDGET-ID 16
     COMBO-BOX-gender AT ROW 5.76 COL 98 COLON-ALIGNED WIDGET-ID 24
     FILL-IN-addrsl1 AT ROW 7.19 COL 19 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-Addressline2 AT ROW 7.19 COL 71 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-city AT ROW 8.62 COL 19 COLON-ALIGNED WIDGET-ID 12
     FILL-IN-state AT ROW 8.62 COL 71 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-country AT ROW 10.05 COL 19 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-postalcode AT ROW 10.05 COL 71 COLON-ALIGNED WIDGET-ID 22
     BUTTON-add AT ROW 11.71 COL 33 WIDGET-ID 26
     BUTTON-Update AT ROW 11.71 COL 48 WIDGET-ID 28
     BUTTON-Delete AT ROW 11.71 COL 63 WIDGET-ID 30
     BROWSE-2 AT ROW 13.86 COL 5 WIDGET-ID 200
     BUTTON-Details AT ROW 18.86 COL 46 WIDGET-ID 36
     RECT-1 AT ROW 2.19 COL 5 WIDGET-ID 32
     RECT-2 AT ROW 4.81 COL 5 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.8 BY 19.81 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 19.81
         WIDTH              = 121.8
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 178.8
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 178.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

    {src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 BUTTON-Delete fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-customerAccount.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 wWin
ON MOUSE-MOVE-DBLCLICK OF BROWSE-2 IN FRAME fMain
DO:
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 wWin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-2 IN FRAME fMain
DO:
        RUN LoanAccountDetail.w(tt-customerAccount.AcctNum, tt-customerAccount.AccSubType).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-search wWin
ON CHOOSE OF BUTTON-search IN FRAME fMain /* Search */
DO:
        DEFINE VARIABLE icustid           AS INTEGER          NO-UNDO.
        DEFINE VARIABLE lcCustomerData    AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE lcCustomerAccount AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE cInfo             AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE hcustomerDeatil   AS HANDLE           NO-UNDO.
        DEFINE VARIABLE hcustomeraccnt    AS HANDLE           NO-UNDO.
        DEFINE VARIABLE oJsonObj          AS JsonObject       NO-UNDO.
        DEFINE VARIABLE lcExtent          AS LONGCHAR         NO-UNDO EXTENT.
        DEFINE VARIABLE oServerConnection AS ServerConnection NO-UNDO.
        icustid = int (FILL-IN-custid:SCREEN-VALUE).
        
        hcustomerDeatil = BUFFER tt-customerDeatil:HANDLE.
        hcustomeraccnt = BUFFER tt-customerAccount:HANDLE.
        
        oJsonObj = NEW JsonObject().
        oJsonObj:add("CustId",icustid).
        oJsonObj:add("cInfo","CustomerDetail").

        oServerConnection = NEW ServerConnection().
        oServerConnection:ServerConnect().
        
        IF oServerConnection:lgserver THEN
        DO:
            RUN Junction.p ON oServerConnection:AppServerHandle (INPUT-OUTPUT oJsonObj ).
            hcustomerDeatil:READ-JSON("JsonObject ",oJsonObj,"empty").
            FIND tt-customerDeatil.
            ASSIGN
                FILL-IN-firstname:SCREEN-VALUE    = tt-customerDeatil.FirstName
                FILL-IN-Addressline2:SCREEN-VALUE = tt-customerDeatil.Address2
                FILL-IN-addrsl1:SCREEN-VALUE      = tt-customerDeatil.Address1
                FILL-IN-country:SCREEN-VALUE      = tt-customerDeatil.Country
                FILL-IN-city:SCREEN-VALUE         = tt-customerDeatil.City
                FILL-IN-lastname:SCREEN-VALUE     = tt-customerDeatil.LastName
                FILL-IN-postalcode:SCREEN-VALUE   = tt-customerDeatil.ZipCode
                FILL-IN-state:SCREEN-VALUE        = tt-customerDeatil.State.
            
            cInfo = "CustomerAccounts,AccountType".
            oJsonObj = NEW JsonObject().
            oJsonObj:add("CustId",icustid).
            oJsonObj:add("cInfo",cInfo).
            RUN Junction.p ON oServerConnection:AppServerHandle (INPUT-OUTPUT oJsonObj).
            oServerConnection:AppServerHandle:DISCONNECT ().
            hcustomeraccnt:READ-JSON("JsonObject ",oJsonObj,"empty").
            OPEN QUERY BROWSE-2 FOR EACH tt-customerAccount.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN-custid FILL-IN-firstname FILL-IN-lastname COMBO-BOX-gender 
          FILL-IN-addrsl1 FILL-IN-Addressline2 FILL-IN-city FILL-IN-state 
          FILL-IN-country FILL-IN-postalcode 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 FILL-IN-custid BUTTON-search BUTTON-advancesearch 
         FILL-IN-firstname FILL-IN-lastname COMBO-BOX-gender FILL-IN-addrsl1 
         FILL-IN-Addressline2 FILL-IN-city FILL-IN-state FILL-IN-country 
         FILL-IN-postalcode BUTTON-add BUTTON-Update BUTTON-Delete BROWSE-2 
         BUTTON-Details 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
                          Purpose:  Window-specific override of this procedure which destroys 
                                    its contents and itself.
                            Notes:  
                        ------------------------------------------------------------------------------*/

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFcustomerAccnt wWin 
PROCEDURE pFcustomerAccnt :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInfo AS CHARACTER NO-UNDO.
    cInfo = "CustomerAccounts,AccountType".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

