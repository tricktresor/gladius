REPORT zglds_demo_generate_solution.

CLASS passport DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS
      get
        RETURNING VALUE(result) TYPE REF TO passport.
ENDCLASS.

CLASS passport IMPLEMENTATION.

  METHOD get.
    CREATE OBJECT result.
  ENDMETHOD.

ENDCLASS.

CLASS test_runner DEFINITION
  INHERITING FROM cl_aucv_test_runner_abstract.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !i_passport   TYPE REF TO object
      RETURNING
        VALUE(result) TYPE REF TO test_runner .

    METHODS run_for_program_keys       REDEFINITION .
    METHODS run_for_test_class_handles REDEFINITION .
ENDCLASS.



CLASS test_runner IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_AUCV_TEST_RUNNER_ACP=>CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PASSPORT                     TYPE REF TO OBJECT
* | [<-()] RESULT                         TYPE REF TO ZCL_AUCV_TEST_RUNNER_ACP
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create.
    DATA:
      passport_name TYPE string.

    passport_name = cl_abap_classdescr=>get_class_name( i_passport ).
    CREATE OBJECT result.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AUCV_TEST_RUNNER_ACP->RUN_FOR_PROGRAM_KEYS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LIMIT_ON_DURATION_CATEGORY   TYPE        SAUNIT_D_ALLOWED_RT_DURATION
* | [--->] I_LIMIT_ON_RISK_LEVEL          TYPE        SAUNIT_D_ALLOWED_RISK_LEVEL
* | [--->] I_PROGRAM_KEYS                 TYPE        SABP_T_TADIR_KEYS
* | [<---] E_COVERAGE_RESULT              TYPE REF TO IF_AUCV_CVRG_RSLT_PROVIDER
* | [<---] E_AUNIT_RESULT                 TYPE REF TO IF_SAUNIT_INTERNAL_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run_for_program_keys.

    DATA:
      tadir_key TYPE sabp_s_tadir_key,
      factory   TYPE REF TO cl_aunit_factory,
      task      TYPE REF TO if_aunit_task,
      listener  TYPE REF TO if_saunit_internal_listener.

    listener = cl_saunit_gui_service=>create_listener( ).
    CREATE OBJECT factory.
    task = factory->create_task( listener ).

    IF ( i_limit_on_risk_level IS NOT INITIAL ).
      task->restrict_risk_level( i_limit_on_risk_level ).
    ENDIF.
    IF ( i_limit_on_duration_category IS NOT INITIAL ).
      task->restrict_duration_category( i_limit_on_duration_category ).
    ENDIF.

    LOOP AT i_program_keys INTO tadir_key.
      CASE tadir_key-obj_type.
        WHEN 'CLAS'.
          task->add_class_pool( tadir_key-obj_name ).
        WHEN 'PROG'.
          task->add_program( tadir_key-obj_name ).
        WHEN 'FUGR'.
          task->add_function_group( tadir_key-obj_name ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.

    task->run( if_aunit_task=>c_run_mode-catch_short_dump ).
    e_aunit_result = listener->get_result_after_end_of_task( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_AUCV_TEST_RUNNER_ACP->RUN_FOR_TEST_CLASS_HANDLES
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LIMIT_ON_DURATION_CATEGORY   TYPE        IF_AUNIT_TASK=>TY_D_DURATION_CATEGORY
* | [--->] I_LIMIT_ON_RISK_LEVEL          TYPE        IF_AUNIT_TASK=>TY_D_RISK_LEVEL
* | [--->] I_TEST_CLASS_HANDLES           TYPE        IF_AUNIT_TEST_CLASS_HANDLE=>TY_T_TESTCLASS_HANDLES
* | [--->] I_CUSTOM_DURATION              TYPE        IF_AUNIT_TASK=>TY_S_DURATION_SETTING(optional)
* | [--->] I_MODE                         TYPE        IF_AUNIT_TASK=>TY_D_RUN_MODE (default =IF_AUNIT_TASK=>C_RUN_MODE-CATCH_SHORT_DUMP)
* | [--->] I_PACKAGES_TO_MEASURE          TYPE        STRING_SORTED_TABLE(optional)
* | [<---] E_COVERAGE_RESULT              TYPE REF TO IF_AUCV_CVRG_RSLT_PROVIDER
* | [<---] E_AUNIT_RESULT                 TYPE REF TO IF_SAUNIT_INTERNAL_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD run_for_test_class_handles.
    DATA:
      test_class_handle TYPE REF TO if_aunit_test_class_handle,
      factory           TYPE REF TO cl_aunit_factory,
      task              TYPE REF TO if_aunit_task,
      listener          TYPE REF TO if_saunit_internal_listener.

    listener = cl_saunit_gui_service=>create_listener( ).
    CREATE OBJECT factory.
    task = factory->create_task( listener ).

    IF ( i_limit_on_risk_level IS NOT INITIAL ).
      task->restrict_risk_level( i_limit_on_risk_level ).
    ENDIF.
    IF ( i_limit_on_duration_category IS NOT INITIAL ).
      TRY.
          task->restrict_duration_category( i_limit_on_duration_category ).
        CATCH cx_root. "EC *
      ENDTRY.
    ENDIF.

    IF ( i_custom_duration IS NOT INITIAL ).
      task->set_duration_limit( i_custom_duration ).
    ENDIF.

    LOOP AT i_test_class_handles INTO test_class_handle.
      task->add_test_class_handle( test_class_handle ).
    ENDLOOP.

    task->run( i_mode ).
    e_aunit_result = listener->get_result_after_end_of_task( ).

  ENDMETHOD.
ENDCLASS.

CLASS display DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: txt_line TYPE c LENGTH 72,
           txt      TYPE STANDARD TABLE OF txt_line
                    WITH DEFAULT KEY.
    CLASS-DATA:
      read_only TYPE abap_bool READ-ONLY VALUE abap_true.

    CLASS-METHODS:
      class_constructor,
      main IMPORTING i_template TYPE syrepid,
      fill_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                                   text   TYPE  txt,
      read_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                         EXPORTING text   TYPE txt.

    CLASS-DATA template TYPE syrepid.
  PRIVATE SECTION.
    CLASS-METHODS:
      prepare_editors,
      create_abap_editor
        IMPORTING parent_container TYPE REF TO cl_gui_custom_container
        RETURNING VALUE(editor)    TYPE REF TO cl_gui_abapedit.

ENDCLASS.

CLASS program DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      execute IMPORTING check_only TYPE abap_bool DEFAULT abap_false,
      build_source          RETURNING VALUE(rc) TYPE sy-subrc,
      check_declarations    RETURNING VALUE(rc) TYPE sy-subrc,
      check_implementation  RETURNING VALUE(rc) TYPE sy-subrc,
      check_syntax          RETURNING VALUE(rc) TYPE sy-subrc.

  PRIVATE SECTION.
    CLASS-DATA source   TYPE display=>txt.

ENDCLASS.

DATA g_links          TYPE TABLE OF tline.
DATA g_ok_code        TYPE sy-ucomm.

DATA g_editor1        TYPE REF TO cl_gui_abapedit.
DATA g_editor2        TYPE REF TO cl_gui_abapedit.

DATA g_declarations   TYPE display=>txt.
DATA g_implementation TYPE display=>txt.
DATA container3       TYPE REF TO cl_gui_custom_container.

PARAMETERS p_tmpl TYPE syrepid DEFAULT 'ZGLDS_DEMO_TEMPLATE'.

START-OF-SELECTION.
  display=>main( p_tmpl ).

CLASS program IMPLEMENTATION.
  METHOD execute.
    DATA program TYPE progname.
    DATA class TYPE string.

    IF check_declarations( )   <> 0 OR
       check_implementation( ) <> 0.
      RETURN.
    ENDIF.

    IF build_source( ) <> 0.
      RETURN.
    ENDIF.

    IF check_only = abap_true.
      MESSAGE TEXT-sok TYPE 'S'.
      RETURN.
    ENDIF.

    DATA(source_name) = 'SOURCE'.
    FIELD-SYMBOLS <source> TYPE STANDARD TABLE.
    ASSIGN (source_name) TO <source>.

    program = |$ZGLDS_SOLUTION_{ sy-uname }|.

    INSERT REPORT program FROM <source>.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      MESSAGE TEXT-srf TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    class = `\PROGRAM=` && program && `\CLASS=LCL_SOLUTION`.

    TRY.

        DATA aunit_result         TYPE REF TO if_saunit_internal_result.
        DATA cvrg_rslt_provider   TYPE REF TO if_aucv_cvrg_rslt_provider.
        DATA runner               TYPE REF TO cl_aucv_test_runner_abstract.

        runner = test_runner=>create( passport=>get( ) ).

        runner->run_for_program_keys(
          EXPORTING
            i_limit_on_risk_level        = if_aunit_attribute_enums=>c_risk_level-harmless
            i_limit_on_duration_category = if_aunit_attribute_enums=>c_duration-short
            i_program_keys               = VALUE #( ( obj_name = program obj_type = 'PROG' ) )
          IMPORTING
            e_aunit_result =    aunit_result
            e_coverage_result = cvrg_rslt_provider ). " can be initial

        DELETE REPORT program.
        COMMIT WORK.

        IF container3 IS BOUND.
          container3->free( ).
          CLEAR container3.
        ENDIF.

        CREATE OBJECT container3
          EXPORTING
            container_name = 'CUSTOM_CONTAINER3'.


        DATA task_data               TYPE if_saunit_internal_rt_v3=>ty_s_task.
        DATA task_result_casted TYPE REF TO cl_saunit_internal_result.
        task_result_casted ?= aunit_result.
        CALL FUNCTION '_SAUNIT_CREATE_CTRL_VIEWER_V3'
          EXPORTING
            task_data = task_result_casted->f_task_data
            container = container3.

      CATCH cx_root INTO DATA(exc) ##CATCH_ALL.
        MESSAGE exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.


  ENDMETHOD.

  METHOD build_source.
    DATA idx TYPE sy-tabix.
    DATA subrc TYPE sy-subrc.

    TRY.
        READ REPORT display=>template INTO source.
        subrc = sy-subrc.
      CATCH cx_sy_read_src_line_too_long.
        subrc = 4.
    ENDTRY.

    IF subrc = 0.
      FIND '* declarations' IN TABLE source MATCH LINE idx.
      subrc = sy-subrc.
      DELETE source INDEX idx.
      INSERT LINES OF g_declarations INTO source INDEX idx.
    ENDIF.

    IF subrc = 0.
      FIND '* implementation' IN TABLE source MATCH LINE idx.
      subrc = sy-subrc.
      DELETE source INDEX idx.
      INSERT LINES OF g_implementation INTO source INDEX idx.
    ENDIF.

    IF subrc <> 0.
      MESSAGE TEXT-wtl TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    rc = check_syntax( ).

  ENDMETHOD.

  METHOD check_declarations.
    DATA: code     LIKE source,
          mess     TYPE string,
          lin      TYPE i ##needed,
          wrd      TYPE string ##needed,
          warnings TYPE  STANDARD TABLE OF rslinlmsg.

    "Normal syntax check to get typos
    code = VALUE #( ( 'PROGRAM.' ) ).
    APPEND LINES OF g_declarations TO code.
    SYNTAX-CHECK FOR code MESSAGE mess LINE lin WORD wrd
                     ID 'MSG' TABLE warnings
                     PROGRAM sy-repid.
    rc = sy-subrc.
    IF rc <> 0.
      MESSAGE mess TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF warnings IS NOT INITIAL.
      DATA(warning) = warnings[ 1 ].
      MESSAGE warning-message TYPE 'I' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    "Restrict to declarative statements
    code = VALUE #( ( 'PROGRAM.' )
                    ( 'CLASS class DEFINITION.' )
                    ( 'PUBLIC SECTION.' )
                    ( 'ENDCLASS.' ) ) ##no_text.
    INSERT LINES OF g_declarations INTO code INDEX lines( code ).
    SYNTAX-CHECK FOR code MESSAGE mess LINE lin WORD wrd
                     ID 'MSG' TABLE warnings
                     PROGRAM 'ZDUMMY'.
    rc = sy-subrc.
    IF rc <> 0.
      MESSAGE TEXT-dcl TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    IF warnings IS NOT INITIAL.
      warning = warnings[ 1 ].
      MESSAGE warning-message TYPE 'I' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_implementation.
    "Only a very limited set of statements is allowed
    DATA black_list TYPE cl_demo_secure_abap_code=>string_table.
    DATA white_list TYPE cl_demo_secure_abap_code=>string_table.

    "Blacklist
    black_list = VALUE #(
      ( `->` )
      ( `=>` ) ).

    "Whitelist
    white_list = VALUE #(
      ( `FIELD-SYMBOLS`        )

      ( `CHECK`                )
      ( `EXIT`                 )
      ( `RETURN`               )

      ( `DO`                   )
      ( `ENDDO`                )
      ( `WHILE`                )
      ( `ENDWHILE`             )
      ( `CASE`                 )
      ( `WHEN`                 )
      ( `ENDCASE`              )
      ( `IF`                   )
      ( `ELSEIF`               )
      ( `ELSE`                 )
      ( `ENDIF`                )

      ( `MOVE-CORRESPONDING`   )
      ( `ASSIGN`               )
      ( `UNASSIGN`             )
      ( `CLEAR`                )
      ( `FREE`                 )

      ( `FIND`                 )
      ( `REPLACE`              )

      ( `APPEND`               )
      ( `INSERT`               )
      ( `MODIFY`               )
      ( `DELETE`               )
      ( `COLLECT`              )
      ( `READ`                 )
      ( `LOOP`                 )
      ( `ENDLOOP`              )
      ( `SORT`                 ) ).

    rc = cl_demo_secure_abap_code=>check(
      source_code  = g_implementation
      black_list   = black_list
      white_list   = white_list
      declarations = g_declarations ).
    IF rc <> 0.
      MESSAGE TEXT-exe TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD check_syntax.
    DATA: mess     TYPE string,
          lin      TYPE i ##needed,
          wrd      TYPE string ##needed,
          warnings TYPE  STANDARD TABLE OF rslinlmsg.
    "Syntax check for implementations with declarations
    SYNTAX-CHECK FOR source MESSAGE mess LINE lin WORD wrd
                     ID 'MSG' TABLE warnings
                     PROGRAM 'ZDUMMY'.
    rc = sy-subrc.
    IF rc <> 0.
      MESSAGE mess TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
    IF warnings IS NOT INITIAL.
      DATA(warning) = warnings[ 1 ].
      MESSAGE warning-message TYPE 'I' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS display IMPLEMENTATION.
  METHOD class_constructor.
    "Security checks

    "Not allowed in production systems
    IF cl_abap_demo_services=>is_production_system( ).
      MESSAGE TEXT-prs TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    "Only users who are allowed to use the ABAP Editor
    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = 'SE38'
      EXCEPTIONS
        ok     = 1
        not_ok = 2
        OTHERS = 3.

    "Only users who are allowed to create and run $TMP programs
    IF sy-subrc < 2.
      AUTHORITY-CHECK OBJECT 'S_DEVELOP'
        ID 'DEVCLASS' FIELD '$TMP'
        ID 'OBJTYPE'  FIELD 'PROG'
        ID 'OBJNAME'  DUMMY
        ID 'P_GROUP'  DUMMY
        ID 'ACTVT'    FIELD '02'.
      IF sy-subrc =  0.
        read_only = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

    MESSAGE TEXT-aut TYPE 'S' DISPLAY LIKE 'E'.

  ENDMETHOD.

  METHOD main.
    template = i_template.
    prepare_editors( ).
    CALL SCREEN 100.

  ENDMETHOD.

  METHOD prepare_editors.
    DATA container1 TYPE REF TO cl_gui_custom_container.
    DATA container2 TYPE REF TO cl_gui_custom_container.


    g_declarations = VALUE #(  ( '*== AVE! Place needed data declarations here...' ) )
    ##no_text.
    g_implementation = VALUE #( ( '*== Place your solution code here...' )
                          ( 'out = in.' ) )
      ##no_text.

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CUSTOM_CONTAINER1'.
    g_editor1 = display=>create_abap_editor( container1 ).

    CREATE OBJECT container2
      EXPORTING
        container_name = 'CUSTOM_CONTAINER2'.
    g_editor2 = display=>create_abap_editor( container2 ).

  ENDMETHOD.

  METHOD create_abap_editor.
    CREATE OBJECT editor
      EXPORTING
        parent = parent_container.
    editor->set_toolbar_mode( 0 ).
    editor->set_statusbar_mode( 0 ).
    IF read_only = abap_true.
      editor->set_readonly_mode( 1 ).
    ELSE.
      editor->set_readonly_mode( 0 ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_abap_editor.
    editor->set_text( text ).
  ENDMETHOD.

  METHOD read_abap_editor.
    editor->get_text( IMPORTING table = text ).
  ENDMETHOD.

ENDCLASS.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR  'TITLE_100'.
  display=>fill_abap_editor( editor = g_editor1
                             text   = g_declarations ).
  display=>fill_abap_editor( editor = g_editor2
                             text   = g_implementation ).
  cl_gui_control=>set_focus( g_editor2 ).
ENDMODULE.

MODULE cancel_0100 INPUT.
  LEAVE PROGRAM.
ENDMODULE.

MODULE user_command_0100.
  IF g_ok_code = 'INFO'.
    CALL FUNCTION 'HELP_OBJECT_SHOW'
      EXPORTING
        dokclass = 'RE'
        doklangu = sy-langu
        dokname  = display=>template
      TABLES
        links    = g_links.
    CLEAR g_ok_code.
    RETURN.
  ENDIF.
  IF display=>read_only = abap_true.
    MESSAGE TEXT-aut TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR g_ok_code.
    RETURN.
  ENDIF.
  display=>read_abap_editor( EXPORTING editor = g_editor1
                             IMPORTING text   = g_declarations ).
  display=>read_abap_editor( EXPORTING editor = g_editor2
                             IMPORTING text   = g_implementation ).
  CASE g_ok_code.
    WHEN 'EXECUTE'.
      program=>execute( ).
    WHEN 'CHECK'.
      program=>execute( check_only = abap_true ).
    WHEN 'CLEAR'.
      CLEAR g_declarations.
      CLEAR g_implementation.
  ENDCASE.
  CLEAR g_ok_code.
ENDMODULE.
