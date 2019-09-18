REPORT zglds_demo_generate_solution2.

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

    "Customizinig for shorter duration limits
    DATA(aunit_setup) = cl_aunit_customizing=>get_setup( ).
    aunit_setup-host-duration_short = 5.
    cl_aunit_customizing=>set_setup( aunit_setup ).


    task->run( if_aunit_task=>c_run_mode-external ).
*    task->run( if_aunit_task=>c_run_mode-catch_short_dump ).
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
    TYPES: txt_line TYPE string,
           txt      TYPE STANDARD TABLE OF txt_line
                    WITH DEFAULT KEY.
    CLASS-DATA read_only       TYPE abap_bool READ-ONLY VALUE abap_true.
    CLASS-DATA main_splitter   TYPE REF TO cl_gui_easy_splitter_container.
    CLASS-DATA info_splitter   TYPE REF TO cl_gui_easy_splitter_container.
    CLASS-DATA list_splitter   TYPE REF TO cl_gui_easy_splitter_container.
    CLASS-DATA editor_splitter TYPE REF TO cl_gui_splitter_container.
    CLASS-DATA result_splitter TYPE REF TO cl_gui_easy_splitter_container.

    CLASS-METHODS:
      class_constructor,
      save,
      main IMPORTING i_config TYPE seoclsname,

      fill_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                                   text   TYPE  txt,
      read_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                         EXPORTING text   TYPE txt,
      pretty_printer     IMPORTING editor TYPE REF TO cl_gui_abapedit
                                   otext  TYPE txt
                         EXPORTING ntext  TYPE txt,
      set_metrics.
    CLASS-DATA template TYPE syrepid.
    CLASS-DATA config   TYPE seoclsname.
    CLASS-DATA o_config TYPE REF TO zif_glds_demo_test_interface.
    CLASS-DATA metrics_defined  TYPE zif_glds_demo_metrics=>ts_metrics.
    CLASS-DATA metrics_reached  TYPE zif_glds_demo_metrics=>ts_metrics.

    TYPES: BEGIN OF ts_metric,
             name  TYPE c LENGTH 40,
             value_defined TYPE c LENGTH 20,
             value_reached type c length 20,
             icon  TYPE icon_d,
           END OF ts_metric,
           tt_metrics TYPE STANDARD TABLE OF ts_metric WITH DEFAULT KEY.
    CLASS-DATA metric_data TYPE tt_metrics.

  PRIVATE SECTION.
    CLASS-METHODS:
      load,
      prepare_editors,
      create_abap_editor
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(editor)    TYPE REF TO cl_gui_abapedit,
      create_text
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(text)      TYPE REF TO cl_gui_textedit,
      create_white_list
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(info)      TYPE REF TO cl_salv_table,
      create_black_list
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(info)      TYPE REF TO cl_salv_table,
      create_metrics
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(data)      TYPE REF TO cl_salv_table.

ENDCLASS.

CLASS program DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      execute IMPORTING check_only   TYPE abap_bool DEFAULT abap_false
                        unit_tests   TYPE abap_bool DEFAULT abap_false
                        secret_tests TYPE abap_bool DEFAULT space,
      build_source          RETURNING VALUE(rc) TYPE sy-subrc,
      check_implementation  RETURNING VALUE(rc) TYPE sy-subrc,
      check_syntax          RETURNING VALUE(rc) TYPE sy-subrc.

  PRIVATE SECTION.
    CLASS-DATA source   TYPE display=>txt.
    CLASS-DATA mr_salv_table TYPE REF TO cl_salv_table.
    CLASS-DATA mt_secret_tests TYPE zglds_demo_secret_test_t.
    CLASS-DATA mv_program TYPE progname.

ENDCLASS.

DATA g_links          TYPE TABLE OF tline.
DATA g_ok_code        TYPE sy-ucomm.

DATA g_editor         TYPE REF TO cl_gui_abapedit.
DATA g_metrics        TYPE REF TO cl_salv_table.
DATA g_descr          TYPE REF TO cl_gui_textedit.
DATA g_white          TYPE REF TO cl_salv_table.
DATA g_black          TYPE REF TO cl_salv_table.

DATA g_implementation TYPE display=>txt.

*PARAMETERS p_tmpl TYPE syrepid    DEFAULT 'ZGLDS_DEMO_TEST_TEMPLATE'.
PARAMETERS p_config TYPE seoclsname DEFAULT 'ZCL_GLDS_DEMO_TEST_CFG'.

START-OF-SELECTION.
  display=>main( p_config ).

CLASS program IMPLEMENTATION.
  METHOD execute.

    DATA class   TYPE string.


    CLEAR mt_secret_tests.
    mv_program = |$ZGLDS_SOLUTION_{ sy-uname }{ secret_tests }|.

    IF check_implementation( ) <> 0.
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

    CASE abap_true.
      WHEN unit_tests.
        LOOP AT <source> ASSIGNING FIELD-SYMBOL(<line_ut>).
          REPLACE '*UT:' WITH space INTO <line_ut>.
        ENDLOOP.
      WHEN secret_tests.
        LOOP AT <source> ASSIGNING FIELD-SYMBOL(<line_st>).
          REPLACE '*ST:' WITH space INTO <line_st>.
        ENDLOOP.
    ENDCASE.



    INSERT REPORT mv_program FROM <source>.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      MESSAGE TEXT-srf TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    class = `\PROGRAM=` && mv_program && `\CLASS=LCL_SOLUTION`.


    "get testclasses / methods of object
    DATA prog_info  TYPE if_aunit_prog_info_types=>ty_s_program.
    DATA tadir_key  TYPE saunit_s_tadir_key.

    cl_aunit_prog_info=>progname_to_tadir(
      EXPORTING
        progname = mv_program
      IMPORTING
        obj_type = tadir_key-obj_type
        obj_name = tadir_key-obj_name ).

    prog_info = cl_aunit_prog_info=>get_program_info(
      obj_type        = tadir_key-obj_type
      obj_name        = tadir_key-obj_name
      skip_class_info = abap_false ).

    TRY.

        DATA aunit_result         TYPE REF TO if_saunit_internal_result.
        DATA cvrg_rslt_provider   TYPE REF TO if_aucv_cvrg_rslt_provider.
*        DATA runner               TYPE REF TO cl_aucv_test_runner_abstract.

        GET RUN TIME FIELD DATA(start).

        test_runner=>create( passport=>get( ) )->run_for_program_keys(
          EXPORTING
            i_limit_on_risk_level        = if_aunit_attribute_enums=>c_risk_level-harmless
            i_limit_on_duration_category = if_aunit_attribute_enums=>c_duration-short
            i_program_keys               = VALUE #( ( obj_name = mv_program obj_type = 'PROG' ) )
          IMPORTING
            e_aunit_result =    aunit_result
            e_coverage_result = cvrg_rslt_provider ). " can be initial

        GET RUN TIME FIELD DATA(stopp).

        IF unit_tests = abap_true.
          display=>metrics_reached = zcl_glds_demo_check_abap_code=>retrieve_metrics( source_code  = g_implementation ).
          display=>metrics_reached-runtime = stopp - start.
          display=>set_metrics( ).
          g_metrics->refresh( ).
        ENDIF.

        DATA task_data          TYPE if_saunit_internal_rt_v3=>ty_s_task.
        DATA task_result_casted TYPE REF TO cl_saunit_internal_result.
        task_result_casted ?= aunit_result.


        CASE 'X'.
          WHEN unit_tests.
            LOOP AT display=>result_splitter->top_left_container->children INTO DATA(child).
              child->free( ).
              CLEAR child.
            ENDLOOP.
            CALL FUNCTION '_SAUNIT_CREATE_CTRL_VIEWER_V3'
              EXPORTING
                task_data = task_result_casted->f_task_data
                container = display=>result_splitter->top_left_container.
          WHEN secret_tests.
            DATA ls_secret_test LIKE LINE OF mt_secret_tests.
            DATA(fails) = lines( task_result_casted->f_task_data-alerts_by_indicies ).

            "check which of the secret tests has failed
            LOOP AT prog_info-testclasses INTO DATA(ls_test_class).
              LOOP AT ls_test_class-methods INTO DATA(ls_test_method).
                CLEAR ls_secret_test.
                ls_secret_test-name   = ls_test_method-name.
                ls_secret_test-number = sy-tabix.
                ls_secret_test-status = icon_okay.


                LOOP AT task_result_casted->f_task_data-alerts_by_indicies INTO DATA(alert_by_index).
                  LOOP AT alert_by_index-alerts INTO DATA(alert).
                    READ TABLE alert-header-params INTO DATA(lv_header_param) INDEX 1.
                    IF sy-subrc > 0.
                      CLEAR lv_header_param.
                    ENDIF.
                    LOOP AT alert-text_infos INTO DATA(ls_text_info) WHERE id = 'AA03'.
                      READ TABLE ls_text_info-params INTO DATA(lv_param) INDEX 3.
                      IF lv_param = ls_test_method-name.

                        LOOP AT alert_by_index-alerts INTO DATA(alert_detail).
                          LOOP AT alert_detail-header-params INTO DATA(param).
                            ls_secret_test-status = icon_cancel.
                            "MSG from ASSERT of unit test
                            ls_secret_test-info = lv_header_param.
                          ENDLOOP.
                        ENDLOOP.
                      ENDIF.
                    ENDLOOP.
                  ENDLOOP.
                ENDLOOP.
                APPEND ls_secret_test TO mt_secret_tests.

*                ELSE.
*                  APPEND VALUE #( status = icon_okay number = sy-tabix info = ls_test_method-name ) TO mt_secret_tests_failed.
*                ENDIF.
              ENDLOOP.
            ENDLOOP.

            IF mr_salv_table IS INITIAL.
              TRY.
                  cl_salv_table=>factory(
                    EXPORTING
                      r_container    = display=>result_splitter->bottom_right_container
                    IMPORTING
                      r_salv_table   = mr_salv_table
                    CHANGING
                      t_table        = mt_secret_tests
                  ).

                  DATA(lr_disp) = mr_salv_table->get_display_settings( ).
                  lr_disp->set_list_header( value = 'Secret Tests' ).

                  DATA(lr_columns) = mr_salv_table->get_columns( ).
                  lr_columns->set_optimize( abap_true ).

                  mr_salv_table->display( ).
                CATCH cx_salv_msg.    "
              ENDTRY.
            ELSE.
              mr_salv_table->refresh( ).
            ENDIF.

            IF fails = 0.
              MESSAGE 'Great! All secret tests passed!!' TYPE 'S'.
            ENDIF.
        ENDCASE.



      CATCH cx_root INTO DATA(exc) ##CATCH_ALL.
        DELETE REPORT mv_program.
        COMMIT WORK.

        MESSAGE exc->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    DELETE REPORT mv_program.
    COMMIT WORK.


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



  METHOD check_implementation.

    "Maybe Only a very limited set of statements is allowed

    DATA lo_list  TYPE REF TO zif_glds_demo_command_list.


    TRY.
        CREATE OBJECT lo_list TYPE (display=>config).


        zcl_glds_demo_check_abap_code=>check(
          source_code  = g_implementation
          black_list   = lo_list->get_black_list( )
          white_list   = lo_list->get_white_list( ) ).

      CATCH cx_sy_create_object_error.
        RETURN.
      CATCH zcx_glds_demo INTO DATA(error).
        MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

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
    config = i_config.
    CREATE OBJECT o_config TYPE (i_config).
    template = o_config->get_template( ).
    prepare_editors( ).
    CALL SCREEN 100.
  ENDMETHOD.


  METHOD load.

    SELECT SINGLE sourcecode FROM zglds_demo_usr INTO @DATA(source_code)
      WHERE username = @sy-uname
        AND repid    = @template.
    IF source_code IS INITIAL.
      g_implementation = VALUE #(
        ( `*== Place your solution code here...` )
        ( `out = in.` ) )  ##no_text.
    ELSE.
      APPEND source_code TO g_implementation.
    ENDIF.
  ENDMETHOD.

  METHOD prepare_editors.

    load( ).

    main_splitter   = NEW cl_gui_easy_splitter_container(
                            parent      = NEW cl_gui_custom_container( container_name = 'CUSTOM_CONTAINER' )
                            orientation = cl_gui_easy_splitter_container=>orientation_horizontal ).

*    editor_splitter = NEW cl_gui_easy_splitter_container(
*                            parent      = main_splitter->top_left_container
*                            sash_position = 70
*                            orientation = cl_gui_easy_splitter_container=>orientation_vertical ).
    editor_splitter = NEW cl_gui_splitter_container(
                            parent      = main_splitter->top_left_container
                            rows = 3
                            columns = 1 ).
    editor_splitter->set_row_height( id = 1 height = 60 ).
    editor_splitter->set_row_height( id = 2 height = 20 ).


    info_splitter   = NEW cl_gui_easy_splitter_container(
                            parent      = editor_splitter->get_container( row = 2 column = 1 )
                            sash_position = 60
                            orientation = cl_gui_easy_splitter_container=>orientation_horizontal ).

    list_splitter   = NEW cl_gui_easy_splitter_container(
                            parent      = info_splitter->bottom_right_container
                            sash_position = 50
                            orientation = cl_gui_easy_splitter_container=>orientation_horizontal ).


    result_splitter = NEW cl_gui_easy_splitter_container(
                            parent      = main_splitter->bottom_right_container
                            sash_position = 70
                            orientation = cl_gui_easy_splitter_container=>orientation_vertical ).


    g_editor = display=>create_abap_editor( editor_splitter->get_container( row = 1 column = 1 ) ).
    g_metrics = display=>create_metrics( editor_splitter->get_container( row = 3 column = 1 ) ).
    g_descr  = display=>create_text( info_splitter->top_left_container ).
    g_white  = display=>create_white_list( list_splitter->top_left_container ).
    g_black  = display=>create_black_list( list_splitter->bottom_right_container ).

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

  METHOD create_text.

    CREATE OBJECT text EXPORTING parent = parent_container.
    text->set_toolbar_mode( 0 ).
    text->set_readonly_mode( 1 ).
    text->set_statusbar_mode( 0 ).

    DATA lo_descr TYPE REF TO zif_glds_demo_description.

    CREATE OBJECT lo_descr TYPE (config).

    text->set_textstream( lo_descr->get( ) ).

  ENDMETHOD.

  METHOD create_white_list.

    DATA lo_list TYPE REF TO zif_glds_demo_command_list.

    CREATE OBJECT lo_list TYPE (config).

    DATA(list) = lo_list->get_white_list( ).
    TYPES: BEGIN OF ts_textline,
             line TYPE c LENGTH 1000,
           END OF ts_textline,
           tt_textstream TYPE STANDARD TABLE OF ts_textline WITH DEFAULT KEY.
    DATA cmd   TYPE ts_textline.
    DATA cmds  TYPE tt_textstream.

    LOOP AT list INTO DATA(list_entry).
      cmd-line = list_entry.
      APPEND cmd TO cmds.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = parent_container
          IMPORTING
            r_salv_table   = info
          CHANGING
            t_table        = cmds
        ).
        info->get_display_settings( )->set_list_header( 'Whitelist' ).
        info->get_columns( )->set_headers_visible( abap_false ).
        info->display( ).
      CATCH cx_salv_msg.
    ENDTRY.


  ENDMETHOD.

  METHOD create_black_list.
    DATA lo_list TYPE REF TO zif_glds_demo_command_list.

    CREATE OBJECT lo_list TYPE (config).

    DATA(list) = lo_list->get_black_list( ).
    TYPES: BEGIN OF ts_textline,
             line TYPE c LENGTH 1000,
           END OF ts_textline,
           tt_textstream TYPE STANDARD TABLE OF ts_textline WITH DEFAULT KEY.
    DATA cmd   TYPE ts_textline.
    DATA cmds  TYPE tt_textstream.

    LOOP AT list INTO DATA(list_entry).
      cmd-line = list_entry.
      APPEND cmd TO cmds.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = parent_container
          IMPORTING
            r_salv_table   = info
          CHANGING
            t_table        = cmds
        ).
        info->get_display_settings( )->set_list_header( 'Blacklist' ).
        info->get_columns( )->set_headers_visible( abap_false ).
        info->display( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.

  METHOD create_metrics.

    set_metrics( ).

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = parent_container
          IMPORTING
            r_salv_table   = data
          CHANGING
            t_table        = metric_data
        ).
        data->get_display_settings( )->set_list_header( 'Metrics' ).
        data->get_columns( )->set_headers_visible( abap_true ).
        data(columns) = data->get_columns( ).
        columns->get_column( 'NAME' )->set_medium_text( conv #('metric name'(m01) ) ).
        columns->get_column( 'VALUE_DEFINED' )->set_medium_text( conv #( 'defined value'(m02) ) ).
        columns->get_column( 'VALUE_REACHED' )->set_medium_text( conv #( 'achieved value'(m03) ) ).
        columns->get_column( 'ICON' )->set_medium_text( conv #('target accomplished '(m04) ) ).

        data->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD set_metrics.

    DATA lo_metrics TYPE REF TO zif_glds_demo_metrics.
    TRY.
        CREATE OBJECT lo_metrics TYPE (config).
        lo_metrics->set( ).
        metrics_defined = lo_metrics->data.
      CATCH cx_sy_create_object_error .
        RETURN.
    ENDTRY.

    metric_data = VALUE tt_metrics(
      ( name = 'Number of statements' value_defined = CONV #( metrics_defined-number_of_statements )
              icon = SWITCH #( lo_metrics->chk( metrics_reached )-number_of_statements
                      WHEN abap_true  THEN icon_okay
                      WHEN abap_false THEN icon_cancel
                      ELSE icon_led_inactive )
              value_reached = metrics_reached-number_of_statements )
      ( name = 'Number of tokens'     value_defined = CONV #( metrics_defined-number_of_tokens ) value_reached = metrics_reached-number_of_tokens )
      ( name = 'Number of characters' value_defined = CONV #( metrics_defined-number_of_characters  ) value_reached = metrics_reached-number_of_characters  )
      ( name = 'Run time'             value_defined = CONV #( metrics_defined-runtime ) value_reached = metrics_reached-runtime  )
       ).

  ENDMETHOD.

  METHOD fill_abap_editor.
    editor->set_text( text ).
  ENDMETHOD.

  METHOD read_abap_editor.
    editor->get_text( IMPORTING table = text ).
  ENDMETHOD.

  METHOD pretty_printer.


    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = space
      TABLES
        ntext              = ntext
        otext              = otext
      EXCEPTIONS
        enqueue_table_full = 1
        include_enqueued   = 2
        include_readerror  = 3
        include_writeerror = 4
        OTHERS             = 5.
    IF sy-subrc = 0.
      editor->set_text( ntext ).
    ENDIF.

  ENDMETHOD.


  METHOD save.

    DATA glds_user TYPE zglds_demo_usr.

    glds_user-username = sy-uname.
    glds_user-repid    = template.
    LOOP AT g_implementation INTO DATA(line).
      IF sy-tabix = 1.
        glds_user-sourcecode = line.
      ELSE.
        CONCATENATE glds_user-sourcecode line INTO glds_user-sourcecode SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.
    ENDLOOP.
    MODIFY zglds_demo_usr FROM glds_user.

  ENDMETHOD.


ENDCLASS.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE'.
  display=>fill_abap_editor( editor = g_editor
                             text   = g_implementation ).
  cl_gui_control=>set_focus( g_editor ).
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
  display=>read_abap_editor( EXPORTING editor = g_editor
                             IMPORTING text   = g_implementation ).
  CASE g_ok_code.
    WHEN 'EXECUTE'.
      program=>execute( unit_tests   = abap_true ).
      program=>execute( secret_tests = abap_true ).
    WHEN 'CHECK'.
      program=>execute( check_only = abap_true ).
    WHEN 'CLEAR'.
      CLEAR g_implementation.
    WHEN 'SAVE'.
      display=>save( ).
    WHEN 'PRETTY_PRINTER'.
      display=>pretty_printer(
        EXPORTING
          editor = g_editor
          otext = g_implementation
        IMPORTING
          ntext = g_implementation ).
  ENDCASE.
  CLEAR g_ok_code.
ENDMODULE.
