*&---------------------------------------------------------------------*
*& Report  ZSB_COMP_VERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsb_comp_vers.

TYPE-POOLS: trwbo, slis, trwb1, triwb, abap, icon, spta.

TABLES: e070.

CLASS cl_object DEFINITION DEFERRED.
CLASS cl_request DEFINITION DEFERRED.
CLASS cl_objects_list DEFINITION DEFERRED.
CLASS cl_event_handler DEFINITION DEFERRED.

TYPES:  ty_object_key TYPE trwb1_s_e071_object,
       BEGIN OF ty_object,
         key        TYPE ty_object_key,
         objfunc    LIKE e071-objfunc,
         lockflag   LIKE e071-lockflag,
         result     LIKE trpari-s_checked,
         cli_dep    LIKE trpari-w_cli_dep,
         t_pgmid    LIKE tadir-pgmid,
         t_object   LIKE tadir-object,
         t_obj_name LIKE e071-obj_name,
         srcsystem  LIKE tadir-srcsystem,
         author     LIKE tadir-author,
         masterlang LIKE tadir-masterlang,
         devclass   LIKE tadir-devclass,
         devlayer   LIKE tcerele-translayer,
         target     LIKE e070-tarsystem,
         repair     LIKE tadir-srcdep,
         tcolor     TYPE lvc_t_scol,
       END OF   ty_object,
       BEGIN OF ty_system,
         versno     LIKE vrsd-versno,
         chbox      TYPE c,
         korrnum    LIKE vrsd-korrnum,
         author     LIKE vrsd-author,
         datum      LIKE vrsd-datum,
         zeit       LIKE vrsd-zeit,
         versmode   LIKE vrsd-versmode,
       END OF   ty_system,

       ty_ref_objects TYPE REF TO cl_object,
       tyt_ref_objects TYPE STANDARD TABLE OF ty_ref_objects,

       BEGIN OF ty_spta_package,
         t_systems TYPE ehs_char3_t,
         object TYPE xstring,
       END OF ty_spta_package.

INCLUDE rsvcutct.
INCLUDE rddkorri.
INCLUDE rddkorf4.
INCLUDE ltmscdef.
INCLUDE ltmsccon.
INCLUDE ltmswcon.

DATA: gt_trkorr TYPE STANDARD TABLE OF trkorr.
DATA: sctsobject TYPE sctsobject.
DATA: vers_dest TYPE vers_dest.
DATA: go_objects_list TYPE REF TO cl_objects_list.

*----------------------------------------------------------------------*
*       CLASS cl_objects_list DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_objects_list DEFINITION FRIENDS cl_event_handler.
  PUBLIC SECTION.
    CLASS-DATA: gt_excluded_systems TYPE STANDARD TABLE OF tmscsys,
                gt_allowed_systems TYPE STANDARD TABLE OF tmscsys.
    CLASS-METHODS get_systems.
    CLASS-METHODS get_apps_instance RETURNING value(_ro_apps) TYPE REF TO cl_objects_list.
    DATA: gt_systems TYPE STANDARD TABLE OF char3.
    METHODS build_list.
    METHODS display_list.
    METHODS find_object IMPORTING _is_key TYPE ty_object_key
                        RETURNING value(_ro_object) TYPE REF TO cl_object.
    METHODS get_next_object RETURNING value(_ro_object) TYPE REF TO cl_object.
    METHODS fill_versions_object IMPORTING _io_object TYPE REF TO cl_object.
  PROTECTED SECTION.
    CLASS-DATA: gr_itab TYPE REF TO data.
  PRIVATE SECTION.
    CLASS-DATA: gr_instance_objects_list TYPE REF TO cl_objects_list.
    DATA: gv_index TYPE sy-index.
    DATA: gt_ref_objects TYPE tyt_ref_objects.
    DATA: gt_sysname_intsys TYPE STANDARD TABLE OF char3,
          gt_components_sys TYPE abap_component_tab,
          gt_components_obj TYPE abap_component_tab,
          gt_components_ver TYPE abap_component_tab.
    DATA: gr_structdescr TYPE REF TO cl_abap_structdescr,
          gr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA: gr_salv TYPE REF TO cl_salv_table,
          gr_layout TYPE REF TO cl_salv_layout,
          gs_key TYPE salv_s_layout_key,
          gr_columns TYPE REF TO cl_salv_columns_table,
          gr_column TYPE REF TO cl_salv_column_table,
          gr_sort TYPE REF TO cl_salv_sorts,
          gr_functions TYPE REF TO cl_salv_functions_list,
          gr_display TYPE REF TO cl_salv_display_settings,
          gr_events TYPE REF TO cl_salv_events_table.

    METHODS build_dynamic_table.
    METHODS get_versions_serial_exec.
    METHODS get_versions_parall_exec.
    METHODS copy_components IMPORTING _is_version_list TYPE vrsd
                                      _iv_sysnam TYPE tmscsys-sysnam
                            CHANGING _cs_result TYPE any.
    METHODS get_korrnum IMPORTING _is_itab_ob TYPE any
                                  _iv_sysnam TYPE tmscsys-sysnam
                        RETURNING value(_rv_korrnum) TYPE verskorrno.
ENDCLASS.                    "cl_object DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_request DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_request DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_objects IMPORTING _iv_request TYPE e070-trkorr
                              CHANGING _ct_objects TYPE tyt_ref_objects.
ENDCLASS.                    "cl_request DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_object DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    TYPES: BEGIN OF ty_versions,
             tarsystem TYPE tmscsys-sysnam,
             version_list TYPE vrsd_tab,
             versno_list TYPE vrsn_tab,
           END OF ty_versions,
           tyt_versions TYPE STANDARD TABLE OF ty_versions.
    CLASS-METHODS factory IMPORTING _is_e071 TYPE e071 OPTIONAL
                                    _iv_xstr TYPE xstring OPTIONAL
                          RETURNING value(_ro_object) TYPE REF TO cl_object.
    METHODS constructor IMPORTING _is_e071 TYPE e071.
    METHODS get_key RETURNING value(_rs_key) TYPE ty_object_key.
    METHODS fill_result CHANGING _cs_result TYPE any.
    METHODS get_versions IMPORTING _iv_tarsystem TYPE tmscsys-sysnam
                         EXPORTING _et_version_list TYPE vrsd_tab
                                   _et_versno_list TYPE vrsn_tab
                         EXCEPTIONS _ex_rfc_error.
    METHODS serialize_me RETURNING value(_rv_xstr) TYPE xstring.
  PRIVATE SECTION.
    CLASS-DATA: st_tadir TYPE STANDARD TABLE OF tadir.
    DATA: s_object TYPE ty_object.
    DATA: t_versions TYPE tyt_versions.
ENDCLASS.                    "cl_object DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS on_added_function
        FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
        IMPORTING e_salv_function.
    CLASS-METHODS on_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.                    "cl_event_handler DEFINITION

SELECTION-SCREEN: BEGIN OF BLOCK b1.
SELECT-OPTIONS: requests FOR e070-trkorr MEMORY ID kor.
SELECT-OPTIONS: object FOR sctsobject-object.
SELECT-OPTIONS: target FOR vers_dest-sysname.
PARAMETER: history AS CHECKBOX DEFAULT space.
PARAMETER: parallel AS CHECKBOX DEFAULT space USER-COMMAND uc1.
PARAMETER: rfcgroup TYPE spta_rfcgr MEMORY ID spta_rfcgr MODIF ID prl.
PARAMETER: maxtasks LIKE sy-index DEFAULT '10' MODIF ID prl.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
INITIALIZATION.
  cl_objects_list=>get_systems( ).
  PERFORM authority_check.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK screen-group1 EQ 'PRL'.
    CASE parallel.
      WHEN abap_true.
        screen-active = 1.
        screen-intensified = 1.
      WHEN abap_false.
        screen-active = 0.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR requests-low.
  PERFORM f4_help_on_trkorr            USING requests-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR requests-high.
  PERFORM f4_help_on_trkorr            USING requests-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR target-low.
  PERFORM f4_help_on_target            USING target-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR target-high.
  PERFORM f4_help_on_target            USING target-high.

AT SELECTION-SCREEN ON rfcgroup.
  LOOP AT SCREEN.
    CHECK screen-name EQ 'RFCGROUP' AND screen-active = 1 AND parallel EQ abap_true.
    SELECT SINGLE classname INTO rfcgroup FROM rzllitab
      WHERE classname = rfcgroup
        AND grouptype = 'S'. "rfc-group
    CHECK sy-subrc NE 0.
    MESSAGE e110(sci).
*   Invalid server group
  ENDLOOP.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  SHIFT requests-low LEFT DELETING LEADING space.
  IF requests[] IS INITIAL.
    MESSAGE e885(tk).
*    No request was entered
  ENDIF.
  SELECT trkorr FROM e070 INTO TABLE gt_trkorr WHERE trkorr IN requests.
  IF sy-subrc <> 0.
    MESSAGE e880(tk) WITH requests-low.
*   Request &1 does not exist
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  go_objects_list = cl_objects_list=>get_apps_instance( ).
  go_objects_list->build_list( ).
  go_objects_list->display_list( ).

*----------------------------------------------------------------------*
*       CLASS cl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_objects_list IMPLEMENTATION.
  METHOD get_apps_instance.
    IF gr_instance_objects_list IS INITIAL.
      CREATE OBJECT gr_instance_objects_list.
    ENDIF.
    _ro_apps = gr_instance_objects_list.
  ENDMETHOD.                    "get_apps_instance

  METHOD build_list.
    DATA: lv_trkorr TYPE trkorr.
    LOOP AT gt_trkorr INTO lv_trkorr.
      cl_request=>get_objects( EXPORTING _iv_request = lv_trkorr
                               CHANGING _ct_objects = gt_ref_objects[] ).
    ENDLOOP.
    me->build_dynamic_table( ).
    CASE parallel.
      WHEN abap_true.
        me->get_versions_parall_exec( ).
      WHEN abap_false.
        me->get_versions_serial_exec( ).
    ENDCASE.
  ENDMETHOD.                    "build_list

  METHOD find_object.
    DATA: ls_key TYPE ty_object_key.
    LOOP AT gt_ref_objects INTO _ro_object.
      ls_key = _ro_object->get_key( ).
      CHECK ls_key EQ _is_key.
      RETURN.
    ENDLOOP.
    CLEAR _ro_object.
  ENDMETHOD.                    "find_object

  METHOD get_next_object.
    IF LINES( gt_ref_objects[] ) LT gv_index.
      CLEAR _ro_object.
      RETURN.
    ENDIF.
    ADD 1 TO gv_index.
    READ TABLE gt_ref_objects INTO _ro_object INDEX gv_index.
  ENDMETHOD.                    "get_next_object

  METHOD get_systems.
    DATA: lt_all_systems TYPE STANDARD TABLE OF tmscsyslst,
          ls_loc_sys TYPE tmscsyslst,
          ls_system TYPE tmscsys.
    DATA: lv_system TYPE tmscsys-sysnam.
    DATA: ls_component TYPE LINE OF abap_component_tab.

    lv_system = sy-sysid.

    CALL FUNCTION 'TMS_CI_GET_SYSTEMLIST'
      EXPORTING
        iv_system                   = lv_system
        iv_only_active              = 'X'
      TABLES
        tt_syslst                   = lt_all_systems
      EXCEPTIONS
        tms_is_not_active           = 1
        invalid_ci_conf_with_domain = 2
        no_systems                  = 3
        OTHERS                      = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE lt_all_systems INTO ls_loc_sys INDEX 1.
    REFRESH lt_all_systems.

    CALL FUNCTION 'TMS_CI_GET_SYSTEMLIST'
      EXPORTING
        iv_domain                   = ls_loc_sys-domnam
        iv_only_active              = 'X'
      TABLES
        tt_syslst                   = lt_all_systems
      EXCEPTIONS
        tms_is_not_active           = 1
        invalid_ci_conf_with_domain = 2
        no_systems                  = 3
        OTHERS                      = 4.

    LOOP AT lt_all_systems INTO ls_loc_sys.
      CLEAR ls_system.
      MOVE-CORRESPONDING ls_loc_sys TO ls_system.
      IF ls_loc_sys-saprel(1) = '2' OR ls_loc_sys-saprel(1) = '3' OR ls_loc_sys-saprel(3) = '40A' OR ls_loc_sys-saprel(3) = '40B'.
        APPEND ls_system TO gt_excluded_systems.
      ELSE.
        APPEND ls_system TO gt_allowed_systems.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "get_systems

  METHOD build_dynamic_table.
    DATA: lt_sysname_consys TYPE STANDARD TABLE OF char3,
          lt_sysname_deliv_to TYPE STANDARD TABLE OF char3,
          lv_sysname TYPE char3,
          lt_release TYPE triwb_t_release,
          ls_release TYPE triwb_s_release,
          lt_deliver TYPE triwb_t_deliver,
          lt_trlayer TYPE triwb_t_trlayer,
          lt_target TYPE triwb_t_target,
          ls_target TYPE triwb_s_target,
          ls_deliver TYPE triwb_s_deliver,
          ls_object TYPE ty_object,
          ls_system TYPE ty_system.
    DATA: lr_structdesc TYPE REF TO cl_abap_structdescr,
          lt_components TYPE abap_component_tab,
          ls_components TYPE abap_componentdescr.
    DATA: lt_key TYPE abap_keydescr_tab,
          ls_key TYPE LINE OF abap_keydescr_tab.

    DEFINE add_components.
      loop at &1 into lv_sysname.
        loop at gt_components_sys into ls_components.
          concatenate ls_components-name '_' lv_sysname into ls_components-name.
          append ls_components to lt_components.
          append ls_components to gt_components_ver.
        endloop.
      endloop.
      append lines of &1 to gt_systems.
    END-OF-DEFINITION.

    lr_structdesc ?= cl_abap_structdescr=>describe_by_data( ls_object ).
    gt_components_obj[] = lr_structdesc->get_components( ).
    APPEND LINES OF gt_components_obj[] TO lt_components[].
    FREE lr_structdesc.
    lr_structdesc ?= cl_abap_structdescr=>describe_by_data( ls_system ).
    gt_components_sys[] = lr_structdesc->get_components( ).

    CALL FUNCTION 'TMS_WBO_CONFIG_READ'
      EXPORTING
        iv_language             = sy-langu
      IMPORTING
        et_trlayer              = lt_trlayer[]
        et_release              = lt_release[]
        et_deliver              = lt_deliver[]
        et_target               = lt_target[]
      EXCEPTIONS
        configuration_not_found = 1
        OTHERS                  = 2.
    CASE sy-subrc.
      WHEN 0.
        LOOP AT lt_release INTO ls_release WHERE intsys NE 'SAP'.
          READ TABLE gt_allowed_systems WITH KEY sysnam = ls_release-intsys(3) TRANSPORTING NO FIELDS.
          CHECK sy-subrc EQ 0.
* integration system
          COLLECT ls_release-intsys(3) INTO gt_sysname_intsys.
          IF ls_release-consys(1) = '/'.
* destination group
            LOOP AT lt_target INTO ls_target
                              WHERE targ_group = ls_release-consys AND
                                    tarsystem NE space.
              READ TABLE gt_allowed_systems WITH KEY sysnam = ls_target-tarsystem(3) TRANSPORTING NO FIELDS.
              CHECK sy-subrc EQ 0.
              COLLECT ls_target-tarsystem(3) INTO lt_sysname_consys.
            ENDLOOP.
          ELSE.
* consolidation system
            READ TABLE gt_allowed_systems WITH KEY sysnam = ls_release-consys(3) TRANSPORTING NO FIELDS.
            CHECK sy-subrc EQ 0.
            COLLECT ls_release-consys(3) INTO lt_sysname_consys.
          ENDIF.
        ENDLOOP.
        LOOP AT lt_sysname_consys INTO lv_sysname.
          LOOP AT lt_deliver INTO  ls_deliver
                             WHERE fromsystem EQ lv_sysname.
            IF ls_deliver-tosystem(1) = '/'.
* destination group
              LOOP AT lt_target INTO ls_target
                                WHERE targ_group = ls_deliver-tosystem AND
                                      tarsystem NE space.
                READ TABLE gt_allowed_systems WITH KEY sysnam = ls_target-tarsystem(3) TRANSPORTING NO FIELDS.
                CHECK sy-subrc EQ 0.
                COLLECT ls_target-tarsystem(3) INTO lt_sysname_deliv_to.
              ENDLOOP.
            ELSE.
* destination system
              READ TABLE gt_allowed_systems WITH KEY sysnam = ls_deliver-tosystem(3) TRANSPORTING NO FIELDS.
              CHECK sy-subrc EQ 0.
              COLLECT ls_deliver-tosystem(3) INTO lt_sysname_deliv_to.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        LOOP AT lt_sysname_consys INTO lv_sysname.
          READ TABLE gt_sysname_intsys FROM lv_sysname TRANSPORTING NO FIELDS.
          CHECK sy-subrc EQ 0.
          DELETE lt_sysname_consys.
        ENDLOOP.
        add_components: gt_sysname_intsys,
                        lt_sysname_consys,
                        lt_sysname_deliv_to.
        gr_structdescr ?= cl_abap_structdescr=>create( lt_components ).
        ls_key-name = 'KEY'.
        APPEND ls_key TO lt_key.
        gr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type  = gr_structdescr
                                                     p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                     p_unique     = abap_false
                                                     p_key        = lt_key[]
                                                     p_key_kind   = cl_abap_tabledescr=>keydefkind_user ).
        CREATE DATA gr_itab TYPE HANDLE gr_tabledescr.
      WHEN 1.
        CALL FUNCTION 'TRINT_TCE_MESSAGE'
          EXPORTING
            iv_number = 48
            iv_type   = 'I'
            iv_par2   = sy-sysid
            iv_par1   = '0000'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "build_dynamic_table

  METHOD copy_components.
    DATA: ls_components TYPE abap_componentdescr,
          lv_fname TYPE string.
    FIELD-SYMBOLS: <lv_fname1> TYPE ANY,
                   <lv_fname2> TYPE ANY.

    LOOP AT gt_components_sys INTO ls_components.
      UNASSIGN: <lv_fname1>, <lv_fname2>.
      ASSIGN COMPONENT ls_components-name OF STRUCTURE _is_version_list TO <lv_fname1>.
      CONCATENATE ls_components-name '_' _iv_sysnam INTO lv_fname.
      ASSIGN COMPONENT lv_fname OF STRUCTURE _cs_result TO <lv_fname2>.
      CHECK <lv_fname1> IS ASSIGNED AND <lv_fname2> IS ASSIGNED.
      <lv_fname2> = <lv_fname1>.
    ENDLOOP.
  ENDMETHOD.                    "copy_components

  METHOD get_korrnum.
    DATA: lv_fname TYPE string.
    FIELD-SYMBOLS: <lv_fname> TYPE ANY.
    CONCATENATE 'KORRNUM_' _iv_sysnam INTO lv_fname.
    ASSIGN COMPONENT lv_fname OF STRUCTURE _is_itab_ob TO <lv_fname>.
    CHECK <lv_fname> IS ASSIGNED.
    _rv_korrnum = <lv_fname>.
  ENDMETHOD.                    "get_korrnum

  METHOD get_versions_serial_exec.
    DATA: lo_object TYPE REF TO cl_object.

    LOOP AT me->gt_ref_objects[] INTO lo_object.
      me->fill_versions_object( EXPORTING _io_object = lo_object ).
    ENDLOOP.
  ENDMETHOD.                    "get_versions

  METHOD get_versions_parall_exec.
    CALL FUNCTION 'SPTA_PARA_PROCESS_START_2'
      EXPORTING
        max_no_of_tasks          = maxtasks
        server_group             = rfcgroup
        before_rfc_callback_form = 'BEFORE_RFC'
        in_rfc_callback_form     = 'IN_RFC'
        after_rfc_callback_form  = 'AFTER_RFC'
        callback_prog            = sy-repid
      EXCEPTIONS
        invalid_server_group     = 1
        no_resources_available   = 2
        OTHERS                   = 3.
  ENDMETHOD.                    "get_versions_parall_exec

  METHOD fill_versions_object.
    DATA: lv_fname TYPE string,
          lv_destination TYPE rfcdes-rfcdest.
    DATA: lv_system TYPE tmscsys-sysnam,
          ls_systems TYPE tmscsys-sysnam,
          version_list TYPE STANDARD TABLE OF vrsd,
          lv_sysname_intsys TYPE char3,
          ls_version_list TYPE vrsd,
          ls_version_list_sel TYPE vrsd,
          lv_korrnum TYPE verskorrno,
          lv_korrnum_before TYPE verskorrno.
    DATA: lv_del(1), lv_red(1), lv_ind LIKE sy-tabix.
    DATA: lr_itab_obj TYPE REF TO data.
    FIELD-SYMBOLS: <ls_wa> TYPE ANY,
                   <lv_fname1> TYPE ANY,
                   <ls_color> TYPE lvc_s_scol,
                   <lt_itab> TYPE STANDARD TABLE,
                   <lt_itab_obj> TYPE STANDARD TABLE.

    DEFINE set_color_cell.
      assign component 'TCOLOR' of structure <ls_wa> to <lt_itab>.
      append initial line to <lt_itab> assigning <ls_color>.
      <ls_color>-fname = &1.
      <ls_color>-color-col = &2.
    END-OF-DEFINITION.

    CREATE DATA lr_itab_obj TYPE HANDLE gr_tabledescr.
    ASSIGN lr_itab_obj->* TO <lt_itab_obj>.

    LOOP AT gt_systems INTO lv_system.

      CALL METHOD _io_object->get_versions( EXPORTING _iv_tarsystem = lv_system
                                            IMPORTING _et_version_list = version_list
                                            EXCEPTIONS _ex_rfc_error = 1 ).
      CASE sy-subrc.
        WHEN 0.
          LOOP AT version_list INTO ls_version_list.
            TRY.
                LOOP AT <lt_itab_obj> ASSIGNING <ls_wa>.
                  LOOP AT gt_systems INTO ls_systems WHERE table_line NE lv_system.
                    CHECK me->get_korrnum( _is_itab_ob = <ls_wa> _iv_sysnam = ls_systems ) EQ ls_version_list-korrnum.
                    RAISE EXCEPTION TYPE cx_exception00.
                  ENDLOOP.
                ENDLOOP.
                RAISE EXCEPTION TYPE cx_exception01.
              CATCH cx_exception00.
                IF ls_version_list-korrnum IS NOT INITIAL AND
                   ls_version_list-korrnum NOT IN requests AND
                   ls_version_list-korrnum NE lv_korrnum_before AND
                   lv_system IN target.
                  lv_red = 'X'.
                ENDIF.
              CATCH cx_exception01.
                APPEND INITIAL LINE TO <lt_itab_obj> ASSIGNING <ls_wa>.
                _io_object->fill_result( CHANGING _cs_result = <ls_wa> ).
                IF ls_version_list-korrnum IN requests.
                  ls_version_list_sel = ls_version_list.
                ENDIF.
                IF lv_korrnum_before IS INITIAL AND
                   ls_version_list-korrnum NOT IN requests AND
                   ls_version_list-versno LT ls_version_list_sel-versno.
                  lv_korrnum_before = ls_version_list-korrnum.
                ENDIF.
            ENDTRY.
            me->copy_components( EXPORTING _is_version_list = ls_version_list
                                           _iv_sysnam = lv_system
                                 CHANGING _cs_result = <ls_wa> ).
            CHECK ls_version_list-korrnum IN requests.
            CONCATENATE 'KORRNUM_' lv_system INTO lv_fname.
            set_color_cell lv_fname '3'.
          ENDLOOP.
        WHEN OTHERS.
          lv_red = 'X'.
          IF LINES( <lt_itab_obj>[] ) = 0.
            APPEND INITIAL LINE TO <lt_itab_obj> ASSIGNING <ls_wa>.
            _io_object->fill_result( CHANGING _cs_result = <ls_wa> ).
          ELSE.
            READ TABLE <lt_itab_obj> ASSIGNING <ls_wa> INDEX 1.
          ENDIF.
          CONCATENATE 'KORRNUM_' lv_system INTO lv_fname.
          ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_wa> TO <lv_fname1>.
          <lv_fname1> = 'RFC_ERROR'.
          set_color_cell lv_fname '6'.
      ENDCASE.
    ENDLOOP.

    CHECK <lt_itab_obj>[] IS NOT INITIAL.

    LOOP AT <lt_itab_obj> ASSIGNING <ls_wa>.
      IF lv_red = 'X'.
        set_color_cell: 'KEY-PGMID' '6', 'KEY-OBJECT' '6', 'KEY-OBJ_NAME' '6'.
      ELSE.
        set_color_cell: 'KEY-PGMID' '5', 'KEY-OBJECT' '5', 'KEY-OBJ_NAME' '5'.
      ENDIF.
    ENDLOOP.

    IF history IS NOT INITIAL.
      lv_ind = LINES( <lt_itab_obj>[] ).
      DO.
        lv_del = 'X'.
        READ TABLE <lt_itab_obj> ASSIGNING <ls_wa> INDEX lv_ind.
        IF sy-subrc NE 0.
          EXIT.
        ELSE.
          LOOP AT gt_systems INTO lv_system.
            lv_korrnum = me->get_korrnum( _is_itab_ob = <ls_wa> _iv_sysnam = lv_system ).
            READ TABLE gt_sysname_intsys FROM lv_sysname_intsys TRANSPORTING NO FIELDS.
            CHECK sy-subrc EQ 0 AND lv_korrnum IN requests OR
                  sy-subrc NE 0 AND lv_korrnum IS NOT INITIAL.
            lv_del = space.
            EXIT.
          ENDLOOP.
          IF lv_del EQ space.
            EXIT.
          ELSE.
            DELETE <lt_itab_obj> INDEX lv_ind.
          ENDIF.
          SUBTRACT 1 FROM lv_ind.
        ENDIF.
      ENDDO.
    ENDIF.

    ASSIGN gr_itab->* TO <lt_itab>.
    APPEND LINES OF <lt_itab_obj>[] TO <lt_itab>[].
  ENDMETHOD.                    "fill_versions_object

  METHOD display_list.
    DATA: ls_components TYPE abap_componentdescr,
          lv_name TYPE lvc_fname.
    DATA: lx_msg TYPE REF TO cx_salv_msg.
    DATA: lv_ltext TYPE scrtext_l,
          lv_mtext TYPE scrtext_m,
          lv_stext TYPE scrtext_s.

    FIELD-SYMBOLS: <lt_itab> TYPE STANDARD TABLE.

    ASSIGN gr_itab->* TO <lt_itab>.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gr_salv
          CHANGING
            t_table      = <lt_itab>.
      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.
* activate ALV functions
    gr_functions = gr_salv->get_functions( ).
    gr_functions->set_all( abap_true ).
*    gr_functions->set_default( abap_true ).
* set display setting
    gr_display = gr_salv->get_display_settings( ).
    gr_display->set_striped_pattern( 'X' ).
* Events
    gr_events = gr_salv->get_event( ).
    SET HANDLER cl_event_handler=>on_added_function FOR gr_events.
    SET HANDLER cl_event_handler=>on_double_click FOR gr_events.
* Layout
    gr_layout = gr_salv->get_layout( ).
    gs_key-report = sy-repid.
    gr_layout->set_key( gs_key ).
    gr_layout->set_default( 'X' ).
    gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
* Columns
    gr_columns = gr_salv->get_columns( ).
    gr_columns->set_optimize( 'X' ).
    gr_columns->set_color_column( 'TCOLOR' ).
    gr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).
    TRY.
        LOOP AT gt_components_obj INTO ls_components WHERE name NE 'KEY'
                                                       AND name NE 'TCOLOR'.
          lv_name = ls_components-name.
          gr_column ?= gr_columns->get_column( lv_name ).
          gr_column->set_visible( if_salv_c_bool_sap=>false ).
        ENDLOOP.
        LOOP AT gt_components_ver INTO ls_components.
          lv_name = ls_components-name.
          gr_column ?= gr_columns->get_column( lv_name ).
          gr_column->set_zero( if_salv_c_bool_sap=>false ).
          CHECK lv_name CP 'KORRNUM_*'.
          lv_ltext = lv_mtext = lv_stext = lv_name+8.
          gr_column->set_short_text( lv_stext ).
          gr_column->set_medium_text( lv_mtext ).
          gr_column->set_long_text( lv_ltext ).
        ENDLOOP.
      CATCH cx_salv_not_found.

    ENDTRY.
* Sort
    gr_sort = gr_salv->get_sorts( ).
    TRY.
        gr_sort->add_sort( EXPORTING columnname = 'KEY-PGMID' subtotal   = if_salv_c_bool_sap=>true ).
        gr_sort->add_sort( EXPORTING columnname = 'KEY-OBJECT' subtotal   = if_salv_c_bool_sap=>true ).
        gr_sort->add_sort( EXPORTING columnname = 'KEY-OBJ_NAME' subtotal   = if_salv_c_bool_sap=>true ).
      CATCH cx_salv_error .
    ENDTRY.

    gr_salv->display( ).

  ENDMETHOD.                    "display_list
ENDCLASS.                    "cl_request DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_object IMPLEMENTATION.
  METHOD factory.
    IF _iv_xstr IS NOT INITIAL.
      CALL TRANSFORMATION id_indent
        SOURCE XML _iv_xstr
        RESULT obj = _ro_object.
    ELSEIF _is_e071 IS NOT INITIAL.
      CREATE OBJECT _ro_object
        EXPORTING
          _is_e071 = _is_e071.
    ENDIF.
  ENDMETHOD.                    "cl_object

  METHOD constructor.
    DATA: ls_tadir_key TYPE tadir,
          ls_tadir TYPE tadir.

    MOVE-CORRESPONDING _is_e071 TO s_object-key.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071    = _is_e071
      IMPORTING
        pe_result  = s_object-result
        we_cli_dep = s_object-cli_dep
        we_tadir   = ls_tadir_key.
    IF s_object-result CA 'L'.
      CLEAR ls_tadir.
      READ TABLE st_tadir INTO ls_tadir
                          WITH KEY pgmid   = ls_tadir_key-pgmid
                                  object   = ls_tadir_key-object
                                  obj_name = ls_tadir_key-obj_name
                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        SELECT SINGLE * FROM  tadir
                        INTO  ls_tadir
                        WHERE pgmid    = ls_tadir_key-pgmid
                        AND   object   = ls_tadir_key-object
                        AND   obj_name = ls_tadir_key-obj_name.
        INSERT ls_tadir INTO st_tadir INDEX sy-tabix.
      ENDIF.
      IF ls_tadir-obj_name <> space.
        s_object-t_pgmid       = ls_tadir-pgmid.
        s_object-t_object      = ls_tadir-object.
        s_object-t_obj_name    = ls_tadir-obj_name.
        s_object-srcsystem     = ls_tadir-srcsystem.
        s_object-devclass      = ls_tadir-devclass.
        s_object-author        = ls_tadir-author.
        s_object-masterlang    = ls_tadir-masterlang.
        s_object-repair        = ls_tadir-srcdep.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD get_key.
    _rs_key = s_object-key.
  ENDMETHOD.                    "get_key

  METHOD fill_result.
    DATA: lv_ind TYPE i VALUE 1.
    FIELD-SYMBOLS: <ls_wa>   TYPE ANY,
                   <lv_fname1> TYPE ANY,
                   <lv_fname2> TYPE ANY.
    DO.
      UNASSIGN: <lv_fname1>, <lv_fname2>.
      ASSIGN COMPONENT lv_ind OF STRUCTURE s_object TO <lv_fname1>.
      ASSIGN COMPONENT lv_ind OF STRUCTURE _cs_result TO <lv_fname2>.
      IF <lv_fname1> IS ASSIGNED AND <lv_fname2> IS ASSIGNED.
        <lv_fname2> = <lv_fname1>.
        ADD 1 TO lv_ind.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.                    "fill_result

  METHOD get_versions.
    DATA: ls_versions LIKE LINE OF me->t_versions.
    DATA: lv_destination TYPE rfcdes-rfcdest.
    DATA: lv_tardomain TYPE tmsdomnam.
    DATA: ls_exception TYPE stmscalert.
    DATA: destination TYPE rfcdes-rfcdest.
    DATA: objtype TYPE vrsd-objtype,
          objname TYPE vrsd-objname.

    READ TABLE me->t_versions[] INTO ls_versions WITH KEY tarsystem = _iv_tarsystem.
    IF sy-subrc EQ 0.
      _et_versno_list = ls_versions-versno_list.
      _et_version_list = ls_versions-version_list.
    ELSE.
      CLEAR: _et_version_list[], _et_versno_list[].
      objtype = s_object-key-object.
      objname = s_object-key-obj_name.

      IF _iv_tarsystem EQ sy-sysid.
        CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
          EXPORTING
            objname      = objname
            objtype      = objtype
          TABLES
            lversno_list = _et_versno_list
            version_list = _et_version_list
          EXCEPTIONS
            no_entry     = 1.
      ELSE.
        CALL FUNCTION 'SVRS_LONG2SHORT_NAME'
          EXPORTING
            objtype       = objtype
            objname_long  = objname
          IMPORTING
            objname_short = objname.

        ci_clear_container.
        ci_encode_param 'VERSION_INFO_FUNCNAME' 'SVRS_GET_VERSION_DIRECTORY_40'.
        ci_encode_param 'DESTINATION' destination.
        ci_encode_param 'OBJNAME' objname.
        ci_encode_param 'OBJTYPE' objtype.
        ci_encode_table 'LVERSNO_LIST' 'VRSN' _et_versno_list.
        ci_encode_table 'VERSION_LIST' 'VRSD' _et_version_list.

        CALL FUNCTION 'TMS_CFG_GET_DOMAIN_NAME'
          EXPORTING
            iv_system      = _iv_tarsystem
          IMPORTING
            ev_domain_name = lv_tardomain.

        CALL FUNCTION 'TMS_CI_SEND_COMMAND'
          EXPORTING
            iv_tarsystem = _iv_tarsystem
            iv_tardomain = lv_tardomain
            iv_service   = gc_tms_wbo
            iv_command   = gc_tms_wbo_get_version_info
          IMPORTING
            es_exception = ls_exception
          EXCEPTIONS
            alert        = 1.
        ci_decode_table 'LVERSNO_LIST' _et_versno_list.
        ci_decode_table 'VERSION_LIST' _et_version_list.
      ENDIF.

      CASE ls_exception-error.
        WHEN 'COMMUNICATION_FAILURE' OR 'SYSTEM_FAILURE'.
          RAISE _ex_rfc_error.
        WHEN OTHERS.
          ls_versions-tarsystem = _iv_tarsystem.
          ls_versions-versno_list = _et_versno_list.
          ls_versions-version_list = _et_version_list.
          APPEND ls_versions TO me->t_versions[].
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "get_versions
  METHOD serialize_me.
    CALL TRANSFORMATION id_indent
        SOURCE obj = me
        RESULT XML _rv_xstr.
  ENDMETHOD.                    "serialize_me
ENDCLASS.                    "cl_request DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_request IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_request IMPLEMENTATION.
  METHOD get_objects.
    DATA: lt_e071 TYPE STANDARD TABLE OF e071,
          lt_obj_tab TYPE STANDARD TABLE OF e071,
          ls_e071 TYPE e071,
          ls_object TYPE ty_object_key.
    DATA: lo_objects_list TYPE REF TO cl_objects_list,
          lo_object TYPE REF TO cl_object.

    lo_objects_list = cl_objects_list=>get_apps_instance( ).
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_e071
      FROM e071
      WHERE trkorr EQ _iv_request.
    LOOP AT lt_e071 INTO ls_e071.
      CLEAR lt_obj_tab[].
      CALL FUNCTION 'SUMO_RESOLVE_E071_OBJ'
        EXPORTING
          e071_obj = ls_e071
        TABLES
          obj_tab  = lt_obj_tab.
      LOOP AT lt_obj_tab INTO ls_e071 WHERE object IN object.
        MOVE-CORRESPONDING ls_e071 TO ls_object.
        lo_object = lo_objects_list->find_object( ls_object ).
        CHECK lo_object IS NOT BOUND.
        lo_object = cl_object=>factory( _is_e071 = ls_e071 ).
        APPEND lo_object TO _ct_objects.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.                    "factory
ENDCLASS.                    "cl_request DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_event_handler IMPLEMENTATION.
  METHOD on_added_function.

  ENDMETHOD.                    "on_added_function

  METHOD on_double_click.
    DATA: rep_disp TYPE trdir-name,
          objtype TYPE vrsd-objtype,
          objname TYPE vrsd-objname,
          korrnum TYPE trkorr.
    FIELD-SYMBOLS: <lt_itab> TYPE STANDARD TABLE,
                   <ls_wa> TYPE ANY,
                   <lv_field> TYPE ANY.

    DEFINE get_component.
      unassign <lv_field>.
      assign component &1 of structure <ls_wa> to <lv_field>.
      if <lv_field> is assigned.
        &2 = <lv_field>.
      endif.
    END-OF-DEFINITION.

    ASSIGN cl_objects_list=>gr_itab->* TO <lt_itab>.
    READ TABLE <lt_itab> ASSIGNING <ls_wa> INDEX row.
    IF column EQ 'KEY-OBJ_NAME'.
      get_component 'KEY-OBJECT' objtype.
      get_component 'KEY-OBJ_NAME' objname.
      CALL FUNCTION 'SVRS_GET_OBJECT_REPORTS'
        EXPORTING
          objtype  = objtype
        IMPORTING
          rep_disp = rep_disp.
      IF rep_disp IS NOT INITIAL.
        SUBMIT (rep_disp) AND RETURN
                WITH objname = objname
                WITH objtype = objtype.
      ENDIF.
    ELSEIF column CP 'KORRNUM*'.
      get_component column korrnum.
      IF korrnum IS NOT INITIAL.
        PERFORM display_single_request IN PROGRAM rddm0001 USING korrnum.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "cl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  authority_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check.
  CALL FUNCTION 'TR_AUTHORITY_CHECK_DISPLAY'
    EXCEPTIONS
      e_no_authority = 1.
  IF sy-subrc <> 0.
    MESSAGE ID    sy-msgid
            TYPE  'S'
            NUMBER sy-msgno
            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE.
  ENDIF.
ENDFORM.                               " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  f4_help_on_trkorr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_TRKORR  text
*----------------------------------------------------------------------*
FORM f4_help_on_trkorr
  USING
    pv_trkorr LIKE e070-trkorr.
  CALL FUNCTION 'TR_F4_REQUESTS'
    EXPORTING
      iv_trkorr_pattern   = pv_trkorr
      iv_title            = 'Analyze objects...   '
    IMPORTING
      ev_selected_request = pv_trkorr.
ENDFORM.                               " F4_HELP_ON_TRKORR
*&---------------------------------------------------------------------*
*&      Form  f4_help_on_target
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_TRKORR  text
*----------------------------------------------------------------------*
FORM f4_help_on_target
  USING
    pv_target LIKE vers_dest-sysname.
  CALL FUNCTION 'TMS_UI_F4_SYSTEMS'
    EXPORTING
      iv_plus_virtual  = abap_false
      iv_plus_external = abap_false
      iv_only_active   = abap_true
      iv_title         = 'Target system selection...'
    TABLES
      tt_exclude       = cl_objects_list=>gt_excluded_systems
    CHANGING
      cv_system        = pv_target
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "f4_help_on_target
*&---------------------------------------------------------------------*
*&      Form  before_rfc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM before_rfc
   USING
      p_before_rfc_imp     TYPE spta_t_before_rfc_imp
   CHANGING
      p_before_rfc_exp     TYPE spta_t_before_rfc_exp
      pt_rfcdata           TYPE spta_t_indxtab
      p_failed_objects     TYPE spta_t_failed_objects
      p_objects_in_process TYPE spta_t_objects_in_process
      p_user_param.

  DATA: lo_objects_list TYPE REF TO cl_objects_list.
  DATA: lo_object TYPE REF TO cl_object.
  DATA: lv_xstr TYPE xstring.
  DATA: ls_spta_package TYPE ty_spta_package.

  lo_objects_list = cl_objects_list=>get_apps_instance( ).
  lo_object = lo_objects_list->get_next_object( ).
  IF lo_object IS NOT BOUND.
    CLEAR p_before_rfc_exp-start_rfc.
    RETURN.
  ENDIF.

  ls_spta_package-object = lo_object->serialize_me( ).
  ls_spta_package-t_systems = lo_objects_list->gt_systems.
  p_before_rfc_exp-start_rfc = 'X'.
  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = ls_spta_package
    IMPORTING
      indxtab = pt_rfcdata.
ENDFORM.                    "before_rfc
*&---------------------------------------------------------------------*
*&      Form  in_rfc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IN_RFC_IMP  text
*      <--P_IN_RFC_EXP  text
*      <--P_RFCDATA     text
*----------------------------------------------------------------------*
FORM in_rfc
   USING
      p_in_rfc_imp  TYPE spta_t_in_rfc_imp
   CHANGING
      p_in_rfc_exp  TYPE spta_t_in_rfc_exp
      p_rfcdata     TYPE spta_t_indxtab.

  DATA: lo_object TYPE REF TO cl_object.
  DATA: ls_spta_package TYPE ty_spta_package.
  DATA: ls_systems TYPE tmscsys-sysnam.
  DATA: lv_destination TYPE rfcdes-rfcdest.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = p_rfcdata
    IMPORTING
      data    = ls_spta_package.

  lo_object = cl_object=>factory( _iv_xstr = ls_spta_package-object ).

  LOOP AT ls_spta_package-t_systems INTO ls_systems.
    CALL METHOD lo_object->get_versions( EXPORTING _iv_tarsystem = ls_systems
                                         EXCEPTIONS _ex_rfc_error = 1 ).
  ENDLOOP.

  ls_spta_package-object = lo_object->serialize_me( ).
  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = ls_spta_package
    IMPORTING
      indxtab = p_rfcdata.
ENDFORM.                    "in_rfc
*&---------------------------------------------------------------------*
*&      Form  after_rfc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RFCDATA             text
*      -->P_RFCSUBRC            text
*      -->P_RFCMSG              text
*      -->P_OBJECTS_IN_PROCESS  text
*      -->P_AFTER_RFC_IMP       text
*      <--P_AFTER_RFC_EXP       text
*      <--P_USER_PARAM          text
*----------------------------------------------------------------------*
FORM after_rfc
   USING
      p_rfcdata            TYPE spta_t_indxtab
      p_rfcsubrc           TYPE sy-subrc
      p_rfcmsg             TYPE spta_t_rfcmsg
      p_objects_in_process TYPE spta_t_objects_in_process
      p_after_rfc_imp      TYPE spta_t_after_rfc_imp
   CHANGING
      p_after_rfc_exp      TYPE spta_t_after_rfc_exp
      p_user_param.

  DATA: lo_objects_list TYPE REF TO cl_objects_list.
  DATA: lo_object TYPE REF TO cl_object.
  DATA: ls_spta_package TYPE ty_spta_package.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = p_rfcdata
    IMPORTING
      data    = ls_spta_package.

  lo_object = cl_object=>factory( _iv_xstr = ls_spta_package-object ).
  lo_objects_list = cl_objects_list=>get_apps_instance( ).
  lo_objects_list->fill_versions_object( EXPORTING _io_object = lo_object ).
ENDFORM.                    "after_rfc