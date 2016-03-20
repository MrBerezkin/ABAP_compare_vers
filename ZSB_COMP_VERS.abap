*&---------------------------------------------------------------------*
*& Report  ZSB_COMP_VERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsb_comp_vers.

TYPE-POOLS: trwbo, slis, trwb1, triwb, abap, icon.

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
       tyt_ref_objects TYPE STANDARD TABLE OF ty_ref_objects.

INCLUDE rsvcutct.
INCLUDE rddkorri.
INCLUDE rddkorf4.

DATA: gt_trkorr TYPE STANDARD TABLE OF trkorr.
DATA: sctsobject TYPE sctsobject.
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
    METHODS build_list.
    METHODS display_list.
    METHODS find_object IMPORTING _is_key TYPE ty_object_key
                        RETURNING value(_ro_object) TYPE REF TO cl_object.
  PROTECTED SECTION.
    CLASS-DATA: gr_itab TYPE REF TO data.
  PRIVATE SECTION.
    DATA: gt_ref_objects TYPE tyt_ref_objects.
    DATA: gt_sysname_intsys TYPE STANDARD TABLE OF char3,
          gt_systems TYPE STANDARD TABLE OF char3,
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
    METHODS get_versions.
    METHODS copy_components IMPORTING _is_version_list TYPE vrsd
                                      _iv_sysnam TYPE tmssysnam
                            CHANGING _cs_result TYPE any.
    METHODS get_korrnum IMPORTING _is_itab_ob TYPE any
                                  _iv_sysnam TYPE char3
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
                                        _ip_objects_list TYPE REF TO cl_objects_list
                              CHANGING _ct_objects TYPE tyt_ref_objects.
ENDCLASS.                    "cl_request DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_object DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS factory IMPORTING _is_e071 TYPE e071
                          RETURNING value(_ro_object) TYPE REF TO cl_object.
    METHODS constructor IMPORTING _is_e071 TYPE e071.
    METHODS get_key RETURNING value(_rs_key) TYPE ty_object_key.
    METHODS fill_result CHANGING _cs_result TYPE any.
  PRIVATE SECTION.
    DATA: s_object TYPE ty_object.
    CLASS-DATA: st_tadir TYPE STANDARD TABLE OF tadir.
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
PARAMETER: target TYPE vers_dest-sysname.
PARAMETER: history AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
INITIALIZATION.
  cl_objects_list=>get_systems( ).
  PERFORM authority_check.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR requests-low.
  PERFORM f4_help_on_trkorr            USING requests-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR requests-high.
  PERFORM f4_help_on_trkorr            USING requests-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR target.
  PERFORM f4_help_on_target            USING target.

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

  CREATE OBJECT go_objects_list.
  go_objects_list->build_list( ).
  go_objects_list->display_list( ).

*----------------------------------------------------------------------*
*       CLASS cl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_objects_list IMPLEMENTATION.
  METHOD build_list.
    DATA: lv_trkorr TYPE trkorr.
    LOOP AT gt_trkorr INTO lv_trkorr.
      cl_request=>get_objects( EXPORTING _iv_request = lv_trkorr
                                         _ip_objects_list = me
                               CHANGING _ct_objects = gt_ref_objects[] ).
    ENDLOOP.
    me->build_dynamic_table( ).
    me->get_versions( ).
  ENDMETHOD.                    "build_list
  METHOD find_object.
    DATA: ls_key TYPE ty_object_key.
    LOOP AT gt_ref_objects INTO _ro_object.
      ls_key = _ro_object->get_key( ).
      IF ls_key EQ _is_key.
        RETURN.
      ELSE.
        CLEAR _ro_object.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "find_object
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
  METHOD get_versions.
    DATA: lo_object TYPE REF TO cl_object.
    DATA: objtype TYPE vrsd-objtype,
          objname TYPE vrsd-objname,
          returncode LIKE sy-subrc,
          ls_color TYPE lvc_s_scol,
          lv_fname TYPE string,
          lv_error TYPE string,
          ls_obj_key TYPE ty_object_key,
          ls_components TYPE abap_componentdescr.
    DATA: lv_system TYPE char3.
    DATA: lv_error_txt TYPE char20.
    DATA: ls_tmscsys TYPE tmscsys,
          lv_destination TYPE rfcdes-rfcdest.
    DATA: lversno_list TYPE STANDARD TABLE OF vrsn,
          version_list TYPE STANDARD TABLE OF vrsd,
          lv_sysname LIKE LINE OF gt_sysname_intsys,
          ls_version_list TYPE vrsd,
          ls_version_list1 TYPE vrsd,
          lv_korrnum TYPE verskorrno,
          lv_korrnum_before TYPE verskorrno.
    DATA: lr_target TYPE RANGE OF verssysnam,
          ls_target LIKE LINE OF lr_target.
    DATA: lv_del(1), lv_red(1), lv_ind LIKE sy-tabix.
    DATA: lr_itab_obj TYPE REF TO data.
    FIELD-SYMBOLS: <ls_wa>   TYPE ANY,
                   <lv_fname1> TYPE ANY,
                   <lv_fname2> TYPE ANY,
                   <lt_color> TYPE STANDARD TABLE,
                   <lt_itab> TYPE STANDARD TABLE,
                   <lt_itab_obj> TYPE STANDARD TABLE.

    DEFINE set_color_cell.
      ls_color-fname = &1.
      ls_color-color-col = &2.
      unassign <lt_color>.
      assign component 'TCOLOR' of structure <ls_wa> to <lt_color>.
      if <lt_color> is assigned.
        append ls_color to <lt_color>.
      endif.
    END-OF-DEFINITION.

    IF target NE space.
      CONCATENATE 'IEQ' target INTO ls_target.
      APPEND ls_target TO lr_target.
    ENDIF.

    ASSIGN gr_itab->* TO <lt_itab>.
    CREATE DATA lr_itab_obj TYPE HANDLE gr_tabledescr.
    ASSIGN lr_itab_obj->* TO <lt_itab_obj>.
    LOOP AT gt_ref_objects INTO lo_object.
      ls_obj_key = lo_object->get_key( ).
      objtype = ls_obj_key-object.
      objname = ls_obj_key-obj_name.
      CALL FUNCTION 'SVRS_SHORT2LONG_NAME'
        EXPORTING
          objtype       = objtype
          objname_short = objname
        IMPORTING
          objname_long  = objname.
      CLEAR: lv_korrnum_before, lv_red, <lt_itab_obj>[].
      LOOP AT gt_systems INTO lv_system.
        LOOP AT gt_allowed_systems INTO ls_tmscsys WHERE sysnam EQ lv_system.
          CLEAR: lv_destination, version_list[], lversno_list[], returncode.
          CONCATENATE gc_tmsadm ls_tmscsys-sysnam '.' ls_tmscsys-domnam INTO lv_destination.
          IF sy-sysid EQ ls_tmscsys-sysnam.
            lv_destination = space.
          ENDIF.

          CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
            EXPORTING
              destination           = lv_destination
              objname               = objname
              objtype               = objtype
            TABLES
              version_list          = version_list
              lversno_list          = lversno_list
            EXCEPTIONS
              no_entry              = 1
              system_failure        = 2
              communication_failure = 3
              OTHERS                = 4.
          CASE sy-subrc.
            WHEN 0. "versions exist
              LOOP AT version_list INTO ls_version_list.
                TRY.
                    LOOP AT <lt_itab_obj> ASSIGNING <ls_wa>.
                      LOOP AT gt_systems INTO lv_system WHERE table_line NE ls_tmscsys-sysnam.
                        CHECK me->get_korrnum( _is_itab_ob = <ls_wa> _iv_sysnam = lv_system ) EQ ls_version_list-korrnum.
                        RAISE EXCEPTION TYPE cx_exception00.
                      ENDLOOP.
                    ENDLOOP.
                    RAISE EXCEPTION TYPE cx_exception01.
                  CATCH cx_exception00.
                    IF ls_version_list-korrnum IS NOT INITIAL AND
                       ls_version_list-korrnum NOT IN requests AND
                       ls_version_list-korrnum NE lv_korrnum_before AND
                       ls_tmscsys-sysnam IN lr_target.
                      lv_red = 'X'.
                    ENDIF.
                  CATCH cx_exception01.
                    APPEND INITIAL LINE TO <lt_itab_obj> ASSIGNING <ls_wa>.
                    lo_object->fill_result( CHANGING _cs_result = <ls_wa> ).
                    IF lv_korrnum_before IS INITIAL AND ls_version_list-korrnum NOT IN requests.
                      lv_korrnum_before = ls_version_list-korrnum.
                    ENDIF.
                ENDTRY.
                me->copy_components( EXPORTING _is_version_list = ls_version_list
                                               _iv_sysnam = ls_tmscsys-sysnam
                                     CHANGING _cs_result = <ls_wa> ).
                CHECK ls_version_list-korrnum IN requests.
                CONCATENATE 'KORRNUM_' ls_tmscsys-sysnam INTO lv_fname.
                set_color_cell lv_fname '3'.
              ENDLOOP.
            WHEN 2 OR 3.
              lv_red = 'X'.
              IF sy-subrc EQ 2.
                lv_error_txt = 'SYSTEM_ERROR'.
              ELSEIF sy-subrc EQ 3.
                lv_error_txt = 'COMMUNICATION_ERROR'.
              ENDIF.
              IF LINES( <lt_itab_obj>[] ) = 0.
                APPEND INITIAL LINE TO <lt_itab_obj> ASSIGNING <ls_wa>.
                lo_object->fill_result( CHANGING _cs_result = <ls_wa> ).
              ELSE.
                READ TABLE <lt_itab_obj> ASSIGNING <ls_wa> INDEX 1.
              ENDIF.
              CONCATENATE 'KORRNUM_' ls_tmscsys-sysnam INTO lv_fname.
              ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_wa> TO <lv_fname1>.
              CHECK <lv_fname1> IS ASSIGNED.
              <lv_fname1> = lv_error_txt.
              set_color_cell lv_fname '6'.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
      CHECK <lt_itab_obj>[] IS NOT INITIAL.
      LOOP AT <lt_itab_obj> ASSIGNING <ls_wa>.
        IF lv_red = 'X'.
          set_color_cell: 'KEY-PGMID' '6', 'KEY-OBJECT' '6', 'KEY-OBJ_NAME' '6'.
        ELSE.
          set_color_cell: 'KEY-PGMID' '5', 'KEY-OBJECT' '5', 'KEY-OBJ_NAME' '5'.
        ENDIF.
      ENDLOOP.
      APPEND LINES OF <lt_itab_obj>[] TO <lt_itab>[].
    ENDLOOP.

    CHECK history IS NOT INITIAL.

    LOOP AT gt_ref_objects INTO lo_object.
      ls_obj_key = lo_object->get_key( ).
      LOOP AT <lt_itab> ASSIGNING <ls_wa>.
        UNASSIGN <lv_fname1>.
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_wa> TO <lv_fname1>.
        IF <lv_fname1> IS ASSIGNED AND <lv_fname1> = ls_obj_key.
          lv_ind = sy-tabix.
        ENDIF.
      ENDLOOP.
      CHECK lv_ind IS NOT INITIAL.
      DO.
        lv_del = 'X'.
        READ TABLE <lt_itab> ASSIGNING <ls_wa> INDEX lv_ind.
        IF sy-subrc NE 0.
          EXIT.
        ELSE.
          LOOP AT gt_systems INTO lv_system.
            CONCATENATE 'KORRNUM' '_' lv_system INTO lv_fname.
            UNASSIGN <lv_fname1>.
            ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_wa> TO <lv_fname1>.
            CHECK <lv_fname1> IS ASSIGNED.
            READ TABLE gt_sysname_intsys FROM lv_system TRANSPORTING NO FIELDS.
            CHECK sy-subrc EQ 0 AND <lv_fname1> IN requests OR
                  sy-subrc NE 0 AND <lv_fname1> IS NOT INITIAL.
            lv_del = space.
            EXIT.
          ENDLOOP.
          IF lv_del EQ space.
            EXIT.
          ELSE.
            DELETE <lt_itab> INDEX lv_ind.
          ENDIF.
          SUBTRACT 1 FROM lv_ind.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.                    "get_versions
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
    CREATE OBJECT _ro_object
      EXPORTING
        _is_e071 = _is_e071.
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
    DATA: lo_object TYPE REF TO cl_object.

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
        lo_object = _ip_objects_list->find_object( ls_object ).
        CHECK lo_object IS NOT BOUND.
        lo_object = cl_object=>factory( ls_e071 ).
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
FORM f4_help_on_trkorr           USING pv_trkorr LIKE e070-trkorr.
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
FORM f4_help_on_target  USING pv_target LIKE vers_dest-sysname.
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