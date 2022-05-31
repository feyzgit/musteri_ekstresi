*&---------------------------------------------------------------------*
*& Include YFIN_I_ALV_GLOBAL
*&---------------------------------------------------------------------*
CONSTANTS : color_light_blue   LIKE lipov-color VALUE 'C100',
            color_blue         LIKE lipov-color VALUE 'C110',
            color_white        LIKE lipov-color VALUE 'C200',
            color_grey         LIKE lipov-color VALUE 'C210',
            color_light_yellow LIKE lipov-color VALUE 'C300',
            color_yellow       LIKE lipov-color VALUE 'C310',
            color_light_blue2  LIKE lipov-color VALUE 'C400',
            color_blue2        LIKE lipov-color VALUE 'C410',
            color_light_green  LIKE lipov-color VALUE 'C500',
            color_green        LIKE lipov-color VALUE 'C510',
            color_light_red    LIKE lipov-color VALUE 'C600',
            color_red          LIKE lipov-color VALUE 'C610',
            color_light_brown  LIKE lipov-color VALUE 'C700',
            color_brown        LIKE lipov-color VALUE 'C710'.

TYPE-POOLS : icon.
TYPE-POOLS: slis.
TYPE-POOLS : kkblo.

DATA: v_default_recname     TYPE slis_tabname  VALUE 'IT_REPORT',
      v_default_report_name LIKE disvariant-report.

DATA : v_datum LIKE sy-datum,
       v_uzeit LIKE sy-uzeit.

DATA : BEGIN OF ex_events OCCURS 17,
       event(20),
       END OF ex_events.

DATA : BEGIN OF ap_events OCCURS 17,
       event(20),
       END OF ap_events.

DATA : BEGIN OF it_fcatfies OCCURS 0.
        INCLUDE STRUCTURE dfies.
DATA : tb LIKE dfies-tabname,
       fn LIKE dfies-fieldname,
       END OF it_fcatfies.

DATA : lvc_fieldcat TYPE lvc_t_fcat WITH HEADER LINE ,
       lvc_layout TYPE lvc_s_layo .

DATA : it_keyinfo      TYPE kkblo_keyinfo,
       it_sp_groups    TYPE kkblo_t_sp_group     WITH HEADER LINE,
       it_sort         TYPE kkblo_t_sortinfo     WITH HEADER LINE,
       it_extab        TYPE kkblo_t_extab        WITH HEADER LINE,
       it_filter       TYPE kkblo_t_filter,
       v_line           LIKE sy-tabix.

DATA: gs_layout TYPE slis_layout_alv,
      g_exit_caused_by_caller,
      gs_exit_caused_by_user TYPE slis_exit_by_user,
      g_repid LIKE sy-repid,
      gs_grid_set TYPE  lvc_s_glay.

DATA: gt_events      TYPE slis_t_event,
      gt_list_top_of_page TYPE slis_t_listheader.

DATA: lt_fieldcatalog TYPE slis_fieldcat_alv,
      lt_t_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      lt_excluding    TYPE slis_t_extab.
DATA: gs_selfield TYPE slis_selfield.

DATA : color     TYPE kkblo_t_specialcol WITH HEADER LINE.

DATA: gs_variant LIKE disvariant,
      g_save(1) TYPE c  VALUE 'A',
      g_exit(1) TYPE c.
DATA : r_tool(2)   VALUE 'LT'.

DEFINE set_alvcol.
  &1-fieldname = &2.
  &1-color-col = &3.
  &1-color-int = &4.
  &1-color-inv = &5.
  &1-nokeycol = &6.
  append &1.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  LT_F4_FOR_VARIANTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PVARI      text
*----------------------------------------------------------------------*
FORM lt_f4_for_variants USING   pvari.
*
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = gs_variant
      i_save              = g_save
*     it_default_fieldcat =
    IMPORTING
      e_exit              = g_exit
      es_variant          = gs_variant
    EXCEPTIONS
      not_found           = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      pvari = gs_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " lt_f4_for_variants
*---------------------------------------------------------------------*

FORM set_layout  USING pvari drname.
  gs_variant-report    = drname.
  gs_variant-username  = '/GENEL'.
  gs_variant-variant   = pvari.
  gs_variant-text      = pvari.
  PERFORM set_layout_user_exit USING pvari drname.
ENDFORM.                    "SET_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  GET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DTNAME     text
*----------------------------------------------------------------------*
FORM get_fcat USING    dtname.
  DATA xrc LIKE sy-subrc.
  REFRESH it_fcatfies.
  CALL FUNCTION 'GET_FIELDTAB'
    EXPORTING
      langu               = sy-langu
      only                = ' '
      tabname             = dtname
      withtext            = 'X'
    IMPORTING
      rc                  = xrc
    TABLES
      fieldtab            = it_fcatfies
    EXCEPTIONS
      internal_error      = 1
      no_texts_found      = 2
      table_has_no_fields = 3
      table_not_activ     = 4.

ENDFORM.                    " get_fcat

*&---------------------------------------------------------------------*
*&      Form  LAYOUT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RS_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout_init USING rs_layout TYPE slis_layout_alv.
*"Build layout for list display
  rs_layout-detail_popup      = 'X'.
  rs_layout-zebra = 'X'.
ENDFORM.                    "LAYOUT_INIT
*---------------------------------------------------------------------*
*       FORM EVENTTAB_BUILD                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EVENTS                                                     *
*---------------------------------------------------------------------*
FORM eventtab_build USING rt_events TYPE slis_t_event.
*"Registration of events to happen during list display
  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = rt_events.
  READ TABLE rt_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    LOOP AT ex_events.
      DELETE rt_events WHERE name EQ ex_events-event.
    ENDLOOP.

    LOOP AT ap_events.
      CLEAR ls_event.
      READ TABLE rt_events INTO ls_event
                    WITH KEY name = ap_events-event.
      IF sy-subrc NE 0.
        ls_event-name = ap_events-event.
        APPEND ls_event TO rt_events.
      ENDIF.
    ENDLOOP.

    LOOP AT rt_events INTO ls_event.
      CONCATENATE 'SET_' ls_event-name INTO ls_event-form.
      MODIFY rt_events FROM ls_event INDEX sy-tabix.
    ENDLOOP.

  ENDIF.
ENDFORM.                    "EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  exclude_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EX_EVENTS  text
*      -->P_2983   text
*----------------------------------------------------------------------*
FORM exclude_events TABLES   ex_events STRUCTURE ex_events
                    USING    p_event.
  ex_events-event = p_event.
  APPEND ex_events.
ENDFORM.                    " exclude_events
*---------------------------------------------------------------------*
*       FORM APPEND_EVENTS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  AP_EVENTS                                                     *
*  -->  P_EVENT                                                       *
*---------------------------------------------------------------------*
FORM append_events TABLES   ap_events STRUCTURE ap_events
                    USING    p_event.
  ap_events-event = p_event.
  APPEND ap_events.
ENDFORM.                    " exclude_events

*----------------------------------------------------------------------*
FORM show_report_fcat TABLES it_report
               USING    pvari
                        gs_variant
                        default_report_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING pvari default_report_name.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_BACKGROUND_ID         = 'ALV_BACKGROUND'
*     i_buffer_active         = 'X'
      i_bypassing_buffer      = 'X'
      i_callback_program      = default_report_name
*     i_structure_name        = default_tab_name
      i_grid_settings         = gs_grid_set
      is_layout               = gs_layout
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
      it_excluding            = lt_excluding
      it_fieldcat             = lt_t_fieldcatalog[]
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.
    IF g_exit_caused_by_caller = 'X'.
*"  Forced Exit by calling program
*"  <do_something>.
    ELSE.
*"  User left list via F3, F12 or F15
      IF gs_exit_caused_by_user-back = 'X'.       "F3
*"    <do_something>.
      ELSE.
        IF gs_exit_caused_by_user-exit = 'X'.     "F15
*"      <do_something>.
        ELSE.
          IF gs_exit_caused_by_user-cancel = 'X'. "F12
*"        <do_something>.
          ELSE.
*"        should not occur!
*"        <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
  ENDIF.
ENDFORM.                    " show_report_fcat

*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT_FCAT_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_REPORT            text
*      -->PVARI                text
*      -->GS_VARIANT           text
*      -->DEFAULT_REPORT_NAME  text
*      -->DEFAULT_RECNAME      text
*----------------------------------------------------------------------*
FORM show_report_fcat_lvc TABLES it_report
               USING    pvari
                        gs_variant
                        default_report_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING pvari default_report_name.

  DATA : lvc_fieldcat TYPE lvc_t_fcat WITH HEADER LINE ,
         lvc_layout TYPE lvc_s_layo .

  CLEAR : lvc_fieldcat , lvc_fieldcat[] , lvc_layout .

  PERFORM convert_to_lvc USING lt_t_fieldcatalog[] gs_layout
                         CHANGING lvc_fieldcat[] lvc_layout .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer      = 'X'
      i_buffer_active         = 'X'
      i_callback_program      = default_report_name
      i_grid_settings         = gs_grid_set
      is_layout_lvc           = lvc_layout
      it_fieldcat_lvc         = lvc_fieldcat[]
      it_excluding            = lt_excluding
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " show_report_fcat

*&---------------------------------------------------------------------*
*&      Form  SHOW_REPORT_FCAT_LVC2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_REPORT            text
*      -->PVARI                text
*      -->GS_VARIANT           text
*      -->DEFAULT_REPORT_NAME  text
*      -->DEFAULT_RECNAME      text
*----------------------------------------------------------------------*
FORM show_report_fcat_lvc2 TABLES it_report
               USING    pvari
                        gs_variant
                        default_report_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING pvari default_report_name.

  CLEAR : lvc_fieldcat , lvc_fieldcat[] , lvc_layout .

  PERFORM convert_to_lvc USING lt_t_fieldcatalog[] gs_layout
                         CHANGING lvc_fieldcat[] lvc_layout .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer      = 'X'
      i_buffer_active         = 'X'
      i_callback_program      = default_report_name
      i_grid_settings         = gs_grid_set
      is_layout_lvc           = lvc_layout
      it_fieldcat_lvc         = lvc_fieldcat[]
      it_excluding            = lt_excluding
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
      i_screen_start_column   = 20
      i_screen_start_line     = 5
      i_screen_end_column     = 160
      i_screen_end_line       = 25
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " show_report_fcat


*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FIELDNAME  text
*      -->COL        text
*      -->INT        text
*      -->INV        text
*      -->NOKEYCOL   text
*----------------------------------------------------------------------*
FORM set_color USING fieldname col int inv nokeycol.
  CLEAR color.
  REFRESH color.
  set_alvcol : color fieldname col int inv nokeycol.
  "Col de#erleri
  "1 = Mavi
  "2 = A##k mavi
  "3 = Sar#
  "5 = Ye#il
  "6 = K#rm#z#
  "7 = A##k kahverengi
ENDFORM.                " set_color
*&---------------------------------------------------------------------*
*&      Form  set_color_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*----------------------------------------------------------------------*
FORM set_color_table TABLES it_color TYPE kkblo_t_specialcol USING idx.

  DATA : idm TYPE i.

  CHECK it_color IS INITIAL.

  idm = idx MOD 2.

  IF idm EQ 0.
    PERFORM set_color USING '' '1' '0' '0' ' '.
  ELSE.
    PERFORM set_color USING '' '0' '1' '0' ' '.
  ENDIF.
  APPEND LINES OF color TO it_color.

ENDFORM.                    " set_color_table

*----------------------------------------------------------------------*
FORM set_line_field_cat TABLES
                     t_fieldcatalog STRUCTURE  lt_t_fieldcatalog
                        USING  p_tabname TYPE  slis_tabname
                               p_fieldname
                               p_property
                               p_value.

  DATA: v_field  TYPE STANDARD TABLE OF slis_fieldname WITH HEADER LINE,
      v_property TYPE STANDARD TABLE OF slis_fieldname WITH HEADER LINE.


  DATA tabix LIKE sy-tabix.
  FIELD-SYMBOLS: <f1d>.
  SPLIT p_fieldname AT '/' INTO TABLE v_field.
  SPLIT p_property  AT '/' INTO TABLE v_property.
  LOOP AT v_field.
    READ TABLE t_fieldcatalog WITH KEY tabname   = p_tabname
                                       fieldname = v_field.
    tabix = sy-tabix.
    IF sy-subrc = 0.
      LOOP AT v_property.
       ASSIGN COMPONENT v_property OF STRUCTURE t_fieldcatalog TO <f1d>.
        <f1d> = p_value.
        MODIFY t_fieldcatalog INDEX tabix.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_line_field_cat

*&---------------------------------------------------------------------*
*&      Form  convert_to_lvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FV_FIELDCAT      text
*      -->FV_LAYOUT        text
*      -->FV_FIELDCAT_LVC  text
*      -->FV_LAYOUT_LVC    text
*----------------------------------------------------------------------*
FORM convert_to_lvc USING fv_fieldcat TYPE slis_t_fieldcat_alv
                          fv_layout TYPE slis_layout_alv
                    CHANGING fv_fieldcat_lvc TYPE lvc_t_fcat
                             fv_layout_lvc TYPE lvc_s_layo .

  DATA : lv_fieldcat TYPE kkblo_t_fieldcat ,
         lv_layout   TYPE kkblo_layout .

  CLEAR : fv_fieldcat_lvc , fv_layout_lvc .

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
    EXPORTING
      it_fieldcat = fv_fieldcat
      is_layout   = fv_layout
    IMPORTING
      et_fieldcat = lv_fieldcat
      es_layout   = lv_layout.

  CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
    EXPORTING
      it_fieldcat_kkblo = lv_fieldcat
      is_layout_kkblo   = lv_layout
    IMPORTING
      et_fieldcat_lvc   = fv_fieldcat_lvc
      es_layout_lvc     = fv_layout_lvc.

ENDFORM .                    "convert_to_lvc
