*
* Documentation :
* Program name : YFIN_FI010_P_001
* Tcode:
* Description : Müşteri Ektresi
* Author : Emir KARPUZ / emir.karpuz@finpro.com.tr

* Responsible person : Emrullah KAPLAN / emrullah.kaplan@finpro.com.tr

* R/3 Release : ECC 6.0
*=======================================================================
* Program type : Executable Program
* Package : YFIN_FI010
*-----------------------------------------------------------------------
* History of changes
*
* Date         by        <trans.num., CR-number, what has changed >
* ----------- ---------- -----------------------------------------------
* xx.xx.20xx  xxxxx      xxxKxxxxxx
************************************************************************

REPORT yfin_fi010_p001.

INCLUDE YFIN_FI010_P001_I01 .
TABLES: bsid, bsad, kna1, knb1, bkpf,bseg.

DATA: BEGIN OF it_report OCCURS 0,

*        ktokd       LIKE kna1-ktokd,
        kunnr       LIKE kna1-kunnr,
        name1       LIKE kna1-name1,
*  akont like knb1-akont,
        hkont       LIKE bseg-hkont,
        txt50       LIKE skat-txt50,
        budat       LIKE bkpf-budat,
        bldat       LIKE bkpf-bldat,
        vade_t      LIKE bsid-zfbdt,
        stblg       LIKE bkpf-stblg,
        belnr       LIKE bsid-belnr,
        buzei       LIKE bsid-buzei,
        xblnr       LIKE bkpf-xblnr,
        vbelv       LIKE vbfa-vbelv,
        blart       LIKE bkpf-blart,
        ltext       LIKE t003t-ltext,
        umskz       LIKE bsid-umskz,
        borc        LIKE bsid-dmbtr,
        alacak      LIKE bsid-dmbtr,
        devir       LIKE bsid-dmbtr,
        borc_bp     LIKE bsid-wrbtr,
        alacak_pb   LIKE bsid-wrbtr,
        belg_pb     LIKE bsid-waers,
*        satıcı_dm   LIKE bsik-dmbtr,
        sgtxt       LIKE bseg-sgtxt,
        bezei       LIKE tvgrt-bezei,
        bakiye_bpb  LIKE bsid-dmbtr,
        kidno       LIKE bseg-kidno,
        rowcolor(4),

      END OF it_report,
      gs_report LIKE it_report.

DATA: it_report2      LIKE TABLE OF it_report WITH HEADER LINE.
DATA: it_report_duzen LIKE TABLE OF it_report WITH HEADER LINE.

DATA: BEGIN OF it_kna1 OCCURS 0,
        kunnr LIKE kna1-kunnr,
        name1 LIKE kna1-name1,
        akont LIKE knb1-akont,
      END OF it_kna1.


DATA: gv_date LIKE bkpf-budat.
DATA: keybalance LIKE bapi3007_3 OCCURS 0 WITH HEADER LINE.
DATA: lineitems LIKE bapi3007_2 OCCURS 0 WITH HEADER LINE.
DATA: ht_skat LIKE HASHED TABLE OF skat
      WITH UNIQUE KEY saknr
      WITH HEADER LINE.


DATA: it_bsid_devir LIKE bsid OCCURS 0 WITH HEADER LINE.
DATA: it_bsik_devir LIKE bsik OCCURS 0 WITH HEADER LINE.
DATA: it_bsid LIKE bsid OCCURS 0 WITH HEADER LINE.
DATA: it_bsik LIKE bsik OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF lt_bezei OCCURS 0,
         kunnr LIKE knvv-kunnr,
         bezei LIKE tvgrt-bezei,
         name1 LIKE kna1-name1,
       END OF lt_bezei.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002 .

PARAMETERS:

  p_bukrs LIKE t001-bukrs OBLIGATORY DEFAULT '1000',
  p_kunnr LIKE kna1-kunnr OBLIGATORY MEMORY ID kun.

SELECT-OPTIONS:
*                 FOR kna1-kunnr,
                s_budat FOR bkpf-budat OBLIGATORY NO-EXTENSION,
                s_hkont FOR bsid-hkont , "DEFAULT '120*' ,
                s_waers FOR bkpf-waers.

SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-000.
PARAMETERS : p_doviz AS CHECKBOX DEFAULT ' '  USER-COMMAND d.
SELECTION-SCREEN END OF BLOCK b3 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .


*PARAMETERS : p_odk AS CHECKBOX.
*
*SELECT-OPTIONS : s_odk FOR bseg-umskz MODIF ID m1.

PARAMETERS : p_ters AS CHECKBOX DEFAULT 'X',
             p_denk AS CHECKBOX DEFAULT 'X',
             p_sat  AS CHECKBOX DEFAULT 'X'.


SELECTION-SCREEN END OF BLOCK b2 .


AT SELECTION-SCREEN OUTPUT.

  IF s_waers-low IS NOT INITIAL AND s_waers-low NE 'TRY'
                                AND p_doviz EQ 'X'.
    MESSAGE s001(zfi).
    SUBMIT zfi_must_ekstre VIA SELECTION-SCREEN.
  ELSEIF s_waers-low IS INITIAL AND p_doviz EQ 'X'.
    MESSAGE s002(zfi).
  ENDIF.

AT SELECTION-SCREEN.

INITIALIZATION.

  s_budat-high = sy-datum .
  s_budat-low = sy-datum .
  s_budat-low+4(4) = '0101' .
  s_budat-sign = 'I' .
  s_budat-option = 'EQ' .
  APPEND s_budat.

  s_hkont-low = '120*' .
  s_hkont-sign = 'I' .
  s_hkont-option = 'CP' .
  APPEND s_hkont .
  s_hkont-low = '320*' .
  APPEND s_hkont .


START-OF-SELECTION.

  IF s_budat-high IS INITIAL.

    MESSAGE 'Ekstre başlangıç ve bitiş tarihi boş olamaz' TYPE 'E' .

  ENDIF.

  PERFORM get_kunnr.

  IF p_doviz NE 'X'.
    PERFORM get_data .
    PERFORM process_data.
  ELSE.
    DATA: lx_waers TYPE waers.
    lx_waers = s_waers-low.
    CLEAR: s_waers,s_waers[].
    PERFORM get_data .
    PERFORM process_data.
    it_report2[] = it_report[].

    CLEAR: it_bsid_devir,it_bsid,it_bsik_devir,it_bsik,it_report.
    CLEAR: it_bsid_devir[],it_bsid[],
           it_bsik_devir[],it_bsik[],it_report[].

    s_waers-low = lx_waers.
    s_waers-sign = 'I' .
    s_waers-option = 'EQ' .
    APPEND s_waers.

    PERFORM get_data .
    PERFORM process_data.

  ENDIF.



*  LOOP AT it_kna1.
*
*    IF p_odk IS NOT INITIAL.
*
*  PERFORM odkli.
*
*    ELSE.
*
*      PERFORM odksiz.
*
*    ENDIF.
*
*  ENDLOOP.




  CLEAR : lt_t_fieldcatalog , lt_t_fieldcatalog[] .
  v_default_recname = 'IT_REPORT' .
  v_default_report_name = sy-repid .
  PERFORM set_report_fcat.
  PERFORM show_report_fcat TABLES it_report
                      USING  '' "P_VARI
                             gs_variant
                             v_default_report_name
                             v_default_recname.


  """" ALV FORMS HERE """"
*&---------------------------------------------------------------------*
*&      Form  SET_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_top_of_page.
  CLEAR : gt_list_top_of_page[] , gt_list_top_of_page .
  PERFORM comment_build USING gt_list_top_of_page[].
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "set_top_of_page
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.

  DATA : tarih1(10) TYPE c.
  DATA : tarih2(10) TYPE c.
  DATA : tarih3(25) TYPE c.

  CLEAR: tarih1, tarih2, tarih3.

  WRITE s_budat-low TO tarih1 DD/MM/YYYY.
  WRITE s_budat-high TO tarih2 DD/MM/YYYY.

  CONCATENATE tarih1 '-' tarih2 INTO tarih3.

  CLEAR : ls_line .
  ls_line-typ = 'S' .
  ls_line-key = 'Şirket Kodu :' .
  ls_line-info = p_bukrs .
  APPEND ls_line TO lt_top_of_page.

  CLEAR : ls_line .
  ls_line-typ = 'S' .
  ls_line-key = 'Ekstre Dönemi :' .
  ls_line-info = tarih3 .
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    "comment_build


*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .   "#EC CALLED
  PERFORM set_excluding_tab TABLES rt_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab[].
ENDFORM.                    "f01_set_status
*&---------------------------------------------------------------------*
*&      Form  excluding_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluding_events.

  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT'.
*  PERFORM exclude_events TABLES ex_events USING 'USER_COMMAND'.
*  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE'.
*  PERFORM exclude_events TABLES ex_events USING 'PF_STATUS_SET'.
  PERFORM exclude_events TABLES ex_events USING 'LIST_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'AFTER_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'BEFORE_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'REPREP_SEL_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'SUBTOTAL_TEXT'.
  PERFORM exclude_events TABLES ex_events USING 'GROUPLEVEL_CHANGE'.

*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'DATA_CHANGED'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'ITEM_DATA_EXPAND'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'GROUPLEVEL_CHANGE'.
ENDFORM.                    " excluding_events

*&---------------------------------------------------------------------*
*&      Form  SET_EXCLUDING_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EXTAB      text
*----------------------------------------------------------------------*
FORM set_excluding_tab TABLES extab.
  REFRESH extab.
*  EXTAB = '&ABC'.      APPEND EXTAB.
*  extab = '&UMC'.      append extab.
*  extab = '%SL' .      append extab.
*  extab = '&SUM'.      append extab.
*  extab = '&OL0'.      append extab.
*  extab = '&OAD'.      append extab.
*  extab = '&AVE'.      append extab.
*  extab = '&ILT'.      append extab.
*  extab = '&ETA'.      append extab.
*  extab = '%PC' .      append extab.
*  extab = '&ALL'.      append extab.
*  extab = '&SAL'.      append extab.
*  EXTAB = '&EB9'.      APPEND EXTAB.
*  EXTAB = '&REFRESH'.  APPEND EXTAB.
*  extab = '&OUP'.      append extab.
*  extab = '&ODN'.      append extab.
*  extab = '&RNT_PREV'. append extab.
*  extab = '&VEXCEL'.   append extab.
*  extab = '&AOW'.      append extab.
*  EXTAB = '&GRAPH'.    APPEND EXTAB.
*  EXTAB = '&INFO'.     APPEND EXTAB.
*  EXTAB = '&DET'.     APPEND EXTAB.
*if c_sip is INITIAL .
*  EXTAB = '&ESLE'. APPEND EXTAB.
*endif .

ENDFORM.                    " set_excluding_tab

*&---------------------------------------------------------------------*
*&      Form  SET_REPORT_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_report_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = v_default_report_name
      i_internal_tabname     = v_default_recname
      i_inclname             = v_default_report_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_t_fieldcatalog[]
    EXCEPTIONS
      OTHERS                 = 3.

  PERFORM set_field_cat_user_exit.

ENDFORM.                    " set_report_fcat

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CAT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_cat_user_exit .
  DATA: recname TYPE slis_tabname.
  DATA : v_title(42) TYPE c.
  DATA : lv_title(42) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC' TO v_title.
  recname = v_default_recname .

*  CONCATENATE keydate+6(2) '.' keydate+4(2) '.'
*               keydate(4) ' Bakiyesi' INTO lv_title.

*        netwr LIKE vbak-netwr,      " bağlantı tutarı
*        tahtut LIKE bsid-dmbtr,     " tahsilat tutarı
*        bakiye LIKE bsid-dmbtr,     " cari hesap bakiyesi
*        fattut LIKE bsid-dmbtr,     " Fatura tutarı bağlantılı
*        denfat LIKE bsid-dmbtr,     " Denkleşen faturalar  bağlantılı
*        acikfat LIKE bsid-dmbtr,    " açık faturaları bağlantılı
*        denktah LIKE bsid-dmbtr,    " denkl. tahsilatlar
*        aciktah LIKE bsid-dmbtr,    " açık tahsilatlar

  PERFORM

   set_line_field_cat TABLES lt_t_fieldcatalog USING :
          recname 'BORC'      v_title 'BORÇ UPB' ,
          recname 'BORC_BP'   v_title 'BORÇ BPB' ,
          recname 'BEZEI'   v_title 'Satış Grubu' ,
          recname 'VADE_T'   v_title 'Vade Tarihi' ,
          recname 'SATICI_DM'   v_title 'SATICI BAKİYE' ,
          recname 'ALACAK'    v_title 'ALACAK UPB' ,
          recname 'ALACAK_PB' v_title 'ALACAK BPB' ,
          recname 'DEVIR'     v_title 'BAKİYE UPB' ,
          recname '3301'      v_title '3301' ,
          recname 'N04' v_title 'SSK İşv Payı Sakatlık Haz',
          recname 'NER' v_title 'SSK işveren payı',
          recname 'IER' v_title 'ISY İşveren kesintisi',
          recname 'SGTXT' v_title 'AÇIKLAMA' ,
          recname 'VBELV' v_title 'Satış Belgesi' ,
          recname 'ICON'  v_title 'Durum' ,
          recname 'ICON' 'ICON' 'X',
          recname 'BAKIYE_BPB'   v_title 'BAKIYE BPB' ,
          recname 'VBELV'  'HOTSPOT' 'X' ,
          recname 'VBELV'  v_title 'M.Sipariş Numarası' .
  .
*          recname 'Chbak' 'EMPHASIZE' color_light_green.

  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'ROWCOLOR' .
  DELETE lt_t_fieldcatalog WHERE fieldname EQ 'STBLG'.

*if C_SIP is not initial .
*  DELETE lt_t_fieldcatalog WHERE fieldname eq 'LICENSE_NUM' or
*                                 fieldname eq 'CHASSIS_NUM' or
*                                 fieldname eq 'EQUNR' .
*endif .

*                               fieldname eq 'SAYAC' OR
*                               fieldname eq 'KALEM_TIPI' OR
*                               fieldname eq 'MATRAH' or
*                               fieldname eq 'ROWCOLOR' .

ENDFORM.                    " set_field_cat_user_exit

*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_USER_EXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PVARI    text
*      -->P_DRNAME   text
*----------------------------------------------------------------------*
FORM set_layout_user_exit USING    p_pvari
                                   p_drname.

*  GS_GRID_SET-EDT_CLL_CB = 'X'.

*  GS_LAYOUT-GET_SELINFOS       = 'X'.
*  GS_LAYOUT-COLTAB_FIELDNAME   = 'COLOR'.
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.

  gs_layout-zebra = 'X' .
  gs_layout-colwidth_optimize = 'X' .
  IF v_default_recname EQ 'IT_REPORT' .
    gs_layout-info_fieldname = 'ROWCOLOR'.
*  GS_LAYOUT-BOX_FIELDNAME = 'SELKZ'.
  ELSE .
    CLEAR : gs_layout-edit , gs_layout-info_fieldname ,
            gs_layout-box_fieldname .
  ENDIF .

ENDFORM.                    " set_layout_user_exit

*FORM SHOW_REPORT_FCAT_POP TABLES IT_REPORT
*               USING    PVARI
*                        GS_VARIANT
*                        DEFAULT_REPORT_NAME
*                        DEFAULT_RECNAME.
*  PERFORM LAYOUT_INIT USING GS_LAYOUT.
*  PERFORM EXCLUDING_EVENTS.
*  PERFORM EVENTTAB_BUILD USING GT_EVENTS[].
*  PERFORM SET_LAYOUT USING PVARI DEFAULT_REPORT_NAME.
*
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
**      I_BACKGROUND_ID    = 'ALV_BACKGROUND'
**        i_buffer_active    = 'X'
*         I_BYPASSING_BUFFER = 'X'
*         I_CALLBACK_PROGRAM = DEFAULT_REPORT_NAME
**         i_structure_name   = default_tab_name
*         I_GRID_SETTINGS    = GS_GRID_SET
*         IS_LAYOUT          = GS_LAYOUT
*         I_SAVE             = G_SAVE
*         IS_VARIANT         = GS_VARIANT
*         IT_EVENTS          = GT_EVENTS[]
*         IT_EXCLUDING = LT_EXCLUDING
*         IT_FIELDCAT        = LT_T_FIELDCATALOG[]
*         I_SCREEN_START_COLUMN = 5
*         I_SCREEN_START_LINE   = 2
*         I_SCREEN_END_COLUMN   = 150
*         I_SCREEN_END_LINE     = 18
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER = G_EXIT_CAUSED_BY_CALLER
*         ES_EXIT_CAUSED_BY_USER  = GS_EXIT_CAUSED_BY_USER
*    TABLES
*         T_OUTTAB = IT_REPORT[]
*    EXCEPTIONS
*         PROGRAM_ERROR = 1
*         OTHERS        = 2.
*ENDFORM.                    " show_report_fcat



FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED
  DATA : lv_baslik TYPE c LENGTH 100.
  DATA : lv_body TYPE c LENGTH 100.
  DATA : ans TYPE c .
  CASE r_ucomm .

    WHEN '&IC1' .

      IF rs_selfield-sel_tab_field EQ 'IT_REPORT-VBELV'.
        READ TABLE it_report INDEX rs_selfield-tabindex .
        IF it_report-vbelv(1) NE 'G'.


          IF sy-subrc IS INITIAL .
            SET PARAMETER ID 'AUN' FIELD it_report-vbelv .
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN .
          ENDIF .

        ELSE.

          READ TABLE it_report INTO gs_report INDEX rs_selfield-tabindex
          .

          SELECT vbelv FROM vbfa
          INTO TABLE @DATA(lt_siparis)
          WHERE vbfa~vbeln EQ @gs_report-kidno(10)
            AND vbfa~vbtyp_n EQ 'M'
            AND vbfa~vbtyp_v EQ 'C'.

          CONCATENATE gs_report-kidno(10)
          'Faturasına İilişkin Sipariş Numaraları'
          INTO lv_baslik.
          LOOP AT lt_siparis INTO DATA(ls_siparis).
            IF sy-tabix EQ 1.
              lv_body =  ls_siparis-vbelv.
            ELSE.
              CONCATENATE lv_body '/' ls_siparis-vbelv INTO lv_body.
            ENDIF.


          ENDLOOP.


          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = lv_baslik
              text_question         = lv_body
              text_button_1         = 'OK'
              icon_button_1         = 'ICON_CHECKED'
              text_button_2         = 'CANCEL'
              icon_button_2         = 'ICON_CANCEL'
              display_cancel_button = ' '
              popup_type            = 'ICON_MESSAGE_INFORMATION'
            IMPORTING
              answer                = ans.

          IF ans EQ 3.
            LEAVE PROGRAM.

          ENDIF.

        ENDIF.
      ELSE.
        CLEAR it_report .
        READ TABLE it_report INDEX rs_selfield-tabindex .
        IF sy-subrc IS INITIAL .


          SET PARAMETER ID 'BUK' FIELD p_bukrs .
          SET PARAMETER ID 'BLN' FIELD it_report-belnr .
          SET PARAMETER ID 'GJR' FIELD it_report-bldat(4) .

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
        ENDIF .
      ENDIF .
      .
    WHEN 'PRINT'.

      PERFORM yazdir.

  ENDCASE.
ENDFORM.                    "set_user_command
*&---------------------------------------------------------------------*
*&      Form  GET_KUNNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_kunnr .

  gv_date = s_budat-low - 1.

*  SELECT kna1~kunnr kna1~name1 knb1~akont
*    INTO CORRESPONDING FIELDS OF TABLE it_kna1
*
*     FROM kna1 JOIN knb1 ON kna1~kunnr EQ knb1~kunnr
*    WHERE kna1~kunnr IN s_kunnr AND
*          knb1~bukrs EQ p_bukrs. "AND
**          kna1~ktokd IN s_ktokd.

  SELECT * FROM skat INTO TABLE ht_skat
    WHERE ktopl EQ '1000' AND
          spras EQ sy-langu.



  SELECT a~kunnr
         b~bezei
         c~name1
    FROM knvv  AS a
    INNER JOIN tvgrt AS b ON a~vkgrp EQ b~vkgrp
    JOIN kna1 AS c ON  a~kunnr EQ c~kunnr
    INTO CORRESPONDING FIELDS OF TABLE lt_bezei
    WHERE a~kunnr EQ p_kunnr
      AND b~spras EQ sy-langu.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ODKLI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM odkli .
*  DATA : lv_vadetarih TYPE datum.
*
*  DATA : lv_dmbtr TYPE dmbtr.
*  DATA : BEGIN OF lt_bsik OCCURS 0,
*           lifnr LIKE bsik-lifnr,
*           shkzg LIKE bsik-shkzg,
*           dmbtr LIKE bsik-dmbtr,
*         END OF lt_bsik .
*
*  DATA : BEGIN OF lt_collect OCCURS 0,
*           lifnr LIKE bsik-lifnr,
*           dmbtr LIKE bsik-dmbtr,
*         END OF lt_collect.





*  CLEAR: keybalance, keybalance[].
*  CLEAR: it_report.
*
*  CALL FUNCTION 'BAPI_AR_ACC_GETKEYDATEBALANCE'
*    EXPORTING
*      companycode  = p_bukrs
*      customer     = it_kna1-kunnr
*      keydate      = gv_date
*      balancespgli = 'X'
**     NOTEDITEMS   = ' '
** IMPORTING
**     RETURN       =
*    TABLES
*      keybalance   = keybalance[].
*
*  it_report-kunnr = it_kna1-kunnr.
*  it_report-name1 = it_kna1-name1.
**      it_report-akont = it_kna1-akont.
*  it_report-rowcolor = color_light_red.
*
*  it_report-sgtxt = 'DEVİR BAKİYESİ'.
*
*  LOOP AT keybalance WHERE sp_gl_ind IN s_odk AND
*                           currency IN s_waers.
*
*    ADD keybalance-lc_bal TO it_report-devir.
*
*  ENDLOOP.
*
*  APPEND it_report.
*
*  CLEAR : lineitems, lineitems[].
*  CLEAR: it_report-rowcolor.
*
*  CALL FUNCTION 'BAPI_AR_ACC_GETSTATEMENT'
*    EXPORTING
*      companycode = p_bukrs
*      customer    = it_kna1-kunnr
*      date_from   = s_budat-low
*      date_to     = s_budat-high
**     NOTEDITEMS  = ' '
** IMPORTING
**     RETURN      =
*    TABLES
*      lineitems   = lineitems[].
*
*
*  SORT lineitems BY pstng_date doc_date doc_no.
*
*  IF p_denk IS NOT INITIAL.
*    DELETE lineitems WHERE doc_type EQ 'D1' .
*    DELETE lineitems WHERE doc_type EQ 'D2' .
*  ENDIF.
*
*  CLEAR it_report-sgtxt.
*
*  LOOP AT lineitems WHERE sp_gl_ind IN s_odk AND
*                          currency IN s_waers.
*
*
*    it_report-budat = lineitems-pstng_date.
*    it_report-bldat = lineitems-doc_date.
*    it_report-belnr = lineitems-doc_no.
*    it_report-buzei = lineitems-item_num.
*    it_report-blart = lineitems-doc_type.
*    it_report-sgtxt = lineitems-item_text.
*    it_report-umskz = lineitems-sp_gl_ind.
*
*    SELECT SINGLE ltext FROM t003t
*      INTO it_report-ltext
*      WHERE blart EQ it_report-blart
*        AND spras EQ 'T'.
*
*    CLEAR lv_vadetarih.
*    lv_vadetarih = lineitems-bline_date + lineitems-dsct_days1
*                                        + lineitems-dsct_days2.
*
*    IF  lv_vadetarih IS NOT INITIAL.
*      it_report-vade_t = lv_vadetarih.
*    ENDIF.
*
*    CLEAR: it_report-stblg, it_report-xblnr.
*    SELECT SINGLE xblnr stblg FROM bkpf
*      INTO (it_report-xblnr,it_report-stblg)
*      WHERE bukrs EQ p_bukrs AND
*            belnr EQ it_report-belnr AND
*            gjahr EQ lineitems-fisc_year.
*
*    IF p_ters IS NOT INITIAL AND
*       it_report-stblg IS NOT INITIAL.
*      CONTINUE.
*
*    ENDIF.
*
*    CLEAR: it_report-alacak, it_report-borc,
*           it_report-alacak_pb , it_report-borc_bp.
*
*    CASE lineitems-db_cr_ind.
*      WHEN 'S'.
*
*        it_report-borc = lineitems-lc_amount.
*        it_report-borc_bp = lineitems-amt_doccur.
*        it_report-devir = it_report-devir + it_report-borc.
*
*      WHEN 'H'.
*
*        it_report-alacak = lineitems-lc_amount.
*        it_report-alacak_pb = lineitems-amt_doccur.
*        it_report-devir = it_report-devir - it_report-alacak.
*
*    ENDCASE.
*
*    it_report-belg_pb = lineitems-currency.
*
*    SELECT SINGLE hkont FROM bseg INTO it_report-hkont
*      WHERE bukrs EQ p_bukrs AND
*            belnr EQ it_report-belnr AND
*            buzei EQ it_report-buzei AND
*            gjahr EQ lineitems-fisc_year.
*
*    IF it_report-hkont IS NOT INITIAL.
*      READ TABLE ht_skat WITH KEY saknr = it_report-hkont.
*
*      CHECK sy-subrc IS INITIAL.
*
*      it_report-txt50 = ht_skat-txt50.
*
*    ENDIF.
*    READ TABLE lt_collect WITH KEY lifnr = it_report-kunnr.
*    IF sy-subrc IS INITIAL.
*      lv_dmbtr = lt_collect-dmbtr.
*    ENDIF.
*
*    READ TABLE lt_bezei WITH KEY kunnr = it_report-kunnr.
*    IF  sy-subrc IS INITIAL.
*      it_report-bezei = lt_bezei-bezei.
*    ENDIF.
*    APPEND it_report.
*
*  ENDLOOP.
*
*
*
*ENDFORM.
*&-------------------------------------------------------------------*
*&      Form  ODKSIZ
*&-------------------------------------------------------------------*
*       text
*--------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*--------------------------------------------------------------------*
*FORM odksiz .
*
*  DATA : lv_vadetarih TYPE datum.
*  DATA : BEGIN OF lt_bsik OCCURS 0,
*           lifnr LIKE bsik-lifnr,
*           shkzg LIKE bsik-shkzg,
*           dmbtr LIKE bsik-dmbtr,
*         END OF lt_bsik .
*
*  DATA : BEGIN OF lt_collect OCCURS 0,
*           lifnr LIKE bsik-lifnr,
*           dmbtr LIKE bsik-dmbtr,
*         END OF lt_collect.
*
*  DATA : BEGIN OF lt_bezei OCCURS 0,
*           kunnr LIKE knvv-kunnr,
*           bezei LIKE tvgrt-bezei,
*         END OF lt_bezei.
*
*  SELECT a~kunnr
*         b~bezei
*    FROM knvv  AS a
*    INNER JOIN tvgrt AS b ON a~vkgrp EQ b~vkgrp
*    INTO CORRESPONDING FIELDS OF TABLE lt_bezei
*    WHERE a~kunnr IN s_kunnr
*      AND b~spras EQ 'TR'.
*
*  SELECT lifnr
*         shkzg
*         dmbtr
*    FROM bsik
*    INTO TABLE lt_bsik
*    WHERE lifnr IN s_kunnr
*      AND umskz EQ space
*      AND budat IN s_budat
*      AND bukrs EQ p_bukrs.
*
*  LOOP AT lt_bsik.
*    IF lt_bsik-shkzg EQ 'H'.
*      lt_bsik-dmbtr = lt_bsik-dmbtr * -1.
*    ENDIF.
*    lt_collect-lifnr = lt_bsik-lifnr.
*    lt_collect-dmbtr = lt_bsik-dmbtr.
*    COLLECT lt_collect.
*  ENDLOOP.
*
*
*
*
*
*
*
*  CLEAR: keybalance, keybalance[].
*  CLEAR: it_report.
*
*  CALL FUNCTION 'BAPI_AR_ACC_GETKEYDATEBALANCE'
*    EXPORTING
*      companycode  = p_bukrs
*      customer     = it_kna1-kunnr
*      keydate      = gv_date
*      balancespgli = 'X'
**     NOTEDITEMS   = ' '
** IMPORTING
**     RETURN       =
*    TABLES
*      keybalance   = keybalance[].
*
*  it_report-kunnr = it_kna1-kunnr.
*  it_report-name1 = it_kna1-name1.
**      it_report-akont = it_kna1-akont.
*  it_report-rowcolor = color_light_red.
*
*  it_report-sgtxt = 'DEVİR BAKİYESİ'.
*
*  LOOP AT keybalance WHERE sp_gl_ind EQ space.
*
*    ADD keybalance-lc_bal TO it_report-devir.
*
*  ENDLOOP.
*
*  APPEND it_report.
*
*  CLEAR : lineitems, lineitems[].
*  CLEAR: it_report-rowcolor.
*
*  CALL FUNCTION 'BAPI_AR_ACC_GETSTATEMENT'
*    EXPORTING
*      companycode = p_bukrs
*      customer    = it_kna1-kunnr
*      date_from   = s_budat-low
*      date_to     = s_budat-high
**     NOTEDITEMS  = ' '
** IMPORTING
**     RETURN      =
*    TABLES
*      lineitems   = lineitems[].
*
*  SORT lineitems BY pstng_date doc_date doc_no.
*
*  CLEAR it_report-sgtxt.
*
*  IF p_denk IS NOT INITIAL.
*    DELETE lineitems WHERE doc_type EQ 'AB' .
*  ENDIF.
*
*  LOOP AT lineitems WHERE sp_gl_ind EQ space.
*
*
*    it_report-budat = lineitems-pstng_date.
*    it_report-bldat = lineitems-doc_date.
*    it_report-belnr = lineitems-doc_no.
*    it_report-buzei = lineitems-item_num.
*    it_report-blart = lineitems-doc_type.
*    it_report-sgtxt = lineitems-item_text.
*
*    SELECT SINGLE ltext FROM t003t
*      INTO it_report-ltext
*      WHERE blart EQ it_report-blart
*        AND spras EQ 'T'.
*
*    CLEAR lv_vadetarih.
*    lv_vadetarih = lineitems-bline_date + lineitems-dsct_days1
*                                        + lineitems-dsct_days2.
*
*    IF  lv_vadetarih IS NOT INITIAL.
*      it_report-vade_t = lv_vadetarih.
*    ENDIF.
*
*    CLEAR: it_report-stblg, it_report-xblnr.
*    SELECT SINGLE xblnr stblg FROM bkpf
*      INTO (it_report-xblnr,it_report-stblg)
*      WHERE bukrs EQ p_bukrs AND
*            belnr EQ it_report-belnr AND
*            gjahr EQ lineitems-fisc_year.
*
*    IF p_ters IS NOT INITIAL AND
*       it_report-stblg IS NOT INITIAL.
*      CONTINUE.
*
*    ENDIF.
*
*    CLEAR: it_report-alacak, it_report-borc,
*           it_report-alacak_pb , it_report-borc_bp.
*    CASE lineitems-db_cr_ind.
*      WHEN 'S'.
*
*        it_report-borc    = lineitems-lc_amount.
*        it_report-borc_bp = lineitems-amt_doccur.
*        it_report-devir   = it_report-devir + it_report-borc.
*
*      WHEN 'H'.
*
*        it_report-alacak    = lineitems-lc_amount.
*        it_report-alacak_pb = lineitems-amt_doccur.
*        it_report-devir     = it_report-devir - it_report-alacak.
*
*    ENDCASE.
*    it_report-belg_pb = lineitems-currency.
*
*    SELECT SINGLE hkont FROM bseg INTO it_report-hkont
*      WHERE bukrs EQ p_bukrs AND
*            belnr EQ it_report-belnr AND
*            buzei EQ it_report-buzei AND
*            gjahr EQ lineitems-fisc_year.
*
*    IF it_report-hkont IS NOT INITIAL.
*      READ TABLE ht_skat WITH KEY saknr = it_report-hkont.
*
*      CHECK sy-subrc IS INITIAL.
*
*      it_report-txt50 = ht_skat-txt50.
*
*    ENDIF.
*
*    READ TABLE lt_collect WITH KEY lifnr = it_report-kunnr.
*    IF sy-subrc IS INITIAL.
*      it_report-satici_dm = lt_collect-dmbtr.
*    ENDIF.
*
*    READ TABLE lt_bezei WITH KEY kunnr = it_report-kunnr.
*    IF sy-subrc IS INITIAL.
*      it_report-bezei = lt_bezei-bezei.
*    ENDIF.
*
*
*    APPEND it_report.
*
*  ENDLOOP.



*ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .


  SELECT * FROM bsad
    INTO CORRESPONDING FIELDS OF TABLE it_bsid_devir
    WHERE kunnr EQ p_kunnr     AND
          bukrs EQ p_bukrs     AND
          budat LT s_budat-low AND
          augdt GE s_budat-low AND
          hkont IN s_hkont     AND
          waers IN s_waers.

  SELECT * FROM bsid
    APPENDING CORRESPONDING FIELDS OF TABLE it_bsid_devir
    WHERE kunnr EQ p_kunnr     AND
          bukrs EQ p_bukrs     AND
          budat LT s_budat-low AND
          hkont IN s_hkont     AND
          waers IN s_waers
          .


  SELECT * FROM bsad
    INTO CORRESPONDING FIELDS OF TABLE it_bsid
    WHERE kunnr EQ p_kunnr AND
          bukrs EQ p_bukrs AND
          budat IN s_budat AND
          hkont IN s_hkont AND "14.02.2019 ekaplan
          waers IN s_waers.

  SELECT * FROM bsid
    APPENDING CORRESPONDING FIELDS OF TABLE it_bsid
    WHERE kunnr EQ p_kunnr AND
          bukrs EQ p_bukrs AND
          budat IN s_budat AND
          hkont IN s_hkont AND
          waers IN s_waers
          .

  IF p_sat IS NOT INITIAL.

    SELECT * FROM bsak
    INTO CORRESPONDING FIELDS OF TABLE it_bsik_devir
    WHERE lifnr EQ p_kunnr AND
          bukrs EQ p_bukrs AND
          budat LT s_budat-low AND
          augdt GE s_budat-low AND
          hkont IN s_hkont AND
          waers IN s_waers.

    SELECT * FROM bsik
      APPENDING CORRESPONDING FIELDS OF TABLE it_bsik_devir
      WHERE lifnr EQ p_kunnr AND
            bukrs EQ p_bukrs AND
            budat LT s_budat-low AND
            hkont IN s_hkont AND
            waers IN s_waers
            .


    SELECT * FROM bsak
      INTO CORRESPONDING FIELDS OF TABLE it_bsik
      WHERE lifnr EQ p_kunnr AND
            bukrs EQ p_bukrs AND
            budat IN s_budat AND
            hkont IN s_hkont AND "14.02.2019 ekaplan
            waers IN s_waers.

    SELECT * FROM bsik
      APPENDING CORRESPONDING FIELDS OF TABLE it_bsik
      WHERE lifnr EQ p_kunnr AND
            bukrs EQ p_bukrs AND
            budat IN s_budat AND
            hkont IN s_hkont AND
            waers IN s_waers
            .

  ENDIF.

  IF p_denk IS NOT INITIAL.

    DELETE it_bsid WHERE blart EQ 'D1'.
    DELETE it_bsid WHERE blart EQ 'D2'.
    DELETE it_bsik WHERE blart EQ 'D1'.
    DELETE it_bsik WHERE blart EQ 'D2'.

    LOOP AT it_bsid WHERE blart EQ 'AB'.
      IF it_bsid-augbl = it_bsid-belnr.
*        DELETE it_bsid INDEX sy-tabix.
        DELETE it_bsid WHERE belnr EQ it_bsid-belnr.
      ELSE.
        READ TABLE it_bsik WITH KEY augbl = it_bsid-belnr.
        IF sy-subrc IS INITIAL.
          DELETE it_bsid WHERE belnr EQ it_bsid-belnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT it_bsik WHERE blart EQ 'AB'.
      IF it_bsik-augbl = it_bsik-belnr.
*        DELETE it_bsik INDEX sy-tabix.
        DELETE it_bsik WHERE belnr EQ it_bsik-belnr.
      ELSE.
        READ TABLE it_bsid WITH KEY augbl = it_bsik-belnr.
        IF sy-subrc IS INITIAL.
          DELETE it_bsik WHERE belnr EQ it_bsik-belnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data .

  DATA: lv_wrbtr_usd TYPE bsid-wrbtr,
        lv_wrbtr_try TYPE bsid-wrbtr,
        lv_wrbtr_eur TYPE bsid-wrbtr,
        lv_control   TYPE c.

  DATA: BEGIN OF lt_bkpf OCCURS 0,
          bukrs     LIKE bkpf-bukrs,
          belnr     LIKE bkpf-belnr,
          gjahr     LIKE bkpf-gjahr,
          budat     LIKE bkpf-budat,
          awref_rev LIKE bkpf-awref_rev,
        END OF lt_bkpf.

  DATA : lv_count TYPE i.

  LOOP AT it_bsid_devir.
    IF it_bsid_devir-shkzg = 'H'.
      MULTIPLY it_bsid_devir-dmbtr BY -1 .
      MULTIPLY it_bsid_devir-wrbtr BY -1 .
    ENDIF.
    ADD it_bsid_devir-dmbtr TO it_report-devir .
    ""haltundal 22.02.19
    IF it_bsid_devir-waers = 'TRY'.
      ADD it_bsid_devir-wrbtr TO lv_wrbtr_try.
    ELSEIF it_bsid_devir-waers = 'EUR'.
      ADD it_bsid_devir-wrbtr TO lv_wrbtr_eur.
    ELSEIF it_bsid_devir-waers = 'USD'.
      ADD it_bsid_devir-wrbtr TO lv_wrbtr_usd.
    ENDIF.
    ""haltundal 22.02.19
  ENDLOOP.

  IF p_sat IS NOT INITIAL AND
     it_bsik_devir[] IS NOT INITIAL.
    LOOP AT it_bsik_devir.
      IF it_bsik_devir-shkzg = 'H'.
        MULTIPLY it_bsik_devir-dmbtr BY -1 .
        MULTIPLY it_bsik_devir-wrbtr BY -1 .
      ENDIF.
      ADD it_bsik_devir-dmbtr TO it_report-devir .
      ""haltundal 22.02.19
      IF it_bsik_devir-waers = 'TRY'.
        ADD it_bsik_devir-wrbtr TO lv_wrbtr_try.
      ELSEIF it_bsik_devir-waers = 'EUR'.
        ADD it_bsik_devir-wrbtr TO lv_wrbtr_eur.
      ELSEIF it_bsik_devir-waers = 'USD'.
        ADD it_bsik_devir-wrbtr TO lv_wrbtr_usd.
      ENDIF.
      ""haltundal 22.02.19
    ENDLOOP.
  ENDIF.
  """"haltundal 22.02.19 Emrullah FI
  IF it_report-devir > 0.
    it_report-borc   = it_report-devir.
  ELSE.
    it_report-alacak = it_report-devir.
  ENDIF.

  """"haltundal 22.02.19 Emrullah FI
  it_report-kunnr = it_kna1-kunnr.
  it_report-name1 = it_kna1-name1.
*      it_report-akont = it_kna1-akont.
  it_report-rowcolor = color_light_red.

  it_report-sgtxt = 'DEVİR BAKİYESİ'.

  APPEND it_report.
  CLEAR : it_report-borc   ,
          it_report-alacak .
  ""haltundal 22.02.19
  IF s_waers[] IS INITIAL.
    IF lv_wrbtr_try NE 0.
      it_report-bakiye_bpb = lv_wrbtr_try.
      it_report-belg_pb = 'TRY'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
      """""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_report-borc_bp , it_report-alacak_pb.
      """""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.
    IF lv_wrbtr_eur NE 0.
      it_report-bakiye_bpb = lv_wrbtr_eur.
      it_report-belg_pb = 'EUR'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
      """""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_report-borc_bp , it_report-alacak_pb.
      """""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.
    IF lv_wrbtr_usd NE 0.
      it_report-bakiye_bpb = lv_wrbtr_usd.
      it_report-belg_pb = 'USD'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
      """""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_report-borc_bp , it_report-alacak_pb.
      """""""""""""""""""""""""""""""""""""""""""""""""""
    ENDIF.
  ELSE.

    CLEAR: it_report-borc_bp , it_report-alacak_pb.

    IF line_exists( s_waers[ low = 'TRY' ]  ).
      it_report-bakiye_bpb = lv_wrbtr_try.
      it_report-belg_pb = 'TRY'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
    ENDIF.
    IF line_exists( s_waers[ low = 'EUR' ] ).
      it_report-bakiye_bpb = lv_wrbtr_eur.
      it_report-belg_pb = 'EUR'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
    ENDIF.
    IF line_exists( s_waers[ low = 'USD' ] ).
      it_report-bakiye_bpb = lv_wrbtr_usd.
      it_report-belg_pb = 'USD'.
      IF it_report-bakiye_bpb > 0.
        it_report-borc_bp   = it_report-bakiye_bpb.
      ELSE.
        it_report-alacak_pb = it_report-bakiye_bpb.
      ENDIF.
      APPEND it_report.
    ENDIF.
  ENDIF.
  ""haltundal 22.02.19
  CLEAR: it_report-rowcolor.

  IF p_sat IS NOT INITIAL.
    LOOP AT it_bsik.
      CLEAR it_bsid.
      MOVE-CORRESPONDING it_bsik TO it_bsid.
      it_bsid-kunnr = it_bsik-lifnr .
      APPEND it_bsid .
    ENDLOOP.
  ENDIF.

*  SORT it_bsid BY budat belnr .
*  SORT it_bsid BY waers budat ASCENDING.
  SORT it_bsid BY  budat ASCENDING.
  LOOP AT it_bsid .
    ON CHANGE OF it_bsid-waers.
      CLEAR lv_control.
    ENDON.
    IF it_bsid-waers = 'TRY' AND
       lv_control IS INITIAL .
      it_report-bakiye_bpb = lv_wrbtr_try.
      lv_control = 'X'.
    ENDIF.
    IF it_bsid-waers = 'EUR' AND
       lv_control IS INITIAL .
      it_report-bakiye_bpb = lv_wrbtr_eur.
      lv_control = 'X'.
    ENDIF.
    IF it_bsid-waers = 'USD' AND
       lv_control IS INITIAL .
      it_report-bakiye_bpb = lv_wrbtr_usd.
      lv_control = 'X'.
    ENDIF.
    MOVE-CORRESPONDING it_bsid TO it_report.

*    IF it_bsid-shkzg = 'H'.
*      MULTIPLY it_bsid_devir-dmbtr BY -1 .
*      MULTIPLY it_bsid_devir-dwrbtr BY -1 .
*    ENDIF.

    SELECT SINGLE ltext FROM t003t
    INTO it_report-ltext
    WHERE blart EQ it_report-blart
      AND spras EQ sy-langu.

*    CLEAR lv_vadetarih.
    it_report-vade_t = + it_bsid-zfbdt
                       + it_bsid-zbd1t
                       + it_bsid-zbd2t
                       + it_bsid-zbd3t .

    IF  it_report-vade_t IS INITIAL.
      it_report-vade_t = it_report-budat.
    ENDIF.

    CLEAR: it_report-stblg, it_report-xblnr.

    IF p_ters IS NOT INITIAL .

      SELECT SINGLE xblnr stblg FROM bkpf
        INTO (it_report-xblnr,it_report-stblg)
        WHERE bukrs EQ p_bukrs AND
              belnr EQ it_bsid-belnr AND
              gjahr EQ it_bsid-gjahr.


      IF  it_report-stblg IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ENDIF.

    CLEAR: it_report-alacak, it_report-borc,
           it_report-alacak_pb , it_report-borc_bp.

    CASE it_bsid-shkzg.
      WHEN 'S'.

        it_report-borc = it_bsid-dmbtr.
        it_report-borc_bp = it_bsid-wrbtr.
        it_report-devir = it_report-devir + it_report-borc.
        it_report-bakiye_bpb = it_report-bakiye_bpb
                        + it_report-borc_bp.

      WHEN 'H'.

        it_report-alacak = it_bsid-dmbtr.
        it_report-alacak_pb = it_bsid-wrbtr.
        it_report-devir = it_report-devir -
                          it_report-alacak.
        it_report-bakiye_bpb = it_report-bakiye_bpb
                             - it_report-alacak_pb.
    ENDCASE.





    it_report-belg_pb = it_bsid-waers .

*    SELECT SINGLE hkont FROM bseg INTO it_report-hkont
*      WHERE bukrs EQ p_bukrs AND
*            belnr EQ it_report-belnr AND
*            buzei EQ it_report-buzei AND
*            gjahr EQ lineitems-fisc_year.

    IF it_report-hkont IS NOT INITIAL.
      READ TABLE ht_skat WITH KEY saknr = it_report-hkont.

      CHECK sy-subrc IS INITIAL.

      it_report-txt50 = ht_skat-txt50.

    ENDIF.

*    READ TABLE lt_collect WITH KEY lifnr = it_report-kunnr.
*    IF sy-subrc IS INITIAL.
*      lv_dmbtr = lt_collect-dmbtr.
*    ENDIF.

    READ TABLE lt_bezei WITH KEY kunnr = it_report-kunnr.
    IF  sy-subrc IS INITIAL.
      it_report-bezei = lt_bezei-bezei.
      it_report-name1 = lt_bezei-name1.

    ENDIF.
    IF it_report-name1 IS INITIAL.
      SELECT SINGLE name1 FROM kna1 INTO it_report-name1
        WHERE kunnr EQ p_kunnr .
    ENDIF.

    "Sipariş No
    APPEND it_report.
    CLEAR : it_report-name1 , it_report-bezei ,it_report-name1.

  ENDLOOP.
  " SORT it_report BY budat belnr .
  IF p_ters IS NOT INITIAL.
    CLEAR lt_bkpf[].
    SELECT * FROM bkpf
      INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
      FOR ALL ENTRIES IN it_report
      WHERE belnr     = it_report-belnr
        AND budat     = it_report-budat.



    SELECT belnr ,gjahr, kidno FROM bseg
      INTO TABLE @DATA(lt_fatura)
      FOR ALL ENTRIES IN @it_report
      WHERE bukrs EQ '1000'
        AND belnr EQ @it_report-belnr
        AND gjahr EQ @it_report-budat(4)
        AND buzei EQ 1.

    IF lt_fatura[] IS NOT INITIAL.

      SELECT vbeln, vbelv FROM vbfa
      INTO TABLE @DATA(lt_siparis)
      FOR ALL ENTRIES IN @lt_fatura
      WHERE vbfa~vbeln EQ @lt_fatura-kidno(10)
        AND vbfa~vbtyp_n EQ 'M'
        AND vbfa~vbtyp_v EQ 'C'.

    ENDIF.


    LOOP AT it_report INTO gs_report.
      CLEAR lv_count.
      READ TABLE lt_fatura INTO DATA(ls_fatura) WITH KEY belnr =
      gs_report-belnr
                                                         gjahr =
gs_report-budat(4).
      IF sy-subrc IS INITIAL.
        gs_report-kidno = ls_fatura-kidno.
      ENDIF.

      LOOP AT lt_siparis INTO DATA(ls_siparis) WHERE vbeln EQ
      gs_report-kidno.
        lv_count = lv_count + 1.

      ENDLOOP.

      CLEAR gs_report-vbelv.
      IF lv_count EQ 1.
        READ TABLE lt_siparis INTO ls_siparis WITH KEY vbeln =
        gs_report-kidno.
        gs_report-vbelv = ls_siparis-vbelv.

      ELSEIF lv_count GT 1.
        gs_report-vbelv = 'GÖSTER'.
      ENDIF.

      MODIFY it_report FROM gs_report.

      CLEAR lt_bkpf.
      READ TABLE lt_bkpf WITH KEY belnr =  gs_report-belnr.
      IF  sy-subrc IS INITIAL
      AND lt_bkpf-awref_rev IS NOT INITIAL.
        DELETE it_report.
      ENDIF.
      CLEAR gs_report.
    ENDLOOP.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DATA: alacak_dmbtr TYPE dmbtr,
        borc_dmbtr   TYPE dmbtr.

  LOOP AT it_report REFERENCE INTO DATA(devir_duzelt)
                                  WHERE rowcolor IS NOT INITIAL
                                    AND belg_pb  IS NOT INITIAL.


    LOOP AT it_bsik_devir REFERENCE INTO DATA(bsik_devir)
                          WHERE waers EQ devir_duzelt->belg_pb.
      CASE bsik_devir->shkzg.
        WHEN 'H'.
          alacak_dmbtr = alacak_dmbtr + bsik_devir->dmbtr.
        WHEN 'S'.
          borc_dmbtr = borc_dmbtr + bsik_devir->dmbtr.
      ENDCASE.
    ENDLOOP.


    LOOP AT it_bsid_devir REFERENCE INTO DATA(bsid_devir)
                          WHERE waers EQ devir_duzelt->belg_pb.
      CASE bsid_devir->shkzg.
        WHEN 'H'.
          alacak_dmbtr = alacak_dmbtr + bsid_devir->dmbtr.
        WHEN 'S'.
          borc_dmbtr = borc_dmbtr + bsid_devir->dmbtr.
      ENDCASE.
    ENDLOOP.

    IF alacak_dmbtr IS NOT INITIAL.
      devir_duzelt->alacak = alacak_dmbtr.
*           MULTIPLY devir_duzelt->alacak BY -1 .
    ENDIF.

    IF borc_dmbtr IS NOT INITIAL.
      devir_duzelt->borc = borc_dmbtr.
    ENDIF.

*      devir_duzelt->devir = devir_duzelt->borc + devir_duzelt->alacak.
    devir_duzelt->devir = devir_duzelt->borc - devir_duzelt->alacak.

    CLEAR: alacak_dmbtr,borc_dmbtr.
  ENDLOOP.

  it_report_duzen[] = it_report[].
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
ENDFORM.
*&---------------------------------------------------------------------*
*& Form YAZDIR
*&---------------------------------------------------------------------*
FORM yazdir .
  DATA: lfd_name          TYPE rs38l_fnam,
        lfd_form          TYPE tdsfname,
        ls_control_param  TYPE ssfctrlop,
        ls_composer_param TYPE ssfcompop,
        ls_job_info       TYPE ssfcrescl,
        lfd_name1         TYPE name1_gp,
        lfd_waers         TYPE waers,
        lfd_donem(25),
        lfd_low(10),
        lfd_high(10).

  DATA: lfd_borc_eur          TYPE wrbtr,
        lfd_borc_usd          TYPE wrbtr,
        lfd_borc_try          TYPE dmbtr,
        lfd_alacak_eur        TYPE wrbtr,
        lfd_alacak_usd        TYPE wrbtr,
        lfd_alacak_try        TYPE dmbtr,
        lfd_bakiye_eur        TYPE wrbtr,
        lfd_bakiye_usd        TYPE wrbtr,
        lfd_bakiye_try        TYPE dmbtr,
        toplam_borc_try       TYPE dmbtr,
        toplam_alacak_try     TYPE dmbtr,
        toplam_bakiye_try     TYPE dmbtr,
        toplam_bakiye_eur_try TYPE dmbtr,
        toplam_bakiye_usd_try TYPE dmbtr.

  DATA: lt_eur LIKE TABLE OF it_report WITH HEADER LINE,
        lt_usd LIKE TABLE OF it_report WITH HEADER LINE,
        lt_try LIKE TABLE OF it_report WITH HEADER LINE.

  DATA: it_print       TYPE TABLE OF yfin_fi010_s_001 WITH HEADER
  LINE,
        it_report_temp LIKE TABLE OF it_report WITH HEADER LINE.

  DATA: lv_devir_up TYPE dmbtr,
        lv_devir_bp TYPE wrbtr.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  IF it_report_duzen[] IS NOT INITIAL.
    it_report[] = it_report_duzen[].
  ENDIF.

  IF p_doviz EQ 'X'.
    it_report_temp[] = it_report2[].
  ELSE.
    it_report_temp[] = it_report[].
  ENDIF.

  LOOP AT it_report_temp REFERENCE INTO DATA(lrd_report_temp).
*                                       WHERE rowcolor IS INITIAL.
*    CLEAR: lrd_report_temp->devir,lrd_report_temp->bakiye_bpb.
*
*    IF lv_devir_up IS INITIAL.
*      lv_devir_up = lrd_report_temp->devir      =
*      lrd_report_temp->borc - lrd_report_temp->alacak.
*    ELSE.
*      lv_devir_up = lrd_report_temp->devir      =
*      lv_devir_up + lrd_report_temp->borc - lrd_report_temp->alacak.
*    ENDIF.
*
*    IF lv_devir_bp IS INITIAL.
*       lv_devir_bp = lrd_report_temp->bakiye_bpb =
*       lrd_report_temp->borc_bp - lrd_report_temp->alacak_pb.
*    ELSE.
*       lv_devir_bp = lrd_report_temp->bakiye_bpb =
*       lv_devir_bp + lrd_report_temp->borc_bp -
*                     lrd_report_temp->alacak_pb.
*    ENDIF.

    IF p_doviz EQ 'X'.
      lrd_report_temp->belg_pb = 'TRY'.
      lrd_report_temp->alacak_pb  = lrd_report_temp->alacak.
      lrd_report_temp->borc_bp    = lrd_report_temp->borc.
    ENDIF.

    MULTIPLY lrd_report_temp->alacak BY -1 .
    IF lrd_report_temp->alacak_pb > 0.
      MULTIPLY lrd_report_temp->alacak_pb BY -1 .
    ENDIF.

  ENDLOOP.

*  LOOP AT it_report REFERENCE INTO DATA(lrd_report)
  LOOP AT it_report_temp REFERENCE INTO DATA(lrd_report)
                                   WHERE belg_pb IS NOT INITIAL.
*                                  WHERE rowcolor IS INITIAL.

    APPEND INITIAL LINE TO it_print REFERENCE INTO DATA(lrd_print).

    MOVE-CORRESPONDING lrd_report->* TO lrd_print->*.

    IF lrd_report->rowcolor IS NOT INITIAL.
      lrd_print->ltext = 'DEVİR BAKİYESİ'.
      lrd_print->bldat = s_budat-low.
*        lrd_print->bldat = s_budat-high.
    ENDIF.


    CASE lrd_report->belg_pb.
      WHEN 'EUR'.
        APPEND INITIAL LINE TO lt_eur REFERENCE INTO DATA(lrd_eur).
        MOVE-CORRESPONDING lrd_report->* TO lrd_eur->*.
        IF lrd_report->sgtxt = 'DEVİR BAKİYESİ'.
          lrd_eur->alacak_pb = lrd_eur->alacak_pb * -1.
        ENDIF.
        lfd_borc_eur   = lfd_borc_eur   + lrd_eur->borc_bp.
        lfd_alacak_eur = lfd_alacak_eur + lrd_eur->alacak_pb.
      WHEN 'USD'.
        APPEND INITIAL LINE TO lt_usd REFERENCE INTO DATA(lrd_usd).
        MOVE-CORRESPONDING lrd_report->* TO lrd_usd->*.
        IF lrd_report->sgtxt = 'DEVİR BAKİYESİ'.
          lrd_usd->alacak_pb = lrd_usd->alacak_pb * -1.
        ENDIF.
        lfd_borc_usd   = lfd_borc_usd   + lrd_usd->borc_bp.
        lfd_alacak_usd = lfd_alacak_usd + lrd_usd->alacak_pb.
      WHEN 'TRY'.
        APPEND INITIAL LINE TO lt_try REFERENCE INTO DATA(lrd_try).
        MOVE-CORRESPONDING lrd_report->* TO lrd_try->*.
        lfd_borc_try   = lfd_borc_try   + lrd_try->borc_bp.
        lfd_alacak_try = lfd_alacak_try + lrd_try->alacak_pb.
      WHEN OTHERS.
    ENDCASE.

    toplam_borc_try   = toplam_borc_try   +  lrd_report->borc.
    toplam_alacak_try = toplam_alacak_try +  lrd_report->alacak.

  ENDLOOP.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DESCRIBE TABLE lt_eur LINES DATA(sayi_eur).

  IF sayi_eur IS NOT INITIAL.
    READ TABLE lt_eur REFERENCE INTO DATA(temp_eur) INDEX sayi_eur.
    IF sy-subrc IS INITIAL.
*        lfd_bakiye_eur        = temp_eur->bakiye_bpb.
      toplam_bakiye_eur_try = temp_eur->devir.
    ENDIF.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DESCRIBE TABLE lt_usd LINES DATA(sayi_usd).

  IF sayi_usd IS NOT INITIAL.
    READ TABLE lt_usd REFERENCE INTO DATA(temp_usd) INDEX sayi_usd.
    IF sy-subrc IS INITIAL.
*        lfd_bakiye_usd        = temp_usd->bakiye_bpb.
      toplam_bakiye_usd_try = temp_usd->devir.
    ENDIF.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DESCRIBE TABLE lt_try LINES DATA(sayi_try).

  IF sayi_try IS NOT INITIAL.
    READ TABLE lt_try REFERENCE INTO DATA(temp_try) INDEX sayi_try.
    IF sy-subrc IS INITIAL.
*        lfd_bakiye_try    = temp_try->bakiye_bpb.
*        toplam_bakiye_try = temp_try->devir.
    ENDIF.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  lfd_bakiye_eur    = lfd_borc_eur + lfd_alacak_eur.
*    lfd_bakiye_eur    = lfd_borc_eur - lfd_alacak_eur.
  lfd_bakiye_usd    = lfd_borc_usd + lfd_alacak_usd.
*    lfd_bakiye_usd    = lfd_borc_usd - lfd_alacak_usd.
  lfd_bakiye_try    = lfd_borc_try + lfd_alacak_try.
*    lfd_bakiye_try    = lfd_borc_try - lfd_alacak_try.
  toplam_bakiye_try = toplam_borc_try + toplam_alacak_try.
*    toplam_bakiye_try = toplam_borc_try - toplam_alacak_try.

  SELECT SINGLE name1 FROM kna1
    INTO lfd_name1
    WHERE kunnr EQ p_kunnr.

  READ TABLE s_budat REFERENCE INTO DATA(lrd_budat) INDEX 1.

  IF sy-subrc IS INITIAL.

    CONCATENATE lrd_budat->low+6(2)
                lrd_budat->low+4(2)
                lrd_budat->low(4) INTO lfd_low SEPARATED BY '.'.

    CONCATENATE lrd_budat->high+6(2)
                lrd_budat->high+4(2)
                lrd_budat->high(4) INTO lfd_high SEPARATED BY '.'.

    CONCATENATE lfd_low lfd_high
           INTO lfd_donem SEPARATED BY  '-'.

  ENDIF.

  READ TABLE s_waers REFERENCE INTO DATA(lrd_waers) INDEX 1.

  IF sy-subrc IS INITIAL.
    lfd_waers = lrd_waers->low.
  ENDIF.

  IF s_waers-low IS INITIAL.
    lfd_form = 'ZFI_SF_MUSTERI_EKSTRE_ALL'.
  ELSE.
    lfd_form = 'ZFI_SF_MUSTERI_EKSTRE_DIKEY'.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lfd_form
    IMPORTING
      fm_name            = lfd_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  CALL FUNCTION lfd_name
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_composer_param
      user_settings      = space
      name1              = lfd_name1
      kunnr              = p_kunnr
      waers              = lfd_waers
      donem              = lfd_donem
      borc_eur           = lfd_borc_eur
      borc_usd           = lfd_borc_usd
      borc_try           = lfd_borc_try
      alacak_eur         = lfd_alacak_eur
      alacak_usd         = lfd_alacak_usd
      alacak_try         = lfd_alacak_try
      bakiye_eur         = lfd_bakiye_eur
      bakiye_usd         = lfd_bakiye_usd
      bakiye_try         = lfd_bakiye_try
      top_borc_try       = toplam_borc_try
      top_alacak_try     = toplam_alacak_try
      top_bakiye_try     = toplam_bakiye_try
      top_bakiye_eur_try = toplam_bakiye_eur_try
      top_bakiye_usd_try = toplam_bakiye_usd_try
    IMPORTING
      job_output_info    = ls_job_info
    TABLES
      t_print            = it_print
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.
