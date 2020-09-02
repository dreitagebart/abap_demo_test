*&---------------------------------------------------------------------*
*& Report zdemo_sql_performance
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_sql_performance MESSAGE-ID /witt/sd_msg_class.

*TABLES: mara.
DATA: gs_mara TYPE mara.

TYPES: BEGIN OF ts_output,
         material         TYPE matnr,
         description      TYPE maktx,
         material_type    TYPE mtart,
         material_group   TYPE matkl,
         purchasing_group TYPE ekgrp,
       END OF ts_output.

DATA: gt_output TYPE TABLE OF ts_output,
      go_grid   TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE gv_sel.
SELECT-OPTIONS: s_matnr FOR gs_mara-matnr,
                s_mtart FOR gs_mara-mtart,
                s_matkl FOR gs_mara-matkl.
SELECTION-SCREEN END OF BLOCK sel.

INITIALIZATION.
*  MESSAGE s000 INTO gv_sel.
  gv_sel = 'Selektion'. "don't do this in production!

START-OF-SELECTION.
  BREAK buechst.
  SELECT matnr, matkl, mtart FROM mara UP TO 100 ROWS INTO TABLE @DATA(lt_mara) WHERE matnr IN @s_matnr
                                                                                  AND matkl IN @s_matkl
                                                                                  AND mtart IN @s_mtart.
  BREAK buechst.
*  LOOP AT lt_mara INTO DATA(ls_mara).
*    ls_mara-matkl = 'Test'.
*
*    MODIFY lt_mara FROM ls_mara.
*  ENDLOOP.

  SELECT matnr, maktx FROM makt INTO TABLE @DATA(lt_makt)
    FOR ALL ENTRIES IN @lt_mara WHERE matnr = @lt_mara-matnr
                                  AND spras = 'D'.


  SELECT matnr, ekgrp FROM marc INTO TABLE @DATA(lt_marc)
    FOR ALL ENTRIES IN @lt_mara WHERE matnr = @lt_mara-matnr
                                  AND werks = '0100'.

*  DATA: lv_bool TYPE abap_bool.
*
*  lv_bool = abap_true.

*  DATA(lv_bool) = abap_true.

  LOOP AT lt_mara REFERENCE INTO DATA(lr_mara).
*    TRY.
*        DATA(ls_mara) = lt_marc[ matnr = lr_mara->matnr ].
*      CATCH cx_sy_itab_line_not_found.
*    ENDTRY.

*    READ TABLE lt_marc REFERENCE INTO DATA(lr_marc) WITH KEY matnr = lr_mara->matnr.

*    SELECT SINGLE ekgrp FROM marc INTO @DATA(lv_purchasing_group) WHERE matnr = @lr_mara->matnr
*                                                                    AND werks = '0100'.

    APPEND INITIAL LINE TO gt_output REFERENCE INTO DATA(lr_output).
    lr_output->material         = lr_mara->matnr.
    lr_output->material_type    = lr_mara->mtart.
    lr_output->material_group   = lr_mara->matkl.

    TRY.
        DATA(lr_marc) = REF #( lt_marc[ matnr = lr_mara->matnr ] ).

        lr_output->purchasing_group = lr_marc->ekgrp.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        DATA(lr_makt) = REF #( lt_makt[ matnr = lr_mara->matnr ] ).

        lr_output->description = lr_makt->maktx.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.


*    lr_output->material_group = 'Test'.
  ENDLOOP.
  "--- old

  TRY.
      cl_salv_table=>factory(
*       EXPORTING
*         list_display   = if_salv_c_bool_sap=>false " ALV wird im Listenmodus angezeigt
*         r_container    =                           " Abstracter Container fuer GUI Controls
*          container_name =
        IMPORTING
          r_salv_table   = go_grid                          " Basisklasse einfache ALV Tabellen
        CHANGING
          t_table        = gt_output
      ).

      go_grid->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      go_grid->display( ).
    CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung
  ENDTRY.
*  TYPES: BEGIN OF ts_mara,
*           material       TYPE matnr,
*           material_type  TYPE mtart,
*           material_group TYPE matkl,
*         END OF ts_mara.
*
*  DATA: lt_mara TYPE TABLE OF ts_mara.
*
*  SELECT matnr matkl mtart FROM mara INTO TABLE lt_mara WHERE matnr IN s_matnr
*                                                          AND matkl IN s_matkl
*                                                          AND mtart IN s_mtart.
