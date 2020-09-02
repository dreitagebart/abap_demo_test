*&---------------------------------------------------------------------*
*& Report zdemo_oop
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdemo_oop.

CLASS lcl_app DEFINITION DEFERRED. "put the class definition part at the beginning of code / make it present

DATA: gv_matnr TYPE matnr,
      gv_ekgrp TYPE ekgrp,
      go_app   TYPE REF TO lcl_app,
      ok_code  TYPE syucomm.

CLASS lcl_app DEFINITION CREATE PRIVATE. "means you cannot call the constructor.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_app) TYPE REF TO lcl_app.

    METHODS:
      at_selection_screen,
      at_selection_screen_output,
      constructor,
      pai,
      pbo,
      start_of_selection.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ts_output,
             material         TYPE matnr,
             description      TYPE maktx,
             purchasing_group TYPE ekgrp,
           END OF ts_output.

    CLASS-DATA: mo_app TYPE REF TO lcl_app.

    DATA: mo_output_cc TYPE REF TO cl_gui_custom_container,
          mo_output    TYPE REF TO cl_salv_table,
          mt_output    TYPE TABLE OF ts_output.

    METHODS:
      on_output_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,
      pai_2000,
      pbo_2000.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE gv_sel.
SELECT-OPTIONS: s_matnr FOR gv_matnr,
                s_ekgrp FOR gv_ekgrp.
SELECTION-SCREEN SKIP.
PARAMETERS: p_max TYPE int4 DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK sel.

INITIALIZATION.
  gv_sel = 'Selektion'. "don't do this in production
  go_app = lcl_app=>get_instance( ).
*  go_app = NEW lcl_app( ).

AT SELECTION-SCREEN OUTPUT.
  "pbo
  go_app->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  "pai
  go_app->at_selection_screen( ).

START-OF-SELECTION.
  go_app->start_of_selection( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  go_app->pbo( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  go_app->pai( ).
ENDMODULE.

CLASS lcl_app IMPLEMENTATION.
  METHOD get_instance.
    IF mo_app IS NOT BOUND.
*      mo_app = NEW #( ).
      mo_app = NEW lcl_app( ).
*       CREATE OBJECT lcl_app.
    ENDIF.

    ro_app = mo_app.
  ENDMETHOD.

  METHOD start_of_selection.
    SELECT matnr FROM mara INTO TABLE @DATA(lt_mara) UP TO @p_max ROWS WHERE matnr IN @s_matnr.

    IF lt_mara IS NOT INITIAL.
      SELECT matnr, maktx FROM makt INTO TABLE @DATA(lt_makt) FOR ALL ENTRIES IN @lt_mara "if table is empty, it selects ALL material
        WHERE matnr = @lt_mara-matnr
          AND spras = @sy-langu.
    ENDIF.

    LOOP AT lt_mara REFERENCE INTO DATA(lr_mara).
      APPEND INITIAL LINE TO mt_output REFERENCE INTO DATA(lr_output).
      lr_output->material = lr_mara->matnr.

      TRY.
          DATA(lr_makt) = REF #( lt_makt[ matnr = lr_mara->matnr ] ).

          lr_output->description = lr_makt->maktx.
        CATCH cx_sy_itab_line_not_found INTO DATA(lx_error).
          MESSAGE lx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDLOOP.

    CALL SCREEN 2000.
  ENDMETHOD.

  METHOD constructor.

  ENDMETHOD.

  METHOD at_selection_screen.

  ENDMETHOD.

  METHOD at_selection_screen_output.

  ENDMETHOD.

  METHOD pai.
    CASE sy-dynnr.
      WHEN 1000.
        "do nothing
        "will be handled in at_selection_screen method
      WHEN 2000.
        pai_2000( ).
      WHEN OTHERS.
        "do nothing
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    CASE sy-dynnr.
      WHEN 1000.
        "do nothing
        "will be handled in at_selection_screen_output method
      WHEN 2000.
        pbo_2000( ).
    ENDCASE.
  ENDMETHOD.

  METHOD pbo_2000.
    SET PF-STATUS '2000'.
    SET TITLEBAR '2000'.

    CHECK mo_output_cc IS NOT BOUND.

    mo_output_cc = NEW #(
      container_name = 'CC_OUTPUT'
    ).

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false " ALV wird im Listenmodus angezeigt
            r_container    = mo_output_cc                          " Abstracter Container fuer GUI Controls
            container_name = 'CC_OUTPUT'
          IMPORTING
            r_salv_table   = mo_output                          " Basisklasse einfache ALV Tabellen
          CHANGING
            t_table        = mt_output
        ).

*        DATA(lo_columns) = mo_output->get_columns( ).
*
*        DATA lo_column TYPE REF TO cl_salv_column_table.
*
*        TRY.
*            lo_column ?= lo_columns->get_column( 'MATERIAL' ).
*
*            lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*          CATCH cx_salv_not_found. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
*        ENDTRY.

        LOOP AT mo_output->get_columns( )->get( ) REFERENCE INTO DATA(lr_column).
          DATA(lo_column) = CAST cl_salv_column_table( lr_column->r_column ).

          CASE lr_column->columnname.
            WHEN 'MATERIAL'.
              lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
            WHEN OTHERS.
              "do nothing
          ENDCASE.
        ENDLOOP.

        mo_output->get_functions( )->set_all( ).

        mo_output->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        SET HANDLER on_output_click FOR mo_output->get_event( ).

        mo_output->display( ).
      CATCH cx_salv_msg. " ALV: Allg. Fehlerklasse  mit Meldung
    ENDTRY.
  ENDMETHOD.

  METHOD on_output_click.
    BREAK buechst.
    READ TABLE mt_output INTO DATA(ls_row) INDEX row.
    IF sy-subrc = 0.
      SET PARAMETER ID 'MAT' FIELD ls_row-material.
      SET PARAMETER ID 'MXX' FIELD 'K'.
      TRY.
          CALL TRANSACTION 'MM03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
        CATCH cx_sy_authorization_error INTO DATA(lx_error).
          MESSAGE lx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD pai_2000.
    DATA(lv_okcode) = ok_code.
    CLEAR ok_code.

    CASE lv_okcode.
      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        LEAVE TO SCREEN 0.

*        SET SCREEN 0.
*        LEAVE SCREEN.
      WHEN OTHERS.
        "do nothing
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
